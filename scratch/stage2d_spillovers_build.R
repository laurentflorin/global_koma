# Build the stage-2d spillover artefacts.
#
#   OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 Rscript scratch/stage2d_spillovers_build.R [step ...]
#
# Every step caches to data/cache/stage2d_spillovers/ and is skipped if
# present, so this can be run in pieces -- which it has to be, because the
# whole battery is roughly forty minutes of `koma::forecast()` calls and this
# machine cannot afford to hold two systems at once. Steps:
#
#   base      the extended fit and its shared baseline forecast (both seeds)
#   demand    a +1pp GDP demand shock at horizon 1, one per entity, seed A
#   monetary  +100bp on ea_policy_rate for four quarters, seeds A and B
#   oil       a +50% oil price level shock, seed A
#   placebo   the demand battery again at seed B, for the seed placebo
#
# The seed placebo is not optional. `CLAUDE.md` records that re-running the
# same battery on the SAME fit with a different seed moves an off-diagonal
# spillover cell by 0.35pp on average for stage 2b, which is larger than most
# of the differences anyone wants to report. A stage-2d number that does not
# survive a second seed is noise, and this script produces both so the report
# can say which is which.

devtools::load_all(".", quiet = TRUE)

out_dir <- file.path("data", "cache", "stage2d_spillovers")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

HORIZON <- 8
SEED_A <- 20240101
SEED_B <- 20240102
COUNTRIES <- stage2d_countries()
# Entities re-run at the second seed. All six would double the runtime for no
# extra information: four is enough to compare a cross-system difference
# against seed-to-seed noise, and it includes both constructed entities.
PLACEBO_SRC <- c("de", "us", "cn", "reu")

path_of <- function(name) file.path(out_dir, paste0(name, ".rds"))
cached <- function(name, expr) {
  p <- path_of(name)
  if (file.exists(p)) {
    cli::cli_inform("cached: {name}")
    return(readRDS(p))
  }
  value <- force(expr)
  saveRDS(value, p)
  cli::cli_inform("built: {name}")
  value
}

load_system <- function() {
  tuned <- readRDS(file.path("data", "cache", "stage2d", "tuned.rds"))
  panel <- readRDS(file.path("data", "cache", "stage2d", "panel.rds"))
  # koma silently shortens a horizon whose exogenous series run out, so the
  # extension has to happen before anything else -- see extend_forecast_horizon().
  ext <- extend_forecast_horizon(tuned$fit, panel, HORIZON)
  rm(tuned)
  gc(full = TRUE)
  ext
}

baseline_for <- function(ext, seed) {
  set.seed(seed)
  fc <- suppressWarnings(koma::forecast(
    ext$fit, dates = ext$dates,
    options = list(approximate = FALSE, probs = c(0.05, 0.95))
  ))
  attr(fc, "scenario_diff_seed") <- seed
  fc
}

args <- commandArgs(trailingOnly = TRUE)
steps <- if (length(args) > 0) args else c("base", "demand", "monetary", "oil", "placebo")

ext <- load_system()
cached("extension_summary", ext$extension)

# The baselines are the expensive shared input: scenario_diff() pairs each
# scenario against a baseline drawn under the SAME seed (common random
# numbers), so computing them once and passing them in halves the work.
base_a <- cached("baseline_a", baseline_for(ext, SEED_A))
if (any(c("monetary", "placebo") %in% steps)) {
  base_b <- cached("baseline_b", baseline_for(ext, SEED_B))
}

if ("base" %in% steps) {
  cached("baseline_explosive", {
    vars <- country_var(COUNTRIES, "gdp")
    do.call(rbind, lapply(vars, function(v) {
      draws <- vapply(base_a$forecasts, function(d) d[, v], numeric(HORIZON))
      data.frame(variable = v, horizon = seq_len(HORIZON),
                 explosive_frac = rowMeans(abs(draws) > 100 | !is.finite(draws)),
                 stringsAsFactors = FALSE)
    }))
  })
}

if ("demand" %in% steps) {
  for (cc in COUNTRIES) {
    cached(paste0("demand_", cc, "_a"), scenario_diff(
      ext$fit, restrictions = gdp_demand_shock(base_a, cc, size = 1),
      horizon = HORIZON, seed = SEED_A, baseline_forecast = base_a
    ))
  }
}

if ("placebo" %in% steps) {
  for (cc in PLACEBO_SRC) {
    cached(paste0("demand_", cc, "_b"), scenario_diff(
      ext$fit, restrictions = gdp_demand_shock(base_b, cc, size = 1),
      horizon = HORIZON, seed = SEED_B, baseline_forecast = base_b
    ))
  }
}

# A SUSTAINED restriction (+100bp held for four quarters) makes some posterior
# draws fail outright, and koma drops a failed draw by subsetting the list --
# recording nothing about which index it was. scenario_diff() therefore refuses
# to difference legs of unequal length rather than mispair them, which is
# correct and which means the failing indices have to be recovered first.
# failed_restriction_draws() re-derives them by tracing koma:::forecast_draw().
# Measured here: 25 of 1000 draws fail the euro-area shock.
monetary_diff <- function(name, base, rate, seed) {
  restrictions <- policy_rate_shock(base, rate, size = 1, quarters = 4)
  failed <- cached(paste0(name, "_failed_draws"),
                   failed_restriction_draws(ext$fit, restrictions, HORIZON, seed = seed))
  cached(name, scenario_diff(
    ext$fit, restrictions = restrictions, horizon = HORIZON, seed = seed,
    baseline_forecast = base, drop_baseline_draws = failed
  ))
}

if ("monetary" %in% steps) {
  monetary_diff("monetary_a", base_a, "ea_policy_rate", SEED_A)
  monetary_diff("monetary_b", base_b, "ea_policy_rate", SEED_B)
  # China is the point of this stage, so its own rule gets the same treatment.
  monetary_diff("monetary_cn_a", base_a, "cn_policy_rate", SEED_A)
}

if ("oil" %in% steps) {
  oil_fit <- oil_price_shock(ext$fit, ext$panel, ext$dates, size = 1.5)
  cached("oil_a", scenario_diff(
    ext$fit, restrictions = NULL, horizon = HORIZON, scenario_fit = oil_fit,
    seed = SEED_A, baseline_forecast = base_a
  ))
  rm(oil_fit)
  gc(full = TRUE)
}

# --- matrices and sanity checks, once the diffs exist --------------------
have_all_a <- all(file.exists(vapply(COUNTRIES, function(cc) path_of(paste0("demand_", cc, "_a")), character(1))))
if (have_all_a) {
  diffs_a <- stats::setNames(lapply(COUNTRIES, function(cc) readRDS(path_of(paste0("demand_", cc, "_a")))), COUNTRIES)
  mat_a <- cached("matrix_a", spillover_matrix(diffs_a, COUNTRIES))
  if (file.exists(path_of("monetary_a"))) {
    lw <- readRDS(file.path("data", "cache", "stage2d", "linkage_weights.rds"))
    cached("sanity_a", spillover_sanity_checks(
      mat_a, readRDS(path_of("monetary_a")), COUNTRIES, lw
    ))
  }
}
have_all_b <- all(file.exists(vapply(PLACEBO_SRC, function(cc) path_of(paste0("demand_", cc, "_b")), character(1))))
if (have_all_b) {
  diffs_b <- stats::setNames(lapply(PLACEBO_SRC, function(cc) readRDS(path_of(paste0("demand_", cc, "_b")))), PLACEBO_SRC)
  # `countries` is the full entity set, not PLACEBO_SRC: only the placebo
  # sources have a seed-B row, but every entity is still a RECEIVER, and the
  # placebo compares cells against seed A's matrix cell for cell. Passing
  # PLACEBO_SRC here would silently drop the fr/it columns.
  cached("matrix_b", spillover_matrix(diffs_b, COUNTRIES))
}

cli::cli_inform("Stage-2d spillover artefacts in {.file {out_dir}}.")
