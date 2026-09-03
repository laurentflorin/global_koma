# Build the stage-3d artefacts: stage 2a's labour and disaggregated-price
# block on stage 2d's regional core.
#
#   OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 Rscript scratch/stage3d_build.R [step ...]
#
# Steps, each cached to data/cache/stage3d/ and skipped if present:
#
#   panel     fetch the labour concepts for DE/FR/IT and the seven bloc
#             members, aggregate the bloc, add the US and Chinese trade price
#             indices  (SLOW: ~20 Eurostat queries the first time)
#   system    weights, shares, spec, sys_eq, preflight, identity check
#   fit       estimate, then tau-tune
#   diag      coefficients, signs, acceptance, lag stability, reachability
#
# This is the configuration `reports/stage3c_rollout.qmd` concluded could not
# be afforded. That conclusion was right for the eleven-country partition and
# does not bind on the six-entity one -- see stage3d_config().

devtools::load_all(".", quiet = TRUE)

out_dir <- file.path("data", "cache", "stage3d")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
path_of <- function(n) file.path(out_dir, paste0(n, ".rds"))
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

LABOUR <- c("de", "reu")
MEMBERS <- reu_members()
dates <- stage2d_dates()
countries <- stage2d_countries()

args <- commandArgs(trailingOnly = TRUE)
steps <- if (length(args) > 0) args else c("panel", "system", "fit", "diag")

gdp_weights <- targets::tar_read(gdp_weights)
eamdqd <- targets::tar_read(eamdqd)
base_panel <- targets::tar_read(panel)

# --- panel ---------------------------------------------------------------
# Every entity that carries a block needs the full concept set; every other
# entity needs only `export_prices`, because a partner contributes nothing to
# the block except the export price the foreign-price index is built from.
# NOT `TRUE`: resolve_stage3a_concepts() expands that to the stage-3a AND
# stage-3b concept sets, and `house_prices` does not exist for Greece or
# Portugal in this vintage. The labour block needs only `stage3a_concepts`.
full_set <- stage3a_concepts
stage3a_spec <- c(
  stats::setNames(rep(list(full_set), length(MEMBERS)), MEMBERS),
  list(de = full_set, fr = full_set, it = full_set),
  list(us = "export_prices")
)

panel <- cached("panel", {
  cli::cli_inform("fetching labour concepts for {length(stage3a_spec)} countries (Eurostat; slow uncached)")
  wide <- build_global_panel(modelled_countries, eamdqd = eamdqd,
                             row_weights = targets::tar_read(row_weights),
                             stage3a = stage3a_spec)
  # Align to the ESTABLISHED window with extend = TRUE. Eurostat publishes the
  # non-energy HICP a quarter behind the rest, and letting align_panel() pick
  # bounds automatically would take the earliest end across everything and
  # silently shorten the forecast horizon -- the trap CLAUDE.md records.
  wide <- fill_internal_gaps(align_panel(
    wide,
    start = num_to_period(stats::tsp(base_panel[[1]])[1], 4),
    end = num_to_period(stats::tsp(base_panel[[1]])[2], 4),
    extend = TRUE
  ))
  p <- add_stage2d_countries(wide, gdp_weights, dates = dates,
                             members = MEMBERS, stage3a = TRUE)
  # China leaves row_gdp: it is modelled now, and leaving it in would load
  # Chinese demand into every partner's foreign_demand identity twice.
  p[["row_gdp"]] <- align_panel(
    list(row_gdp = build_row_gdp(row_gdp_weights(exclude = c("us", "cn")))),
    start = stats::start(p[[1]]), end = stats::end(p[[1]]), extend = TRUE
  )[[1]]
  harmonise_panel_attrs(p)
})

if (!any(c("system", "fit", "diag") %in% steps)) quit(save = "no")

# --- HICP weights --------------------------------------------------------
# A bloc has no published basket, so its split is its members' splits averaged
# with the same weights its series were built from.
hicp <- cached("hicp_weights", {
  member_w <- stats::setNames(
    lapply(MEMBERS, function(cc) hicp_weights(iso2_to_eamdqd[[cc]], dates)$weights), MEMBERS
  )
  list(
    de = hicp_weights(iso2_to_eamdqd[["de"]], dates)$weights,
    reu = bloc_hicp_weights(member_w, bloc_gdp_weights(gdp_weights, MEMBERS)),
    members = member_w
  )
})

# --- system --------------------------------------------------------------
cfg <- stage3d_config(gdp_weights, hicp_weights = hicp[LABOUR], labour_countries = LABOUR)
saveRDS(cfg$labour_countries, path_of("labour_countries"))

W <- cached("trade_weights", suppressWarnings(build_stage2d_trade_weights(gdp_weights)))
gw <- collapse_gdp_weights(gdp_weights, cfg$blocs)
lw <- cached("linkage_weights", stage2_linkage_weights(
  countries, W, gw, threshold = cfg$threshold, demand_concept = cfg$demand_concept))

spec <- cached("spec", stage2_spec(countries, stage2_shares(countries, panel, dates, cfg$opts),
                                   lw, opts = cfg$opts))
sys_eq <- cached("sys_eq", build_stage2_system(spec))

stage_panel <- cached("stage_panel", build_stage2_panel(
  panel, lw, dummies = cfg$dummies,
  labour_countries = cfg$labour_countries, hicp_weights = hicp[LABOUR],
  spread_countries = cfg$spread_countries,
  policy_rate = policy_rate_map(cfg$opts)))

cached("preflight", stage2_preflight(sys_eq, stage_panel, dates = dates))
cached("identity_check", identity_consistency(sys_eq, stage_panel))
cached("frontier", stage3d_frontier())

k <- length(sys_eq$total_exogenous_variables)
t_obs <- estimation_length(stage_panel, dates)
cached("shape", data.frame(
  labour_entities = length(LABOUR),
  economies_covered = 1L + length(MEMBERS),
  stochastic = length(sys_eq$stochastic_equations),
  identities = length(sys_eq$identities),
  equations = length(sys_eq$endogenous_variables),
  k = k, t_obs = t_obs, df = t_obs - k, stringsAsFactors = FALSE
))
cached("foreign_price_weights", stats::setNames(
  lapply(LABOUR, function(cc) foreign_price_weights(lw, cc)), LABOUR))

if (!any(c("fit", "diag") %in% steps)) quit(save = "no")

# --- fit -----------------------------------------------------------------
cached("core", fit_stage2(sys_eq, stage_panel, dates, workers = 6))
tuned <- cached("tuned", tune_tau_system(spec, stage_panel, dates, workers = 6))

if (!"diag" %in% steps) quit(save = "no")

# --- diagnostics ---------------------------------------------------------
coefs <- cached("coefs", coefficient_table(tuned$fit))
cached("acceptance", check_acceptance_rates(tuned$fit))
cached("lag_stability", check_lag_stability(coefs))
cached("insample", rmse_in_sample(tuned$fit))
cached("tuning_history", tuned$history)
cached("signs", do.call(rbind, lapply(countries, function(cc) {
  s <- sign_checks(coefs, cc, stage2c = cfg$refinements,
                   labour = cc %in% LABOUR,
                   merged_demand = cc %in% cfg$merged_demand_countries)
  s$iso2 <- cc
  s
})))
cached("reachability", stats::setNames(
  lapply(c("ea_policy_rate", "us_policy_rate", "cn_policy_rate"),
         function(v) contemporaneous_reachability(sys_eq, v)),
  c("ea_policy_rate", "us_policy_rate", "cn_policy_rate")))

cli::cli_inform("Stage-3d artefacts in {.file {out_dir}}.")
