# Build the eight-quarter forecast artefacts every stage report reads.
#
# Run from the project root, with BLAS pinned:
#
#   OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 Rscript scratch/forecasts_build.R
#
# One stage at a time, with the fit dropped and gc() run between each, because
# the stage-2c/3a/3b fits are 75-90 MB apiece and this machine does not have
# room to hold several at once. Each stage's artefact is a few tens of KB (the
# tidy `paths` frame plus diagnostics -- no koma objects), cached under
# data/cache/forecasts/ and skipped if present. Delete a file to force a
# re-run.
#
# Stage 3c is deliberately absent. It is a rollout *feasibility* study whose
# only fitted system is the stage-2b benchmark it compares against, so
# forecasting it would just be forecasting stage 2b under a second name.

devtools::load_all(".", quiet = TRUE)

out_dir <- file.path("data", "cache", "forecasts")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

HORIZON <- 8
SEED <- 20260101

# fit_path/panel_path are read one at a time; `pick` pulls the fit and panel
# out of whatever shape that stage's cache happens to have.
stages <- list(
  stage2a = list(
    label = "Stage 2a (DE+FR pilot)",
    file = file.path("data", "cache", "stage2a", "pilot_tuned.rds"),
    pick = function(o) list(fit = o$fit, panel = o$panel)
  ),
  stage2b = list(
    label = "Stage 2b (eleven countries)",
    file = file.path("data", "cache", "stage2b", "full_tuned.rds"),
    pick = function(o) list(fit = o$fit, panel = o$panel)
  ),
  stage2c = list(
    label = "Stage 2c (refined core)",
    file = file.path("data", "cache", "stage2c", "noic_full_tuned.rds"),
    pick = function(o) list(fit = o$fit, panel = o$panel)
  ),
  stage2d = list(
    label = "Stage 2d (regional core with China)",
    file = file.path("data", "cache", "stage2d", "tuned.rds"),
    pick = function(o) list(fit = o$fit, panel = readRDS(file.path("data", "cache", "stage2d", "panel.rds")))
  ),
  stage3a = list(
    label = "Stage 3a (German labour and price block)",
    file = file.path("data", "cache", "stage3a", "fit_a_final.rds"),
    pick = function(o) list(fit = o$fit, panel = o$panel)
  ),
  stage3b = list(
    label = "Stage 3b (external + fiscal + financial)",
    file = file.path("data", "cache", "stage3b", "fit_financial.rds"),
    pick = function(o) list(fit = o$fit, panel = o$panel)
  ),
  stage3d = list(
    label = "Stage 3d (labour block on the regional core)",
    file = file.path("data", "cache", "stage3d", "tuned.rds"),
    pick = function(o) list(fit = o$fit,
                            panel = readRDS(file.path("data", "cache", "stage3d", "stage_panel.rds")))
  )
)

run_stage <- function(name, spec) {
  path <- file.path(out_dir, paste0(name, ".rds"))
  if (file.exists(path)) {
    cli::cli_inform("cached: {name}")
    return(invisible(NULL))
  }
  if (!file.exists(spec$file)) {
    cli::cli_warn("{name}: {.file {spec$file}} not found -- skipped.")
    return(invisible(NULL))
  }
  cli::cli_inform("{name}: {spec$label}")

  gc(reset = TRUE, full = TRUE)
  started <- Sys.time()
  obj <- readRDS(spec$file)
  parts <- spec$pick(obj)
  rm(obj)
  gc(full = TRUE)

  fc <- stage_forecast(parts$fit, parts$panel, horizon = HORIZON, seed = SEED)
  fc$label <- spec$label
  fc$stage <- name
  fc$elapsed_s <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  fc$peak_mb <- round(sum(gc()[, "max used"] * c(8, 8) / 1024^2))

  saveRDS(fc, path)
  cli::cli_inform("  {HORIZON}q from {fc$origin[1]}Q{fc$origin[2]}, {fc$n_draws} draws, {round(fc$elapsed_s)}s, peak {fc$peak_mb} MB")
  rm(parts, fc)
  gc(full = TRUE)
  invisible(NULL)
}

# --- stage 1: eleven separate per-country fits ---------------------------
run_stage1 <- function() {
  path <- file.path(out_dir, "stage1.rds")
  if (file.exists(path)) {
    cli::cli_inform("cached: stage1")
    return(invisible(NULL))
  }
  panel <- readRDS(file.path("data", "cache", "stage1_diagnostics", "panel.rds"))
  started <- Sys.time()
  gc(reset = TRUE, full = TRUE)

  per_country <- lapply(modelled_countries, function(cc) {
    f <- file.path("data", "cache", "stage1", paste0(cc, ".rds"))
    if (!file.exists(f)) return(NULL)
    cli::cli_inform("  stage1: {cc}")
    fit <- readRDS(f)
    one <- stage_forecast(fit, panel, horizon = HORIZON, seed = SEED)
    rm(fit)
    gc(full = TRUE)
    one$paths$iso2 <- cc
    one$anker$iso2 <- cc
    one
  })
  per_country <- Filter(Negate(is.null), per_country)

  out <- list(
    stage = "stage1",
    label = "Stage 1 (per-country satellite models)",
    paths = do.call(rbind, lapply(per_country, function(x) x$paths)),
    anker = do.call(rbind, lapply(per_country, function(x) x$anker)),
    extension = per_country[[1]]$extension,
    origin = per_country[[1]]$origin,
    horizon = HORIZON,
    n_draws = per_country[[1]]$n_draws,
    n_actual = sum(vapply(per_country, function(x) x$n_actual, numeric(1))),
    elapsed_s = as.numeric(difftime(Sys.time(), started, units = "secs")),
    peak_mb = round(sum(gc()[, "max used"] * c(8, 8) / 1024^2))
  )
  saveRDS(out, path)
  cli::cli_inform("  stage1 done: {length(per_country)} countries, {round(out$elapsed_s)}s, peak {out$peak_mb} MB")
  invisible(NULL)
}

args <- commandArgs(trailingOnly = TRUE)
wanted <- if (length(args) > 0) args else c("stage1", names(stages))

if ("stage1" %in% wanted) run_stage1()
for (nm in intersect(wanted, names(stages))) run_stage(nm, stages[[nm]])

cli::cli_inform("Forecast artefacts in {.file {out_dir}}.")
