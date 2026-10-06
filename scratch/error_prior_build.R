# Does koma's degrees-of-freedom penalty come from the model or from koma?
#
# Re-estimates stage 2d and stage 2b with koma's OWN default error-term prior,
# `{n + 2, 0.001}`, written explicitly onto every stochastic equation (see
# default_error_priors()). The prior adds no information, but any prior moves
# an equation onto koma's informative sampler, which draws the residual
# covariance from riwish(T + df, .) instead of riwish(T - k, .) with the whole
# system's k. If the explosive-draw share falls a lot, part of the df cost
# this project has been reporting is a koma artefact rather than estimation
# uncertainty. No priors go on contemporaneous endogenous terms: koma (0.3.1 and
# 0.4.0) discards the likelihood in the Metropolis target when one is set.
#
# Run from the project root, with BLAS pinned:
#
#   OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 Rscript scratch/error_prior_build.R [stage2d] [stage2b]
#
# Inputs, per stage, in order of preference:
#
#   stage2d  data/cache/stage2d/{spec,panel}.rds from scratch/stage2d_build.R,
#            baseline fit from data/cache/stage2d/tuned.rds
#   stage2b  data/cache/stage2b/full_tuned.rds (fit + panel); the spec is
#            taken from it if present, else from the targets store
#
# A missing baseline fit is re-tuned here without priors, so the comparison is
# always like for like. The baseline forecast is read from
# data/cache/forecasts/<stage>.rds when present (same horizon and seed as
# scratch/forecasts_build.R) and recomputed otherwise. Everything lands under
# data/cache/error_priors/ and is skipped if present.

devtools::load_all(".", quiet = TRUE)

out_dir <- file.path("data", "cache", "error_priors")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

HORIZON <- 8
SEED <- 20260101 # scratch/forecasts_build.R's, so a cached baseline is reusable
WORKERS <- as.integer(Sys.getenv("WORKERS", "6"))

cached <- function(name, expr) {
  path <- file.path(out_dir, paste0(name, ".rds"))
  if (file.exists(path)) {
    cli::cli_inform("cached: {name}")
    return(readRDS(path))
  }
  started <- Sys.time()
  value <- force(expr)
  saveRDS(value, path)
  cli::cli_inform("{name}: {round(as.numeric(difftime(Sys.time(), started, units = 'mins')), 1)} min")
  value
}

read_if <- function(...) {
  path <- file.path(...)
  if (file.exists(path)) readRDS(path) else NULL
}

# --- inputs ---------------------------------------------------------------

stage_inputs <- list(
  stage2d = function() {
    dir <- file.path("data", "cache", "stage2d")
    spec <- read_if(dir, "spec.rds")
    panel <- read_if(dir, "panel.rds")
    if (is.null(spec) || is.null(panel)) {
      cli::cli_abort("Stage 2d inputs missing from {.file {dir}}; run {.file scratch/stage2d_build.R} first.")
    }
    tuned <- read_if(dir, "tuned.rds")
    list(spec = spec, panel = panel, dates = stage2d_dates(),
         baseline = if (!is.null(tuned)) tuned[c("fit", "tau", "converged")])
  },
  stage2b = function() {
    obj <- read_if("data", "cache", "stage2b", "full_tuned.rds")
    panel <- obj$panel %||% targets::tar_read(stage2b_panel)
    spec <- obj$spec %||% targets::tar_read(stage2b_spec)
    list(spec = spec, panel = panel, dates = stage2b_dates(),
         baseline = if (!is.null(obj)) list(fit = obj$fit, tau = obj$tau %||% obj$fit$tau))
  }
)

# --- comparison helpers ---------------------------------------------------

# The overview deck's own definition (reports/overview_presentation.qmd):
# per-horizon share in rate space, averaged over a concept's variables.
explosive_by_concept <- function(fc) {
  d <- fc$paths[fc$paths$kind == "forecast", ]
  d$concept <- sub("^[a-z]{2,3}_", "", d$variable)
  d$group <- ifelse(d$concept %in% c("gdp", "prices"), d$concept, "other")
  rbind(
    stats::aggregate(explosive_frac ~ group + horizon, data = d, FUN = mean),
    transform(stats::aggregate(explosive_frac ~ horizon, data = d[d$group != "other", ], FUN = mean),
              group = "gdp+prices"),
    transform(stats::aggregate(explosive_frac ~ horizon, data = d, FUN = mean), group = "all")
  )
}

# Posterior median of each equation's structural residual variance -- the
# quantity the riwish(T - k, .) draw inflates -- and its own-lag coefficient.
equation_diagnostics <- function(fit) {
  sigma2 <- vapply(fit$estimates, function(e) {
    stats::median(vapply(e$omega_tilde_jw, function(o) as.matrix(o)[1, 1], numeric(1)))
  }, numeric(1))
  coefs <- coefficient_table(fit)
  own <- coefs[coefs$term == paste0(coefs$equation, ".L(1)"), ]
  acc <- check_acceptance_rates(fit)
  data.frame(
    equation = names(sigma2),
    sigma2 = unname(sigma2),
    own_lag = own$estimate[match(names(sigma2), own$equation)],
    own_lag_width = (own$ci_high - own$ci_low)[match(names(sigma2), own$equation)],
    acceptance = acc$acceptance_rate[match(names(sigma2), acc$equation)],
    stringsAsFactors = FALSE
  )
}

# --- per stage --------------------------------------------------------------

run_stage <- function(stage) {
  cli::cli_h1(stage)
  inp <- stage_inputs[[stage]]()
  spec <- inp$spec
  panel <- inp$panel
  dates <- inp$dates

  plain <- build_stage2_system(spec)
  k <- length(plain$total_exogenous_variables)
  t_obs <- estimation_length(panel, dates)
  cli::cli_inform("{length(plain$stochastic_equations)} stochastic equations, k = {k}, T = {t_obs}, df = {t_obs - k}")

  baseline <- inp$baseline %||% cached(paste0(stage, "_baseline_tuned"), {
    tuned <- tune_tau_system(spec, panel, dates, workers = WORKERS)
    list(fit = tuned$fit, tau = tuned$tau, history = tuned$history, converged = tuned$converged)
  })

  # The spec must reproduce the baseline system, or the comparison would be
  # between two different models rather than two samplers.
  if (!identical(baseline$fit$sys_eq$character_gamma_matrix, plain$character_gamma_matrix) ||
      !identical(baseline$fit$sys_eq$character_beta_matrix, plain$character_beta_matrix)) {
    cli::cli_abort("{stage}: the spec does not reproduce the baseline fit's system.")
  }

  prior <- cached(paste0(stage, "_prior_tuned"), {
    tuned <- tune_tau_system(spec, panel, dates, error_priors = default_error_priors(spec),
                             workers = WORKERS)
    list(fit = tuned$fit, tau = tuned$tau, history = tuned$history, converged = tuned$converged)
  })

  fc_baseline <- read_if("data", "cache", "forecasts", paste0(stage, ".rds"))
  if (is.null(fc_baseline) || fc_baseline$horizon != HORIZON) {
    fc_baseline <- cached(paste0(stage, "_baseline_forecast"),
                          stage_forecast(baseline$fit, panel, horizon = HORIZON, seed = SEED))
  } else {
    cli::cli_inform("baseline forecast: data/cache/forecasts/{stage}.rds")
  }
  fc_prior <- cached(paste0(stage, "_prior_forecast"),
                     stage_forecast(prior$fit, panel, horizon = HORIZON, seed = SEED))

  explosive <- merge(
    stats::setNames(explosive_by_concept(fc_baseline), c("group", "horizon", "baseline")),
    stats::setNames(explosive_by_concept(fc_prior), c("group", "horizon", "error_prior"))
  )
  explosive$stage <- stage

  diag_b <- equation_diagnostics(baseline$fit)
  diag_p <- equation_diagnostics(prior$fit)
  equations <- merge(diag_b, diag_p, by = "equation", suffixes = c("_baseline", "_prior"))
  equations$sigma2_ratio <- equations$sigma2_baseline / equations$sigma2_prior
  equations$own_lag_width_ratio <- equations$own_lag_width_baseline / equations$own_lag_width_prior
  equations$stage <- stage

  out <- list(
    stage = stage, k = k, t_obs = t_obs, df = t_obs - k,
    explosive = explosive[order(explosive$group, explosive$horizon), ],
    equations = equations,
    tau = list(baseline = baseline$tau, prior = prior$tau),
    converged = c(baseline = baseline$converged %||% NA, prior = prior$converged)
  )
  saveRDS(out, file.path(out_dir, paste0(stage, "_comparison.rds")))

  h8 <- out$explosive[out$explosive$horizon == HORIZON, ]
  cli::cli_h2("{stage}: explosive share at horizon {HORIZON} (df = {out$df})")
  print(h8[, c("group", "baseline", "error_prior")], row.names = FALSE, digits = 3)
  cli::cli_h2("{stage}: per-equation medians, baseline / prior")
  print(stats::setNames(
    round(c(stats::median(equations$sigma2_ratio), stats::median(equations$own_lag_width_ratio, na.rm = TRUE)), 2),
    c("residual variance", "own-lag 90% width")
  ))
  invisible(out)
}

args <- commandArgs(trailingOnly = TRUE)
wanted <- if (length(args) > 0) args else c("stage2d", "stage2b")
unknown <- setdiff(wanted, names(stage_inputs))
if (length(unknown) > 0) cli::cli_abort("Unknown stage{?s}: {.val {unknown}}.")

warn_if_blas_threaded(WORKERS)
for (s in wanted) run_stage(s)
cli::cli_inform("Comparisons in {.file {out_dir}}.")
