# Wrappers around koma's MCMC diagnostics, applied project-wide (across
# every country in a fitted stage-1 or stage-2 model) rather than one
# equation at a time.

#' Summarise MCMC acceptance rates across a whole fit
#'
#' Wraps koma's per-equation acceptance-rate logic (see
#' `?koma::estimate`, "What tau does" in `docs/koma-api.md`) across every
#' stochastic equation in a fit, flagging any outside the target band.
#'
#' koma exposes no accessor for this: the per-draw 0/1 acceptance
#' indicator lives at `fit$estimates[[equation]]$count_accepted`, and the
#' rate is its mean. Equations with **no contemporaneous endogenous
#' regressor** have no Metropolis step at all -- their gamma block is
#' empty and `count_accepted` is `NA` throughout. Those are reported with
#' `has_mh_step = FALSE` and an `NA` rate, and are never flagged; koma
#' excludes them from its own warning for the same reason.
#'
#' The default band matches `koma:::get_default_acceptance_prob()`, which
#' returns `c(0.2, 0.6)`. (koma's `equations` vignette prose says
#' 30%-60%; the code is authoritative and says 20%-60%.)
#'
#' @param fit A `koma::koma_estimate` object.
#' @param band Numeric length-2 vector, the acceptable acceptance-rate
#'   range. Default `c(0.2, 0.6)`, matching koma's own default.
#'
#' @return A `data.frame` with columns `equation`, `has_mh_step`,
#'   `acceptance_rate`, `flagged`.
#' @export
check_acceptance_rates <- function(fit, band = c(0.2, 0.6)) {
  estimates <- fit$estimates
  if (is.null(estimates) || length(estimates) == 0) {
    cli::cli_abort("{.arg fit} has no {.field estimates}; is it a {.cls koma_estimate}?")
  }
  if (length(band) != 2 || band[1] >= band[2]) {
    cli::cli_abort("{.arg band} must be two increasing numbers, got {.val {band}}.")
  }

  rate <- vapply(estimates, function(equation) {
    accepted <- equation$count_accepted
    if (is.null(accepted) || all(is.na(accepted))) {
      return(NA_real_)
    }
    mean(accepted, na.rm = TRUE)
  }, numeric(1))

  has_mh_step <- !is.na(rate)

  data.frame(
    equation = names(estimates),
    has_mh_step = has_mh_step,
    acceptance_rate = unname(rate),
    flagged = has_mh_step & (rate < band[1] | rate > band[2]),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

#' Diagnostic plots for a set of variables across countries
#'
#' Wraps `koma::trace_plot()`, `koma::acf_plot()`, and
#' `koma::running_mean_plot()` for every `<iso2>_<concept>` combination
#' implied by `countries` x `concepts`, returning one plot per
#' variable/kind rather than requiring the caller to loop. Each returned
#' plot already facets over every coefficient of that equation, since
#' that is what koma's own plotting functions do when given one
#' `variables` name.
#'
#' A `concepts` name with no matching stochastic equation in `fit` (e.g.
#' `"exports"` or `"prices"`, which have no Metropolis step and therefore
#' nothing to trace) is skipped with a warning naming it, rather than
#' erroring the whole grid.
#'
#' @param fit A `koma::koma_estimate` object.
#' @param countries Character vector of ISO-2 country codes.
#' @param concepts Character vector of concepts, e.g. `c("gdp", "prices")`.
#' @param kind One of `"trace"`, `"acf"`, `"running_mean"`.
#'
#' @return A named list of `ggplot` objects, keyed by variable name.
#' @export
diagnostics_grid <- function(fit, countries, concepts, kind = c("trace", "acf", "running_mean")) {
  kind <- match.arg(kind)
  plot_fn <- switch(kind,
    trace = koma::trace_plot,
    acf = koma::acf_plot,
    running_mean = koma::running_mean_plot
  )

  variables <- unlist(lapply(countries, function(cc) country_var(cc, concepts)), use.names = FALSE)
  present <- intersect(variables, names(fit$estimates))
  missing <- setdiff(variables, present)
  if (length(missing) > 0) {
    cli::cli_warn("No stochastic equation (no Metropolis step) for {.val {missing}}; skipping.")
  }

  stats::setNames(
    lapply(present, function(v) plot_fn(fit, variables = v)),
    present
  )
}

#' Save stage-1 diagnostic plots to disk
#'
#' Runs [diagnostics_grid()] for every stochastic equation actually
#' present in each fit -- so the US's extra `policy_rate` equation is
#' included automatically and no country tries to plot an equation it
#' does not have -- for each of `kind`, and writes one PNG per
#' `(country, equation, kind)` to `<out_dir>/<iso2>/<kind>_<equation>.png`.
#'
#' @param fits A named list of `koma_estimate`, as from [fit_stage1_all()].
#' @param out_dir Base output directory. Default `reports/stage1`.
#' @param kind One or more of `"trace"`, `"running_mean"`, `"acf"`.
#' @param width,height Passed to `ggplot2::ggsave()`, in inches.
#'
#' @return Invisibly, a character vector of the file paths written.
#' @export
save_stage1_plots <- function(fits, out_dir = file.path("reports", "stage1"),
                              kind = c("trace", "running_mean", "acf"),
                              width = 8, height = 5) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort("{.pkg ggplot2} is required to save diagnostic plots.")
  }

  written <- character(0)
  for (iso2 in names(fits)) {
    fit <- fits[[iso2]]
    concepts <- sub(paste0("^", iso2, "_"), "", names(fit$estimates))
    country_out_dir <- file.path(out_dir, iso2)
    dir.create(country_out_dir, recursive = TRUE, showWarnings = FALSE)

    for (k in kind) {
      plots <- diagnostics_grid(fit, iso2, concepts, kind = k)
      for (v in names(plots)) {
        path <- file.path(country_out_dir, paste0(k, "_", v, ".png"))
        ggplot2::ggsave(path, plots[[v]], width = width, height = height)
        written <- c(written, path)
      }
    }
  }

  cli::cli_inform("Wrote {length(written)} diagnostic plot{?s} to {.path {out_dir}}.")
  invisible(written)
}

#' Flag coefficients whose running mean has not stabilised
#'
#' `koma::running_mean(fit)` returns the cumulative posterior mean of
#' every coefficient after every saved draw. A chain that has converged
#' should show a running mean that is flat by the end of the chain; one
#' still drifting in its last third has not converged, regardless of
#' whether its Metropolis acceptance rate looks healthy -- acceptance
#' rate and convergence are different diagnostics.
#'
#' **Stability test.** `koma::running_mean()` returns the *cumulative*
#' mean after each draw, which is heavily autocorrelated from one draw to
#' the next -- an ordinary least-squares trend test on the running mean
#' itself was tried first and rejected: OLS assumes independent
#' residuals, and a cumulative mean's residuals are anything but, so it
#' produced wildly inflated t-statistics (dozens, not the low single
#' digits a real trend would show) and flagged almost every coefficient
#' regardless of how flat it actually was.
#'
#' Instead this recovers the **raw per-draw values** the running mean was
#' built from (`raw[t] = t * mean[t] - (t-1) * mean[t-1]`, undoing the
#' cumulative average) and restricts to `tail_fraction` of the
#' (non-grace) chain -- the last third by default. A coefficient is
#' flagged if the running mean's drift across that tail -- the second
#' half's mean minus the first half's -- exceeds `z_crit` Monte Carlo
#' standard errors, where the MCSE is the raw tail draws' own standard
#' deviation divided by `sqrt(n_tail)`. This ignores residual
#' autocorrelation between draws (so the true MCSE is somewhat larger,
#' making this a slightly liberal test -- documented, not hidden), but is
#' far better calibrated than treating draws as independent altogether.
#'
#' @param fit A `koma::koma_estimate` object.
#' @param tail_fraction Fraction of the (non-grace) chain treated as
#'   "the last third". Default `1/3`, per the brief.
#' @param z_crit Flag if the tail's drift exceeds this many Monte Carlo
#'   standard errors. Default `2` (approximately a 95% test).
#'
#' @return A `data.frame` with columns `equation`, `param`, `coef`,
#'   `drift`, `mcse`, `flagged`.
#' @export
check_running_mean_stability <- function(fit, tail_fraction = 1 / 3, z_crit = 2) {
  rm_full <- koma::running_mean(fit)

  labels <- unique(rm_full$label)
  rows <- lapply(labels, function(lbl) {
    full <- rm_full[rm_full$label == lbl, ]
    full <- full[order(full$draw_position), ]

    # Undo the cumulative mean to recover each draw's raw value.
    lagged_value <- c(0, full$value[-nrow(full)])
    lagged_pos <- c(0, full$draw_position[-nrow(full)])
    raw <- full$draw_position * full$value - lagged_pos * lagged_value

    g <- full[!full$in_grace_window, ]
    raw_g <- raw[!full$in_grace_window]

    n_tail <- max(3, ceiling(nrow(g) * tail_fraction))
    tail_idx <- utils::tail(seq_len(nrow(g)), n_tail)
    tail_value <- g$value[tail_idx]
    tail_raw <- raw_g[tail_idx]

    half <- floor(length(tail_idx) / 2)
    drift <- mean(tail_value[(half + 1):length(tail_value)]) - mean(tail_value[seq_len(half)])
    mcse <- stats::sd(tail_raw) / sqrt(length(tail_raw))

    data.frame(
      equation = g$variable[1], param = g$param[1], coef = g$coef[1],
      drift = drift, mcse = mcse,
      flagged = if (mcse > 0) abs(drift) > z_crit * mcse else abs(drift) > 1e-8,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

#' Search for a per-equation `tau` that lands every equation in-band
#'
#' **Search strategy.** koma's own documentation notes "roughly, doubling
#' tau halves the acceptance rate" (`docs/koma-api.md` §3). Each
#' iteration, every currently-flagged equation gets its `tau` multiplied
#' by `factor` (default 2) if its rate is above the band, or divided by
#' `factor` if below; equations already in-band keep their current `tau`.
#' The whole country is then **fully re-estimated** -- changing a `[tau =
#' ]` equation setting is not a change to the symbolic regressor
#' structure, so it is not something koma's `estimates =` warm-start
#' re-estimation-index logic would pick up (verified: it compares only
#' the symbolic Gamma/Beta matrices); a full re-estimate is the only way
#' to actually apply the new `tau`.
#'
#' Stops early once every equation is in-band, or after `max_iter`
#' iterations, whichever comes first (`max_iter` bounds the run whether
#' or not it converges).
#'
#' @param iso2 Two-letter lowercase ISO country code.
#' @param panel Named list of `koma_ts`, as built by [build_global_panel()].
#' @param dates koma `dates` list, e.g. from [stage1_dates()].
#' @param band Acceptance-rate band. Default `c(0.2, 0.6)`.
#' @param max_iter Maximum number of re-estimation rounds after the
#'   baseline (`tau = 1.1` everywhere) run.
#' @param factor Multiplicative step applied to a flagged equation's `tau`.
#' @param options Passed through to `koma::estimate(options = )`.
#'
#' @return A list with `iso2`, `fit` (the final `koma_estimate`),
#'   `tau` (named vector of the final per-equation overrides), `history`
#'   (a `data.frame` of every iteration's acceptance rates, with
#'   `iteration` and `tau` columns added), and `converged` (logical).
#' @export
tune_tau <- function(iso2, panel, dates, band = c(0.2, 0.6), max_iter = 4,
                     factor = 2, options = list()) {
  iso2 <- tolower(iso2)
  shares <- expenditure_shares(panel, iso2, dates)

  tau <- list()
  history <- list()
  fit <- NULL
  flagged <- data.frame()

  for (iteration in 0:max_iter) {
    tau_arg <- if (length(tau) > 0) unlist(tau) else NULL
    sys_eq <- stage1_country_equations(iso2, stage1_spec(iso2, shares), tau = tau_arg)
    fit <- estimate_stage1_system(iso2, sys_eq, panel, dates, options = options)

    acceptance <- check_acceptance_rates(fit, band = band)
    acceptance$iteration <- iteration
    acceptance$tau <- vapply(acceptance$equation, function(e) tau[[e]] %||% 1.1, numeric(1))
    history[[length(history) + 1]] <- acceptance

    flagged <- acceptance[acceptance$flagged %in% TRUE, ]
    if (nrow(flagged) == 0 || iteration == max_iter) {
      break
    }
    for (i in seq_len(nrow(flagged))) {
      eq <- flagged$equation[i]
      current <- tau[[eq]] %||% 1.1
      tau[[eq]] <- if (flagged$acceptance_rate[i] > band[2]) current * factor else current / factor
    }
  }

  list(
    iso2 = iso2,
    fit = fit,
    tau = if (length(tau) > 0) unlist(tau) else stats::setNames(numeric(0), character(0)),
    history = do.call(rbind, history),
    converged = nrow(flagged) == 0
  )
}

#' Run [tune_tau()] for every country, in parallel
#'
#' Mirrors [fit_stage1_all()]'s parallelism: countries are the wide axis
#' (11 independent searches), so the plan is set once here and spread
#' across workers with `future.apply::future_lapply()`.
#'
#' @inheritParams tune_tau
#' @param countries Character vector of ISO-2 country codes.
#' @param parallel Logical; if `FALSE`, runs sequentially.
#'
#' @return A named list of [tune_tau()] results, one per country.
#' @export
tune_tau_all <- function(countries = modelled_countries, panel, dates,
                         band = c(0.2, 0.6), max_iter = 4, factor = 2,
                         options = list(), parallel = TRUE) {
  if (parallel) {
    old_plan <- future::plan(stage1_parallel_strategy(), workers = stage1_parallel_workers(countries))
    on.exit(future::plan(old_plan), add = TRUE)
  }

  results <- future.apply::future_lapply(
    countries,
    function(iso2) tune_tau(iso2, panel, dates, band = band, max_iter = max_iter, factor = factor, options = options),
    future.seed = TRUE
  )
  stats::setNames(results, countries)
}

#' Posterior coefficient table for a fitted system
#'
#' One row per (equation, term), with the posterior mean and a credible
#' interval. Works via `summary(fit, use_texreg = FALSE)`, which falls
#' back to a plain list even when `texreg` is not installed (verified) --
#' so this needs no reporting package beyond base R.
#'
#' @param fit A `koma::koma_estimate` object.
#' @param ci_low,ci_up Lower/upper quantile, in percent. Default `5`/`95`,
#'   i.e. a 90% interval, matching the brief.
#'
#' @return A `data.frame` with columns `equation`, `term`, `estimate`,
#'   `ci_low`, `ci_high`.
#' @export
coefficient_table <- function(fit, ci_low = 5, ci_up = 95) {
  s <- suppressMessages(summary(fit, ci_low = ci_low, ci_up = ci_up, use_texreg = FALSE))

  rows <- lapply(s$stats, function(eq) {
    data.frame(
      equation = eq$model.name, term = eq$coef.names,
      estimate = unname(eq$coef), ci_low = unname(eq$ci.low), ci_high = unname(eq$ci.up),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Plausibility (sign) checks on a country's stage-1 coefficients
#'
#' Three checks, all against the coefficient's **posterior mean** --
#' flagged, never silently corrected:
#'
#' - `mpc_in_0_1`: the consumption equation's loading on GDP should be a
#'   plausible marginal propensity to consume, in `(0, 1)`.
#' - `import_elasticity_positive`: the imports equation's loading on
#'   domestic demand should be positive -- more demand, more imports.
#' - `long_rate_loads_on_policy_rate`: the long-rate equation's loading
#'   on the (shared euro-area, or US) policy rate should be positive --
#'   the sovereign yield should move with the policy rate, not against
#'   it.
#'
#' @param coef_table A `data.frame` from [coefficient_table()].
#' @param iso2 Two-letter lowercase ISO country code.
#'
#' @return A `data.frame` with columns `check`, `equation`, `term`,
#'   `estimate`, `ok`.
#' @export
sign_checks <- function(coef_table, iso2) {
  iso2 <- tolower(iso2)
  v <- function(concept) country_var(iso2, concept)
  policy_var <- if (identical(iso2, "us")) "us_policy_rate" else "ea_policy_rate"

  find_estimate <- function(eq, term) {
    row <- coef_table[coef_table$equation == eq & coef_table$term == term, ]
    if (nrow(row) == 0) NA_real_ else row$estimate[1]
  }

  mpc <- find_estimate(v("consumption"), v("gdp"))
  import_elasticity <- find_estimate(v("imports"), v("domestic_demand"))
  rate_loading <- find_estimate(v("long_rate"), policy_var)

  data.frame(
    check = c("mpc_in_0_1", "import_elasticity_positive", "long_rate_loads_on_policy_rate"),
    equation = c(v("consumption"), v("imports"), v("long_rate")),
    term = c(v("gdp"), v("domestic_demand"), policy_var),
    estimate = c(mpc, import_elasticity, rate_loading),
    ok = c(
      isTRUE(mpc > 0 && mpc < 1),
      isTRUE(import_elasticity > 0),
      isTRUE(rate_loading > 0)
    ),
    stringsAsFactors = FALSE
  )
}

#' In-sample RMSE per equation
#'
#' Reconstructs each equation's fitted value from its posterior mean
#' coefficients and koma's own design matrices (`fit$x_matrix` for
#' predetermined/exogenous regressors, `fit$y_matrix` for contemporaneous
#' endogenous ones -- every coefficient name in `summary()`'s
#' `coef.names` is a column of exactly one of the two), then compares to
#' the actual value. Both matrices are already on the **rate** (`diff_log`
#' growth, in percent) scale koma estimates on -- the same scale
#' `coefficient_table()`'s estimates apply to -- so this is not
#' comparable in magnitude to [pseudo_oos_rmse()], which reports on
#' levels (koma's own default for `model_evaluation()`); see that
#' function's roxygen for why.
#'
#' @param fit A `koma::koma_estimate` object.
#'
#' @return A `data.frame` with columns `equation`, `rmse`, `nobs`.
#' @export
rmse_in_sample <- function(fit) {
  s <- suppressMessages(summary(fit, use_texreg = FALSE))
  x <- fit$x_matrix
  y <- fit$y_matrix

  rows <- lapply(s$stats, function(eq) {
    coefs <- eq$coef
    names(coefs) <- eq$coef.names
    design <- vapply(names(coefs), function(term) {
      if (term %in% colnames(x)) x[, term] else y[, term]
    }, numeric(nrow(x)))

    fitted <- as.numeric(design %*% coefs)
    actual <- y[, eq$model.name]
    resid <- actual - fitted

    data.frame(
      equation = eq$model.name,
      rmse = sqrt(mean(resid^2, na.rm = TRUE)),
      nobs = sum(!is.na(resid)),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Rescale an exogenous variable's forecast-horizon **level** path
#'
#' `koma::estimate()` stores its `ts_data` already `rate()`-transformed
#' (verified: `fit$ts_data$oil_price` carries `series_type = "rate"`,
#' `method = "diff_log"`, with values like `-8.71` -- quarterly percent
#' changes, not a price level around $50-90). `koma::forecast()` reads
#' an exogenous variable's *future* path straight from that stored
#' `ts_data`, so a conditioning scenario expressed on the **level**
#' (e.g. "oil is 50% higher") has to be built by: taking the original
#' level series, applying `shock_fn` to its forecast-horizon values,
#' re-deriving the rate-space series with koma's own `rate()` (so the
#' transform matches exactly what `estimate()` used, including how the
#' one-time level jump shows up as one elevated growth-rate observation
#' at the transition quarter), and substituting that back into a copy of
#' the fit's `ts_data`. `rate()` is applied to the whole series, not just
#' the forecast window, because `diff_log` needs the preceding
#' observation to compute a difference -- verified: the recomputed
#' history matches the original stored `ts_data` exactly.
#'
#' Only valid for variables that are **exogenous** in `fit`'s system.
#' An endogenous variable's future path is a model output, not an input,
#' and has to be conditioned on via `koma::forecast(restrictions = )`
#' instead -- see [conditional_forecast_check()].
#'
#' @param fit A `koma::koma_estimate` object.
#' @param panel Named list of `koma_ts`, holding `variable` in levels.
#' @param variable Name of the exogenous variable to shock.
#' @param dates koma `dates` list (uses `dates$forecast`).
#' @param shock_fn A function taking the baseline level path (numeric
#'   vector) and returning the shocked level path.
#'
#' @return A copy of `fit` with `ts_data[[variable]]`'s forecast-horizon
#'   values replaced by the shocked, rate-transformed path.
#' @keywords internal
shock_exogenous_level <- function(fit, panel, variable, dates, shock_fn) {
  level <- panel[[variable]]
  if (is.null(level)) {
    cli::cli_abort("{.val {variable}} not found in {.arg panel}.")
  }

  shocked_level <- level
  baseline_path <- stats::window(level, start = dates$forecast$start, end = dates$forecast$end)
  stats::window(shocked_level, start = dates$forecast$start, end = dates$forecast$end) <-
    shock_fn(as.numeric(baseline_path))

  shocked_rate <- koma::rate(shocked_level)
  shocked_forecast_window <- stats::window(shocked_rate, start = dates$forecast$start, end = dates$forecast$end)

  out <- fit
  stats::window(out$ts_data[[variable]], start = dates$forecast$start, end = dates$forecast$end) <-
    shocked_forecast_window
  out
}

#' Conditional-forecast sanity check: does the sign match economic theory?
#'
#' Two scenarios, both computed from the country's **existing** stage-1
#' fit (no re-estimation -- a scenario forecast reuses the posterior
#' already drawn, just under a different exogenous/conditioning path):
#'
#' - `"oil"`: oil's price **level** is shocked +50% from the forecast
#'   start onward (see [shock_exogenous_level()]); the loading is on
#'   `<iso2>_prices`, which should rise.
#' - `"policy"`: the policy rate is shocked +100bp (1 percentage point)
#'   from the forecast start onward; the check is on `<iso2>_gdp`, which
#'   should fall.
#'
#' `oil_price` is exogenous in every stage-1 system, so its scenario
#' always goes through [shock_exogenous_level()]. The policy rate is
#' exogenous for the ten EA countries (`ea_policy_rate`, shared) but
#' **endogenous** for the US (`us_policy_rate`, via its own Taylor
#' equation) -- so the US scenario is instead imposed with
#' `koma::forecast(restrictions = )`, koma's mechanism for conditioning
#' on an *endogenous* variable's future path.
#'
#' **A structural finding, not a bug**: in the stage-1 template (see
#' `stage1_spec()`), `<iso2>_long_rate` is the *only* equation the policy
#' rate feeds into, and nothing downstream depends on `long_rate` -- it
#' is a satellite equation with no further effect, exactly like the
#' Switzerland vignette's own `interest_rate`. So the `"policy"` scenario
#' cannot move GDP at stage 1 by construction, for every country: the
#' transmission channel does not exist yet. That is not something this
#' function papers over -- it reports the (zero) effect and its
#' `sign_ok = FALSE` result plainly. Building that channel is exactly the
#' kind of cross-equation interaction stage 2's joint estimation is for.
#'
#' **`forecast()` is genuinely stochastic between calls, not just between
#' draws within a call**: verified, two `forecast(fit, dates)` calls on
#' the identical fitted object gave `de_gdp` means of `(0.27, -0.57, 6.40,
#' 13.94)` and `(0.33, -0.12, 2.74, 0.01)` -- differences an order of
#' magnitude bigger than any real scenario effect, swamping it entirely,
#' worst at the longer horizons inside the 2020-2022 conditional-fill
#' window. Baseline and scenario are therefore both forecast with
#' `options = list(approximate = TRUE)`: a single deterministic pass from
#' the posterior mean/median coefficients (`docs/koma-api.md` §4),
#' verified to return bit-identical results across repeated calls. This
#' trades the predictive distribution for a repeatable, well-defined
#' point comparison -- exactly what a *sign* check needs.
#'
#' @param fit A `koma::koma_estimate` object.
#' @param iso2 Two-letter lowercase ISO country code.
#' @param panel Named list of `koma_ts`, as built by [build_global_panel()].
#' @param scenario `"oil"` or `"policy"`.
#'
#' @return A `data.frame` with columns `country`, `scenario`,
#'   `description`, `target`, `horizon`, `baseline`, `scenario_value`,
#'   `diff`, `expected_sign`, `sign_ok`.
#' @export
conditional_forecast_check <- function(fit, iso2, panel, scenario = c("oil", "policy")) {
  scenario <- match.arg(scenario)
  iso2 <- tolower(iso2)
  dates <- fit$dates
  forecast_options <- list(approximate = TRUE)

  if (identical(scenario, "oil")) {
    shocked_var <- "oil_price"
    shock_fn <- function(x) x * 1.5
    target_concept <- "prices"
    expected_sign <- "positive"
    description <- "oil price level +50%"
  } else {
    shocked_var <- if (identical(iso2, "us")) "us_policy_rate" else "ea_policy_rate"
    shock_fn <- function(x) x + 1.0
    target_concept <- "gdp"
    expected_sign <- "negative"
    description <- "policy rate +100bp"
  }

  base_fc <- koma::forecast(fit, dates = dates, options = forecast_options)

  if (shocked_var %in% fit$sys_eq$endogenous_variables) {
    baseline_path <- as.numeric(stats::window(panel[[shocked_var]], start = dates$forecast$start, end = dates$forecast$end))
    shocked_path <- shock_fn(baseline_path)
    restrictions <- stats::setNames(list(list(horizon = seq_along(shocked_path), value = shocked_path)), shocked_var)
    scen_fc <- koma::forecast(fit, dates = dates, restrictions = restrictions, options = forecast_options)
  } else {
    shocked_fit <- shock_exogenous_level(fit, panel, shocked_var, dates, shock_fn)
    scen_fc <- koma::forecast(shocked_fit, dates = dates, options = forecast_options)
  }

  target_var <- country_var(iso2, target_concept)
  base_path <- as.numeric(base_fc$mean[[target_var]])
  scen_path <- as.numeric(scen_fc$mean[[target_var]])
  diff <- scen_path - base_path

  data.frame(
    country = iso2, scenario = scenario, description = description, target = target_var,
    horizon = seq_along(diff), baseline = base_path, scenario_value = scen_path, diff = diff,
    expected_sign = expected_sign,
    sign_ok = if (identical(expected_sign, "positive")) diff > 0 else diff < 0,
    stringsAsFactors = FALSE
  )
}

#' Subtract one period from a `c(year, period)` date
#' @keywords internal
prev_period <- function(yq, frequency = 4) {
  year <- yq[1]
  period <- yq[2] - 1
  if (period < 1) {
    period <- frequency
    year <- year - 1
  }
  c(year, period)
}

#' Pseudo-out-of-sample RMSE via an expanding window
#'
#' Thin wrapper around `koma::model_evaluation()`, koma's own
#' rolling-origin evaluator: starting from `eval_start`, it re-estimates
#' the country's system with the sample expanded by one quarter each
#' iteration, forecasts `horizon` quarters ahead, and returns the RMSE
#' per equation against the actually-observed values -- **re-estimating
#' at every origin**, so cost is `(number of origins) x estimate()`
#' (`docs/koma-api.md`, "Out-of-sample evaluation").
#'
#' Reported on **levels** (`evaluate_on_levels = TRUE`, koma's own
#' default), not the growth-rate scale [rmse_in_sample()] uses. That is
#' deliberate, not an oversight: `evaluate_on_levels = FALSE` was tried
#' first and produced pathological outliers (RMSEs in the thousands of
#' percent) on some origins, traced to compounding growth-rate forecasts
#' over a short, low-draw expanding window -- the standard failure mode
#' of chaining multiplicative log-differences under sampling noise.
#' Levels are also more directly interpretable here. **Do not compare
#' this function's RMSEs to [rmse_in_sample()]'s directly -- different
#' units.**
#'
#' `options$gibbs$ndraws` defaults far below koma's own 2000, purely for
#' runtime: at `(eval_end - eval_start - horizon + 1)` origins per
#' country, the full default would make an 11-country diagnostic run
#' impractically slow. This is a documented speed/precision tradeoff for
#' a diagnostic, not the setting stage-1 fits themselves use.
#'
#' @param iso2 Two-letter lowercase ISO country code.
#' @param panel Named list of `koma_ts`, as built by [build_global_panel()].
#' @param eval_start,eval_end `c(year, quarter)` bounds of the evaluation
#'   window. Default `2016Q1`-`2019Q4`, per the brief.
#' @param horizon Forecast horizon, in quarters, evaluated at every origin.
#' @param options Passed through to `koma::model_evaluation(options = )`.
#'
#' @return A `data.frame`: one row per origin, one column per equation,
#'   `attr(, "iso2")` set.
#' @export
pseudo_oos_rmse <- function(iso2, panel, eval_start = c(2016, 1), eval_end = c(2019, 4),
                            horizon = 4, options = list(gibbs = list(ndraws = 500))) {
  iso2 <- tolower(iso2)
  full_dates <- stage1_dates(panel)

  shares <- expenditure_shares(panel, iso2, list(estimation = list(
    start = full_dates$estimation$start, end = prev_period(eval_start)
  )))
  sys_eq <- stage1_country_equations(iso2, stage1_spec(iso2, shares))

  needed <- c(sys_eq$endogenous_variables, sys_eq$exogenous_variables, sys_eq$weight_variables)
  gaps <- internal_gaps(panel[needed])
  if (length(gaps) > 0) {
    cli::cli_abort("{.val {names(gaps)}} {?has/have} internal {.val NA}s; run {.fn fill_internal_gaps} first.")
  }
  ts_data <- harmonise_panel_attrs(panel[needed])

  dates <- list(
    estimation = list(start = full_dates$estimation$start, end = prev_period(eval_start)),
    forecast = list(start = eval_start, end = eval_end)
  )

  result <- koma::model_evaluation(
    sys_eq, variables = sys_eq$stochastic_equations, horizon = horizon,
    ts_data = ts_data, dates = dates, options = options
  )
  attr(result, "iso2") <- iso2
  result
}

#' Run [pseudo_oos_rmse()] for every country, in parallel
#'
#' Same parallelism pattern as [fit_stage1_all()]/[tune_tau_all()]:
#' countries are the wide, independent axis, so the plan is set once and
#' spread across workers.
#'
#' @inheritParams pseudo_oos_rmse
#' @param countries Character vector of ISO-2 country codes.
#' @param parallel Logical; if `FALSE`, runs sequentially.
#'
#' @return A named list of [pseudo_oos_rmse()] results, one per country.
#' @export
pseudo_oos_rmse_all <- function(countries = modelled_countries, panel,
                                eval_start = c(2016, 1), eval_end = c(2019, 4),
                                horizon = 4, options = list(gibbs = list(ndraws = 500)),
                                parallel = TRUE) {
  if (parallel) {
    old_plan <- future::plan(stage1_parallel_strategy(), workers = stage1_parallel_workers(countries))
    on.exit(future::plan(old_plan), add = TRUE)
  }

  results <- future.apply::future_lapply(
    countries,
    function(iso2) pseudo_oos_rmse(iso2, panel, eval_start, eval_end, horizon, options),
    future.seed = TRUE
  )
  stats::setNames(results, countries)
}

#' Identification pre-check for a multi-country system
#'
#' Runs `koma::model_identification()` and reports failures per equation
#' with the offending country/concept, rather than koma's single
#' system-wide abort.
#'
#' `koma::model_identification()` is called automatically inside
#' `koma::estimate()`, but only aborts -- it does not report which
#' equation failed. Calling it here up front is cheap (it is a purely
#' symbolic check on the `Gamma`/`Beta` matrices, with no data and no
#' MCMC) and catches a mis-specified system before committing to a long
#' run.
#'
#' Note the argument koma actually wants is the whole `identities` list
#' (post-weight-resolution), not just the weights -- passing the weights
#' alone fails with an opaque "NA/NaN/Inf in foreign function call".
#'
#' @param sys_eq A `koma::koma_seq` object.
#'
#' @return A `data.frame` with columns `equation`, `order_condition`,
#'   `rank_condition`.
#' @export
check_identification <- function(sys_eq) {
  if (!koma::is_system_of_equations(sys_eq)) {
    cli::cli_abort("{.arg sys_eq} must be a {.cls koma_seq} from {.fn koma::system_of_equations}.")
  }

  equations <- sys_eq$stochastic_equations
  result <- tryCatch(
    {
      koma::model_identification(
        sys_eq$character_gamma_matrix,
        sys_eq$character_beta_matrix,
        sys_eq$identities
      )
      NULL
    },
    error = function(e) conditionMessage(e)
  )
  identified <- is.null(result)

  if (!identified) {
    cli::cli_warn(c(
      "!" = "The system is not identified.",
      "i" = result
    ))
  }

  data.frame(
    equation = equations,
    order_condition = identified,
    rank_condition = identified,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}
