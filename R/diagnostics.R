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

#' Plausibility (sign) checks on a country's coefficients
#'
#' Checks against the coefficient's **posterior mean** -- flagged, never
#' silently corrected. The base three:
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
#' With `labour = TRUE`, eighteen more from [stage3a_sign_rules()], covering
#' the wage and price Phillips curves, exchange-rate and oil pass-through,
#' and the relative-price terms in trade volumes.
#'
#' Only the posterior **mean** is tested; `ci_low`/`ci_high` are ignored, so
#' a "pass" says the central estimate has the right sign, not that the sign
#' is statistically distinguishable from zero.
#'
#' @param coef_table A `data.frame` from [coefficient_table()].
#' @param iso2 Two-letter lowercase ISO country code.
#' @param labour Also apply the stage-3a labour and disaggregated-price
#'   checks (see [stage3a_sign_rules()]). `FALSE` by default; a country
#'   without the block has none of those equations and every row would be
#'   `NA`/`FALSE`, which reads as fifteen failures rather than "not
#'   applicable".
#'
#' @return A `data.frame` with columns `check`, `equation`, `term`,
#'   `estimate`, `expected`, `ok`. A term the fit does not contain gives
#'   `estimate = NA` and `ok = FALSE` -- a check that could not be evaluated
#'   is not a check that passed.
#' @export
sign_checks <- function(coef_table, iso2, labour = FALSE, external = FALSE,
                        fiscal = FALSE, financial = FALSE, stage2c = FALSE) {
  iso2 <- tolower(iso2)
  rules <- base_sign_rules(iso2)
  if (isTRUE(stage2c)) {
    # Stage 2c models the spread, so `long_rate` is an identity over the spread
    # and the policy rate: pass-through is IMPOSED at 1, not estimated, and
    # there is no coefficient left whose sign could be checked. Same reasoning
    # as the financial block below.
    rules <- Filter(function(r) r$check != "long_rate_loads_on_policy_rate", rules)
    rules <- c(rules, stage2c_sign_rules(iso2))
  }
  if (isTRUE(labour)) rules <- c(rules, stage3a_sign_rules(iso2))
  # Under the external block exports load on the competitiveness difference
  # rather than the two separate prices, so those two stage-3a rules no longer
  # have terms to evaluate. Drop them rather than report permanent failures.
  if (isTRUE(external)) {
    superseded <- c("exports_fall_in_own_price", "exports_rise_in_competitor_price")
    rules <- Filter(function(r) !r$check %in% superseded, rules)
    rules <- c(rules, external_sign_rules(iso2))
  }
  if (isTRUE(fiscal)) rules <- c(rules, fiscal_sign_rules(iso2))
  if (isTRUE(financial)) {
    # Under the financial block the long rate is an identity over the spread and
    # the policy rate, so the policy-rate loading is IMPOSED at 1 rather than
    # estimated -- there is no coefficient left to check the sign of.
    rules <- Filter(function(r) r$check != "long_rate_loads_on_policy_rate", rules)
    rules <- c(rules, financial_sign_rules(iso2))
  }

  find_estimate <- function(eq, term) {
    row <- coef_table[coef_table$equation == eq & coef_table$term == term, ]
    if (nrow(row) == 0) NA_real_ else row$estimate[1]
  }

  rows <- lapply(rules, function(r) {
    est <- find_estimate(r$equation, r$term)
    data.frame(
      check = r$check, equation = r$equation, term = r$term,
      estimate = est, expected = r$expected, ok = isTRUE(r$test(est)),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' The stage-1/2 sign rules
#'
#' One list entry per check: `check`, `equation`, `term`, a human-readable
#' `expected`, and a `test` predicate. Split out from [sign_checks()] so the
#' stage-3a rules can extend the set without touching the original three.
#' @keywords internal
base_sign_rules <- function(iso2) {
  v <- function(concept) country_var(iso2, concept)
  policy_var <- if (identical(iso2, "us")) "us_policy_rate" else "ea_policy_rate"
  list(
    list(check = "mpc_in_0_1", equation = v("consumption"), term = v("gdp"),
         expected = "in (0, 1)", test = function(x) x > 0 && x < 1),
    list(check = "import_elasticity_positive", equation = v("imports"),
         term = v("domestic_demand"), expected = "> 0", test = function(x) x > 0),
    list(check = "long_rate_loads_on_policy_rate", equation = v("long_rate"),
         term = policy_var, expected = "> 0", test = function(x) x > 0)
  )
}

#' Sign rules for the stage-2c refinements
#'
#' Four testable claims, one per refinement that introduces a coefficient
#' (the `foreign_demand` re-weighting introduces none -- it changes an
#' identity's components, whose weights are fixed trade shares, not
#' estimates).
#'
#' - **`phillips_curve_positive`**: the price equation's loading on GDP must
#'   be positive -- faster growth, faster price growth. This is the
#'   refinement stage 2c exists for, so a negative value is a red flag rather
#'   than a result: it would say the fitted system believes demand is
#'   *dis*inflationary, and it would invert the monetary loop the refinement
#'   is meant to close (the ECB would need to *cut* to fight inflation).
#' - **`consumption_rate_channel_negative`**: consumption's loading on the
#'   long rate must be negative -- intertemporal substitution. A positive
#'   value would mean higher rates raise consumption, reversing the second
#'   monetary channel this refinement adds.
#' - **`import_content_positive`**: imports' loading on exports must be
#'   positive -- exporting more requires importing more intermediates. Note
#'   this is a *volume* claim; it is not the import-*price* term rejected
#'   four times in stage 3a.
#' - **`spread_loads_on_prices_positive`**: the spread's loading on prices
#'   must be positive -- an inflation risk premium. This is the weakest of
#'   the four a priori (a flight-to-quality episode can compress spreads
#'   while inflation rises), so treat a failure here as informative rather
#'   than disqualifying.
#'
#' Note what is deliberately **absent**: `long_rate_loads_on_policy_rate`.
#' [sign_checks()] drops it under `stage2c = TRUE`, because the stage-2c
#' `long_rate` is an identity whose policy-rate weight is imposed at exactly
#' 1. Leaving it in would report a permanent, meaningless failure.
#'
#' @param iso2 Two-letter lowercase ISO country code.
#'
#' @return A list of rule entries in [base_sign_rules()]'s shape.
#' @keywords internal
stage2c_sign_rules <- function(iso2) {
  v <- function(concept) country_var(iso2, concept)
  list(
    list(check = "phillips_curve_positive", equation = v("prices"), term = v("gdp"),
         expected = "> 0", test = function(x) x > 0),
    list(check = "consumption_rate_channel_negative", equation = v("consumption"),
         term = v("long_rate"), expected = "< 0", test = function(x) x < 0),
    list(check = "import_content_positive", equation = v("imports"),
         term = v("exports"), expected = "> 0", test = function(x) x > 0),
    list(check = "spread_loads_on_prices_positive", equation = v("spread"),
         term = v("prices"), expected = "> 0", test = function(x) x > 0)
  )
}

#' Sign rules for the stage-3a labour and price block
#'
#' The economics each new coefficient is supposed to embody, written down so
#' a wrong sign is reported as a failure rather than presented as a finding.
#' Every one of these is a testable claim about the fitted system, not a
#' constraint imposed on it -- koma estimates them freely.
#'
#' The two that matter most:
#'
#' - **`wage_phillips_curve_negative`**: the wage equation's loading on
#'   unemployment must be negative. A positive coefficient means the fitted
#'   model says wage growth *rises* when unemployment rises, which inverts
#'   the mechanism the whole labour block exists to represent. It is a red
#'   flag, not a result.
#' - **`price_phillips_curve_negative`**: the same claim on the non-energy
#'   price equation.
#'
#' Note two sign conventions that are easy to get backwards:
#' `eur_usd` is quoted **USD per EUR**, so a rise is a euro *appreciation*
#' and must lower euro-denominated import and energy prices -- the expected
#' sign is negative, not positive. And exports fall in their **own** price
#' while rising in competitors' (`foreign_prices`), so those two terms in the
#' same equation carry opposite expected signs.
#'
#' Own-lag persistence is checked separately by [check_lag_stability()],
#' since it is a stability question rather than a sign question.
#'
#' @param iso2 Two-letter lowercase ISO country code.
#' @return A list of rule entries, as [base_sign_rules()].
#' @keywords internal
stage3a_sign_rules <- function(iso2) {
  v <- function(concept) country_var(iso2, concept)
  fx <- "eur_usd"
  pos <- function(x) x > 0
  neg <- function(x) x < 0
  rule <- function(check, equation, term, expected, test) {
    list(check = check, equation = equation, term = term, expected = expected, test = test)
  }
  list(
    rule("okun_employment_positive", v("employment"), v("gdp"), "> 0", pos),
    rule("okun_unemployment_negative", v("unemployment"), v("gdp"), "< 0", neg),
    rule("wage_phillips_curve_negative", v("wages"), v("unemployment"), "< 0", neg),
    rule("wage_price_indexation_in_0_1", v("wages"), v("prices"), "in (0, 1)",
         function(x) x > 0 && x < 1),
    rule("ulc_passthrough_positive", v("nonenergy_prices"), v("ulc"), "> 0", pos),
    rule("import_price_passthrough_positive", v("nonenergy_prices"), v("import_prices"), "> 0", pos),
    rule("price_phillips_curve_negative", v("nonenergy_prices"), v("unemployment"), "< 0", neg),
    rule("energy_prices_load_on_oil", v("energy_prices"), "oil_price", "> 0", pos),
    rule("energy_prices_fall_on_euro_appreciation", v("energy_prices"), fx, "< 0", neg),
    rule("import_prices_fall_on_euro_appreciation", v("import_prices"), fx, "< 0", neg),
    rule("import_prices_load_on_oil", v("import_prices"), "oil_price", "> 0", pos),
    rule("import_prices_load_on_foreign_prices", v("import_prices"), v("foreign_prices"), "> 0", pos),
    rule("export_prices_load_on_ulc", v("export_prices"), v("ulc"), "> 0", pos),
    rule("export_prices_load_on_foreign_prices", v("export_prices"), v("foreign_prices"), "> 0", pos),
    rule("consumption_loads_on_real_income", v("consumption"), v("real_income"), "> 0", pos),
    rule("exports_fall_in_own_price", v("exports"), v("export_prices"), "< 0", neg),
    rule("exports_rise_in_competitor_price", v("exports"), v("foreign_prices"), "> 0", pos)
    # There is deliberately no `imports_fall_in_own_price` rule: the imports
    # equation carries no price term. It was tried four ways and dropped (see
    # [country_block()] and the stage-3a report). Keeping the rule would report
    # a permanent NA/FALSE for a term the specification intentionally omits,
    # which reads as an unfixed failure rather than a settled decision.
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

#' Sign rules for the stage-3b external block
#'
#' `de_imports <- de_terms_of_trade` is the one to watch: it is the fourth
#' specification tried for getting an import price into the volume equation,
#' after three failed in stage 3a. Terms of trade up means imports are cheap
#' relative to exports, so the expected sign is **positive** -- the opposite of
#' the bare own-price term, which is exactly why it is worth testing.
#'
#' `de_exports <- de_competitiveness` replaces the two separate export-price
#' rules: competitiveness is own price minus competitors', so exports should
#' fall in it.
#' @keywords internal
external_sign_rules <- function(iso2) {
  v <- function(concept) country_var(iso2, concept)
  rule <- function(check, equation, term, expected, test) {
    list(check = check, equation = equation, term = term, expected = expected, test = test)
  }
  # There is deliberately no imports rule: terms of trade was the fourth
  # specification tried for an import price in the volume equation and the
  # fourth to fail, so the term was dropped. See country_block().
  list(
    rule("exports_fall_in_competitiveness", v("exports"), v("competitiveness"), "< 0",
         function(x) x < 0),
    rule("current_account_rises_with_exports", v("current_account"), v("exports"), "> 0",
         function(x) x > 0),
    rule("current_account_falls_with_imports", v("current_account"), v("imports"), "< 0",
         function(x) x < 0)
  )
}

#' Sign rules for the stage-3b fiscal block
#'
#' Two claims, both testable:
#'
#' - **Automatic stabilisers**: net borrowing falls when output grows, because
#'   revenue is procyclical and transfers countercyclical. Negative.
#' - **Debt service**: a higher long rate raises the borrowing requirement.
#'   Positive. This is the flow side of the snowball.
#'
#' Government consumption's own rules are omitted deliberately: both its
#' regressors are lagged, so there is no contemporaneous sign to check, and its
#' own-lag persistence is [check_lag_stability()]'s business.
#' @keywords internal
fiscal_sign_rules <- function(iso2) {
  v <- function(concept) country_var(iso2, concept)
  rule <- function(check, equation, term, expected, test) {
    list(check = check, equation = equation, term = term, expected = expected, test = test)
  }
  list(
    rule("automatic_stabilisers_negative", v("netborrowing"), v("gdp"), "< 0",
         function(x) x < 0),
    rule("debt_service_raises_borrowing", v("netborrowing"), v("long_rate"), "> 0",
         function(x) x > 0)
  )
}

#' Sign rules for the stage-3b financial block
#'
#' `de_long_rate <- de_govdebt` is the headline: the sovereign spread widening
#' with the debt ratio is the mechanism behind core-periphery divergence, and
#' the whole reason the fiscal block earns its degrees of freedom. A
#' non-positive coefficient means the channel this block exists to build is not
#' there.
#' @keywords internal
financial_sign_rules <- function(iso2) {
  v <- function(concept) country_var(iso2, concept)
  rule <- function(check, equation, term, expected, test) {
    list(check = check, equation = equation, term = term, expected = expected, test = test)
  }
  list(
    rule("credit_falls_in_long_rate", v("credit"), v("long_rate"), "< 0", function(x) x < 0),
    rule("credit_rises_with_output", v("credit"), v("gdp"), "> 0", function(x) x > 0),
    rule("house_prices_rise_with_credit", v("house_prices"), v("credit"), "> 0",
         function(x) x > 0),
    rule("house_prices_fall_in_long_rate", v("house_prices"), v("long_rate"), "< 0",
         function(x) x < 0),
    rule("house_prices_rise_with_real_income", v("house_prices"), v("real_income"), "> 0",
         function(x) x > 0),
    rule("investment_rises_with_credit", v("investment"), v("credit"), "> 0",
         function(x) x > 0),
    # The headline mechanism, now on the SPREAD rather than the rate level:
    # modelling the level let its own lag run to a unit root and swallow this
    # coefficient (-0.006). See financial_block().
    rule("sovereign_spread_widens_with_debt", v("spread"), v("govdebt"), "> 0",
         function(x) x > 0),
    rule("spread_narrows_with_growth", v("spread"), v("gdp"), "< 0", function(x) x < 0)
  )
}

# --------------------------------------------------------------------------
# Stage-3a diagnostics: lag stability, regressor collinearity, and the
# wage-price loop gain. See reports/stage3a_labour_prices.qmd.
# --------------------------------------------------------------------------

#' Posterior draws of one contemporaneous (gamma) coefficient
#'
#' `coefficient_table()` gives posterior *summaries*; the loop-gain
#' diagnostic needs the draws themselves, so that the share of draws in an
#' explosive region can be counted rather than inferred from a mean.
#'
#' koma stores an equation's contemporaneous endogenous coefficients in
#' `fit$estimates[[eq]]$gamma_jw[[draw]]`, a column vector whose entries line
#' up, **in row order of the system**, with the non-diagonal non-zero entries
#' of that equation's column of `sys_eq$character_gamma_matrix` (the entries
#' rendered `-gammaJ_I`). There is no accessor and no dimnames, so the
#' position has to be recovered from the character matrix -- doing it by
#' assuming an order would silently return a different variable's
#' coefficient.
#'
#' @param fit A `koma::koma_estimate`.
#' @param equation Dependent variable name.
#' @param term A contemporaneous endogenous regressor in that equation.
#'
#' @return Numeric vector, one posterior draw per element.
#' @export
gamma_draws <- function(fit, equation, term) {
  g <- fit$sys_eq$character_gamma_matrix
  if (!equation %in% colnames(g)) {
    cli::cli_abort("{.val {equation}} is not an equation in this system.")
  }
  col <- g[, equation]
  # The diagonal "1" is the dependent variable itself and has no drawn
  # coefficient; everything else non-empty is a gamma parameter.
  regressors <- rownames(g)[nzchar(col) & col != "0" & col != "1"]
  pos <- match(term, regressors)
  if (is.na(pos)) {
    cli::cli_abort(c(
      "{.val {term}} is not a contemporaneous endogenous regressor in {.val {equation}}.",
      "i" = if (length(regressors)) "Available: {.val {regressors}}." else "That equation has none."
    ))
  }
  draws <- fit$estimates[[equation]]$gamma_jw
  vapply(draws, function(d) as.numeric(d)[pos], numeric(1))
}

#' Own-lag stability of every equation
#'
#' An equation whose own-lag coefficient reaches 1 in absolute value has a
#' unit or explosive root, and koma's Gibbs sampler places **no stationarity
#' constraint on draws** -- the sampler will happily return them (see
#' `R/spillovers.R`, where an increasing share of explosive draws is the
#' central obstacle to interpreting any forecast difference). Stage 3a adds
#' seven equations per labour country and cuts the residual degrees of
#' freedom, both of which make this more likely, so it is worth checking
#' directly rather than inferring from downstream symptoms.
#'
#' @param coef_table A `data.frame` from [coefficient_table()].
#' @param threshold Absolute value at or above which an own lag is flagged.
#'
#' @return A `data.frame` with columns `equation`, `term`, `estimate`,
#'   `ci_high`, `flagged`, ordered most-persistent first.
#' @export
check_lag_stability <- function(coef_table, threshold = 1) {
  lags <- coef_table[grepl("\\.L\\(1\\)$", coef_table$term), ]
  own <- lags[sub("\\.L\\(1\\)$", "", lags$term) == lags$equation, ]
  if (nrow(own) == 0) {
    return(data.frame(
      equation = character(0), term = character(0), estimate = numeric(0),
      ci_high = numeric(0), flagged = logical(0), stringsAsFactors = FALSE
    ))
  }
  out <- data.frame(
    equation = own$equation, term = own$term, estimate = own$estimate,
    ci_high = own$ci_high, flagged = abs(own$estimate) >= threshold,
    stringsAsFactors = FALSE
  )
  out[order(-abs(out$estimate)), ]
}

#' Collinearity among an equation's regressors, before estimating
#'
#' The stage-3a price equation takes both `<iso2>_ulc` and
#' `<iso2>_unemployment`, and ULC is partly a function of unemployment by
#' construction (`ulc == wages - productivity`, and wages responds to
#' unemployment). That is **weak identification, not rank failure** -- the
#' coefficients stay estimable but trade off against each other -- and it
#' does not show up in `koma::model_identification()`, which is symbolic and
#' sees only which variables are excluded, never how correlated the included
#' ones are.
#'
#' This measures it directly from the data, in the **rate space koma actually
#' estimates in** (`koma::rate()`, not levels), so it can be run before
#' committing to an estimation. A variance inflation factor above ~10 is the
#' conventional threshold for "this coefficient is not separately identified
#' in practice".
#'
#' @param panel A named list of `koma_ts`, the panel the system will use.
#' @param sys_eq A `koma_seq`.
#' @param dates A koma `dates` list; regressors are windowed to
#'   `dates$estimation`. `NULL` uses each series' full overlap.
#' @param equations Character vector of equations to check. `NULL` checks
#'   every stochastic equation with at least two non-lag regressors.
#'
#' @return A `data.frame` with columns `equation`, `term`, `vif`,
#'   `max_abs_cor`, `worst_partner`, `flagged`, ordered worst-first. An
#'   equation whose regressors are not all in the panel is skipped with a
#'   warning rather than aborting the whole report.
#' @export
block_collinearity <- function(panel, sys_eq, dates = NULL, equations = NULL) {
  stochastic <- setdiff(sys_eq$endogenous_variables, names(sys_eq$identities))
  if (is.null(equations)) equations <- stochastic
  equations <- intersect(equations, stochastic)

  g <- sys_eq$character_gamma_matrix
  b <- sys_eq$character_beta_matrix

  as_rate <- function(x) {
    r <- as.numeric(koma::rate(x))
    stats::ts(r, end = stats::end(x), frequency = stats::frequency(x))
  }

  rows <- lapply(equations, function(eq) {
    gcol <- g[, eq]
    endo <- rownames(g)[nzchar(gcol) & gcol != "0" & gcol != "1"]
    bcol <- if (eq %in% colnames(b)) b[, eq] else character()
    exo <- if (length(bcol)) rownames(b)[nzchar(bcol) & bcol != "0"] else character()
    # Lags and the intercept are not the collinearity question here; the
    # concern is contemporaneous regressors that duplicate one another.
    exo <- exo[!grepl("\\.L\\(", exo) & exo != "constant"]
    terms <- c(endo, exo)
    if (length(terms) < 2) return(NULL)

    missing <- setdiff(terms, names(panel))
    if (length(missing) > 0) {
      cli::cli_warn("Skipping {.val {eq}}: panel has no {.val {missing}}.")
      return(NULL)
    }

    series <- lapply(panel[terms], as_rate)
    if (!is.null(dates)) {
      series <- lapply(series, function(x) {
        stats::window(x, start = dates$estimation$start, end = dates$estimation$end,
                      extend = TRUE)
      })
    }
    m <- stats::na.omit(do.call(cbind, series))
    colnames(m) <- terms
    if (nrow(m) <= length(terms) + 1) {
      cli::cli_warn("Skipping {.val {eq}}: only {nrow(m)} usable observations for {length(terms)} regressors.")
      return(NULL)
    }

    cm <- stats::cor(m)
    vif <- vapply(seq_along(terms), function(i) {
      fit <- stats::lm.fit(cbind(1, m[, -i, drop = FALSE]), m[, i])
      rss <- sum(fit$residuals^2)
      tss <- sum((m[, i] - mean(m[, i]))^2)
      if (tss <= 0 || rss <= 0) return(Inf)
      1 / (1 - (1 - rss / tss))
    }, numeric(1))

    off <- cm
    diag(off) <- 0
    data.frame(
      equation = eq, term = terms, vif = vif,
      max_abs_cor = apply(abs(off), 1, max),
      worst_partner = terms[apply(abs(off), 1, which.max)],
      flagged = vif > 10,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(out)) {
    return(data.frame(
      equation = character(0), term = character(0), vif = numeric(0),
      max_abs_cor = numeric(0), worst_partner = character(0),
      flagged = logical(0), stringsAsFactors = FALSE
    ))
  }
  rownames(out) <- NULL
  out[order(-out$vif), ]
}

#' Posterior distribution of the wage-price loop gain
#'
#' Stage 3a introduces a **contemporaneous feedback loop** that no earlier
#' stage had:
#'
#' ```
#' wages -> ulc -> nonenergy_prices -> prices -> wages
#' ```
#'
#' Two of those links are identities with unit weights (`ulc == wages -
#' productivity`) or known weights (`prices == w_xnrg*nonenergy_prices +
#' ...`), and two are estimated. The round-trip gain is therefore
#'
#' ```
#' gain = beta(nonenergy_prices <- ulc) * w_xnrg * beta(wages <- prices)
#' ```
#'
#' The system is solvable iff `(I - Gamma)` is invertible, and a draw with
#' `|gain| >= 1` is one where a wage rise more than pays for itself through
#' prices -- a self-sustaining spiral. At plausible values the gain is well
#' below 1, but koma constrains no draw to be stationary, so what matters is
#' not the mean gain but **the share of the posterior above 1**. That share
#' is the quantity to watch when the residual degrees of freedom are thin,
#' and it is the mechanism by which stage 3a could make the explosive-draw
#' problem in `R/spillovers.R` worse.
#'
#' @param fit A `koma::koma_estimate` containing the labour block.
#' @param iso2 Two-letter lowercase ISO country code.
#' @param hicp_weight The non-energy weight in the `prices` identity, i.e.
#'   `hicp_weights(...)$weights[["nonenergy_prices"]]`.
#'
#' @return A list with `draws` (the per-draw gain), `mean`, `median`,
#'   `q05`, `q95`, `share_ge_1` and the two component coefficient vectors.
#' @export
wage_price_loop_gain <- function(fit, iso2, hicp_weight) {
  iso2 <- tolower(iso2)
  v <- function(concept) country_var(iso2, concept)
  ulc_passthrough <- gamma_draws(fit, v("nonenergy_prices"), v("ulc"))
  indexation <- gamma_draws(fit, v("wages"), v("prices"))
  gain <- ulc_passthrough * hicp_weight * indexation
  list(
    draws = gain,
    mean = mean(gain), median = stats::median(gain),
    q05 = unname(stats::quantile(gain, 0.05)),
    q95 = unname(stats::quantile(gain, 0.95)),
    share_ge_1 = mean(abs(gain) >= 1),
    ulc_passthrough = ulc_passthrough,
    indexation = indexation
  )
}

#' Tune per-equation `tau` for a whole stage-2/3a system
#'
#' The system-level analogue of [tune_tau()], which is stage-1 shaped (it
#' takes an `iso2` and rebuilds one country's equations). Same doubling rule:
#' an equation whose Metropolis acceptance rate sits above the band gets its
#' `tau` doubled, one below gets it halved, and the system is re-estimated.
#'
#' **Why a stage-2 system needs this even when its equations are unchanged.**
#' Acceptance rates are not a per-equation property in a simultaneous system:
#' the sampler draws a system-wide residual covariance, so adding equations
#' anywhere shifts every equation's acceptance rate. Stage 2b converged on
#' `tau = 2.2` for fourteen equations sitting just above 60%; adding the
#' stage-3a block moves that boundary again and re-tunes from the same rule
#' rather than inheriting stage 2b's answer. Reporting an untuned stage-3a
#' fit against a *tuned* stage-2b baseline would blame the labour block for
#' flags that are really the missing tuning.
#'
#' @param spec A merged spec (`list(stochastic, identities)`), as from
#'   [stage2_spec()].
#' @param panel A stage-2 panel, as from [build_stage2_panel()].
#' @param dates A koma `dates` list.
#' @param band Target acceptance band, matching [check_acceptance_rates()].
#' @param max_iter Maximum re-estimation rounds.
#' @param factor Multiplier applied to a flagged equation's `tau`.
#' @param tau Optional starting `tau` vector, e.g. a previous stage's result.
#' @param ... Passed to [fit_stage2()] (notably `workers`).
#'
#' @return A list with `fit`, `sys_eq`, `tau`, `history` (acceptance rates by
#'   iteration) and `converged`.
#' @export
tune_tau_system <- function(spec, panel, dates, band = c(0.2, 0.6), max_iter = 3,
                            factor = 2, tau = NULL, ...) {
  tau <- if (is.null(tau)) list() else as.list(tau)
  history <- list()
  fit <- NULL
  sys_eq <- NULL
  flagged <- data.frame()

  for (iteration in 0:max_iter) {
    tau_arg <- if (length(tau) > 0) unlist(tau) else NULL
    sys_eq <- build_stage2_system(spec, tau = tau_arg)
    fit <- fit_stage2(sys_eq, panel, dates, ...)

    acceptance <- check_acceptance_rates(fit, band = band)
    acceptance$iteration <- iteration
    acceptance$tau <- vapply(acceptance$equation, function(e) tau[[e]] %||% 1.1, numeric(1))
    history[[length(history) + 1]] <- acceptance

    flagged <- acceptance[acceptance$flagged %in% TRUE, ]
    cli::cli_inform("tau iteration {iteration}: {nrow(flagged)} equation{?s} outside {band[1]*100}-{band[2]*100}%.")
    if (nrow(flagged) == 0 || iteration == max_iter) break

    for (i in seq_len(nrow(flagged))) {
      eq <- flagged$equation[i]
      current <- tau[[eq]] %||% 1.1
      tau[[eq]] <- if (flagged$acceptance_rate[i] > band[2]) current * factor else current / factor
    }
  }

  list(
    fit = fit, sys_eq = sys_eq,
    tau = if (length(tau) > 0) unlist(tau) else stats::setNames(numeric(0), character(0)),
    history = do.call(rbind, history),
    converged = nrow(flagged) == 0
  )
}

#' Posterior gain of an arbitrary contemporaneous loop
#'
#' The general form of [wage_price_loop_gain()]. A contemporaneous cycle in the
#' gamma matrix is solvable only if `(I - Gamma)` is invertible, and a draw
#' whose round-trip gain reaches 1 is one where a shock more than pays for
#' itself going round the loop -- self-sustaining. koma constrains no draw to be
#' stationary, so the quantity that matters is the **share of the posterior at
#' or above 1**, not the average gain.
#'
#' Stage 3b adds a loop that did not exist before:
#'
#' ```
#' gdp -> netborrowing -> govdebt -> long_rate -> investment -> gdp
#' ```
#'
#' with a near-unit-root stock (`govdebt`) inside it, in a system whose largest
#' own lag is already 0.9999. That combination is the most likely way this stage
#' destabilises, which is why it gets measured rather than assumed.
#'
#' @param fit A `koma::koma_estimate`.
#' @param path A named list defining the cycle. Each element is either
#'   `list(equation =, term =)` for an estimated link, whose posterior draws are
#'   read with [gamma_draws()], or a bare numeric for a known identity weight
#'   (a `+/-1` accounting link, or an identity's fixed share). The gain is the
#'   product across the whole path.
#'
#' @return A list with `draws` (per-draw gain), `mean`, `median`, `q05`, `q95`,
#'   `share_ge_1`, and `links` (each link's own posterior mean, so a loop that
#'   is large can be attributed to the link responsible).
#' @export
loop_gain <- function(fit, path) {
  if (length(path) == 0) {
    cli::cli_abort("{.arg path} is empty; a loop needs at least one link.")
  }
  draws <- lapply(path, function(link) {
    if (is.numeric(link)) return(link)
    if (!is.list(link) || !all(c("equation", "term") %in% names(link))) {
      cli::cli_abort("Each {.arg path} element must be a number or {.code list(equation =, term =)}.")
    }
    gamma_draws(fit, link$equation, link$term)
  })

  gain <- Reduce(`*`, draws)
  link_means <- vapply(draws, function(d) mean(d), numeric(1))
  names(link_means) <- names(path) %||% seq_along(path)

  list(
    draws = gain,
    mean = mean(gain), median = stats::median(gain),
    q05 = unname(stats::quantile(gain, 0.05)),
    q95 = unname(stats::quantile(gain, 0.95)),
    share_ge_1 = mean(abs(gain) >= 1),
    links = link_means
  )
}

#' The stage-3b fiscal-financial loop, as a [loop_gain()] path
#'
#' `gdp -> netborrowing -> govdebt -> long_rate -> investment -> gdp`. Two links
#' are identity weights rather than estimated coefficients: `govdebt` takes net
#' borrowing with weight 1 (the accumulation identity), and investment reaches
#' GDP through the domestic-demand and GDP identities, whose weights come from
#' `expenditure_shares()` and must be supplied.
#'
#' @param iso2 Two-letter lowercase ISO country code.
#' @param investment_to_gdp The product of the domestic-demand weight on
#'   investment and the GDP weight on domestic demand, from
#'   [expenditure_shares()].
#' @return A `path` list for [loop_gain()].
#' @export
fiscal_financial_loop <- function(iso2, investment_to_gdp) {
  v <- function(concept) country_var(iso2, concept)
  list(
    `netborrowing <- gdp` = list(equation = v("netborrowing"), term = v("gdp")),
    `govdebt <- netborrowing (identity)` = 1,
    `spread <- govdebt` = list(equation = v("spread"), term = v("govdebt")),
    `long_rate <- spread (identity)` = 1,
    `investment <- long_rate` = list(equation = v("investment"), term = v("long_rate")),
    `gdp <- investment (identities)` = investment_to_gdp
  )
}
