# Cross-country spillover analysis for the fitted stage-2 system.
#
# koma has no impulse-response helper, so spillovers are built the only way
# the package supports: as the DIFFERENCE between two conditional forecasts
# from koma::forecast(restrictions = ) -- one unconditional (baseline) and
# one that pins a shocked variable's path (scenario). See scenario_diff().

#' Add `n` quarters (or periods) to a `c(year, period)` date
#'
#' The inverse of [prev_period()], generalised to `n` steps.
#' @keywords internal
advance_periods <- function(yq, n, frequency = 4) {
  total <- (yq[1] * frequency + (yq[2] - 1)) + n
  year <- total %/% frequency
  period <- total %% frequency + 1
  c(year, period)
}

#' Extend a fit's exogenous series far enough to forecast `quarters` ahead
#'
#' `koma::forecast()` silently **shortens the horizon** when an exogenous
#' series does not reach `dates$forecast$end` -- `forecast_draw()` resets
#' `horizon <- nrow(na.omit(forecast_x_matrix))` and only warns. Requesting
#' an 8-quarter horizon against a panel whose exogenous series end at the
#' fit's native 5-quarter forecast end (`row_gdp`, `oil_price`, `eur_usd` all
#' end 2026Q1, `dates$forecast$end` in `stage2b_dates()`) would therefore
#' return 5 quarters while claiming to have honoured 8. This extends them
#' first, so the requested horizon is actually delivered.
#'
#' **Extrapolation rule, stated plainly because it is an assumption, not a
#' fact**: a level exogenous series (`row_gdp`, `oil_price`, `eur_usd` in
#' this project) is extended by carrying forward the **average of its last 4
#' observed quarterly `diff_log` growth rates** -- a trailing-momentum
#' continuation, not a forecast of oil markets or world trade. A rate/none
#' exogenous (were one ever declared exogenous) is held flat at its recent
#' average instead -- there is no growth rate to extrapolate, and treating
#' its level as a `diff_log` rate would compound it exponentially. COVID
#' dummies extend as zeros (they are 0 already for the whole 2025-2026
#' forecast window).
#'
#' @param fit A `koma::koma_estimate` object (e.g. from [fit_stage2()]).
#' @param panel Named list of `koma_ts`, the **level** panel the fit's
#'   `ts_data` was built from (identity-derived series like `ea_gdp` are not
#'   needed here since they are endogenous, not exogenous).
#' @param quarters Total forecast horizon, in quarters, measured from
#'   `fit$dates$forecast$start`.
#'
#' @return A list with `fit` (a copy with extended `ts_data`), `dates` (a
#'   copy of `fit$dates` with `forecast$end` advanced), `panel` (a copy of
#'   `panel` with the extended **level** series for the exogenous variables
#'   that needed it -- what [shock_exogenous_level()] needs for an
#'   exogenous-variable scenario over the extended horizon), and
#'   `extension`, a `data.frame` recording what was done to each exogenous
#'   variable, for transparency in a report.
#' @export
extend_forecast_horizon <- function(fit, panel, quarters) {
  frequency <- stats::frequency(fit$ts_data[[1]])
  dates <- fit$dates
  native_horizon <- length(seq(
    dates$forecast$start[1] + (dates$forecast$start[2] - 1) / frequency,
    dates$forecast$end[1] + (dates$forecast$end[2] - 1) / frequency,
    by = 1 / frequency
  ))
  extra <- quarters - native_horizon
  if (extra <= 0) {
    dates$forecast$end <- advance_periods(dates$forecast$start, quarters - 1, frequency)
    return(list(fit = fit, dates = dates, panel = panel, extension = data.frame(
      variable = character(0), method = character(0), stringsAsFactors = FALSE
    )))
  }

  out <- fit
  out_panel <- panel
  log <- lapply(fit$sys_eq$exogenous_variables, function(v) {
    if (grepl("^covid_", v)) {
      x <- fit$ts_data[[v]]
      extended <- stats::ts(c(as.numeric(x), rep(0, extra)),
        start = stats::start(x), frequency = frequency
      )
      out$ts_data[[v]] <<- do.call(koma::as_ets, c(list(extended), get_custom_attrs(x)))
      return(data.frame(variable = v, method = "zero (dummy)", periods_added = extra, stringsAsFactors = FALSE))
    }

    level <- panel[[v]]
    if (is.null(level)) {
      cli::cli_abort("{.arg panel} is missing {.val {v}}, needed to extend the forecast horizon.")
    }

    if (identical(attr(level, "series_type"), "rate")) {
      # A rate/none series (e.g. a policy rate, if one is ever exogenous):
      # there is no growth rate to extrapolate -- rate() on it is a no-op,
      # and treating its LEVEL as a diff_log growth rate would compound it
      # exponentially. Hold it flat at its recent average instead.
      flat <- mean(utils::tail(as.numeric(level), 4))
      extended_ts <- stats::ts(c(as.numeric(level), rep(flat, extra)),
        start = stats::start(level), frequency = frequency
      )
      out$ts_data[[v]] <<- do.call(koma::as_ets, c(list(extended_ts), get_custom_attrs(fit$ts_data[[v]])))
      return(data.frame(
        variable = v, method = sprintf("flat at trailing-4Q average (%.2f)", flat),
        periods_added = extra, stringsAsFactors = FALSE
      ))
    }

    growth <- as.numeric(koma::rate(level)) # 100 * diff(log(level)), matches koma's own diff_log
    avg_growth <- mean(utils::tail(growth, 4))
    last_level <- as.numeric(utils::tail(level, 1))
    extended_levels <- last_level * exp(cumsum(rep(avg_growth / 100, extra)))

    extended_level_ts <- stats::ts(c(as.numeric(level), extended_levels),
      start = stats::start(level), frequency = frequency
    )
    extended_level_ets <- do.call(koma::as_ets, c(list(extended_level_ts), get_custom_attrs(level)))
    extended_rate <- koma::rate(extended_level_ets)

    out$ts_data[[v]] <<- do.call(koma::as_ets, c(list(extended_rate), get_custom_attrs(fit$ts_data[[v]])))
    out_panel[[v]] <<- extended_level_ets
    data.frame(
      variable = v, method = sprintf("trailing-4Q avg growth (%.2f%%/q)", avg_growth),
      periods_added = extra, stringsAsFactors = FALSE
    )
  })

  dates$forecast$end <- advance_periods(dates$forecast$start, quarters - 1, frequency)
  list(fit = out, dates = dates, panel = out_panel, extension = do.call(rbind, log))
}

#' Difference two conditional forecasts, with credible bands
#'
#' The core spillover primitive. Runs `koma::forecast()` twice -- once under
#' `baseline_restrictions` (`NULL` by default, i.e. unconditional) and once
#' under `restrictions` -- and returns, for every endogenous variable and
#' horizon, the distribution of `scenario - baseline`.
#'
#' **Matched draws, not independent ones.** koma's stochastic forecasts are
#' famously unstable call-to-call: two identical `forecast()` calls on the
#' same fit gave `de_gdp` means differing by an order of magnitude more than
#' any real scenario effect (see [conditional_forecast_check()]'s roxygen).
#' Differencing two *independent* stochastic forecasts would inherit that
#' noise on both legs and swamp any real spillover.
#'
#' The fix is common random numbers. `koma::forecast()`'s posterior
#' coefficients for draw `i` are read deterministically from
#' `fit$estimates[[eq]]$beta_jw[[i]]` etc. -- no RNG involved. The only
#' randomness is the future-innovation draw `z_matrix <- matrix(rnorm(...))`
#' inside `forecast_draw()` (koma source, `forecast_sem.R`), drawn **before**
#' the restriction branch and consumed identically whether or not
#' restrictions are present -- restrictions only *add* a conditional
#' correction on top. So calling `set.seed(seed)` immediately before each
#' top-level `forecast()` call reproduces the identical innovation draw for
#' draw index `i` in both calls, and the difference at that index isolates
#' the model's response to the restriction. **Verified**: for a variable
#' structurally unrelated to the shocked one, the matched-seed diff was
#' *exactly* zero at every draw; an unmatched-seed diff on the same pair had
#' sd around 1 -- nine orders of magnitude larger, for a quantity that should
#' be zero. See `data/cache/spillovers/` scratch notes for the check.
#'
#' **Alignment guard.** koma drops any draw whose forecast call errors
#' (`purrr::safely`), *by subsetting the list* -- so `fc$forecasts[[i]]`
#' after a drop no longer corresponds to posterior draw `i`. If baseline and
#' scenario drop a different set of draws, pairing by list position would
#' silently mismatch. This aborts rather than risk that if the two calls
#' return different draw counts.
#'
#' **Exogenous-variable scenarios** (e.g. an oil-price shock) cannot use
#' `restrictions` at all -- koma's restriction mechanism only targets
#' `sys_eq$endogenous_variables`, since it conditions the *reduced-form
#' innovations*, and an exogenous variable's future path has no innovation
#' to condition. Those scenarios instead pass a **different `fit`** for the
#' scenario leg via `scenario_fit` -- typically the output of
#' [shock_exogenous_level()] -- with `restrictions = NULL`. The common
#' random-numbers argument still holds: `z_matrix`'s size depends only on
#' `horizon` and the number of endogenous variables, not on `ts_data`'s
#' content, so pairing works identically whether the two legs differ by a
#' restriction or by a different exogenous path.
#'
#' @param fit A `koma::koma_estimate` object, typically pre-extended via
#'   [extend_forecast_horizon()] if `horizon` exceeds its native support.
#'   Used for the baseline leg, and for the scenario leg unless
#'   `scenario_fit` is given.
#' @param restrictions A koma `restrictions` list (see `?koma::forecast`),
#'   or `NULL` for an exogenous-variable scenario driven entirely by
#'   `scenario_fit`.
#' @param baseline_restrictions Restrictions for the baseline leg. `NULL`
#'   (the default) is the ordinary unconditional forecast.
#' @param horizon Forecast horizon, in quarters, from `fit$dates$forecast$start`.
#' @param scenario_fit Optional alternate `koma::koma_estimate` for the
#'   scenario leg (baseline always uses `fit`). Use this for a shock to an
#'   *exogenous* variable, where there is no restriction to impose --
#'   `restrictions` should be `NULL` in that case.
#' @param probs Quantile probabilities for the *level* forecasts koma
#'   returns (passed through to `options$probs`); the diff's own credible
#'   band is always the empirical 5th/95th percentile of the per-draw diffs,
#'   regardless of `probs`.
#' @param seed Seed set immediately before each `forecast()` call. Must be
#'   identical across the baseline and scenario legs to pair them -- change
#'   it only to check sensitivity, never to "retry" a scenario.
#' @param workers If not `NULL`, sets a `future` plan with this many workers
#'   for koma's per-draw fan-out (see [warn_if_blas_threaded()]).
#' @param baseline_forecast An already-computed baseline `koma_forecast`
#'   (e.g. from a previous [scenario_diff()] call's `attr(, "baseline_forecast")`),
#'   reused instead of recomputing -- the unconditional baseline is identical
#'   across every scenario sharing the same `fit`/`horizon`/`seed`, so a
#'   battery of scenarios only needs to compute it once. Must have been
#'   produced with the same `seed`; mismatches abort.
#' @param drop_baseline_draws Integer vector of positions into
#'   `baseline_forecast$forecasts` (1-indexed, into the **unsubsetted**
#'   original `nsave` draws) to remove before pairing with the scenario leg.
#'   `NULL`/`integer(0)` (the default) drops nothing. Use this **only** when
#'   you have independently verified, out of band, that these are exactly
#'   the draw indices koma's restricted solve fails on for the scenario
#'   leg's `restrictions` -- e.g. a sustained multi-quarter restriction can
#'   make `R \%*\% Omega \%*\% t(R)` (the restricted-innovation covariance)
#'   singular for a specific draw's posterior `Omega`, and koma's `safely()`
#'   wrapper drops that draw from `scenario_forecast$forecasts` without
#'   recording which original index it was (see `?scenario_diff`'s "Draw
#'   count mismatches" section) -- you have to re-derive the indices
#'   yourself (e.g. by tracing `koma:::forecast_draw()`) and pass them here.
#'   This is a **documented, explicit exception** to the draw-count-mismatch
#'   abort below, not a way to route around it silently: passing the wrong
#'   indices reintroduces exactly the mispairing that abort exists to catch.
#' @param explosive_threshold A draw is flagged **explosive** at a given
#'   `(variable, horizon)` if either its baseline or its scenario forecast
#'   value there exceeds this in absolute value. koma's Gibbs sampler places
#'   no stationarity constraint on a draw's coefficients, and on a
#'   heavily-parameterised system (the tuned stage-2b system is 103 equations
#'   near the `k < T` wall documented in `CLAUDE.md`) a real fraction of
#'   posterior draws are dynamically explosive -- verified: for the stage-2b
#'   demand-shock battery, the share of explosive draws at `de_gdp` climbs
#'   from 4.8% at horizon 1 to 56.8% at horizon 8, and an explosive draw is
#'   explosive system-wide (one bad draw sends ~90% of all 103 variables to
#'   extreme magnitudes simultaneously, not just the shocked one) -- this is
#'   a property of the fitted system's posterior, not a bug in this
#'   function. `100` (percentage points of quarterly `diff_log` growth, or
#'   of a rate/none level) is deliberately far above any economically
#'   plausible response, so it only catches genuine numerical blow-ups.
#'
#' @return A `data.frame` with columns `variable`, `horizon`, `median_diff`
#'   (the **robust** point estimate -- see Explosive draws below),
#'   `mean_diff`, `ci_low`, `ci_high` (5th/95th percentile of the per-draw
#'   diff), `sd_diff`, and `explosive_frac` (share of draws flagged per
#'   `explosive_threshold`). `attr(, "baseline_forecast")` carries the
#'   baseline `koma_forecast` for reuse in the next call.
#'
#' @section Explosive draws:
#' `mean_diff`/`sd_diff` are the ordinary moments of the per-draw diff and
#' are **not robust** to explosive draws -- a single such draw can be many
#' orders of magnitude larger than every well-behaved draw combined, so
#' `mean_diff` can swing from small and sensible to astronomical from one
#' horizon to the next as the explosive share grows, even though the bulk of
#' the posterior is stable throughout. `median_diff` and `ci_low`/`ci_high`
#' (empirical percentiles) are unaffected by this by construction and are
#' the statistics [spillover_matrix()] and [spillover_sanity_checks()] use.
#' Always check `explosive_frac` before trusting `mean_diff`/`sd_diff` at a
#' given horizon; do not silently drop explosive draws, since which draws
#' are explosive can itself be diagnostic of a specification problem.
#'
#' @section Draw count mismatches:
#' A restriction can make koma's per-draw solve fail outright for some
#' draws, rather than merely returning an extreme value -- verified for the
#' stage-2b system's sustained `ea_policy_rate` +100bp/4-quarter shock,
#' where 38 of 1000 draws fail with `A = R \%*\% Omega \%*\% t(R) is
#' singular`: restricting 4 horizons at once requires inverting a 4x4
#' matrix built from that draw's posterior innovation covariance, and for
#' a draw whose covariance is (numerically) rank-deficient there, no valid
#' conditional innovation exists. All 38 were independently confirmed to
#' also be flagged **explosive** in the unconditional baseline -- the same
#' population of poorly-behaved draws surfaces as an outright failure under
#' a demanding restriction and as a numerical blow-up without one. koma's
#' `safely()` wrapper drops these from `scenario_forecast$forecasts` by
#' subsetting, without recording which original indices they were, so this
#' function cannot realign automatically -- it aborts by default (below),
#' and only proceeds if the caller supplies the verified indices via
#' `drop_baseline_draws`.
#'
#' @export
scenario_diff <- function(fit, restrictions, baseline_restrictions = NULL,
                          horizon, scenario_fit = NULL, probs = c(0.05, 0.95),
                          seed = 20240101, workers = NULL, baseline_forecast = NULL,
                          drop_baseline_draws = NULL, explosive_threshold = 100) {
  dates <- fit$dates
  dates$forecast$end <- advance_periods(dates$forecast$start, horizon - 1,
                                        stats::frequency(fit$ts_data[[1]]))
  forecast_options <- list(approximate = FALSE, probs = probs)

  if (!is.null(workers)) {
    warn_if_blas_threaded(workers)
    old_plan <- future::plan(stage1_parallel_strategy(), workers = workers)
    on.exit(future::plan(old_plan), add = TRUE)
  }

  if (is.null(baseline_forecast)) {
    set.seed(seed)
    baseline_forecast <- koma::forecast(fit, dates = dates, restrictions = baseline_restrictions,
                                        options = forecast_options)
    attr(baseline_forecast, "scenario_diff_seed") <- seed
  } else if (!identical(attr(baseline_forecast, "scenario_diff_seed"), seed)) {
    cli::cli_abort(c(
      "{.arg baseline_forecast} was computed with a different {.arg seed}.",
      "i" = "Common-random-numbers pairing requires the same seed on both legs."
    ))
  }

  if (length(drop_baseline_draws) > 0) {
    # A local copy only -- does not mutate the caller's baseline_forecast,
    # so it stays reusable (at full draw count) for other scenarios in the
    # same battery. See "Draw count mismatches" above for when this is safe.
    baseline_forecast$forecasts <- baseline_forecast$forecasts[-drop_baseline_draws]
  }

  set.seed(seed)
  scenario_forecast <- koma::forecast(scenario_fit %||% fit, dates = dates, restrictions = restrictions,
                                      options = forecast_options)

  n_base <- length(baseline_forecast$forecasts)
  n_scen <- length(scenario_forecast$forecasts)
  if (n_base != n_scen) {
    cli::cli_abort(c(
      "Baseline and scenario returned different numbers of surviving draws
       ({n_base} vs {n_scen}).",
      "i" = "koma drops failed draws by subsetting the list, which would
             silently mispair the common-random-numbers indices. Investigate
             the restriction rather than differencing anyway."
    ))
  }

  variables <- fit$sys_eq$endogenous_variables
  h <- nrow(baseline_forecast$forecasts[[1]])
  if (h < horizon) {
    cli::cli_warn(c(
      "!" = "Requested {horizon} quarters but koma returned {h}.",
      "i" = "An exogenous series probably does not extend far enough; see {.fn extend_forecast_horizon}."
    ))
  }

  # [draw, horizon, variable] array; diff is scenario - baseline at every
  # (draw, horizon, variable) triple -- cheap once both forecasts exist, no
  # further forecast() calls needed.
  base_arr <- vapply(baseline_forecast$forecasts, function(m) m[, variables, drop = FALSE],
                     matrix(0, h, length(variables)))
  scen_arr <- vapply(scenario_forecast$forecasts, function(m) m[, variables, drop = FALSE],
                     matrix(0, h, length(variables)))
  diff_arr <- scen_arr - base_arr # [horizon, variable, draw]
  explosive_arr <- pmax(abs(base_arr), abs(scen_arr)) > explosive_threshold # [horizon, variable, draw]

  rows <- lapply(seq_along(variables), function(vi) {
    d <- diff_arr[, vi, ]
    e <- explosive_arr[, vi, ]
    if (is.null(dim(d))) { # h == 1 edge case
      d <- matrix(d, nrow = h)
      e <- matrix(e, nrow = h)
    }
    data.frame(
      variable = variables[vi],
      horizon = seq_len(h),
      median_diff = apply(d, 1, stats::median),
      mean_diff = rowMeans(d),
      ci_low = apply(d, 1, stats::quantile, probs = 0.05, names = FALSE),
      ci_high = apply(d, 1, stats::quantile, probs = 0.95, names = FALSE),
      sd_diff = apply(d, 1, stats::sd),
      explosive_frac = rowMeans(e),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  attr(out, "baseline_forecast") <- baseline_forecast
  out
}

#' Build a one-quarter demand-shock restriction
#'
#' `+size` percentage points on `<iso2>_gdp`'s growth rate for the first
#' quarter of the forecast only (`horizon = 1`); GDP evolves endogenously
#' from there under the model's own dynamics.
#'
#' **Units, stated because they are a modelling choice, not a fact.**
#' `restrictions` values are in the same *rate* space koma forecasts in
#' (percent quarterly `diff_log` growth for a level/diff_log variable, exact
#' level for a rate/none variable like a policy rate -- see
#' [shock_exogenous_level()]'s roxygen). "de_gdp +1% for one quarter" is
#' read here as a **one-quarter growth-rate shock of +1 percentage point**,
#' not a permanent +1% level step -- the same convention
#' [conditional_forecast_check()] already uses for its endogenous-variable
#' scenarios (`ea_policy_rate +100bp` is `+1.0` added to the rate path).
#'
#' **Anchored to `baseline_forecast$median`, not `$mean`.** koma's own
#' `$mean` summary is an ordinary cross-draw mean and inherits the same
#' explosive-draw contamination documented in [scenario_diff()]'s "Explosive
#' draws" section -- verified: `nl_gdp` at horizon 1 has `$mean` = +21.2 vs
#' `$median` = -1.3, a 22.5pp gap, from explosive draws already present in
#' the raw baseline **before any shock is applied**. Restriction targets are
#' a single absolute value shared by every draw
#' (`target = baseline_h1 + size`, then `diff_i = target - draw_i`), so
#' anchoring to a badly-off-center `$mean` silently bakes a large,
#' meaningless offset into every draw's diff -- e.g. the (pre-fix)
#' `de_gdp` own-shock diff had `median_diff` = -3.3 at horizon 1 despite the
#' restriction being an exact +1 shock, purely because `$mean` and
#' `$median` disagreed by ~4.3pp. `$median` is unaffected by this by
#' construction (same reasoning as `median_diff` there).
#'
#' @param baseline_forecast A `koma_forecast` (for the variable's baseline
#'   path, which the shock is added to).
#' @param iso2 Two-letter lowercase ISO country code.
#' @param size Shock size in percentage points. Default `1` (a "+1%" shock).
#'
#' @return A koma `restrictions` list with one entry.
#' @export
gdp_demand_shock <- function(baseline_forecast, iso2, size = 1) {
  var <- country_var(iso2, "gdp")
  baseline_h1 <- as.numeric(baseline_forecast$median[[var]])[1]
  stats::setNames(list(list(horizon = 1, value = baseline_h1 + size)), var)
}

#' Build a sustained policy-rate restriction
#'
#' `+size` percentage points on `ea_policy_rate` (or `us_policy_rate`) for
#' the first `quarters` quarters of the forecast, additive in level/rate
#' space (the variable is `series_type = "rate", method = "none"`, so its
#' rate-space and level-space values coincide -- the same convention
#' [conditional_forecast_check()] uses for its `"policy"` scenario).
#'
#' Anchored to `baseline_forecast$median`, not `$mean` -- see
#' [gdp_demand_shock()]'s roxygen for why `$mean` is unsafe here.
#'
#' @param baseline_forecast A `koma_forecast`.
#' @param variable `"ea_policy_rate"` or `"us_policy_rate"`.
#' @param size Shock size in percentage points (100bp = `1`).
#' @param quarters Number of quarters the shock is held for.
#'
#' @return A koma `restrictions` list with one entry.
#' @export
policy_rate_shock <- function(baseline_forecast, variable = "ea_policy_rate",
                              size = 1, quarters = 4) {
  baseline_path <- as.numeric(baseline_forecast$median[[variable]])[seq_len(quarters)]
  stats::setNames(list(list(horizon = seq_len(quarters), value = baseline_path + size)), variable)
}

#' Build a shocked fit for a sustained oil-price scenario
#'
#' `oil_price` is **exogenous**, so it cannot be conditioned on via
#' `restrictions` (see [scenario_diff()]'s roxygen) -- instead this returns a
#' whole shocked copy of `fit` for use as [scenario_diff()]'s `scenario_fit`,
#' via [shock_exogenous_level()]. The shock is a sustained `size` multiple on
#' the oil price **level**, held for the entire forecast window -- the same
#' convention [conditional_forecast_check()]'s `"oil"` scenario uses, e.g.
#' `size = 1.5` for "+50%".
#'
#' @param fit A `koma::koma_estimate`, typically pre-extended via
#'   [extend_forecast_horizon()].
#' @param panel Named list of `koma_ts` in **levels**, spanning at least
#'   `dates$forecast`; use `extend_forecast_horizon()`'s `panel` element if
#'   the horizon was extended.
#' @param dates koma `dates` list (uses `dates$forecast`).
#' @param size Multiplier on the oil-price level. Default `1.5` ("+50%").
#'
#' @return A copy of `fit` with `oil_price` shocked, for `scenario_diff(...,
#'   restrictions = NULL, scenario_fit = )`.
#' @export
oil_price_shock <- function(fit, panel, dates, size = 1.5) {
  shock_exogenous_level(fit, panel, "oil_price", dates, function(x) x * size)
}

#' Cumulative-effect spillover matrix from a battery of demand shocks
#'
#' Turns a list of [scenario_diff()] results (one per source country's
#' `<iso2>_gdp` shock) into a square matrix of cumulative GDP effects: row
#' = source (the shocked country), column = receiver, cell = the sum of
#' `median_diff` on `<receiver>_gdp` across every horizon in `diffs`.
#'
#' **Sums `median_diff`, not `mean_diff`.** `mean_diff` is not robust to the
#' explosive posterior draws documented in [scenario_diff()]'s "Explosive
#' draws" section -- summing it across 8 horizons where the explosive share
#' grows from ~5% to ~55% would produce a matrix dominated by numerical
#' noise, not the model's actual cross-country transmission. The median is
#' unaffected by those draws by construction.
#'
#' @param diffs Named list of [scenario_diff()] results, one per source
#'   country, names are ISO-2 codes.
#' @param countries Character vector of ISO-2 codes to include as receivers
#'   (and to order rows/columns). Defaults to `names(diffs)`.
#'
#' @return A numeric matrix, `dimnames = list(source, receiver)`.
#' @export
spillover_matrix <- function(diffs, countries = names(diffs)) {
  m <- matrix(NA_real_, length(countries), length(countries),
             dimnames = list(countries, countries))
  for (src in names(diffs)) {
    d <- diffs[[src]]
    for (rcv in countries) {
      var <- country_var(rcv, "gdp")
      rows <- d[d$variable == var, ]
      m[src, rcv] <- if (nrow(rows) > 0) sum(rows$median_diff) else NA_real_
    }
  }
  m
}

#' Sanity checks on a spillover battery
#'
#' Three checks, reported pass/fail, never silently corrected -- consistent
#' with [sign_checks()]'s philosophy:
#'
#' - **own effect dominates**: for every source country, its own cumulative
#'   GDP response exceeds every cross-country response in magnitude.
#' - **monetary contraction sign**: a policy-rate hike's GDP and price
#'   responses are negative for every country, at the horizon each response
#'   is largest in magnitude (not necessarily horizon 1 -- the rate channel
#'   is a lagged one).
#' - **trade-weight ordering**: cumulative spillover magnitude correlates
#'   (Spearman, since only the *ranking* is claimed, not linearity) with the
#'   bilateral trade weight the receiver's `foreign_demand` identity puts on
#'   the source, across every off-diagonal (source, receiver) pair.
#'
#' @param mat A [spillover_matrix()] result.
#' @param monetary_diff A [scenario_diff()] result for the policy-rate
#'   scenario.
#' @param countries Character vector of ISO-2 codes.
#' @param linkage_weights A [stage2_linkage_weights()] result, for the
#'   trade-weight check.
#'
#' @return A `data.frame` with columns `check`, `ok`, `detail`.
#' @export
spillover_sanity_checks <- function(mat, monetary_diff, countries, linkage_weights) {
  own <- diag(mat)
  cross_max <- vapply(countries, function(cc) {
    max(abs(mat[cc, setdiff(countries, cc)]))
  }, numeric(1))
  own_dominates <- all(abs(own) > cross_max)

  gdp_rows <- monetary_diff[grepl("_gdp$", monetary_diff$variable) &
    sub("_gdp$", "", monetary_diff$variable) %in% countries, ]
  price_rows <- monetary_diff[grepl("_prices$", monetary_diff$variable) &
    sub("_prices$", "", monetary_diff$variable) %in% countries, ]
  peak <- function(rows) {
    stats::setNames(vapply(split(rows, rows$variable), function(d) {
      d$median_diff[which.max(abs(d$median_diff))]
    }, numeric(1)), names(split(rows, rows$variable)))
  }
  gdp_peak <- peak(gdp_rows)
  price_peak <- peak(price_rows)
  monetary_ok <- all(gdp_peak < 0) && all(price_peak < 0)

  pairs <- expand.grid(source = countries, receiver = countries, stringsAsFactors = FALSE)
  pairs <- pairs[pairs$source != pairs$receiver, ]
  pairs$spillover <- abs(mapply(function(s, r) mat[s, r], pairs$source, pairs$receiver))
  pairs$weight <- mapply(function(s, r) {
    w <- linkage_weights$foreign_demand[[r]]
    v <- w[names(w) == country_var(s, "gdp")]
    if (length(v) == 0) 0 else unname(v)
  }, pairs$source, pairs$receiver)
  trade_cor <- stats::cor(pairs$spillover, pairs$weight, method = "spearman")
  trade_ok <- isTRUE(trade_cor > 0.3)

  data.frame(
    check = c("own effect exceeds every cross-country effect",
             "monetary contraction is negative for GDP and prices everywhere",
             "spillover magnitude correlates with bilateral trade weight"),
    ok = c(own_dominates, monetary_ok, trade_ok),
    detail = c(
      sprintf("%d/%d countries: own > max|cross|", sum(abs(own) > cross_max), length(countries)),
      sprintf("GDP peak negative: %d/%d | prices peak negative: %d/%d",
              sum(gdp_peak < 0), length(gdp_peak), sum(price_peak < 0), length(price_peak)),
      sprintf("Spearman rho = %.3f (pass threshold: > 0.3)", trade_cor)
    ),
    stringsAsFactors = FALSE
  )
}
