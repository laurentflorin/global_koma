# Forecasting a fitted system and summarising the predictive distribution.
#
# koma constrains no posterior draw to be stationary, so at longer horizons
# some draws compound to absurd growth rates. Every quantile here is taken
# after dropping them, under two conventions: per horizon in rate space (a
# draw is dropped at the horizons where its growth exceeds the threshold), and
# cumulatively in level space (a draw whose growth exploded at horizon 3 has a
# corrupted level from horizon 3 on). The share dropped is reported alongside,
# because a fan drawn from what survives understates the true uncertainty.

#' Forecast a fitted system and summarise the predictive distribution
#'
#' The forecast counterpart to [coefficient_table()]: one call, one tidy
#' `data.frame` out. Extends the exogenous series first (see
#' [extend_forecast_horizon()]), and aborts if koma still delivers fewer
#' quarters than asked for, which it otherwise does with only a warning.
#'
#' @param fit A `koma::koma_estimate`.
#' @param panel The **full, untruncated** panel the fit was built from -- the
#'   one carrying real observations across the forecast window. Never
#'   `fit$ts_data`: [fit_system()] truncates the endogenous series to the
#'   estimation end, so `fit$ts_data` has nothing to compare a forecast
#'   against. Also used by [extend_forecast_horizon()] to extend the
#'   exogenous series.
#' @param horizon Quarters to forecast. Eight by default.
#' @param variables Endogenous variables to summarise. `NULL` does all of
#'   them; the cost is quantiles over a matrix already in memory, so
#'   restricting this saves very little.
#' @param history Quarters of observed history to return alongside the
#'   forecast, so a fan chart joins onto the past instead of floating.
#' @param probs Outer fan-chart quantiles, applied to the **filtered** draws.
#' @param inner_probs Inner fan-chart quantiles. Two bands rather than one,
#'   because an eight-quarter-ahead interval on *quarterly* growth can be wide
#'   enough to swamp the median path it is meant to qualify.
#' @param seed Set immediately before `koma::forecast()`. koma's stochastic
#'   forecasts are not reproducible call-to-call, so without this the numbers
#'   change on every run.
#' @param explosive_threshold A draw whose quarterly growth exceeds this in
#'   absolute value is dropped (see the file header).
#'
#' @return A list with
#'   `paths` (a `data.frame`, one row per `variable` x `period`, with a `kind`
#'   column of `"history"`/`"forecast"`, the fan quantiles in both rate and
#'   level space, the actual where one exists, and `explosive_frac`),
#'   `extension` (what [extend_forecast_horizon()] did to each exogenous
#'   series), `anker` (see [forecast_ankers()]), `origin`, `horizon`,
#'   `n_draws` and `n_actual`, the number of forecast rows with an outturn.
#' @export
forecast_system <- function(fit, panel, horizon = 8, variables = NULL,
                            history = 8, probs = c(0.1, 0.9),
                            inner_probs = c(0.25, 0.75), seed = 20260101,
                            explosive_threshold = 100) {
  for (nm in c("probs", "inner_probs")) {
    p <- get(nm)
    if (length(p) != 2 || p[1] >= p[2]) {
      cli::cli_abort("{.arg {nm}} must be two increasing quantiles, e.g. {.code c(0.1, 0.9)}.")
    }
  }
  frequency <- stats::frequency(fit$ts_data[[1]])

  ext <- extend_forecast_horizon(fit, panel, horizon)
  set.seed(seed)
  fc <- suppressWarnings(koma::forecast(
    ext$fit,
    dates = ext$dates,
    options = list(approximate = FALSE, probs = probs)
  ))

  delivered <- nrow(fc$forecasts[[1]])
  if (delivered < horizon) {
    # koma reports this as a warning and carries on with a shorter horizon.
    # Silently returning 5 quarters from a function called for 8 is exactly
    # the failure extend_forecast_horizon() exists to prevent, so it is fatal.
    cli::cli_abort(c(
      "koma returned {delivered} quarter{?s}, not the {horizon} requested.",
      "i" = "An exogenous series still does not reach {.val {ext$dates$forecast$end}}; check {.fn extend_forecast_horizon}'s output."
    ))
  }

  endogenous <- fit$sys_eq$endogenous_variables
  variables <- variables %||% endogenous
  unknown <- setdiff(variables, endogenous)
  if (length(unknown) > 0) {
    cli::cli_abort("{.val {unknown}} {?is/are} not endogenous in this system.")
  }

  origin <- ext$dates$forecast$start
  forecast_times <- origin[1] + (origin[2] - 1) / frequency + (seq_len(horizon) - 1) / frequency
  n_draws <- length(fc$forecasts)

  rows <- lapply(variables, function(var) {
    rate_draws <- vapply(fc$forecasts, function(d) d[seq_len(horizon), var], numeric(horizon))
    if (horizon == 1) rate_draws <- matrix(rate_draws, nrow = 1)

    # Per-horizon in rate space, cumulative in level space; see the file
    # header.
    explosive <- !is.finite(rate_draws) | abs(rate_draws) > explosive_threshold
    explosive_cum <- explosive
    for (h in seq_len(horizon)[-1]) {
      explosive_cum[h, ] <- explosive_cum[h - 1, ] | explosive[h, ]
    }

    level_draws <- forecast_draws_level_matrix(fc, var, rate_draws)

    quantiles_of <- function(values, keep) {
      kept <- values[keep]
      if (length(kept) == 0) {
        return(c(median = NA_real_, lower = NA_real_, upper = NA_real_,
                 inner_lower = NA_real_, inner_upper = NA_real_))
      }
      c(median = stats::median(kept),
        lower = unname(stats::quantile(kept, probs[1])),
        upper = unname(stats::quantile(kept, probs[2])),
        inner_lower = unname(stats::quantile(kept, inner_probs[1])),
        inner_upper = unname(stats::quantile(kept, inner_probs[2])))
    }

    per_h <- lapply(seq_len(horizon), function(h) {
      keep_rate <- !explosive[h, ]
      rq <- quantiles_of(rate_draws[h, ], keep_rate)
      lq <- if (is.null(level_draws)) {
        c(median = NA_real_, lower = NA_real_, upper = NA_real_,
          inner_lower = NA_real_, inner_upper = NA_real_)
      } else {
        quantiles_of(level_draws[h, ], !explosive_cum[h, ])
      }
      data.frame(
        variable = var, kind = "forecast", horizon = h, time = forecast_times[h],
        rate_median = rq[["median"]], rate_lower = rq[["lower"]], rate_upper = rq[["upper"]],
        rate_inner_lower = rq[["inner_lower"]], rate_inner_upper = rq[["inner_upper"]],
        level_median = lq[["median"]], level_lower = lq[["lower"]], level_upper = lq[["upper"]],
        level_inner_lower = lq[["inner_lower"]], level_inner_upper = lq[["inner_upper"]],
        explosive_frac = mean(explosive[h, ]),
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, per_h)
  })

  paths <- do.call(rbind, rows)
  paths <- attach_actuals(paths, panel, frequency)
  hist_rows <- forecast_history(panel, variables, origin, history, frequency)

  out <- rbind(hist_rows, paths)
  out <- out[order(out$variable, out$time), ]
  rownames(out) <- NULL

  list(
    paths = out,
    extension = ext$extension,
    anker = forecast_ankers(fc, panel, variables, frequency),
    origin = origin,
    horizon = horizon,
    n_draws = n_draws,
    n_actual = sum(paths$kind == "forecast" & !is.na(paths$actual_rate))
  )
}

#' Invert a whole matrix of rate draws to levels at once
#'
#' The vectorised equivalent of calling `koma::level()` once per draw, which
#' for a hundred variables and 1000 draws takes minutes. Reading
#' `koma:::level.ts` (verified against koma 0.3.1), a `rate`/`diff_log` series
#' is inverted as `exp(cumsum(x/100)) * 100`, prepended with the base `100`,
#' then rescaled by `anker/100` -- which, after dropping the prepended anker
#' itself, is exactly `anker * exp(cumsum(x/100))`. `percentage` is the same
#' with `cumprod(1 + x/100)`.
#'
#' **A `method = "none"` series is returned unchanged, and that is not a
#' shortcut.** Interest rates are `rate`/`none`: koma passes those numbers
#' through untouched, so level space *is* rate space, and they carry no
#' `anker` at all -- `koma::level()` errors on them.
#'
#' `tests/testthat/test-forecasts.R` checks this against `koma::level()`
#' draw by draw, so a change in koma's inversion fails a test rather than
#' drifting silently.
#'
#' @param fc A `koma_forecast`.
#' @param var Variable name.
#' @param rate_draws The `horizon x ndraws` matrix of rate-space draws.
#'
#' @return A `horizon x ndraws` matrix in level space, or `NULL` if the
#'   inversion is not defined for this series.
#' @keywords internal
forecast_draws_level_matrix <- function(fc, var, rate_draws) {
  m <- fc$mean[[var]]
  method <- attr(m, "method") %||% NA_character_
  type <- attr(m, "series_type") %||% NA_character_

  if (identical(method, "none") || identical(type, "level")) {
    return(rate_draws)
  }
  anker <- attr(m, "anker")
  if (is.null(anker) || anyNA(anker[1])) {
    return(NULL)
  }

  cumulative <- switch(method,
    diff_log = exp(apply(rate_draws / 100, 2, cumsum)),
    percentage = apply(1 + rate_draws / 100, 2, cumprod),
    NULL
  )
  if (is.null(cumulative)) {
    return(NULL)
  }
  # apply() drops to a vector when the horizon is 1.
  if (!is.matrix(cumulative)) {
    cumulative <- matrix(cumulative, nrow = nrow(rate_draws))
  }
  unname(anker[1]) * cumulative
}

#' Attach the realised value to each forecast row, where one exists
#'
#' A forecast window already in the past gets an actual for every quarter;
#' one forecasting genuinely forward gets `NA` beyond the data.
#'
#' @param paths Forecast rows, with `variable` and `time`.
#' @param panel The full, untruncated panel.
#' @param frequency Observations per year.
#'
#' @return `paths` with `actual_rate` and `actual_level` columns.
#' @keywords internal
attach_actuals <- function(paths, panel, frequency) {
  paths$actual_rate <- NA_real_
  paths$actual_level <- NA_real_
  for (var in unique(paths$variable)) {
    series <- panel[[var]]
    if (is.null(series)) next
    idx <- which(paths$variable == var)
    # koma::rate() already returns a correctly-dated ts -- it drops the first
    # observation and any trailing NA. Re-dating it by hand shifts series with
    # trailing NAs by a quarter; read its own tsp instead.
    rate_series <- tryCatch(koma::rate(series), error = function(e) NULL)
    paths$actual_level[idx] <- lookup_ts(series, paths$time[idx], frequency)
    if (!is.null(rate_series)) {
      paths$actual_rate[idx] <- lookup_ts(rate_series, paths$time[idx], frequency)
    }
  }
  paths
}

#' The observed history rows a fan chart joins onto
#'
#' @param panel The full, untruncated panel.
#' @param variables Variables to return.
#' @param origin The forecast start, `c(year, period)`.
#' @param history Number of periods before `origin`.
#' @param frequency Observations per year.
#'
#' @return Rows shaped like [forecast_system()]'s `paths`, `kind = "history"`.
#' @keywords internal
forecast_history <- function(panel, variables, origin, history, frequency) {
  if (history <= 0) {
    return(NULL)
  }
  origin_time <- origin[1] + (origin[2] - 1) / frequency
  times <- origin_time - rev(seq_len(history)) / frequency

  rows <- lapply(variables, function(var) {
    series <- panel[[var]]
    if (is.null(series)) return(NULL)
    rate_series <- tryCatch(koma::rate(series), error = function(e) NULL)
    data.frame(
      variable = var, kind = "history", horizon = NA_integer_, time = times,
      rate_median = NA_real_, rate_lower = NA_real_, rate_upper = NA_real_,
      rate_inner_lower = NA_real_, rate_inner_upper = NA_real_,
      level_median = NA_real_, level_lower = NA_real_, level_upper = NA_real_,
      level_inner_lower = NA_real_, level_inner_upper = NA_real_,
      explosive_frac = NA_real_,
      actual_rate = if (is.null(rate_series)) NA_real_ else lookup_ts(rate_series, times, frequency),
      actual_level = lookup_ts(series, times, frequency),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, Filter(Negate(is.null), rows))
}

#' What each variable's level path is anchored on, and whether that is observed
#'
#' **koma anchors a level forecast on the last value in the fit's own
#' `ts_data`, which is not always an observed one.** When the forecast starts
#' after the estimation end, koma *conditionally fills* the gap first and takes
#' the anker off the end of that fill. The level path then starts from the
#' fill, not from the data, while the rate path is unaffected. A gap of zero
#' says the forecast starts straight from the last observation.
#'
#' @param fc A `koma_forecast`.
#' @param panel The full, untruncated panel.
#' @param variables Variables to report.
#' @param frequency Observations per year.
#'
#' @return A `data.frame` with `variable`, `anker_time`, `anker`, `observed`
#'   (the panel's value at that date) and `gap_pct`. Rows are omitted for
#'   `method = "none"` variables, which have no anker.
#' @keywords internal
forecast_ankers <- function(fc, panel, variables, frequency) {
  rows <- lapply(variables, function(var) {
    m <- fc$mean[[var]]
    anker <- attr(m, "anker")
    if (is.null(anker) || length(anker) < 2 || anyNA(anker)) return(NULL)
    observed <- lookup_ts(panel[[var]] %||% stats::ts(NA_real_), anker[2], frequency)
    data.frame(
      variable = var,
      anker_time = quarter_label(anker[2], frequency),
      anker = unname(anker[1]),
      observed = observed,
      gap_pct = 100 * (unname(anker[1]) / observed - 1),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(out)) {
    return(data.frame(variable = character(), anker_time = character(), anker = numeric(),
                      observed = numeric(), gap_pct = numeric(), stringsAsFactors = FALSE))
  }
  rownames(out) <- NULL
  out
}

#' Read a `ts` at given decimal times, returning `NA` off the ends
#'
#' @param x A `ts`.
#' @param times Decimal times.
#' @param frequency Observations per year.
#'
#' @return A numeric vector the length of `times`.
#' @keywords internal
lookup_ts <- function(x, times, frequency) {
  tsp_x <- stats::tsp(x)
  values <- as.numeric(x)
  vapply(times, function(t) {
    i <- round((t - tsp_x[1]) * frequency) + 1L
    if (i < 1 || i > length(values)) NA_real_ else values[i]
  }, numeric(1))
}

#' Format a decimal `ts` time as a quarter label
#'
#' `2025.25` -> `"2025Q2"`.
#'
#' @param t Numeric vector of `stats::time()` values.
#' @param frequency Observations per year.
#' @return A character vector.
#' @export
quarter_label <- function(t, frequency = 4) {
  year <- floor(t + 1e-8)
  period <- round((t - year) * frequency) + 1L
  sprintf("%dQ%d", year, period)
}

#' How much of the predictive distribution had to be discarded
#'
#' The companion to every fan chart: the quantiles are drawn from the draws
#' that did not explode (see the file header), and this says how many did. A
#' variable whose share climbs steeply with the horizon has a fan that
#' understates its true uncertainty.
#'
#' @param paths The `paths` element of a [forecast_system()] result.
#' @param variables Variables to include.
#'
#' @return A `data.frame`, one row per variable, with the explosive share at
#'   horizon 1, the horizon-8 share, and the maximum.
#' @export
forecast_explosive_table <- function(paths, variables = NULL) {
  variables <- variables %||% unique(paths$variable)
  d <- paths[paths$kind == "forecast" & paths$variable %in% variables, ]
  rows <- lapply(variables, function(v) {
    dv <- d[d$variable == v, ]
    data.frame(
      variable = v,
      h1 = dv$explosive_frac[dv$horizon == 1],
      h_last = dv$explosive_frac[dv$horizon == max(dv$horizon)],
      max = max(dv$explosive_frac),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out[order(-out$h_last), ]
}

#' Extend a fit's exogenous series far enough to forecast `quarters` ahead
#'
#' `koma::forecast()` silently **shortens the horizon** when an exogenous
#' series does not reach `dates$forecast$end` -- `forecast_draw()` resets
#' `horizon <- nrow(na.omit(forecast_x_matrix))` and only warns. This extends
#' each exogenous series by its own shortfall first, so the requested horizon
#' is actually delivered.
#'
#' **Extrapolation rule, stated plainly because it is an assumption, not a
#' fact**: a level exogenous series (government consumption, say) is
#' extended by carrying forward the **average of its last 4 observed
#' quarterly `diff_log` growth rates** -- a trailing-momentum continuation,
#' not a forecast. A rate/none exogenous is held flat at its recent average
#' instead: there is no growth rate to extrapolate, and treating its level as
#' a `diff_log` rate would compound it exponentially. COVID dummies extend as
#' zeros.
#'
#' @param fit A `koma::koma_estimate` object (e.g. from [fit_system()]).
#' @param panel Named list of `koma_ts`, the **level** panel the fit's
#'   `ts_data` was built from.
#' @param quarters Total forecast horizon, in quarters, measured from
#'   `fit$dates$forecast$start`.
#'
#' @return A list with `fit` (a copy with extended `ts_data`), `dates` (a
#'   copy of `fit$dates` with `forecast$end` advanced), `panel` (a copy of
#'   `panel` with the extended **level** series for the exogenous variables
#'   that needed it), and `extension`, a `data.frame` recording what was done
#'   to each exogenous variable.
#' @export
extend_forecast_horizon <- function(fit, panel, quarters) {
  frequency <- stats::frequency(fit$ts_data[[1]])
  dates <- fit$dates
  native_horizon <- length(seq(
    dates$forecast$start[1] + (dates$forecast$start[2] - 1) / frequency,
    dates$forecast$end[1] + (dates$forecast$end[2] - 1) / frequency,
    by = 1 / frequency
  ))
  # The target end, and then how far EACH series is from it: exogenous series
  # need not all end at the fit's native forecast end (one built from others
  # stops where the shortest of them does).
  dates$forecast$end <- advance_periods(dates$forecast$start, quarters - 1, frequency)
  target_end <- dates$forecast$end[1] + (dates$forecast$end[2] - 1) / frequency
  periods_short <- function(x) {
    max(0L, as.integer(round((target_end - stats::tsp(x)[2]) * frequency)))
  }

  if (all(vapply(fit$sys_eq$exogenous_variables,
                 function(v) periods_short(fit$ts_data[[v]]) == 0L, logical(1)))) {
    return(list(fit = fit, dates = dates, panel = panel, extension = data.frame(
      variable = character(0), method = character(0), periods_added = integer(0),
      stringsAsFactors = FALSE
    )))
  }

  out <- fit
  out_panel <- panel
  log <- lapply(fit$sys_eq$exogenous_variables, function(v) {
    extra <- periods_short(fit$ts_data[[v]])
    if (extra == 0L) {
      return(data.frame(variable = v, method = "already reaches the horizon",
                        periods_added = 0L, stringsAsFactors = FALSE))
    }
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
    # Trim the RAGGED EDGE before extrapolating from it. A panel series padded
    # with trailing NAs ends at its last observation in the fit (koma::rate()
    # drops them), so extrapolating from `tail(level, 1)` would compound from
    # an NA and produce an all-NA extension, which koma reports as "does not
    # extend into the forecast period" rather than as a bad value.
    observed <- which(!is.na(as.numeric(level)))
    if (length(observed) == 0) {
      cli::cli_abort("{.val {v}} has no observed values to extrapolate from.")
    }
    level <- stats::window(level, end = stats::time(level)[max(observed)])

    if (identical(attr(level, "series_type"), "rate")) {
      # A rate/none series (e.g. an exogenous interest rate): there is no
      # growth rate to extrapolate -- rate() on it is a no-op,
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

  list(fit = out, dates = dates, panel = out_panel, extension = do.call(rbind, log))
}

#' Add `n` quarters (or periods) to a `c(year, period)` date
#'
#' @param yq A date, `c(year, period)`.
#' @param n Periods to add; may be negative.
#' @param frequency Periods per year.
#'
#' @return `c(year, period)`.
#' @keywords internal
advance_periods <- function(yq, n, frequency = 4) {
  total <- (yq[1] * frequency + (yq[2] - 1)) + n
  year <- total %/% frequency
  period <- total %% frequency + 1
  c(year, period)
}

