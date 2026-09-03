# Eight-quarter forecasts, and the fan charts that report them.
#
# Every stage in this project ends in a `koma::estimate()` fit, and every one
# of those fits can be run forward. This file is the one place that does it,
# so the three traps below are handled once rather than in each report:
#
#   1. koma SILENTLY SHORTENS the horizon when an exogenous series runs out
#      (`forecast_draw()` resets `horizon <- nrow(na.omit(forecast_x_matrix))`
#      and only warns). Every stage's exogenous series stop at the fit's own
#      native forecast end, so asking for 8 quarters without extending them
#      first returns 4 or 5 and claims success. `stage_forecast()` extends via
#      `extend_forecast_horizon()` and then CHECKS what came back.
#   2. koma constrains no draw to be stationary, so a sizeable minority of
#      draws explode by horizon 8 -- `reports/evaluation.qmd` measures the
#      share at 0.63 for stage-2c prices. A mean or an unfiltered quantile
#      over those draws is meaningless. Filtering is the same convention
#      `score_forecast()` already uses, and the share dropped is reported
#      rather than hidden.
#   3. A level fan chart is not the exponential of a rate fan chart. Levels
#      come from `forecast_draws_level()`, which reattaches the `anker`
#      attributes koma leaves off the raw draws, and the explosive filter is
#      CUMULATIVE there: a draw whose rate explodes at horizon 3 has corrupted
#      its compounded level from horizon 3 onward.

#' Run a fitted stage forward and summarise the predictive distribution
#'
#' The forecast counterpart to [coefficient_table()]: one call per stage, one
#' tidy `data.frame` out, no koma objects left for the report to handle.
#'
#' **What the horizon means differs by stage, and the report has to say so.**
#' Stages 1 and 2a forecast from 2023Q1 -- the conditional-fill window that
#' neutralises COVID -- so their eight quarters run 2023Q1-2024Q4 and land
#' almost entirely on *observed* data. That makes their fan charts a genuine
#' out-of-sample check rather than a projection. Stages 2b, 2c, 2d, 3a and 3b
#' estimate to 2024Q4 and forecast from 2025Q1, so their eight quarters run to
#' 2026Q4 and are mostly beyond the data. `origin` and `n_actual` in the
#' result record which case a given stage is in.
#'
#' @param fit A `koma::koma_estimate`.
#' @param panel The **full, untruncated** panel the fit was built from -- the
#'   one carrying real observations across the forecast window. Never
#'   `fit$ts_data`: every `fit_stage1()`/`fit_stage2()` fit truncates its
#'   endogenous series to the estimation end, so `fit$ts_data` has nothing to
#'   compare a forecast against (the same trap `score_country_forecast()`
#'   documents). Also used by [extend_forecast_horizon()] to extend the
#'   exogenous series.
#' @param horizon Quarters to forecast. Eight by default.
#' @param variables Endogenous variables to summarise. `NULL` does all of
#'   them; the cost is quantiles over a matrix already in memory, so
#'   restricting this saves very little.
#' @param history Quarters of observed history to return alongside the
#'   forecast, so a fan chart joins onto the past instead of floating.
#' @param probs Outer fan-chart quantiles, applied to the **filtered** draws.
#' @param inner_probs Inner fan-chart quantiles. Two bands rather than one
#'   because a single 10-90 band is unreadable at this project's horizons:
#'   an eight-quarter-ahead interval on *quarterly* growth spans tens of
#'   percentage points, so the band swamps the median path it is meant to
#'   qualify. [plot_forecast_fan()] draws the inner band solid and lets the
#'   outer one run off a clipped panel.
#' @param seed Set immediately before `koma::forecast()`. koma's stochastic
#'   forecasts are not reproducible call-to-call (see [scenario_diff()]), so
#'   without this a report's numbers change on every render.
#' @param explosive_threshold Passed to `filter_explosive_draws()`: a draw
#'   whose quarterly growth exceeds this in absolute value is dropped.
#'
#' @return A list with
#'   `paths` (a `data.frame`, one row per `variable` x `period`, with a `kind`
#'   column of `"history"`/`"forecast"`, the fan quantiles in both rate and
#'   level space, the actual where one exists, and `explosive_frac`),
#'   `extension` (what [extend_forecast_horizon()] did to each exogenous
#'   series), `origin`, `horizon` and `n_draws`.
#' @export
stage_forecast <- function(fit, panel, horizon = 8, variables = NULL,
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

    # Per-horizon in rate space, cumulative in level space -- see the file
    # header and score_forecast(), which uses the same two conventions.
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

#' What each variable's level path is anchored on, and whether that is observed
#'
#' **koma anchors a level forecast on the last value in the fit's own
#' `ts_data`, which is not always an observed one.** Stages 1 and 2a estimate
#' to 2019Q4 and forecast from 2023Q1, so koma *conditionally fills* 2020Q1
#' through 2022Q4 first and takes the anker off the end of that fill. Measured
#' on the stage-2a pilot, `de_prices`'s anker is **85.02** against an observed
#' 2022Q4 value of **93.55** -- a 9% gap that has nothing to do with the
#' forecast and everything to do with the fill not having seen the inflation
#' surge it was filling across.
#'
#' The consequence is specific: for those stages the **rate** fan chart is
#' directly comparable to the actuals and the **level** one is not, because it
#' starts from the wrong place. Stages 2b onward estimate to 2024Q4 and
#' forecast from 2025Q1 with nothing to fill, so their ankers are observed and
#' both spaces are fine. This function returns the numbers so a report can say
#' which case it is in rather than leaving the reader to assume.
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

#' Invert a whole matrix of rate draws to levels at once
#'
#' The vectorised equivalent of [forecast_draws_level()], which calls
#' `koma::level()` once per draw -- 1000 calls per variable, and on the
#' stage-3b system that is 120,000 calls and more than ten minutes. Reading
#' `koma:::level.ts` (verified against koma 0.3.1), a `rate`/`diff_log` series
#' is inverted as `exp(cumsum(x/100)) * 100`, prepended with the base `100`,
#' then rescaled by `anker/100` -- which, after dropping the prepended anker
#' itself, is exactly `anker * exp(cumsum(x/100))`. `percentage` is the same
#' with `cumprod(1 + x/100)`.
#'
#' **A `method = "none"` series is returned unchanged, and that is not a
#' shortcut.** Every policy rate, spread and ratio in this project is
#' `rate`/`none`: koma passes those numbers through untouched, so level space
#' *is* rate space, and they carry no `anker` at all -- `koma::level()` errors
#' on them rather than returning something usable, which would otherwise leave
#' every policy rate blank on a level chart.
#'
#' This is checked against [forecast_draws_level()] in
#' `tests/testthat/test-forecasts.R`; if koma's inversion ever changes, that
#' test fails rather than this silently drifting.
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
#' A stage whose forecast window is already in the past (stages 1 and 2a) gets
#' an actual for every quarter; one forecasting genuinely forward gets `NA`
#' beyond the data. Both are normal and the report distinguishes them.
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
    # trailing NAs by a quarter (see CLAUDE.md); read its own tsp instead.
    rate_series <- tryCatch(koma::rate(series), error = function(e) NULL)
    paths$actual_level[idx] <- lookup_ts(series, paths$time[idx], frequency)
    if (!is.null(rate_series)) {
      paths$actual_rate[idx] <- lookup_ts(rate_series, paths$time[idx], frequency)
    }
  }
  paths
}

#' Read a `ts` at given decimal times, returning `NA` off the ends
#' @keywords internal
lookup_ts <- function(x, times, frequency) {
  tsp_x <- stats::tsp(x)
  values <- as.numeric(x)
  vapply(times, function(t) {
    i <- round((t - tsp_x[1]) * frequency) + 1L
    if (i < 1 || i > length(values)) NA_real_ else values[i]
  }, numeric(1))
}

#' The observed history rows a fan chart joins onto
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

#' Format a decimal `ts` time as a quarter label
#'
#' `2025.25` -> `"2025Q2"`. Used for report tables; the plots keep the numeric
#' time so the axis stays continuous.
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

#' Fan chart for one or more forecast variables
#'
#' Plots two filtered predictive bands -- an inner one drawn solid and an
#' outer one drawn faint -- the median as a line, and the realised path where
#' one exists, with the forecast origin marked. Faceted by variable with free
#' scales, because a price index and a policy rate do not share a range.
#'
#' **The panel is clipped, deliberately, and the caption has to say so.** At
#' eight quarters this project's systems put a 10-90 interval on *quarterly*
#' GDP growth that spans roughly plus or minus 25 percentage points -- an
#' honest number, and one that renders the median path invisible if the axis
#' is scaled to contain it. The y-range is therefore set from the history, the
#' median, the realised path and the **inner** band, so the outer band runs
#' off the top and bottom of the panel. Nothing is hidden: the outer band is
#' still drawn as far as the panel goes, [forecast_uncertainty_table()]
#' reports its full width numerically, and `clip = FALSE` turns the behaviour
#' off entirely.
#'
#' **Read the bands as the *filtered* predictive interval.** They are computed
#' after dropping explosive draws, which is the only way they are legible at
#' all here; [forecast_explosive_table()] says how much was dropped, and a
#' variable with a large share has a fan narrower than the model's true
#' uncertainty.
#'
#' @param paths The `paths` element of a [stage_forecast()] result.
#' @param variables Variables to plot, in the order given. `NULL` plots all.
#' @param space `"level"` (an index, or a rate's own units) or `"rate"`
#'   (quarterly growth in percent, the space koma estimates in).
#' @param ncol Facet columns.
#' @param title,subtitle Plot labels.
#' @param labeller Optional named character vector mapping variable name to
#'   facet label.
#' @param clip Scale the y-axis to the inner band and let the outer band
#'   overflow. `FALSE` shows the full outer band instead.
#'
#' @return A `ggplot` object.
#' @export
plot_forecast_fan <- function(paths, variables = NULL, space = c("level", "rate"),
                              ncol = 2, title = NULL, subtitle = NULL,
                              labeller = NULL, clip = TRUE) {
  space <- match.arg(space)
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort("{.pkg ggplot2} is needed for {.fn plot_forecast_fan}.")
  }
  variables <- variables %||% unique(paths$variable)
  d <- paths[paths$variable %in% variables, ]
  if (nrow(d) == 0) {
    cli::cli_abort("None of {.val {variables}} {?is/are} in {.arg paths}.")
  }
  d$variable <- factor(d$variable, levels = variables)

  prefix <- if (identical(space, "level")) "level" else "rate"
  d$mid <- d[[paste0(prefix, "_median")]]
  d$lo <- d[[paste0(prefix, "_lower")]]
  d$hi <- d[[paste0(prefix, "_upper")]]
  d$ilo <- d[[paste0(prefix, "_inner_lower")]]
  d$ihi <- d[[paste0(prefix, "_inner_upper")]]
  d$actual <- d[[if (identical(space, "level")) "actual_level" else "actual_rate"]]

  # Join the bands onto the last observed point so the fan does not float a
  # quarter away from the history line.
  origin_time <- min(d$time[d$kind == "forecast"])
  last_hist_time <- suppressWarnings(max(d$time[d$kind == "history"]))
  if (is.finite(last_hist_time)) {
    bridge <- d[d$kind == "history" & d$time == last_hist_time, ]
    for (col in c("mid", "lo", "hi", "ilo", "ihi")) bridge[[col]] <- bridge$actual
    d <- rbind(d, bridge)
  }

  # Clip to what the reader needs to see: everything except the outer band.
  limits <- NULL
  if (isTRUE(clip)) {
    limits <- lapply(split(d, d$variable), function(g) {
      v <- c(g$actual, g$mid, g$ilo, g$ihi)
      v <- v[is.finite(v)]
      if (length(v) == 0) return(NULL)
      pad <- diff(range(v)) * 0.12
      if (pad == 0) pad <- max(abs(v)) * 0.1 + 1e-6
      range(v) + c(-pad, pad)
    })
  }

  # CLAMP rather than geom_blank: ggplot2's geom_blank can only *expand* a
  # scale, so an outer ribbon 50 percentage points wide would drag the panel
  # open again and the clipping would do nothing. Pinning the outer band to the
  # limits makes it run to the panel edge instead, which is what "continues
  # beyond this panel" should look like. The numbers are unaffected --
  # forecast_uncertainty_table() reports the true bounds.
  if (!is.null(limits)) {
    for (v in names(limits)) {
      lim <- limits[[v]]
      if (is.null(lim)) next
      i <- d$variable == v
      d$lo[i] <- pmax(d$lo[i], lim[1])
      d$hi[i] <- pmin(d$hi[i], lim[2])
      d$ilo[i] <- pmax(d$ilo[i], lim[1])
      d$ihi[i] <- pmin(d$ihi[i], lim[2])
    }
  }

  if (!is.null(labeller)) {
    levels(d$variable) <- labeller[levels(d$variable)]
    if (!is.null(limits)) names(limits) <- labeller[names(limits)]
  }

  # A `method = "none"` series (every policy rate, spread, unemployment rate and
  # ratio) has no level/rate distinction at all -- level space IS rate space
  # there, which is why stage_forecast() gives it no anker. Labelling such a
  # panel "quarterly growth, %" is simply wrong: the numbers are the rate
  # itself, in percent. Detect it from the data rather than requiring the
  # caller to know, by asking whether the two spaces coincide.
  #
  # The test is RELATIVE, not exact. The two medians are taken over different
  # draw subsets -- the explosive filter is per-horizon in rate space and
  # cumulative in level space -- so even a series where the spaces are
  # identical by construction disagrees a little: `reu_unemployment` differs by
  # 5.4e-04 on a level of 7. A `diff_log` series is not close in this sense at
  # all (`de_wages`: 0.95 against 10.9), so the two cases are orders of
  # magnitude apart and the threshold does not need to be delicate.
  coincide <- vapply(split(d, d$variable), function(g) {
    i <- is.finite(g$rate_median) & is.finite(g$level_median)
    any(i) && isTRUE(all.equal(g$rate_median[i], g$level_median[i], tolerance = 1e-3))
  }, logical(1))
  y_lab <- if (identical(space, "level")) {
    if (all(coincide)) "percent" else "level / index"
  } else if (all(coincide)) {
    "percent (a rate series)"
  } else if (any(coincide)) {
    "growth, % (rate series shown as levels)"
  } else {
    "quarterly growth, %"
  }
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$time)) +
    ggplot2::geom_vline(xintercept = origin_time - 1 / 8, linewidth = 0.3,
                        linetype = "dotted", colour = "grey40") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lo, ymax = .data$hi),
                         fill = "#4477AA", alpha = 0.13, na.rm = TRUE) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$ilo, ymax = .data$ihi),
                         fill = "#4477AA", alpha = 0.30, na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = .data$actual), linewidth = 0.6,
                       colour = "grey15", na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = .data$mid), linewidth = 0.7,
                       colour = "#11336B", na.rm = TRUE) +
    ggplot2::labs(title = title, subtitle = subtitle, x = NULL, y = y_lab) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

  p + ggplot2::facet_wrap(~variable, ncol = ncol, scales = "free_y")
}

#' How wide the predictive band actually is, since the chart clips it
#'
#' [plot_forecast_fan()] scales its panels to the inner band so the median
#' stays readable, which means the outer band leaves the panel. This reports
#' what was cut: the full outer interval at the final horizon, per variable.
#' Printing it next to a clipped chart is what keeps the clipping honest.
#'
#' @param paths The `paths` element of a [stage_forecast()] result.
#' @param variables Variables to include.
#' @param space `"level"` or `"rate"`.
#' @param digits Rounding.
#'
#' @return A `data.frame` with the median, inner and outer band bounds and the
#' outer width, at the last horizon.
#' @export
forecast_uncertainty_table <- function(paths, variables = NULL,
                                       space = c("rate", "level"), digits = 1) {
  space <- match.arg(space)
  variables <- variables %||% unique(paths$variable)
  prefix <- if (identical(space, "level")) "level" else "rate"
  d <- paths[paths$kind == "forecast" & paths$variable %in% variables, ]
  d <- d[d$horizon == max(d$horizon), ]
  out <- data.frame(
    variable = d$variable,
    median = round(d[[paste0(prefix, "_median")]], digits),
    inner_low = round(d[[paste0(prefix, "_inner_lower")]], digits),
    inner_high = round(d[[paste0(prefix, "_inner_upper")]], digits),
    outer_low = round(d[[paste0(prefix, "_lower")]], digits),
    outer_high = round(d[[paste0(prefix, "_upper")]], digits),
    stringsAsFactors = FALSE
  )
  out$outer_width <- round(out$outer_high - out$outer_low, digits)
  out <- out[match(variables, out$variable), ]
  rownames(out) <- NULL
  out
}

#' The forecast summary table a report prints
#'
#' One row per quarter, columns per variable, in whichever space is asked for.
#'
#' @param paths The `paths` element of a [stage_forecast()] result.
#' @param variables Variables to include.
#' @param space `"level"` or `"rate"`.
#' @param digits Rounding.
#'
#' @return A `data.frame` with a `quarter` column and one column per variable,
#'   forecast rows only.
#' @export
forecast_table <- function(paths, variables = NULL, space = c("level", "rate"), digits = 2) {
  space <- match.arg(space)
  variables <- variables %||% unique(paths$variable)
  d <- paths[paths$kind == "forecast" & paths$variable %in% variables, ]
  value_col <- if (identical(space, "level")) "level_median" else "rate_median"

  quarters <- sort(unique(d$time))
  out <- data.frame(quarter = quarter_label(quarters), stringsAsFactors = FALSE)
  for (v in variables) {
    dv <- d[d$variable == v, ]
    out[[v]] <- round(dv[[value_col]][match(quarters, dv$time)], digits)
  }
  out
}

#' How much of the predictive distribution had to be discarded
#'
#' The companion every fan chart in this project needs. koma constrains no
#' draw to be stationary, so at horizon 8 a substantial minority of draws have
#' compounded to absurd growth rates; the fan is drawn from what survives, and
#' this says how much did not. A variable whose share climbs steeply with the
#' horizon has a fan that understates its true uncertainty, and should be read
#' as an ordering rather than a number.
#'
#' @param paths The `paths` element of a [stage_forecast()] result.
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

#' Forecast error against the realised path, per variable and horizon
#'
#' The counterpart to [forecast_explosive_table()]: that one says how much of
#' the predictive distribution survived, this one says how close the surviving
#' median landed. Both read the same `paths` data frame, so a report can put
#' them side by side without loading a fit.
#'
#' **Only quarters with an observed outturn get a row.** Which quarters those
#' are differs by stage and is the whole reason this returns `n` rather than a
#' bare error: stages 1 and 2a forecast from 2023Q1 into a window that is
#' entirely observed, so all eight horizons score; stages 2b onward forecast
#' from 2025Q1, where only the first few quarters have happened. Comparing a
#' stage scored on eight horizons against one scored on five is not a
#' comparison, and pooling them silently is the mistake this function is shaped
#' to make visible.
#'
#' **Score in rate space unless you have a reason not to.** A level error
#' compounds every earlier quarter's error into the current one, so a level
#' MAE at horizon 5 is mostly a restatement of the horizon-1 error; it also
#' inherits the anchoring problem [forecast_ankers()] documents, which makes
#' stages 1 and 2a's level errors incomparable with the others' by
#' construction. Rate space is per-quarter growth and has neither problem.
#'
#' @param paths The `paths` element of a [stage_forecast()] result.
#' @param variables Variables to score. `NULL` does every variable present.
#' @param space `"rate"` (default) or `"level"` -- see above.
#'
#' @return A `data.frame`, one row per `variable` x `horizon` that has an
#'   outturn, with `median`, `actual`, `error` (median minus actual) and
#'   `abs_error`. Variables with no outturn anywhere contribute no rows.
#' @export
forecast_error_table <- function(paths, variables = NULL, space = c("rate", "level")) {
  space <- match.arg(space)
  variables <- variables %||% unique(paths$variable)
  prefix <- if (identical(space, "level")) "level" else "rate"
  d <- paths[paths$kind == "forecast" & paths$variable %in% variables, ]
  med <- d[[paste0(prefix, "_median")]]
  act <- d[[paste0("actual_", prefix)]]
  keep <- !is.na(med) & !is.na(act)
  out <- data.frame(
    variable = d$variable[keep],
    horizon = d$horizon[keep],
    median = med[keep],
    actual = act[keep],
    stringsAsFactors = FALSE
  )
  out$error <- out$median - out$actual
  out$abs_error <- abs(out$error)
  out <- out[order(out$variable, out$horizon), ]
  rownames(out) <- NULL
  out
}

#' Variables that several stages can actually be compared on
#'
#' The guard for any cross-stage chart or scoreboard. Two systems are only
#' comparable on a variable if they both carry it **and** mean the same thing
#' by it, and the second half of that is not automatic in this project:
#'
#' - `<iso2>_prices` is observed headline HICP in stages 2b-2d, but under
#'   [labour_block()] it becomes an **identity** over the energy and non-energy
#'   sub-indices, built with [chain_weighted_index()] because observed HICP
#'   satisfies a fixed-weight identity only approximately. So stage 3a's
#'   `de_prices` is a different series from stage 2b's -- verified, they differ
#'   by up to 0.65pp of quarterly growth -- and `ea_prices` inherits it.
#' - `<iso2>_foreign_demand` is constructed from the partition's own trade
#'   weights and partner basis, so it is a different index in every stage by
#'   construction (up to 4.51pp for the US).
#' - `ea_gdp` aggregates eleven countries in stage 2b and four in stage 2d.
#'
#' None of that is a fault, but scoring a forecast of one against the outturn
#' of the other silently compares two questions. This function drops any
#' variable whose **realised** path disagrees across the stages, and names what
#' it dropped so a report can say so rather than quietly averaging over it.
#'
#' @param fc_list Named list of [stage_forecast()] results.
#' @param concepts Optional concept filter (the part after the entity prefix),
#'   e.g. `c("gdp", "prices")`.
#' @param space `"rate"` (default) or `"level"` -- which realised series to
#'   compare. Rate is the right one: a level path is anchored, and stages that
#'   conditionally fill are anchored on a filled value.
#' @param tolerance Absolute agreement tolerance, in the units of `space`.
#'
#' @return A character vector of variable names, sorted, carrying a `dropped`
#'   attribute: a named numeric of the variables removed and the largest
#'   disagreement found in each.
#' @export
common_forecast_variables <- function(fc_list, concepts = NULL,
                                      space = c("rate", "level"),
                                      tolerance = 1e-8) {
  space <- match.arg(space)
  col <- paste0("actual_", space)
  shared <- Reduce(intersect, lapply(fc_list, function(f) unique(f$paths$variable)))
  if (!is.null(concepts)) shared <- shared[sub("^[a-z]+_", "", shared) %in% concepts]

  gaps <- vapply(shared, function(v) {
    by_stage <- lapply(fc_list, function(f) {
      p <- f$paths[f$paths$variable == v, ]
      stats::setNames(p[[col]], format(p$time, nsmall = 4))
    })
    times <- Reduce(intersect, lapply(by_stage, names))
    if (length(times) == 0) return(Inf)
    m <- vapply(by_stage, function(x) unname(x[times]), numeric(length(times)))
    if (!is.matrix(m)) m <- matrix(m, nrow = length(times))
    max(apply(m, 1, function(r) {
      r <- r[!is.na(r)]
      if (length(r) < 2) 0 else diff(range(r))
    }), na.rm = TRUE)
  }, numeric(1))

  keep <- gaps <= tolerance
  out <- sort(shared[keep])
  attr(out, "dropped") <- sort(gaps[!keep], decreasing = TRUE)
  out
}
