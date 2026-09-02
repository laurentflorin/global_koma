# Out-of-sample scoring, across countries and forecast variants.
#
# Two layers: (1) a thin wrapper around koma::model_evaluation() for scoring
# a single already-fitted system against a single evaluation window (the
# original, minimal API `_targets.R` wires in), and (2) a multi-origin
# re-estimating backtest harness (pseudo_oos_backtest() and friends) that
# sweeps many historical origins across the four specifications this
# project's evaluation report compares. See reports/evaluation.qmd for the
# design decisions (scoring space, origin feasibility, the naive benchmark,
# the DM-test nested-model caveat, the final-vintage bias) this file
# implements.

# --- single-fit scoring (the original, minimal API) ------------------------

#' Score one country's forecast against actuals
#'
#' Wraps `koma::model_evaluation()` for a single country/concept and
#' reshapes its output into this project's scoring schema. `model_evaluation()`
#' re-estimates `fit$sys_eq` at every rolling origin inside
#' `dates$forecast` and needs the **real, unabridged** series to score
#' against -- `fit$ts_data` will not do, because every `fit_stage1()` /
#' `fit_stage2()` fit in this project deliberately truncates its endogenous
#' series to `dates$estimation$end` (that's how the conditional-fill/COVID
#' trick works), so it has no real values past that point at all. `panel`
#' must therefore be the full, untruncated panel the fit's own training data
#' was drawn from.
#'
#' @param fit A `koma::koma_estimate` object, carrying `$sys_eq`.
#' @param iso2 Two-letter lowercase ISO country code.
#' @param concepts Character vector of concepts to score, e.g.
#'   `c("gdp", "prices")`.
#' @param dates koma `dates` list with a `forecast` range to evaluate over.
#' @param horizon Integer forecast horizon (see `?koma::model_evaluation`).
#' @param panel Named list of `koma_ts`, the full (untruncated) panel with
#'   real observations spanning `dates$forecast`.
#'
#' @return A `data.frame` with columns `iso2`, `concept`, `horizon`,
#'   `rmse`.
#' @export
score_country_forecast <- function(fit, iso2, concepts, dates, horizon, panel) {
  variables <- country_var(iso2, concepts)
  ev <- koma::model_evaluation(fit$sys_eq, variables, horizon, panel, dates,
                               evaluate_on_levels = TRUE)
  data.frame(
    iso2 = iso2,
    concept = rep(concepts, each = horizon),
    horizon = rep(seq_len(horizon), times = length(concepts)),
    rmse = as.numeric(as.matrix(ev[, variables, drop = FALSE])),
    stringsAsFactors = FALSE
  )
}

#' Score every country in a fitted system
#'
#' @param fit A `koma::koma_estimate` object for the joint system.
#' @param countries Character vector of ISO-2 country codes.
#' @param concepts Character vector of concepts to score for every
#'   country.
#' @param dates koma `dates` list with a `forecast` range to evaluate over.
#' @param horizon Integer forecast horizon.
#' @param panel Named list of `koma_ts`, passed through to
#'   [score_country_forecast()].
#'
#' @return A `data.frame`, the row-bound output of
#'   [score_country_forecast()] across `countries`.
#' @export
score_all_countries <- function(fit, countries, concepts, dates, horizon, panel) {
  do.call(rbind, lapply(countries, function(cc) {
    score_country_forecast(fit, cc, concepts, dates, horizon, panel)
  }))
}

#' Rank model variants by score
#'
#' @param scores A `data.frame` as returned by [score_all_countries()],
#'   with an added `variant` column identifying which model/spec produced
#'   each row.
#' @param by Column(s) to average `rmse` over before ranking, e.g.
#'   `c("concept")` for a per-concept leaderboard.
#'
#' @return A `data.frame` with columns `variant`, `by` columns, `mean_rmse`,
#'   `rank`, sorted best-first.
#' @export
leaderboard <- function(scores, by = "concept") {
  group_cols <- c("variant", by)
  agg <- stats::aggregate(scores["rmse"], scores[group_cols], mean, na.rm = TRUE)
  names(agg)[names(agg) == "rmse"] <- "mean_rmse"
  agg <- agg[order(agg$mean_rmse), ]
  agg$rank <- seq_len(nrow(agg))
  rownames(agg) <- NULL
  agg
}

# --- scoring-rule primitives -------------------------------------------------

#' Drop explosive draws before summarising a predictive sample
#'
#' koma's Gibbs sampler has no stationarity constraint, so a share of draws
#' diverge -- exactly the pathology documented in `R/spillovers.R`
#' (`scenario_diff()`'s `explosive_threshold`). An unfiltered mean/sd or KDE
#' over a draw sample with a fat exploded tail is not a usable predictive
#' summary; this applies the same threshold convention before any density
#' score is computed.
#'
#' @param draws Numeric vector of posterior draws.
#' @param threshold A draw is dropped if `abs(draws) > threshold`.
#'
#' @return A list with `kept` (the filtered numeric vector) and
#'   `explosive_frac` (share dropped).
#' @keywords internal
filter_explosive_draws <- function(draws, threshold = 100) {
  finite <- is.finite(draws)
  explosive <- finite & abs(draws) > threshold
  list(kept = draws[finite & !explosive],
      explosive_frac = if (sum(finite) > 0) mean(explosive[finite]) else NA_real_)
}

#' Empirical CRPS from a sample of predictive draws
#'
#' The standard unbiased sample estimator (Gneiting & Raftery 2007, eq. 5):
#' `mean(|draw - y|) - 0.5 * mean(|draw_i - draw_j|)` over all draw pairs.
#'
#' @param draws Numeric vector of predictive draws.
#' @param y The realised value.
#'
#' @return A scalar CRPS (lower is better), or `NA_real_` if fewer than two
#'   draws survive.
#' @export
crps_sample <- function(draws, y) {
  draws <- draws[is.finite(draws)]
  n <- length(draws)
  if (n < 2) {
    return(NA_real_)
  }
  term1 <- mean(abs(draws - y))
  term2 <- mean(abs(outer(draws, draws, "-")))
  term1 - term2 / 2
}

#' Closed-form CRPS of a Gaussian predictive distribution
#'
#' Standard formula: `sigma * (z*(2*Phi(z) - 1) + 2*phi(z) - 1/sqrt(pi))`,
#' `z = (y - mu) / sigma`.
#'
#' @param y The realised value.
#' @param mu,sigma Mean and standard deviation of the predictive Gaussian.
#'   `sigma` must be positive.
#'
#' @return A scalar CRPS.
#' @export
crps_gaussian <- function(y, mu, sigma) {
  if (!is.finite(sigma) || sigma <= 0) {
    cli::cli_abort("{.arg sigma} must be a positive, finite number.")
  }
  z <- (y - mu) / sigma
  sigma * (z * (2 * stats::pnorm(z) - 1) + 2 * stats::dnorm(z) - 1 / sqrt(pi))
}

#' Gaussian-kernel KDE log predictive density, evaluated at one point
#'
#' A direct Gaussian-kernel mixture log density (a mixture of `n` Gaussians
#' centred at each draw, bandwidth `bw`), evaluated exactly at `y` via
#' log-sum-exp for numerical stability -- avoids the grid/interpolation
#' edge cases of `stats::density()` + `approx()`.
#'
#' @param draws Numeric vector of predictive draws.
#' @param y The realised value.
#' @param bw Kernel bandwidth. `NULL` (the default) uses
#'   `stats::bw.nrd0(draws)`, falling back to a Silverman-rule bandwidth off
#'   the sample sd if that degenerates (e.g. every draw identical).
#'
#' @return A scalar log score (higher is better), or `NA_real_` if fewer
#'   than two draws survive.
#' @export
log_score_kde <- function(draws, y, bw = NULL) {
  draws <- draws[is.finite(draws)]
  n <- length(draws)
  if (n < 2) {
    return(NA_real_)
  }
  if (is.null(bw)) {
    bw <- stats::bw.nrd0(draws)
  }
  if (!is.finite(bw) || bw <= 0) {
    s <- stats::sd(draws)
    bw <- if (is.finite(s) && s > 0) s * n^(-1 / 5) else 1e-6
  }
  log_terms <- stats::dnorm(y, mean = draws, sd = bw, log = TRUE)
  m <- max(log_terms)
  m + log(mean(exp(log_terms - m)))
}

#' Log density of a Gaussian predictive distribution, evaluated at one point
#'
#' @param y The realised value.
#' @param mu,sigma Mean and standard deviation of the predictive Gaussian.
#'
#' @return A scalar log score.
#' @export
log_score_gaussian <- function(y, mu, sigma) {
  stats::dnorm(y, mean = mu, sd = sigma, log = TRUE)
}

# --- naive AR(1) / random-walk benchmark ------------------------------------

#' Fit AR(1) by OLS
#'
#' `y_t = mu + phi*(y_{t-1} - mu) + e_t`, fit as `y_t = a + b*y_{t-1}`, then
#' `phi = b`, `mu = a / (1 - b)`, `sigma2` the residual variance.
#'
#' @param x Numeric vector, at least 8 observations.
#'
#' @return A list with `phi`, `mu`, `sigma2`, `last`.
#' @keywords internal
fit_ar1 <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 8) {
    cli::cli_abort("Need at least 8 observations to fit an AR(1) naive benchmark; got {length(x)}.")
  }
  y <- x[-1]
  lag1 <- x[-length(x)]
  fit <- stats::lm(y ~ lag1)
  b <- unname(stats::coef(fit)[2])
  a <- unname(stats::coef(fit)[1])
  if (abs(1 - b) < 1e-8) {
    mu <- mean(x)
  } else {
    mu <- a / (1 - b)
  }
  list(phi = b, mu = mu, sigma2 = stats::var(stats::residuals(fit)), last = x[length(x)])
}

#' AR(1) naive benchmark, closed-form multi-step Gaussian forecast
#'
#' The standard textbook AR(1) forecast: `mean_h = mu + phi^h*(y_T - mu)`,
#' `var_h = sigma2 * sum_{j=0}^{h-1} phi^(2j)`.
#'
#' @param x Numeric vector, the training-window series in its own native
#'   (rate) space, ending at the origin.
#' @param horizon Integer, forecast horizon.
#'
#' @return A `data.frame` with columns `horizon`, `mean`, `sd`.
#' @export
naive_ar1_forecast <- function(x, horizon) {
  m <- fit_ar1(x)
  h <- seq_len(horizon)
  mean_h <- m$mu + m$phi^h * (m$last - m$mu)
  var_h <- m$sigma2 * cumsum(m$phi^(2 * (h - 1)))
  data.frame(horizon = h, mean = mean_h, sd = sqrt(pmax(var_h, 0)), stringsAsFactors = FALSE)
}

#' Driftless random-walk naive benchmark, closed-form Gaussian forecast
#'
#' The standard textbook driftless-RW forecast: flat at the last observed
#' level, `var_h = h * sigma_diff^2`.
#'
#' @param x Numeric vector, the training-window series ending at the origin.
#' @param horizon Integer, forecast horizon.
#'
#' @return A `data.frame` with columns `horizon`, `mean`, `sd`.
#' @export
naive_rw_forecast <- function(x, horizon) {
  x <- x[is.finite(x)]
  if (length(x) < 2) {
    cli::cli_abort("Need at least 2 observations for a random-walk naive benchmark; got {length(x)}.")
  }
  sigma_diff2 <- stats::var(diff(x))
  h <- seq_len(horizon)
  data.frame(horizon = h, mean = rep(x[length(x)], horizon),
            sd = sqrt(pmax(h * sigma_diff2, 0)), stringsAsFactors = FALSE)
}

#' Naive benchmark, dispatched by a series' own `method` tag
#'
#' AR(1) for `diff_log`/`percentage` (growth-rate) series, driftless random
#' walk for `rate`/`none` (already-a-rate) series -- the same
#' `series_type`/`method` dispatch convention used throughout this project.
#'
#' @param x A `koma_ts`, the training-window series ending at the origin,
#'   already passed through `koma::rate()` if it started in levels.
#' @param horizon Integer, forecast horizon.
#'
#' @return A `data.frame` with columns `horizon`, `mean`, `sd`, in the same
#'   (rate) space as `x`.
#' @export
naive_forecast <- function(x, horizon) {
  r <- koma::rate(x)
  method <- attr(r, "method") %||% "none"
  vals <- as.numeric(r)
  if (identical(method, "none")) {
    naive_rw_forecast(vals, horizon)
  } else {
    naive_ar1_forecast(vals, horizon)
  }
}

# --- rate -> level inversion for forecast draws -----------------------------

#' Invert every posterior forecast draw of one variable from rate to level
#'
#' `koma::forecast()`'s `$forecasts[[i]]` draws are plain numeric matrices in
#' each series' native (rate) space; only `$mean`/`$median` carry the
#' `series_type`/`method`/`anker` attributes `koma::level()` needs. This
#' reattaches those same attributes (identical across draws -- `anker` is
#' the pre-forecast actual level and its date, common to the whole batch)
#' onto each draw's column and calls `koma::level()`, verified against
#' `docs/koma-api.md`'s own round-trip example (`level(rate(x))` exact) in
#' this file's tests before being trusted on real backtest output.
#'
#' `koma::level()` returns one extra leading value -- the `anker` itself,
#' i.e. the last known **actual** level, not a forecast -- which is
#' dropped.
#'
#' @param fc A `koma::koma_forecast` object.
#' @param var Character scalar, the fully-qualified variable name.
#'
#' @return A numeric matrix, `horizon` rows by `length(fc$forecasts)`
#'   columns, in level space.
#' @export
forecast_draws_level <- function(fc, var) {
  m <- fc$mean[[var]]
  if (is.null(m)) {
    cli::cli_abort("{.arg fc} has no mean forecast for {.val {var}}.")
  }
  needed <- c("series_type", "method", "anker", "ets_attributes", "class")
  src_attrs <- attributes(m)
  tsp_m <- src_attrs$tsp
  h <- nrow(fc$forecasts[[1]])
  vapply(fc$forecasts, function(draw) {
    d <- stats::ts(draw[, var], start = tsp_m[1], frequency = tsp_m[3])
    attributes(d)[needed] <- src_attrs[needed]
    as.numeric(koma::level(d))[-1]
  }, numeric(h))
}

# --- the core reusable scorer --------------------------------------------

#' Score one `koma::forecast()` result against real actuals
#'
#' The one function every backtest driver (`backtest_stage1()` /
#' `backtest_stage2()` / `backtest_stage3()` / `backtest_naive()`) calls at
#' every origin -- the fit-call shape differs across specs (per-country vs.
#' joint-system), but scoring one already-produced forecast never does. One
#' row per `(variable, horizon)`.
#'
#' Explosive-draw filtering (`filter_explosive_draws()`) is applied
#' **per horizon** in rate space -- matching `scenario_diff()`'s own
#' per-cell convention -- and **cumulatively** in level space: a draw whose
#' rate explodes at horizon 3 corrupts its compounded level from horizon 3
#' onward even if horizon 1-2 were fine, so a level-space draw is dropped
#' from horizon `h` onward once its own rate has been explosive at any
#' horizon `<= h`.
#'
#' @param fc A `koma::koma_forecast` object.
#' @param panel Named list of `koma_ts`, the full (untruncated) panel with
#'   real observations spanning the forecast window.
#' @param variables Character vector of fully-qualified variable names.
#' @param origin `c(year, quarter)`, this forecast's origin (recorded, not
#'   used in the scoring itself).
#' @param horizon Integer, how many forecast steps to score.
#' @param explosive_threshold Passed to `filter_explosive_draws()`.
#'
#' @return A `data.frame`, one row per `(variable, horizon)`, with columns
#'   `variable, horizon, origin_year, origin_quarter, point_forecast,
#'   actual, sq_error, abs_error, crps, log_score, explosive_frac,
#'   point_forecast_level, actual_level, sq_error_level, abs_error_level`.
#'   Rows for a variable missing from `fc$mean` or `panel` are silently
#'   omitted (a spec's `variables` list can span countries the caller does
#'   not care to score every concept for).
#' @export
score_forecast <- function(fc, panel, variables, origin, horizon, explosive_threshold = 100) {
  rows <- lapply(variables, function(var) {
    m <- fc$mean[[var]]
    if (is.null(m) || is.null(panel[[var]])) {
      return(NULL)
    }
    tsp_m <- attr(m, "tsp")
    h_avail <- nrow(fc$forecasts[[1]])
    hh <- min(horizon, h_avail)

    actual_rate_full <- koma::rate(panel[[var]])
    actual_rate <- as.numeric(stats::window(actual_rate_full, start = tsp_m[1], frequency = tsp_m[3]))
    actual_level <- as.numeric(stats::window(panel[[var]], start = tsp_m[1], frequency = tsp_m[3]))
    hh <- min(hh, length(actual_rate), length(actual_level))
    if (hh < 1) {
      return(NULL)
    }

    draws_rate <- t(vapply(fc$forecasts, function(d) d[seq_len(hh), var], numeric(hh))) # ndraws x hh
    explosive_raw <- abs(draws_rate) > explosive_threshold
    explosive_cum <- t(apply(explosive_raw, 1, cummax)) # contaminated from first explosion onward
    lv <- forecast_draws_level(fc, var)[seq_len(hh), , drop = FALSE] # hh x ndraws

    do.call(rbind, lapply(seq_len(hh), function(h) {
      draws_h <- draws_rate[, h]
      keep_rate <- !explosive_raw[, h]
      filt <- list(kept = draws_h[keep_rate], explosive_frac = mean(explosive_raw[, h]))
      point <- if (length(filt$kept) > 0) stats::median(filt$kept) else NA_real_
      y <- actual_rate[h]

      lv_h <- lv[h, ]
      keep_level <- !explosive_cum[, h]
      lv_kept <- lv_h[keep_level]
      point_lv <- if (length(lv_kept) > 0) stats::median(lv_kept) else NA_real_
      y_lv <- actual_level[h]

      data.frame(
        variable = var, horizon = h, origin_year = origin[1], origin_quarter = origin[2],
        point_forecast = point, actual = y, sq_error = (point - y)^2, abs_error = abs(point - y),
        crps = crps_sample(filt$kept, y), log_score = log_score_kde(filt$kept, y),
        explosive_frac = filt$explosive_frac,
        point_forecast_level = point_lv, actual_level = y_lv,
        sq_error_level = (point_lv - y_lv)^2, abs_error_level = abs(point_lv - y_lv),
        stringsAsFactors = FALSE
      )
    }))
  })
  do.call(rbind, rows)
}

#' Score the closed-form naive benchmark the same way `score_forecast()` does
#'
#' Uses `crps_gaussian()`/`log_score_gaussian()` in place of the
#' empirical-draw estimators (there is no draw sample, and none of the
#' explosive-draw machinery applies -- a closed-form Gaussian forecast
#' cannot explode). Level space is intentionally **not** scored here: the
#' naive benchmark is fit directly on each series' own native (rate) space,
#' and back-converting a Gaussian rate forecast to levels needs the same
#' `koma::level()` machinery `forecast_draws_level()` wraps around real
#' draws, which buys nothing extra for a benchmark whose whole point is
#' being cheap and simple.
#'
#' @param panel Named list of `koma_ts`.
#' @param variables Character vector of fully-qualified variable names.
#' @param origin `c(year, quarter)`, the training window's end (inclusive).
#' @param horizon Integer forecast horizon.
#'
#' @return A `data.frame`, one row per `(variable, horizon)`, with the same
#'   `point_forecast, actual, sq_error, abs_error, crps, log_score` columns
#'   `score_forecast()` produces (no `explosive_frac` or level-space
#'   columns -- always `NA`, kept so `rbind()` against `score_forecast()`
#'   output works without reshaping).
#' @export
score_naive_forecast <- function(panel, variables, origin, horizon) {
  rows <- lapply(variables, function(var) {
    x <- panel[[var]]
    if (is.null(x)) {
      return(NULL)
    }
    freq <- stats::frequency(x)
    r <- koma::rate(x)
    train <- stats::window(r, end = origin)
    if (sum(is.finite(as.numeric(train))) < 8) {
      return(NULL)
    }
    forecast_start <- advance_periods(origin, 1, freq)
    end_r <- stats::end(r)
    to_idx <- function(yq) yq[1] * freq + (yq[2] - 1)
    hh <- min(horizon, to_idx(end_r) - to_idx(forecast_start) + 1)
    if (!is.finite(hh) || hh < 1) {
      return(NULL)
    }
    f <- tryCatch(naive_forecast(train, hh), error = function(e) NULL)
    if (is.null(f)) {
      return(NULL)
    }
    forecast_end <- advance_periods(origin, hh, freq)
    actual <- as.numeric(stats::window(r, start = forecast_start, end = forecast_end))
    data.frame(
      variable = var, horizon = seq_len(hh), origin_year = origin[1], origin_quarter = origin[2],
      point_forecast = f$mean, actual = actual, sq_error = (f$mean - actual)^2, abs_error = abs(f$mean - actual),
      crps = mapply(crps_gaussian, actual, f$mean, f$sd), log_score = mapply(log_score_gaussian, actual, f$mean, f$sd),
      explosive_frac = NA_real_, point_forecast_level = NA_real_, actual_level = NA_real_,
      sq_error_level = NA_real_, abs_error_level = NA_real_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# --- Diebold-Mariano test ----------------------------------------------------

#' Diebold-Mariano test on a loss differential
#'
#' Newey-West (HAC) variance with lag `h - 1`, the standard choice for
#' `h`-step-ahead loss differentials (their autocorrelation is induced
#' mechanically by the overlapping-horizon forecast errors, not a modelling
#' assumption). This is the **plain** DM test with no small-sample or
#' nested-model correction -- see `reports/evaluation.qmd` for the
#' Clark & McCracken (2001) caveat this project states rather than
#' corrects for, since Stage 1 is nested in Stage 2 which is nested in
#' Stage 3.
#'
#' @param loss_a,loss_b Numeric vectors of equal length, the per-origin (or
#'   per-origin-and-country) loss of variant A and variant B under the same
#'   scoring rule (e.g. squared error, or CRPS).
#' @param h Integer, the forecast horizon these losses were computed at
#'   (sets the HAC lag).
#'
#' @return A list with `statistic` (the DM t-statistic; negative favours A),
#'   `p_value` (two-sided, normal reference), `mean_diff` (`mean(loss_a -
#'   loss_b)`), and `n`.
#' @export
diebold_mariano <- function(loss_a, loss_b, h = 1) {
  d <- loss_a - loss_b
  d <- d[is.finite(d)]
  n <- length(d)
  if (n < 2) {
    cli::cli_abort("Need at least 2 finite loss differentials for a DM test; got {n}.")
  }
  d_bar <- mean(d)
  lag_max <- max(0, h - 1)
  gamma0 <- stats::var(d) * (n - 1) / n
  var_d <- gamma0
  if (lag_max > 0) {
    for (k in seq_len(min(lag_max, n - 1))) {
      cov_k <- sum((d[1:(n - k)] - d_bar) * (d[(1 + k):n] - d_bar)) / n
      var_d <- var_d + 2 * cov_k
    }
  }
  se <- sqrt(max(var_d, 0) / n)
  statistic <- if (se > 0) d_bar / se else NA_real_
  p_value <- if (is.finite(statistic)) 2 * stats::pnorm(-abs(statistic)) else NA_real_
  list(statistic = statistic, p_value = p_value, mean_diff = d_bar, n = n)
}

# --- origin feasibility ------------------------------------------------------

#' Whether a system can be estimated at all on a given window
#'
#' The same `k`-vs-`T` gate `stage2_preflight()` and the stage-3c
#' feasibility frontier already use, exposed as a cheap, pre-`estimate()`
#' check for the backtest harness: `estimation_length()` (existing) against
#' `length(sys_eq$total_exogenous_variables)`.
#'
#' @param sys_eq A `koma_seq`.
#' @param panel Named list of `koma_ts`.
#' @param dates koma `dates` list.
#'
#' @return A one-row `data.frame` with `k`, `t`, `df`, `feasible` (`df > 0`).
#' @export
origin_feasible <- function(sys_eq, panel, dates) {
  k <- length(sys_eq$total_exogenous_variables)
  t <- estimation_length(panel, dates)
  data.frame(k = k, t = t, df = t - k, feasible = (t - k) > 0)
}

# --- backtest origin sequence ------------------------------------------------

#' A sequence of backtest origins
#'
#' @param start,end koma-style `c(year, quarter)` pairs, inclusive.
#' @param frequency_quarters Spacing between origins, in quarters (1 =
#'   every quarter, 4 = annual).
#'
#' @return A list of `c(year, quarter)` origins.
#' @export
backtest_origins <- function(start, end, frequency_quarters = 4) {
  to_idx <- function(yq) yq[1] * 4 + (yq[2] - 1)
  from_idx <- function(idx) c(idx %/% 4, idx %% 4 + 1)
  idx <- seq(to_idx(start), to_idx(end), by = frequency_quarters)
  lapply(idx, from_idx)
}

#' Label an origin for a cache filename or a progress message
#' @keywords internal
origin_label <- function(origin) sprintf("%dQ%d", origin[1], origin[2])

#' Read/write one origin's cached backtest result, or compute it fresh
#'
#' @param cache_dir Directory, or `NULL` to disable caching.
#' @param origin `c(year, quarter)`.
#' @param compute A zero-argument function producing `list(meta, scores)`.
#' @keywords internal
cached_origin_result <- function(cache_dir, origin, compute) {
  if (is.null(cache_dir)) {
    return(compute())
  }
  cache_file <- file.path(cache_dir, paste0(origin_label(origin), ".rds"))
  if (file.exists(cache_file)) {
    return(readRDS(cache_file))
  }
  result <- compute()
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(result, cache_file)
  result
}

# --- stage-1 backtest --------------------------------------------------------

#' Pseudo-out-of-sample backtest of the stage-1 per-country models
#'
#' At each origin: builds contiguous (no COVID-fill gap -- a real backtest
#' forecasts the very next quarter, not three years out) `stage1_dates()`,
#' fits every country via `fit_stage1_all()`, forecasts, and scores every
#' requested concept via `score_forecast()`. Stage 1 has no COVID dummy
#' mechanism (see `reports/evaluation.qmd`'s design notes) -- once an
#' origin passes 2020, the collapse enters its estimation sample as
#' ordinary data, and this is disclosed rather than worked around.
#'
#' @param panel Named list of `koma_ts`, the full (untruncated) panel.
#' @param countries Character vector of ISO-2 codes.
#' @param origins A list of `c(year, quarter)` origins, e.g. from
#'   `backtest_origins()`.
#' @param horizon Integer forecast horizon.
#' @param concepts Character vector of concepts to score for every country.
#' @param options Passed to `fit_stage1_all(options = )`.
#' @param cache_dir Directory to cache each origin's `list(meta, scores)`
#'   into (`data/cache/evaluation/stage1/<origin>.rds`-shaped), or `NULL`.
#'
#' @return A list with `scores` (long `data.frame`) and `meta` (one row per
#'   origin: `k` is `NA` for stage 1 -- there is no joint-system feasibility
#'   gate to report -- `t`, `df`, `feasible` likewise `NA`, `runtime_s`).
#' @export
backtest_stage1 <- function(panel, countries, origins, horizon, concepts, options = list(), cache_dir = NULL) {
  results <- lapply(origins, function(origin) {
    lbl <- origin_label(origin)
    cached_origin_result(cache_dir, origin, function() {
      cli::cli_inform("stage1 origin {lbl}")
      dates <- stage1_dates(panel, estimation_end = origin,
                            forecast_start = advance_periods(origin, 1, 4),
                            forecast_end = advance_periods(origin, horizon, 4))
      started <- Sys.time()
      fits <- tryCatch(
        fit_stage1_all(countries, panel, dates, options = options, cache_dir = NULL),
        error = function(e) {
          cli::cli_warn("stage1 origin {lbl}: {conditionMessage(e)}")
          NULL
        }
      )
      runtime_s <- as.numeric(Sys.time() - started, units = "secs")
      meta <- data.frame(spec = "stage1", origin_year = origin[1], origin_quarter = origin[2],
                         k = NA_real_, t = NA_real_, df = NA_real_, feasible = !is.null(fits),
                         runtime_s = runtime_s, stringsAsFactors = FALSE)
      if (is.null(fits)) {
        return(list(meta = meta, scores = NULL))
      }
      scored <- do.call(rbind, lapply(countries, function(cc) {
        fc <- tryCatch(
          koma::forecast(fits[[cc]], dates = dates,
                         options = list(approximate = FALSE, probs = c(0.05, 0.95))),
          error = function(e) {
            cli::cli_warn("stage1 origin {lbl} ({cc}): forecast failed: {conditionMessage(e)}")
            NULL
          }
        )
        if (is.null(fc)) {
          return(NULL)
        }
        score_forecast(fc, panel, country_var(cc, concepts), origin, horizon)
      }))
      list(meta = meta, scores = scored)
    })
  })
  list(
    scores = do.call(rbind, lapply(results, `[[`, "scores")),
    meta = do.call(rbind, lapply(results, `[[`, "meta"))
  )
}

# --- stage-2 / stage-3 backtest (shared joint-system machinery) -------------

#' Shared driver for a joint (stage-2 or stage-3-shaped) backtest
#'
#' Rebuilds `sys_eq`/panel per origin (cheap -- symbolic spec assembly, not
#' estimation) via the existing production builders, checks
#' `origin_feasible()` **before** calling the expensive `estimate()`, and
#' skips (logging `k`/`T`/`df`) an infeasible origin rather than letting
#' koma fail deep inside a Gibbs sampler run. `opts_fn(origin, dummies)`
#' is the one thing that differs between stage 2 and stage 3 -- everything
#' else (linkage weights, share computation, panel construction, caching,
#' scoring) is identical.
#'
#' @param panel Named list of `koma_ts`.
#' @param countries Character vector of ISO-2 codes.
#' @param trade_weights,gdp_weights Passed to `stage2_linkage_weights()`.
#' @param origins A list of `c(year, quarter)` origins.
#' @param horizon Integer forecast horizon.
#' @param concepts Character vector of concepts to score for every country.
#' @param opts_fn Function `(origin, dummies) -> stage2_options()` result.
#' @param config A `stage2b_config()`-shaped list (`threshold`,
#'   `ireland_proxy`, `opts`).
#' @param tau Passed to `build_stage2_system(tau = )` -- a fixed,
#'   already-tuned tau, reused at every origin (no per-origin re-tuning,
#'   see `reports/evaluation.qmd`).
#' @param workers Passed to `fit_stage2()`.
#' @param cache_dir Directory to cache each origin's `list(meta, scores)`
#'   into, or `NULL`.
#' @param panel_extra_fn Function `(dummies) -> list(...)` of extra
#'   `build_stage2_panel()` arguments (`labour_countries`, `hicp_weights`,
#'   `external_countries`, `financial_countries`, `spread_countries`,
#'   `policy_rate`) -- empty by default (stage 2). Called as
#'   `panel_extra_fn(dummies, opts)`, because a stage-2c panel needs the
#'   policy-rate map derived from that origin's own `opts` to keep the
#'   constructed `<iso2>_spread` series and the long-rate identity in step.
#'   `config$demand_concept` (default `"gdp"`) selects the `foreign_demand`
#'   basis, so a stage-2c backtest re-weights the identity the same way its
#'   production fit does.
#' @param spec_label Character scalar recorded in `meta$spec`.
#'
#' @return A list with `scores` (long `data.frame`) and `meta` (one row per
#'   origin, including infeasible/skipped ones).
#' @keywords internal
backtest_joint_system <- function(panel, countries, trade_weights, gdp_weights, origins, horizon, concepts,
                                  opts_fn, config = stage2b_config(), tau = NULL, workers = NULL,
                                  options = list(), cache_dir = NULL,
                                  panel_extra_fn = function(dummies, opts) list(),
                                  spec_label = "stage2") {
  lw <- stage2_linkage_weights(countries, trade_weights, gdp_weights,
                               threshold = config$threshold %||% 0,
                               ireland_proxy = config$ireland_proxy %||% FALSE,
                               demand_concept = config$demand_concept %||% "gdp")
  results <- lapply(origins, function(origin) {
    lbl <- origin_label(origin)
    cached_origin_result(cache_dir, origin, function() {
      cli::cli_inform("{spec_label} origin {lbl}")
      dates <- list(
        estimation = list(start = c(2000, 1), end = origin),
        forecast = list(start = advance_periods(origin, 1, 4), end = advance_periods(origin, horizon, 4))
      )
      dummies <- stage2b_dummies_through(origin)
      opts <- opts_fn(origin, dummies)
      shares <- stage2_shares(countries, panel, dates, opts)
      spec <- stage2_spec(countries, shares, lw, opts = opts)
      sys_eq <- build_stage2_system(spec, tau = tau)
      panel_extra <- panel_extra_fn(dummies, opts)
      stage_panel <- do.call(build_stage2_panel, c(
        list(panel = panel, linkage_weights = lw, dummies = dummies), panel_extra
      ))
      of <- origin_feasible(sys_eq, stage_panel, dates)
      meta <- data.frame(spec = spec_label, origin_year = origin[1], origin_quarter = origin[2],
                         k = of$k, t = of$t, df = of$df, feasible = of$feasible,
                         runtime_s = NA_real_, stringsAsFactors = FALSE)
      if (!of$feasible) {
        cli::cli_warn("{spec_label} origin {lbl}: infeasible (k={of$k}, T={of$t}, df={of$df}) -- skipped.")
        return(list(meta = meta, scores = NULL))
      }
      started <- Sys.time()
      fit <- tryCatch(
        fit_stage2(sys_eq, stage_panel, dates, options = options, workers = workers),
        error = function(e) {
          cli::cli_warn("{spec_label} origin {lbl}: estimate failed: {conditionMessage(e)}")
          NULL
        }
      )
      meta$runtime_s <- as.numeric(Sys.time() - started, units = "secs")
      if (is.null(fit)) {
        return(list(meta = meta, scores = NULL))
      }
      fc <- tryCatch(
        koma::forecast(fit, dates = dates, options = list(approximate = FALSE, probs = c(0.05, 0.95))),
        error = function(e) {
          cli::cli_warn("{spec_label} origin {lbl}: forecast failed: {conditionMessage(e)}")
          NULL
        }
      )
      scored <- if (!is.null(fc)) {
        variables <- unlist(lapply(countries, function(cc) country_var(cc, concepts)))
        score_forecast(fc, panel, variables, origin, horizon)
      } else {
        NULL
      }
      list(meta = meta, scores = scored)
    })
  })
  list(
    scores = do.call(rbind, lapply(results, `[[`, "scores")),
    meta = do.call(rbind, lapply(results, `[[`, "meta"))
  )
}

#' Pseudo-out-of-sample backtest of the stage-2 linked core system
#'
#' @inheritParams backtest_joint_system
#' @export
backtest_stage2 <- function(panel, countries, trade_weights, gdp_weights, origins, horizon, concepts,
                            config = stage2b_config(), tau = NULL, workers = NULL, options = list(),
                            cache_dir = NULL) {
  opts_fn <- function(origin, dummies) {
    b2b <- config$opts
    stage2_options(include_government = FALSE, extra_regressors = dummies,
                   policy_rule = b2b$policy_rule %||% FALSE, fx = b2b$fx %||% character())
  }
  backtest_joint_system(panel, countries, trade_weights, gdp_weights, origins, horizon, concepts,
                        opts_fn, config = config, tau = tau, workers = workers, options = options,
                        cache_dir = cache_dir, spec_label = "stage2")
}

#' Pseudo-out-of-sample backtest of the stage-2c refined core system
#'
#' The same eleven countries and window logic as [backtest_stage2()], with
#' stage 2c's four refinements ([stage2c_config()]). Because every refinement
#' is `k`-neutral, **stage 2c has exactly the same `k` as stage 2b at every
#' origin**, so the two share a feasibility profile and are scored on an
#' identical origin set -- which is what makes the comparison in
#' `reports/evaluation.qmd` clean rather than confounded by different
#' usable-origin counts.
#'
#' Note the two things this must thread through that a stage-2 backtest does
#' not: `config$demand_concept` (partner imports rather than partner GDP in
#' the `foreign_demand` identity) and, via `panel_extra_fn`, the
#' `spread_countries` panel series plus the [policy_rate_map()] the long-rate
#' identity is written against. Getting the second wrong violates the US
#' identity silently -- see `CLAUDE.md`.
#'
#' @inheritParams backtest_joint_system
#' @param spread_countries Which countries model the spread. Defaults to
#'   `countries`, matching the production configuration.
#' @export
backtest_stage2c <- function(panel, countries, trade_weights, gdp_weights, origins, horizon, concepts,
                             config = stage2c_config(), tau = NULL, workers = NULL, options = list(),
                             cache_dir = NULL, spread_countries = NULL) {
  spread_countries <- spread_countries %||% countries
  opts_fn <- function(origin, dummies) {
    b <- config$opts
    stage2_options(
      include_government = FALSE, extra_regressors = dummies,
      policy_rule = b$policy_rule %||% FALSE, fx = b$fx %||% character(),
      phillips_countries = b$phillips_countries %||% character(),
      consumption_rate_countries = b$consumption_rate_countries %||% character(),
      import_content_countries = b$import_content_countries %||% character(),
      spread_countries = b$spread_countries %||% character()
    )
  }
  panel_extra_fn <- function(dummies, opts) {
    list(spread_countries = spread_countries, policy_rate = policy_rate_map(opts))
  }
  backtest_joint_system(panel, countries, trade_weights, gdp_weights, origins, horizon, concepts,
                        opts_fn, config = config, tau = tau, workers = workers, options = options,
                        cache_dir = cache_dir, panel_extra_fn = panel_extra_fn,
                        spec_label = "stage2c")
}

#' Pseudo-out-of-sample backtest of the stage-3 extended system
#'
#' Germany-only labour/external/fiscal/financial blocks, matching the fitted
#' system `reports/stage3b_external_fiscal_financial.qmd` and
#' `reports/stage3c_rollout.qmd` established -- the full eleven-country
#' rollout was found infeasible (`k` too large against `T`, see stage 3c),
#' so "stage 3" in this backtest is that same DE-only extended
#' configuration, not the (infeasible) rollout.
#'
#' @inheritParams backtest_joint_system
#' @param hicp_weights Named list, `iso2 -> named numeric vector`, one entry
#'   per `labour_countries` member (see `hicp_weights()`). Reused as-is at
#'   every origin rather than recomputed per origin's own growing window --
#'   a disclosed simplification: HICP basket weights re-base annually but
#'   move little year to year (CLAUDE.md), and recomputing them would mean a
#'   fresh Eurostat query per origin.
#' @param labour_countries,external_countries,fiscal_countries,financial_countries
#'   Character vectors, default `"de"` for all four (the configuration
#'   stage 3b/3c estimated and validated).
#' @export
backtest_stage3 <- function(panel, countries, trade_weights, gdp_weights, hicp_weights, origins, horizon, concepts,
                            config = stage2b_config(), tau = NULL, workers = NULL, options = list(),
                            cache_dir = NULL, labour_countries = "de", external_countries = "de",
                            fiscal_countries = "de", financial_countries = "de") {
  opts_fn <- function(origin, dummies) {
    b2b <- config$opts
    stage2_options(
      include_government = if (length(fiscal_countries) > 0) fiscal_countries else FALSE,
      extra_regressors = dummies, policy_rule = b2b$policy_rule %||% FALSE, fx = b2b$fx %||% character(),
      labour_countries = labour_countries, hicp_weights = hicp_weights,
      external_countries = external_countries, fiscal_countries = fiscal_countries,
      financial_countries = financial_countries
    )
  }
  panel_extra_fn <- function(dummies, opts) {
    list(labour_countries = labour_countries, hicp_weights = hicp_weights,
        external_countries = external_countries, financial_countries = financial_countries,
        policy_rate = policy_rate_map(opts))
  }
  backtest_joint_system(panel, countries, trade_weights, gdp_weights, origins, horizon, concepts,
                        opts_fn, config = config, tau = tau, workers = workers, options = options,
                        cache_dir = cache_dir, panel_extra_fn = panel_extra_fn, spec_label = "stage3")
}

# --- naive benchmark backtest ------------------------------------------------

#' Pseudo-out-of-sample "backtest" of the closed-form naive benchmark
#'
#' No `estimate()`/`forecast()` calls -- `score_naive_forecast()` is
#' closed-form and effectively free, so this just sweeps origins and
#' countries.
#'
#' @param panel Named list of `koma_ts`.
#' @param countries Character vector of ISO-2 codes.
#' @param origins A list of `c(year, quarter)` origins.
#' @param horizon Integer forecast horizon.
#' @param concepts Character vector of concepts to score for every country.
#'
#' @return A long `data.frame`, the row-bound output of
#'   `score_naive_forecast()` across `origins` and `countries`.
#' @export
backtest_naive <- function(panel, countries, origins, horizon, concepts) {
  do.call(rbind, lapply(origins, function(origin) {
    variables <- unlist(lapply(countries, function(cc) country_var(cc, concepts)))
    score_naive_forecast(panel, variables, origin, horizon)
  }))
}
