test_that("quarter_label formats decimal ts times as quarters", {
  expect_equal(quarter_label(2025), "2025Q1")
  expect_equal(quarter_label(2025.25), "2025Q2")
  expect_equal(quarter_label(c(2024.75, 2025.5)), c("2024Q4", "2025Q3"))
})

test_that("lookup_ts reads by date and returns NA off the ends", {
  x <- stats::ts(c(10, 20, 30, 40), start = c(2020, 1), frequency = 4)
  expect_equal(lookup_ts(x, c(2020, 2020.5, 2020.75), 4), c(10, 30, 40))
  # Before the start and after the end are NA, not a wrapped or clamped value.
  expect_equal(lookup_ts(x, c(2019.75, 2021), 4), c(NA_real_, NA_real_))
})

test_that("advance_periods adds quarters, including across a year boundary", {
  expect_equal(advance_periods(c(2020, 1), 0), c(2020, 1))
  expect_equal(advance_periods(c(2020, 1), 3), c(2020, 4))
  expect_equal(advance_periods(c(2020, 1), 4), c(2021, 1))
  expect_equal(advance_periods(c(2020, 3), 7), c(2022, 2))
})

test_that("forecast_draws_level_matrix reproduces koma::level() to machine precision", {
  skip_on_cran()
  s <- synthetic_fit()
  ext <- extend_forecast_horizon(s$fit, s$panel, 4)
  set.seed(3)
  fc <- suppressWarnings(koma::forecast(
    ext$fit, dates = ext$dates, options = list(approximate = FALSE, probs = c(0.1, 0.9))
  ))
  h <- nrow(fc$forecasts[[1]])

  # The slow path: koma::level() once per draw, with the mean forecast's
  # attributes put back on each draw (the draws carry none).
  koma_level <- function(var) {
    m <- fc$mean[[var]]
    keep <- c("series_type", "method", "anker", "ets_attributes", "class")
    a <- attributes(m)
    vapply(fc$forecasts, function(draw) {
      d <- stats::ts(draw[, var], start = a$tsp[1], frequency = a$tsp[3])
      attributes(d)[keep] <- a[keep]
      as.numeric(koma::level(d))[-1]
    }, numeric(h))
  }

  compared <- 0
  for (v in s$fit$sys_eq$endogenous_variables) {
    rate_draws <- vapply(fc$forecasts, function(d) d[seq_len(h), v], numeric(h))
    fast <- forecast_draws_level_matrix(fc, v, rate_draws)
    slow <- tryCatch(koma_level(v)[seq_len(h), , drop = FALSE], error = function(e) NULL)
    if (is.null(slow) || is.null(fast)) next
    compared <- compared + 1
    expect_equal(fast, slow, tolerance = 1e-12, info = paste("level inversion differs for", v))
  }
  # If this drops to zero the test has stopped testing anything.
  expect_gt(compared, 0)
})

test_that("a method = 'none' series is returned unchanged rather than dropped", {
  # Interest rates are rate/none: koma passes those numbers through untouched
  # and gives them no `anker`, so koma::level() errors on them. Level space IS
  # rate space there.
  fake <- list(mean = list(
    r = structure(stats::ts(c(1, 2), start = c(2025, 1), frequency = 4),
                  series_type = "rate", method = "none", anker = NA)
  ))
  draws <- matrix(c(1, 2, 3, 4), nrow = 2)
  expect_identical(forecast_draws_level_matrix(fake, "r", draws), draws)
})

test_that("a series with no usable anker yields NULL rather than a wrong level", {
  fake <- list(mean = list(
    p = structure(stats::ts(c(1, 2), start = c(2025, 1), frequency = 4),
                  series_type = "rate", method = "diff_log", anker = c(NA, 2025))
  ))
  expect_null(forecast_draws_level_matrix(fake, "p", matrix(c(1, 2, 3, 4), nrow = 2)))
})

test_that("forecast_system delivers the horizon asked for and labels history", {
  skip_on_cran()
  s <- synthetic_fit()
  f <- forecast_system(s$fit, s$panel, horizon = 6, history = 4,
                       variables = c("de_gdp", "de_prices"))

  expect_equal(f$horizon, 6)
  fore <- f$paths[f$paths$kind == "forecast", ]
  expect_equal(sort(unique(fore$horizon)), 1:6)
  expect_setequal(unique(f$paths$kind), c("history", "forecast"))
  expect_equal(sum(f$paths$kind == "history" & f$paths$variable == "de_gdp"), 4)

  # History runs strictly before the forecast, with no overlap.
  expect_lt(max(f$paths$time[f$paths$kind == "history"]),
            min(f$paths$time[f$paths$kind == "forecast"]))
  # The fan is ordered: lower <= median <= upper wherever it is defined.
  ok <- !is.na(fore$rate_lower)
  expect_true(all(fore$rate_lower[ok] <= fore$rate_median[ok]))
  expect_true(all(fore$rate_median[ok] <= fore$rate_upper[ok]))
  # The forecast window 2018Q1-2019Q2 is inside the panel, so it is scored.
  expect_gt(f$n_actual, 0)
})

test_that("forecast_system is reproducible for a given seed and varies without one", {
  skip_on_cran()
  s <- synthetic_fit()
  a <- forecast_system(s$fit, s$panel, horizon = 4, variables = "de_gdp", seed = 11)
  b <- forecast_system(s$fit, s$panel, horizon = 4, variables = "de_gdp", seed = 11)
  expect_equal(a$paths, b$paths)

  # koma's forecasts are not reproducible call-to-call by default, which is
  # exactly why `seed` exists -- a different seed must give a different path.
  c2 <- forecast_system(s$fit, s$panel, horizon = 4, variables = "de_gdp", seed = 12)
  expect_false(isTRUE(all.equal(a$paths$rate_median, c2$paths$rate_median)))
})

test_that("forecast_system rejects an unknown variable and a bad probs pair", {
  skip_on_cran()
  s <- synthetic_fit()
  expect_error(forecast_system(s$fit, s$panel, variables = "xx_gdp"), "not endogenous")
  expect_error(forecast_system(s$fit, s$panel, probs = c(0.9, 0.1)), "increasing quantiles")
})

test_that("explosive draws are filtered per horizon in rates and cumulatively in levels", {
  skip_on_cran()
  s <- synthetic_fit()
  # A threshold of zero makes every finite draw "explosive", so every quantile
  # must come back NA rather than being computed on an empty vector.
  f <- forecast_system(s$fit, s$panel, horizon = 3, variables = "de_gdp",
                       explosive_threshold = 0)
  fore <- f$paths[f$paths$kind == "forecast", ]
  expect_true(all(fore$explosive_frac == 1))
  expect_true(all(is.na(fore$rate_median)))
  expect_true(all(is.na(fore$level_median)))
})

test_that("forecast_explosive_table summarises the forecast rows only", {
  skip_on_cran()
  s <- synthetic_fit()
  f <- forecast_system(s$fit, s$panel, horizon = 4, variables = c("de_gdp", "fr_gdp"))
  ex <- forecast_explosive_table(f$paths)
  expect_setequal(ex$variable, c("de_gdp", "fr_gdp"))
  expect_true(all(ex$h1 >= 0 & ex$h1 <= 1))
  expect_true(all(ex$max >= ex$h1))
})

test_that("extend_forecast_horizon is a no-op when the horizon is already covered", {
  skip_on_cran()
  s <- synthetic_fit()
  ext <- extend_forecast_horizon(s$fit, s$panel, quarters = 2)
  expect_equal(nrow(ext$extension), 0)
  expect_equal(ext$dates$forecast$end, c(2018, 2))
  expect_equal(length(ext$fit$ts_data$row_imports), length(s$fit$ts_data$row_imports))
})

test_that("no exogenous series is extended when the data already reaches the horizon", {
  skip_on_cran()
  # The panel runs to 2019Q4 and an 8-quarter horizon from 2018Q1 ends exactly
  # there, so nothing is short. Extending anyway would append points past the
  # forecast end that no draw ever reads.
  s <- synthetic_fit()
  ext <- extend_forecast_horizon(s$fit, s$panel, quarters = 8)
  expect_equal(ext$dates$forecast$end, c(2019, 4))
  expect_equal(nrow(ext$extension), 0)
  expect_equal(length(ext$fit$ts_data$row_imports), length(s$fit$ts_data$row_imports))
})

test_that("extend_forecast_horizon extends level exogenous series by trailing growth", {
  skip_on_cran()
  s <- synthetic_fit()
  # 12 quarters from 2018Q1 ends 2020Q4, four past the panel's own end, so the
  # extrapolation actually has to run.
  ext <- extend_forecast_horizon(s$fit, s$panel, quarters = 12)
  added <- stats::setNames(ext$extension$periods_added, ext$extension$variable)

  expect_equal(ext$dates$forecast$end, c(2020, 4))
  expect_equal(unname(added[["row_imports"]]), 4)
  expect_equal(length(ext$fit$ts_data$row_imports), length(s$fit$ts_data$row_imports) + 4)

  # The fit's ts_data is in rate space, so the appended values are the stated
  # trailing-4Q average growth rate itself.
  avg <- mean(utils::tail(as.numeric(koma::rate(s$panel$row_imports)), 4))
  expect_equal(utils::tail(as.numeric(ext$fit$ts_data$row_imports), 4), rep(avg, 4), tolerance = 1e-6)

  # A dummy is extended with zeros.
  expect_equal(ext$extension$method[ext$extension$variable == "covid_2009q1"], "zero (dummy)")
  expect_equal(utils::tail(as.numeric(ext$fit$ts_data$covid_2009q1), 4), rep(0, 4))
})

test_that("extend_forecast_horizon holds a rate/none exogenous flat, not compounding it", {
  skip_on_cran()
  s <- synthetic_fit()
  ext <- extend_forecast_horizon(s$fit, s$panel, quarters = 12)

  tail_vals <- utils::tail(as.numeric(ext$fit$ts_data$us_short_rate), 4)
  # flat, not exponentially growing: all four appended values identical
  expect_equal(tail_vals, rep(tail_vals[1], 4), tolerance = 1e-8)
  # and equal to the trailing-4Q average of the original series (level == rate
  # space here since method = "none")
  expect_equal(tail_vals[1], mean(utils::tail(as.numeric(s$panel$us_short_rate), 4)),
               tolerance = 1e-8)
})

test_that("extend_forecast_horizon aborts when panel lacks a needed exogenous series", {
  skip_on_cran()
  s <- synthetic_fit()
  panel_missing <- s$panel
  panel_missing$row_imports <- NULL
  expect_error(extend_forecast_horizon(s$fit, panel_missing, quarters = 12), "row_imports")
})

test_that("extend_forecast_horizon measures each series' own shortfall", {
  skip_on_cran()
  # An exogenous series built from others stops where its shortest component
  # does. A blanket `quarters - native_horizon` would leave it short of even
  # the native horizon, and koma would refuse the forecast outright.
  s <- synthetic_fit()
  panel <- s$panel
  fit <- s$fit
  short <- stats::window(panel$row_imports, end = c(2018, 4))
  fit$ts_data$row_imports <- koma::rate(short)
  panel$row_imports <- short

  ext <- extend_forecast_horizon(fit, panel, quarters = 8)
  added <- stats::setNames(ext$extension$periods_added, ext$extension$variable)
  # row_imports ends 2018Q4 and the horizon ends 2019Q4: four quarters short.
  expect_equal(unname(added[["row_imports"]]), 4)
  # Everything else already reaches it and is left alone.
  expect_true(all(added[setdiff(names(added), "row_imports")] == 0))
  expect_equal(stats::end(ext$fit$ts_data$row_imports), c(2019, 4))
  expect_false(anyNA(as.numeric(ext$fit$ts_data$row_imports)))
})

test_that("extend_forecast_horizon extrapolates from the last OBSERVED value", {
  skip_on_cran()
  # A panel series padded with trailing NAs ends at its last observation in the
  # fit (koma::rate() drops them). Extrapolating from tail(level, 1) would
  # compound from an NA and produce an all-NA extension, which koma reports as
  # "does not extend into the forecast period".
  s <- synthetic_fit()
  panel <- s$panel
  fit <- s$fit
  padded <- stats::window(panel$row_imports, end = c(2020, 4), extend = TRUE)
  padded[stats::time(padded) > 2018.8] <- NA_real_
  attrs <- get_custom_attrs(panel$row_imports)
  attrs[["ets_attributes"]] <- NULL
  panel$row_imports <- do.call(koma::as_ets, c(list(padded), attrs))
  fit$ts_data$row_imports <- koma::rate(panel$row_imports)

  ext <- extend_forecast_horizon(fit, panel, quarters = 8)
  expect_false(anyNA(as.numeric(ext$fit$ts_data$row_imports)))
  expect_equal(stats::end(ext$fit$ts_data$row_imports), c(2019, 4))
})
