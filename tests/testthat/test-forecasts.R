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

# --------------------------------------------------------------------------
# The vectorised level inversion must reproduce koma's own, exactly
# --------------------------------------------------------------------------

test_that("forecast_draws_level_matrix reproduces koma::level() to machine precision", {
  skip_on_cran()
  s <- diagnostics_synthetic_stage2_fit(ndraws = 60)
  ext <- extend_forecast_horizon(s$fit, s$panel, 4)
  set.seed(3)
  fc <- suppressWarnings(koma::forecast(
    ext$fit, dates = ext$dates, options = list(approximate = FALSE, probs = c(0.1, 0.9))
  ))
  h <- nrow(fc$forecasts[[1]])

  compared <- 0
  for (v in s$fit$sys_eq$endogenous_variables) {
    rate_draws <- vapply(fc$forecasts, function(d) d[seq_len(h), v], numeric(h))
    fast <- forecast_draws_level_matrix(fc, v, rate_draws)
    slow <- tryCatch(forecast_draws_level(fc, v)[seq_len(h), , drop = FALSE],
                     error = function(e) NULL)
    if (is.null(slow) || is.null(fast)) next
    compared <- compared + 1
    expect_equal(fast, slow, tolerance = 1e-12,
                 info = paste("level inversion differs for", v))
  }
  # If this drops to zero the test has stopped testing anything.
  expect_gt(compared, 0)
})

test_that("a method = 'none' series is returned unchanged rather than dropped", {
  # Every policy rate, spread and ratio in this project is rate/none: koma
  # passes those numbers through untouched and gives them no `anker`, so
  # koma::level() errors on them. Level space IS rate space there.
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

# --------------------------------------------------------------------------
# stage_forecast()
# --------------------------------------------------------------------------

test_that("stage_forecast delivers the horizon asked for and labels history", {
  skip_on_cran()
  s <- diagnostics_synthetic_stage2_fit(ndraws = 60)
  f <- stage_forecast(s$fit, s$panel, horizon = 6, history = 4,
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
})

test_that("stage_forecast is reproducible for a given seed and varies without one", {
  skip_on_cran()
  s <- diagnostics_synthetic_stage2_fit(ndraws = 60)
  a <- stage_forecast(s$fit, s$panel, horizon = 4, variables = "de_gdp", seed = 11)
  b <- stage_forecast(s$fit, s$panel, horizon = 4, variables = "de_gdp", seed = 11)
  expect_equal(a$paths, b$paths)

  # koma's forecasts are not reproducible call-to-call by default, which is
  # exactly why `seed` exists -- a different seed must give a different path.
  c2 <- stage_forecast(s$fit, s$panel, horizon = 4, variables = "de_gdp", seed = 12)
  expect_false(isTRUE(all.equal(a$paths$rate_median, c2$paths$rate_median)))
})

test_that("stage_forecast rejects an unknown variable and a bad probs pair", {
  skip_on_cran()
  s <- diagnostics_synthetic_stage2_fit(ndraws = 40)
  expect_error(stage_forecast(s$fit, s$panel, variables = "xx_gdp"), "not endogenous")
  expect_error(stage_forecast(s$fit, s$panel, probs = c(0.9, 0.1)), "increasing quantiles")
})

test_that("explosive draws are filtered per horizon in rates and cumulatively in levels", {
  skip_on_cran()
  s <- diagnostics_synthetic_stage2_fit(ndraws = 60)
  # A threshold of zero makes every finite draw "explosive", so every quantile
  # must come back NA rather than being computed on an empty vector.
  f <- stage_forecast(s$fit, s$panel, horizon = 3, variables = "de_gdp",
                      explosive_threshold = 0)
  fore <- f$paths[f$paths$kind == "forecast", ]
  expect_true(all(fore$explosive_frac == 1))
  expect_true(all(is.na(fore$rate_median)))
  expect_true(all(is.na(fore$level_median)))
})

test_that("forecast_table and forecast_explosive_table summarise the forecast rows only", {
  skip_on_cran()
  s <- diagnostics_synthetic_stage2_fit(ndraws = 60)
  f <- stage_forecast(s$fit, s$panel, horizon = 4, variables = c("de_gdp", "fr_gdp"))

  tab <- forecast_table(f$paths, space = "rate")
  expect_equal(nrow(tab), 4)
  expect_setequal(names(tab), c("quarter", "de_gdp", "fr_gdp"))
  expect_false(any(grepl("^history", tab$quarter)))

  ex <- forecast_explosive_table(f$paths)
  expect_setequal(ex$variable, c("de_gdp", "fr_gdp"))
  expect_true(all(ex$h1 >= 0 & ex$h1 <= 1))
  expect_true(all(ex$max >= ex$h1))
})

test_that("forecast_error_table scores only quarters with an outturn", {
  skip_on_cran()
  s <- diagnostics_synthetic_stage2_fit(ndraws = 60)
  f <- stage_forecast(s$fit, s$panel, horizon = 4, variables = c("de_gdp", "fr_gdp"))

  e <- forecast_error_table(f$paths, space = "rate")
  expect_true(all(c("variable", "horizon", "median", "actual", "error", "abs_error") %in% names(e)))
  # History rows carry an `actual` too; only the forecast window may be scored,
  # or a stage would be graded on the observations it was estimated on.
  expect_true(all(e$horizon >= 1))
  expect_equal(e$abs_error, abs(e$median - e$actual))
  expect_equal(e$error, e$median - e$actual)

  # A quarter with no outturn contributes no row -- that is how the report tells
  # a five-quarter-scored stage from an eight-quarter-scored one.
  n_with_actual <- sum(f$paths$kind == "forecast" &
                         f$paths$variable %in% c("de_gdp", "fr_gdp") &
                         !is.na(f$paths$actual_rate) & !is.na(f$paths$rate_median))
  expect_equal(nrow(e), n_with_actual)

  # Restricting the variable set restricts the rows, and an all-explosive
  # forecast (every median NA) scores nothing rather than erroring.
  expect_setequal(forecast_error_table(f$paths, "de_gdp")$variable, "de_gdp")
  blown <- stage_forecast(s$fit, s$panel, horizon = 3, variables = "de_gdp",
                          explosive_threshold = 0)
  expect_equal(nrow(forecast_error_table(blown$paths)), 0L)
})

test_that("plot_forecast_fan returns a ggplot without evaluating it", {
  skip_if_not_installed("ggplot2")
  skip_on_cran()
  s <- diagnostics_synthetic_stage2_fit(ndraws = 40)
  f <- stage_forecast(s$fit, s$panel, horizon = 4, variables = c("de_gdp", "de_prices"))
  p <- plot_forecast_fan(f$paths, c("de_gdp", "de_prices"), space = "rate")
  expect_s3_class(p, "ggplot")
  expect_s3_class(plot_forecast_fan(f$paths, "de_gdp", space = "level"), "ggplot")
})
