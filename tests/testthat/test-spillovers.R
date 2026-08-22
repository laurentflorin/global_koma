# --- advance_periods ---------------------------------------------------

test_that("advance_periods adds quarters, including across a year boundary", {
  expect_equal(advance_periods(c(2020, 1), 0), c(2020, 1))
  expect_equal(advance_periods(c(2020, 1), 3), c(2020, 4))
  expect_equal(advance_periods(c(2020, 1), 4), c(2021, 1))
  expect_equal(advance_periods(c(2020, 3), 7), c(2022, 2))
})

# --- extend_forecast_horizon --------------------------------------------

test_that("extend_forecast_horizon is a no-op when the horizon is already covered", {
  fx <- diagnostics_synthetic_fit()
  ext <- extend_forecast_horizon(fx$fit, fx$panel, quarters = 2)
  expect_equal(nrow(ext$extension), 0)
  expect_equal(ext$dates$forecast$end, c(2018, 2))
  expect_equal(length(ext$fit$ts_data$row_gdp), length(fx$fit$ts_data$row_gdp))
})

test_that("extend_forecast_horizon extends level exogenous series by trailing growth", {
  fx <- diagnostics_synthetic_fit()
  ext <- extend_forecast_horizon(fx$fit, fx$panel, quarters = 8)

  expect_equal(ext$dates$forecast$end, c(2019, 4))
  expect_true("row_gdp" %in% ext$extension$variable)
  expect_equal(length(ext$fit$ts_data$row_gdp), length(fx$fit$ts_data$row_gdp) + 4)

  # the extension should be a smooth continuation, not a discontinuous jump:
  # the growth rate of the newly appended tail should match the stated
  # trailing-4Q average, not diverge from it
  original_growth <- as.numeric(koma::rate(fx$panel$row_gdp))
  avg <- mean(utils::tail(original_growth, 4))
  extended_growth <- as.numeric(koma::rate(ext$fit$ts_data$row_gdp |>
    (\(x) do.call(koma::as_ets, c(list(x), list(series_type = "rate", method = "none"))))()))
  # ext$fit$ts_data$row_gdp is already rate-space; its appended tail values
  # should equal avg to within rounding
  tail_vals <- utils::tail(as.numeric(ext$fit$ts_data$row_gdp), 4)
  expect_equal(tail_vals, rep(avg, 4), tolerance = 1e-6)
})

test_that("extend_forecast_horizon holds a rate/none exogenous flat, not compounding it", {
  fx <- diagnostics_synthetic_fit()
  ext <- extend_forecast_horizon(fx$fit, fx$panel, quarters = 8)

  tail_vals <- utils::tail(as.numeric(ext$fit$ts_data$ea_policy_rate), 4)
  # flat, not exponentially growing: all four appended values identical
  expect_equal(tail_vals, rep(tail_vals[1], 4), tolerance = 1e-8)
  # and equal to the trailing-4Q average of the ORIGINAL series (level == rate
  # space here since method = "none")
  expect_equal(tail_vals[1], mean(utils::tail(as.numeric(fx$panel$ea_policy_rate), 4)),
              tolerance = 1e-8)
})

test_that("extend_forecast_horizon aborts when panel lacks a needed exogenous series", {
  fx <- diagnostics_synthetic_fit()
  panel_missing <- fx$panel
  panel_missing$row_gdp <- NULL
  expect_error(extend_forecast_horizon(fx$fit, panel_missing, quarters = 8), "row_gdp")
})

# --- scenario_diff -------------------------------------------------------

test_that("scenario_diff gives an exactly-zero diff for a structurally unrelated variable", {
  skip_on_cran()
  fx <- diagnostics_synthetic_fit(ndraws = 80)

  base_fc <- suppressMessages(koma::forecast(fx$fit, dates = fx$fit$dates,
    options = list(approximate = FALSE, probs = c(0.05, 0.95))))
  rest <- gdp_demand_shock(base_fc, "de", size = 1)

  diffs <- suppressMessages(scenario_diff(fx$fit, restrictions = rest, horizon = 4, seed = 42))

  # de_consumption/de_investment/de_exports/de_imports are NOT valid "should
  # be untouched" checks here: they are the literal structural components of
  # the de_gdp accounting identity (gdp == a*dd + b*exports - c*imports), so
  # restricting de_gdp's value correctly requires adjusting them -- that is
  # the model working as intended, not noise. de_long_rate is the genuinely
  # disconnected one in the stage-1 template (`de_long_rate ~ de_prices +
  # ea_policy_rate + de_long_rate.L(1)`, no gdp term, and nothing in the gdp
  # identity depends on it either): common random numbers should make its
  # diff exactly zero at every draw.
  long_rate <- diffs[diffs$variable == "de_long_rate", ]
  expect_equal(long_rate$mean_diff, rep(0, nrow(long_rate)), tolerance = 1e-10)
  expect_equal(long_rate$median_diff, rep(0, nrow(long_rate)), tolerance = 1e-10)
  expect_equal(long_rate$sd_diff, rep(0, nrow(long_rate)), tolerance = 1e-10)
  expect_equal(long_rate$explosive_frac, rep(0, nrow(long_rate)))

  # the shocked variable itself should move, and in the shocked direction
  gdp <- diffs[diffs$variable == "de_gdp", ]
  expect_gt(gdp$mean_diff[1], 0)
  expect_gt(gdp$median_diff[1], 0)
})

test_that("scenario_diff flags explosive draws without corrupting median_diff", {
  skip_on_cran()
  fx <- diagnostics_synthetic_fit(ndraws = 80)
  base_fc <- suppressMessages(koma::forecast(fx$fit, dates = fx$fit$dates,
    options = list(approximate = FALSE, probs = c(0.05, 0.95))))
  rest <- gdp_demand_shock(base_fc, "de", size = 1)

  # a threshold of 0 flags every draw as "explosive" -- degenerate, but
  # exercises the accounting without depending on the fixture happening to
  # produce a genuinely explosive draw
  diffs <- suppressMessages(scenario_diff(fx$fit, restrictions = rest, horizon = 4,
    seed = 42, explosive_threshold = 0))
  expect_true(all(diffs$explosive_frac == 1))

  # an effectively infinite threshold flags nothing
  diffs_none <- suppressMessages(scenario_diff(fx$fit, restrictions = rest, horizon = 4,
    seed = 42, explosive_threshold = Inf))
  expect_true(all(diffs_none$explosive_frac == 0))
  # median_diff is identical regardless of the threshold -- it is not
  # computed by dropping flagged draws, only mean/sd are affected by them
  expect_equal(diffs$median_diff, diffs_none$median_diff)
})

test_that("drop_baseline_draws removes exactly those positions before pairing, without mutating the caller's baseline_forecast", {
  skip_on_cran()
  fx <- diagnostics_synthetic_fit(ndraws = 80)
  base_fc <- suppressMessages(koma::forecast(fx$fit, dates = fx$fit$dates,
    options = list(approximate = FALSE, probs = c(0.05, 0.95))))
  rest <- gdp_demand_shock(base_fc, "de", size = 1)

  d_full <- suppressMessages(scenario_diff(fx$fit, restrictions = rest, horizon = 4, seed = 7))
  full_baseline <- attr(d_full, "baseline_forecast")
  n_full <- length(full_baseline$forecasts)

  # The scenario leg naturally survives all n_full draws on this fixture (no
  # singular-restriction failures), so to exercise drop_baseline_draws
  # without tripping the draw-count-mismatch abort, pad the baseline with 3
  # extra (duplicate) draws and then drop exactly those positions -- the
  # result should come out IDENTICAL to d_full, since the same n_full real
  # draws end up paired either way.
  padded_baseline <- full_baseline
  padded_baseline$forecasts <- c(full_baseline$forecasts, full_baseline$forecasts[1:3])
  n_padded <- length(padded_baseline$forecasts)
  attr(padded_baseline, "scenario_diff_seed") <- attr(full_baseline, "scenario_diff_seed")

  d_dropped <- suppressMessages(scenario_diff(fx$fit, restrictions = rest, horizon = 4, seed = 7,
    baseline_forecast = padded_baseline, drop_baseline_draws = (n_full + 1):n_padded))

  expect_equal(length(attr(d_dropped, "baseline_forecast")$forecasts), n_full)
  expect_equal(d_dropped$mean_diff, d_full$mean_diff)
  expect_equal(d_dropped$median_diff, d_full$median_diff)
  # the caller's object is untouched -- a local copy only
  expect_equal(length(padded_baseline$forecasts), n_padded)
})

test_that("scenario_diff's baseline_forecast can be reused for an identical result", {
  skip_on_cran()
  fx <- diagnostics_synthetic_fit(ndraws = 80)
  base_fc <- suppressMessages(koma::forecast(fx$fit, dates = fx$fit$dates,
    options = list(approximate = FALSE, probs = c(0.05, 0.95))))
  rest <- gdp_demand_shock(base_fc, "de", size = 1)

  d1 <- suppressMessages(scenario_diff(fx$fit, restrictions = rest, horizon = 4, seed = 7))
  d2 <- suppressMessages(scenario_diff(fx$fit, restrictions = rest, horizon = 4, seed = 7,
    baseline_forecast = attr(d1, "baseline_forecast")))

  expect_equal(d1$mean_diff, d2$mean_diff)
  expect_equal(d1$ci_low, d2$ci_low)
})

test_that("scenario_diff refuses a cached baseline computed under a different seed", {
  skip_on_cran()
  fx <- diagnostics_synthetic_fit(ndraws = 80)
  base_fc <- suppressMessages(koma::forecast(fx$fit, dates = fx$fit$dates,
    options = list(approximate = FALSE, probs = c(0.05, 0.95))))
  rest <- gdp_demand_shock(base_fc, "de", size = 1)
  d1 <- suppressMessages(scenario_diff(fx$fit, restrictions = rest, horizon = 4, seed = 7))

  expect_error(
    scenario_diff(fx$fit, restrictions = rest, horizon = 4, seed = 8,
                 baseline_forecast = attr(d1, "baseline_forecast")),
    "different"
  )
})

test_that("scenario_diff aborts rather than silently mispair unequal draw counts", {
  skip_on_cran()
  fx <- diagnostics_synthetic_fit(ndraws = 80)
  base_fc <- suppressMessages(koma::forecast(fx$fit, dates = fx$fit$dates,
    options = list(approximate = FALSE, probs = c(0.05, 0.95))))
  rest <- gdp_demand_shock(base_fc, "de", size = 1)
  d1 <- suppressMessages(scenario_diff(fx$fit, restrictions = rest, horizon = 4, seed = 7))

  corrupted <- attr(d1, "baseline_forecast")
  corrupted$forecasts <- corrupted$forecasts[-1] # drop one draw
  attr(corrupted, "scenario_diff_seed") <- 7

  expect_error(
    scenario_diff(fx$fit, restrictions = rest, horizon = 4, seed = 7, baseline_forecast = corrupted),
    "different numbers of surviving draws"
  )
})

test_that("scenario_diff warns when the requested horizon exceeds what the data supports", {
  skip_on_cran()
  fx <- diagnostics_synthetic_fit(ndraws = 80)
  # artificially shorten an exogenous series so koma must shorten the horizon
  short_fit <- fx$fit
  short_fit$ts_data$row_gdp <- stats::window(short_fit$ts_data$row_gdp, end = c(2018, 2))

  base_fc <- suppressMessages(koma::forecast(short_fit, dates = short_fit$dates,
    options = list(approximate = FALSE, probs = c(0.05, 0.95))))
  rest <- gdp_demand_shock(base_fc, "de", size = 1)

  expect_warning(
    diffs <- suppressMessages(scenario_diff(short_fit, restrictions = rest, horizon = 4, seed = 7)),
    "koma returned"
  )
  expect_lt(max(diffs$horizon), 4)
})

# --- gdp_demand_shock / policy_rate_shock --------------------------------

test_that("scenario_diff supports an exogenous-variable scenario via scenario_fit", {
  skip_on_cran()
  fx <- diagnostics_synthetic_fit(ndraws = 80)

  # The synthetic panel's series are independent random walks with no true
  # oil_price -> de_prices relationship, so the FITTED coefficient's sign is
  # arbitrary on this fixture -- asserting "prices rise" would be asserting a
  # coincidence, not a property of the code. What scenario_fit's plumbing
  # should actually guarantee: a bigger shock produces a proportionally
  # bigger, same-signed response (the model is linear in the shock, given
  # fixed coefficients), and a variable with no path to oil_price is
  # unaffected.
  small_shock <- oil_price_shock(fx$fit, fx$panel, fx$fit$dates, size = 1.2)
  big_shock <- oil_price_shock(fx$fit, fx$panel, fx$fit$dates, size = 1.5)
  diffs_small <- suppressMessages(scenario_diff(fx$fit, restrictions = NULL, horizon = 4,
    scenario_fit = small_shock, seed = 11))
  diffs_big <- suppressMessages(scenario_diff(fx$fit, restrictions = NULL, horizon = 4,
    scenario_fit = big_shock, seed = 11))

  prices_small <- diffs_small[diffs_small$variable == "de_prices", ]$mean_diff
  prices_big <- diffs_big[diffs_big$variable == "de_prices", ]$mean_diff
  expect_true(all(sign(prices_small) == sign(prices_big)))
  expect_true(all(abs(prices_big) > abs(prices_small)))

  # de_investment ~ gdp + investment.L(1): no path to/from oil_price or
  # de_prices at all -- common random numbers should zero this out exactly
  investment <- diffs_big[diffs_big$variable == "de_investment", ]
  expect_equal(investment$mean_diff, rep(0, nrow(investment)), tolerance = 1e-10)
})

test_that("gdp_demand_shock adds `size` to the baseline horizon-1 MEDIAN value only", {
  # mean and median deliberately differ here -- gdp_demand_shock() must
  # anchor to $median, not $mean (see its roxygen: $mean inherits explosive-
  # draw contamination that $median does not).
  fake_baseline <- list(
    mean = list(de_gdp = stats::ts(c(99, 0.4, 0.5), start = c(2018, 1), frequency = 4)),
    median = list(de_gdp = stats::ts(c(0.3, 0.4, 0.5), start = c(2018, 1), frequency = 4))
  )
  rest <- gdp_demand_shock(fake_baseline, "de", size = 1)
  expect_named(rest, "de_gdp")
  expect_equal(rest$de_gdp$horizon, 1)
  expect_equal(rest$de_gdp$value, 1.3)
})

test_that("policy_rate_shock adds `size` across every requested quarter, anchored to the MEDIAN", {
  fake_baseline <- list(
    mean = list(ea_policy_rate = stats::ts(c(999, 2.1, 2.2, 2.3, 2.4), start = c(2025, 1), frequency = 4)),
    median = list(ea_policy_rate = stats::ts(c(2, 2.1, 2.2, 2.3, 2.4), start = c(2025, 1), frequency = 4))
  )
  rest <- policy_rate_shock(fake_baseline, "ea_policy_rate", size = 1, quarters = 4)
  expect_named(rest, "ea_policy_rate")
  expect_equal(rest$ea_policy_rate$horizon, 1:4)
  expect_equal(rest$ea_policy_rate$value, c(3, 3.1, 3.2, 3.3))
})

# --- spillover_matrix / spillover_sanity_checks --------------------------

test_that("spillover_matrix sums median_diff across horizons into a source x receiver matrix", {
  diffs <- list(
    de = data.frame(variable = c("de_gdp", "de_gdp", "fr_gdp", "fr_gdp"),
                    horizon = c(1, 2, 1, 2), median_diff = c(1.0, 0.5, 0.2, 0.1)),
    fr = data.frame(variable = c("de_gdp", "de_gdp", "fr_gdp", "fr_gdp"),
                    horizon = c(1, 2, 1, 2), median_diff = c(0.05, 0.02, 0.8, 0.4))
  )
  m <- spillover_matrix(diffs, countries = c("de", "fr"))
  expect_equal(m["de", "de"], 1.5)
  expect_equal(m["de", "fr"], 0.3)
  expect_equal(m["fr", "fr"], 1.2)
  expect_equal(m["fr", "de"], 0.07)
})

test_that("spillover_sanity_checks flags a matrix where own effect does not dominate", {
  bad <- matrix(c(1, 5, 0.1, 1), 2, 2, dimnames = list(c("de", "fr"), c("de", "fr")))
  good <- matrix(c(2, 0.1, 0.2, 2), 2, 2, dimnames = list(c("de", "fr"), c("de", "fr")))
  monetary <- data.frame(
    variable = c("de_gdp", "fr_gdp", "de_prices", "fr_prices"),
    horizon = c(2, 2, 2, 2), median_diff = c(-0.3, -0.2, -0.1, -0.05)
  )
  lw <- list(foreign_demand = list(de = c(fr_gdp = 0.5), fr = c(de_gdp = 0.2)))

  bad_result <- spillover_sanity_checks(bad, monetary, c("de", "fr"), lw)
  good_result <- spillover_sanity_checks(good, monetary, c("de", "fr"), lw)

  expect_false(bad_result$ok[bad_result$check == "own effect exceeds every cross-country effect"])
  expect_true(good_result$ok[good_result$check == "own effect exceeds every cross-country effect"])
})

test_that("spillover_sanity_checks flags a monetary contraction with the wrong sign", {
  mat <- matrix(c(2, 0.1, 0.2, 2), 2, 2, dimnames = list(c("de", "fr"), c("de", "fr")))
  lw <- list(foreign_demand = list(de = c(fr_gdp = 0.5), fr = c(de_gdp = 0.2)))

  wrong_sign <- data.frame(
    variable = c("de_gdp", "fr_gdp", "de_prices", "fr_prices"),
    horizon = c(2, 2, 2, 2), median_diff = c(0.3, -0.2, -0.1, -0.05) # de_gdp wrong sign
  )
  right_sign <- data.frame(
    variable = c("de_gdp", "fr_gdp", "de_prices", "fr_prices"),
    horizon = c(2, 2, 2, 2), median_diff = c(-0.3, -0.2, -0.1, -0.05)
  )

  r1 <- spillover_sanity_checks(mat, wrong_sign, c("de", "fr"), lw)
  r2 <- spillover_sanity_checks(mat, right_sign, c("de", "fr"), lw)
  expect_false(r1$ok[grepl("monetary contraction", r1$check)])
  expect_true(r2$ok[grepl("monetary contraction", r2$check)])
})

test_that("spillover_sanity_checks catches spillover magnitude out of line with trade weights", {
  countries <- c("de", "fr", "it")
  monetary <- data.frame(variable = character(0), horizon = integer(0), median_diff = numeric(0))

  # trade weights rank de->fr > de->it > fr->it; a matrix that respects that
  # ranking should pass, one that inverts it should fail
  lw <- list(foreign_demand = list(
    de = c(fr_gdp = 0.30, it_gdp = 0.10),
    fr = c(de_gdp = 0.25, it_gdp = 0.05),
    it = c(de_gdp = 0.20, fr_gdp = 0.02)
  ))

  concordant <- matrix(c(
    3.0, 0.30, 0.20,
    0.25, 3.0, 0.02,
    0.10, 0.05, 3.0
  ), 3, 3, byrow = TRUE, dimnames = list(countries, countries))

  inverted <- matrix(c(
    3.0, 0.02, 0.30,
    0.05, 3.0, 0.25,
    0.20, 0.10, 3.0
  ), 3, 3, byrow = TRUE, dimnames = list(countries, countries))

  ok_result <- spillover_sanity_checks(concordant, monetary, countries, lw)
  bad_result <- spillover_sanity_checks(inverted, monetary, countries, lw)
  expect_true(ok_result$ok[grepl("trade weight", ok_result$check)])
  expect_false(bad_result$ok[grepl("trade weight", bad_result$check)])
})
