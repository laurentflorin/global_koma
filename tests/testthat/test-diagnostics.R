test_that("check_acceptance_rates flags equations outside the target band", {
  fit <- list(
    estimates = list(
      de_c = list(count_accepted = rep(c(1, 0), 50)),   # 50%, in band
      de_i = list(count_accepted = rep(NA_real_, 100))  # no MH step
    ),
    sys_eq = list(character_gamma_matrix = matrix("0", 2, 2,
      dimnames = list(c("de_c", "de_i"), c("de_c", "de_i"))))
  )
  class(fit) <- "koma_estimate"
  out <- check_acceptance_rates(fit)
  expect_s3_class(out, "data.frame")
  expect_named(out, c("equation", "has_mh_step", "acceptance_rate", "flagged"))

  expect_equal(out$acceptance_rate[out$equation == "de_c"], 0.5)
  expect_true(out$has_mh_step[out$equation == "de_c"])
  expect_false(out$flagged[out$equation == "de_c"])
})

test_that("check_acceptance_rates reports an all-NA equation as having no MH step", {
  # Equations with no contemporaneous endogenous regressor have no
  # Metropolis step: count_accepted is NA throughout. They must never be
  # flagged -- koma excludes them from its own warning for the same reason.
  fit <- list(estimates = list(de_i = list(count_accepted = rep(NA_real_, 100))))
  class(fit) <- "koma_estimate"
  out <- check_acceptance_rates(fit)

  expect_false(out$has_mh_step)
  expect_true(is.na(out$acceptance_rate))
  expect_false(out$flagged)
})

test_that("check_acceptance_rates flags rates on either side of the band", {
  fit <- list(estimates = list(
    too_low  = list(count_accepted = c(rep(1, 10), rep(0, 90))),  # 10%
    ok       = list(count_accepted = c(rep(1, 40), rep(0, 60))),  # 40%
    too_high = list(count_accepted = c(rep(1, 80), rep(0, 20)))   # 80%
  ))
  class(fit) <- "koma_estimate"
  out <- check_acceptance_rates(fit)

  expect_true(out$flagged[out$equation == "too_low"])
  expect_false(out$flagged[out$equation == "ok"])
  expect_true(out$flagged[out$equation == "too_high"])
})

test_that("check_acceptance_rates defaults to koma's own 20-60% band", {
  # koma:::get_default_acceptance_prob() returns c(0.2, 0.6). koma's
  # `equations` vignette prose says 30-60%; the code is authoritative.
  expect_equal(formals(check_acceptance_rates)$band, quote(c(0.2, 0.6)))
  expect_equal(koma:::get_default_acceptance_prob()$acceptance_prob, c(0.2, 0.6))
})

test_that("check_acceptance_rates rejects a malformed band and an empty fit", {
  fit <- list(estimates = list(a = list(count_accepted = c(1, 0))))
  class(fit) <- "koma_estimate"
  expect_error(check_acceptance_rates(fit, band = c(0.6, 0.2)), "increasing")

  empty <- structure(list(estimates = list()), class = "koma_estimate")
  expect_error(check_acceptance_rates(empty), "estimates")
})

test_that("coefficient_table returns a 90% interval by default with the expected columns", {
  skip_on_cran()
  s <- synthetic_fit()
  ct <- coefficient_table(s$fit)

  expect_named(ct, c("equation", "term", "estimate", "ci_low", "ci_high"))
  expect_true(all(ct$ci_low <= ct$estimate))
  expect_true(all(ct$estimate <= ct$ci_high))
  expect_setequal(unique(ct$equation), s$sys_eq$stochastic_equations)
})

test_that("check_lag_stability flags only own lags at or above the threshold", {
  ct <- data.frame(
    equation = c("de_wages", "de_employment", "de_long_rate", "de_wages"),
    term = c("de_wages.L(1)", "de_employment.L(1)", "de_long_rate.L(1)", "de_prices"),
    estimate = c(1.02, 0.5, 0.94, 0.3),
    ci_high = c(1.2, 0.6, 1.0, 0.4), stringsAsFactors = FALSE
  )
  out <- check_lag_stability(ct)
  expect_equal(nrow(out), 3)  # the non-lag term is excluded
  expect_true(out$flagged[out$equation == "de_wages"])
  expect_false(out$flagged[out$equation == "de_employment"])
  expect_equal(out$equation[1], "de_wages")  # ordered most-persistent first
})

test_that("a policy rate can reach GDP and still reach no price", {
  sys_eq <- synthetic_system()$sys_eq
  reached <- contemporaneous_reachability(sys_eq, "ea_short_rate")

  # The rate moves the real side through long_rate -> consumption/investment
  # -> domestic demand -> GDP ...
  expect_true(all(c("de_gdp", "fr_gdp", "ea_gdp") %in% reached))
  # ... and stops there: the price equations have no contemporaneous
  # endogenous regressor, so the Taylor rule's inflation term is an open loop.
  expect_length(grep("prices$", reached), 0)
  # The rule's output term is closed -- the rate returns to itself through
  # ea_gdp -- which is why self-reachability alone proves little.
  expect_true("ea_short_rate" %in% reached)
})

test_that("a contemporaneous GDP term in the price equations closes the loop", {
  equations <- gsub("(de|fr)_prices ~ constant \\+ (de|fr)_prices\\.L\\(1\\)",
                    "\\1_prices ~ constant + \\1_prices.L(1) + \\1_gdp", synthetic_equations)
  sys_eq <- koma::system_of_equations(equations, exogenous_variables = synthetic_exogenous)
  reached <- contemporaneous_reachability(sys_eq, "ea_short_rate")
  expect_true(all(c("de_prices", "fr_prices", "ea_prices") %in% reached))
})

test_that("contemporaneous_reachability rejects a variable outside the system", {
  expect_error(
    contemporaneous_reachability(synthetic_system()$sys_eq, "de_wages"),
    "not endogenous"
  )
})
