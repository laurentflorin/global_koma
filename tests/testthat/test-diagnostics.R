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

test_that("check_identification reports per-equation order/rank conditions", {
  sys_eq <- koma::system_of_equations(c(
    "consumption ~ gdp + consumption.L(1)",
    "investment ~ investment.L(1)",
    "gdp == 0.6*consumption + 0.4*investment"
  ))
  out <- check_identification(sys_eq)

  expect_s3_class(out, "data.frame")
  expect_named(out, c("equation", "order_condition", "rank_condition"))
  expect_setequal(out$equation, c("consumption", "investment"))
  expect_true(all(out$order_condition))
})

test_that("check_identification rejects a non-koma_seq", {
  expect_error(check_identification(list()), "koma_seq")
})

test_that("diagnostics_grid returns one plot per variable and kind", {
  skip("diagnostics_grid() is still a stub -- plotting wrappers are out of scope for stage 1")
})
