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
})

test_that("diagnostics_grid returns one plot per variable and kind", {
  plots <- diagnostics_grid(fit = list(), countries = c("de", "fr"),
                            concepts = "gdp", kind = "trace")
  expect_named(plots, c("de_gdp", "fr_gdp"))
})

test_that("check_identification reports per-equation order/rank conditions", {
  out <- check_identification(sys_eq = list())
  expect_s3_class(out, "data.frame")
  expect_named(out, c("equation", "order_condition", "rank_condition"))
})
