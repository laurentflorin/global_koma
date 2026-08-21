test_that("stage1_country_equations returns a koma_seq", {
  spec <- list(
    stochastic = list(de_c = list(terms = c("de_gdp", "de_c"), lags = list(de_c = "1"))),
    identities = list(de_gdp = list(de_c = 0.6, de_i = 0.4))
  )
  sys_eq <- stage1_country_equations("de", spec)
  expect_true(koma::is_system_of_equations(sys_eq))
})

test_that("stage1_country_equations names every endogenous variable with the country prefix", {
  spec <- list(
    stochastic = list(de_c = list(terms = "de_gdp")),
    identities = list()
  )
  sys_eq <- stage1_country_equations("de", spec)
  expect_true(all(startsWith(sys_eq$endogenous_variables, "de_")))
})

test_that("fit_stage1 returns a koma_estimate", {
  fit <- fit_stage1("de", panel = list(), dates = list())
  expect_s3_class(fit, "koma_estimate")
})

test_that("fit_stage1_all fits every requested country", {
  fits <- fit_stage1_all(c("de", "fr"), panel = list(), dates = list())
  expect_named(fits, c("de", "fr"))
})
