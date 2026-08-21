test_that("build_system_equations concatenates country and shared equations", {
  country_specs <- list(
    de = list(stochastic = list(de_c = list(terms = "de_gdp")), identities = list()),
    fr = list(stochastic = list(fr_c = list(terms = "fr_gdp")), identities = list())
  )
  weights <- list(gdp = c(de = 0.6, fr = 0.4))
  eqs <- build_system_equations(c("de", "fr"), country_specs, "gdp", weights)
  expect_true(any(grepl("^ea_gdp ==", eqs)))
  expect_true(any(grepl("^de_c ~", eqs)))
  expect_true(any(grepl("^fr_c ~", eqs)))
})

test_that("stage2_exogenous_variables includes truly exogenous plus per-country regressors", {
  ex <- stage2_exogenous_variables(c("de", "fr"), truly_exogenous = "oil_price")
  expect_true("oil_price" %in% ex)
})

test_that("build_stage2_system returns a koma_seq", {
  country_specs <- list(
    de = list(stochastic = list(de_c = list(terms = "de_gdp")), identities = list())
  )
  sys_eq <- build_stage2_system(
    "de", country_specs, shared_concepts = character(),
    weights = list(), truly_exogenous = character()
  )
  expect_true(koma::is_system_of_equations(sys_eq))
})

test_that("fit_stage2 returns a koma_estimate", {
  fit <- fit_stage2(sys_eq = list(), panel = list(), dates = list())
  expect_s3_class(fit, "koma_estimate")
})
