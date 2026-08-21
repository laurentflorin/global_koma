test_that("country_var builds <iso2>_<concept> names", {
  expect_equal(country_var("de", "gdp"), "de_gdp")
  expect_equal(country_var("us", "prices"), "us_prices")
})

test_that("country_var rejects a non-two-letter or uppercase iso2", {
  expect_error(country_var("deu", "gdp"))
  expect_error(country_var("DE", "gdp"))
})

test_that("shared_var builds ea_/world_ names and passes through unprefixed", {
  expect_equal(shared_var("gdp", scope = "ea"), "ea_gdp")
  expect_equal(shared_var("gdp", scope = "world"), "world_gdp")
  expect_equal(shared_var("oil_price", scope = "none"), "oil_price")
})

test_that("is_valid_project_name accepts the documented forms", {
  expect_true(is_valid_project_name("de_gdp"))
  expect_true(is_valid_project_name("ea_gdp"))
  expect_true(is_valid_project_name("world_gdp"))
  expect_true(is_valid_project_name("oil_price"))
})

test_that("is_valid_project_name rejects invalid names", {
  expect_false(is_valid_project_name("DE_gdp"))
  expect_false(is_valid_project_name("de.gdp"))
  expect_false(is_valid_project_name("2de_gdp"))
})

test_that("stochastic_equation builds a koma ~ equation with lags", {
  eq <- stochastic_equation(
    dep = "de_c",
    terms = c("de_gdp", "de_c"),
    lags = list(de_c = "1")
  )
  expect_equal(eq, "de_c ~ de_gdp + de_c.L(1)")
})

test_that("stochastic_equation can drop the intercept", {
  eq <- stochastic_equation("de_c", "de_gdp", intercept = FALSE)
  expect_equal(eq, "de_c ~ de_gdp - 1")
})

test_that("identity_equation builds a koma == equation with numeric weights", {
  eq <- identity_equation("de_gdp", list(de_c = 0.6, de_i = 0.4))
  expect_equal(eq, "de_gdp == 0.6*de_c + 0.4*de_i")
})

test_that("identity_equation supports injected (ratio) weights", {
  eq <- identity_equation("de_gdp", list(de_c = "n_de_c/n_de_gdp"))
  expect_equal(eq, "de_gdp == (n_de_c/n_de_gdp)*de_c")
})

test_that("a full stage-1-style system parses with koma", {
  sys_eq <- koma::system_of_equations(
    c(
      stochastic_equation("de_c", c("de_gdp", "de_c"), lags = list(de_c = "1")),
      identity_equation("de_gdp", list(de_c = 0.6, de_i = 0.4)),
      stochastic_equation("de_i", "de_i", lags = list(de_i = "1"))
    ),
    exogenous_variables = character()
  )
  expect_true(koma::is_system_of_equations(sys_eq))
})
