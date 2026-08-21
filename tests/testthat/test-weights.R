test_that("country_weights returns weights summing to 1", {
  w <- country_weights(c("de", "fr", "it", "es"), basis = "gdp", year = 2019)
  expect_named(w, c("de", "fr", "it", "es"))
  expect_equal(sum(w), 1, tolerance = 1e-8)
})

test_that("country_weights rejects an unknown basis", {
  expect_error(country_weights(c("de", "fr"), basis = "population", year = 2019))
})

test_that("apply_weights produces a single weighted-average koma_ts", {
  x <- koma::as_ets(stats::ts(rep(1, 20), start = c(2010, 1), frequency = 4),
                    series_type = "rate", method = "none")
  y <- koma::as_ets(stats::ts(rep(3, 20), start = c(2010, 1), frequency = 4),
                    series_type = "rate", method = "none")
  panel <- list(de_gdp = x, fr_gdp = y)
  out <- apply_weights(panel, "gdp", c(de = 0.25, fr = 0.75), scope = "ea")
  expect_true(koma::is_ets(out))
  expect_equal(as.numeric(out)[1], 0.25 * 1 + 0.75 * 3)
})

test_that("weighted_identity renders a valid koma identity equation", {
  eq <- weighted_identity("gdp", c(de = 0.6, fr = 0.4), scope = "ea")
  expect_equal(eq, "ea_gdp == 0.6*de_gdp + 0.4*fr_gdp")
})
