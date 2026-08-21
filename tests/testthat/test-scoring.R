test_that("score_country_forecast returns one row per concept/horizon", {
  out <- score_country_forecast(fit = list(), iso2 = "de", concepts = c("gdp", "prices"),
                                dates = list(), horizon = 4)
  expect_s3_class(out, "data.frame")
  expect_named(out, c("iso2", "concept", "horizon", "rmse"))
})

test_that("score_all_countries row-binds every country's scores", {
  out <- score_all_countries(fit = list(), countries = c("de", "fr"),
                             concepts = "gdp", dates = list(), horizon = 4)
  expect_setequal(unique(out$iso2), c("de", "fr"))
})

test_that("leaderboard ranks variants best-first by mean rmse", {
  scores <- data.frame(
    variant = c("a", "a", "b", "b"),
    concept = c("gdp", "gdp", "gdp", "gdp"),
    rmse = c(1.0, 1.2, 0.5, 0.7)
  )
  lb <- leaderboard(scores, by = "concept")
  expect_equal(lb$variant[1], "b")
  expect_true(all(diff(lb$mean_rmse) >= 0))
})
