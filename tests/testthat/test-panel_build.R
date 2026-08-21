test_that("build_country_panel returns a named list of koma_ts", {
  series_map <- data.frame(
    source = "fred", code = "X", concept = "gdp",
    series_type = "level", method = "diff_log",
    stringsAsFactors = FALSE
  )
  panel <- build_country_panel("de", series_map, raw_dir = "data/raw")
  expect_type(panel, "list")
  expect_named(panel, "de_gdp")
  expect_true(koma::is_ets(panel$de_gdp))
})

test_that("build_global_panel keys every series by a valid project name", {
  shared_map <- data.frame(
    source = "fred", code = "Y", concept = "gdp", scope = "world",
    series_type = "level", method = "diff_log",
    stringsAsFactors = FALSE
  )
  panel <- build_global_panel(c("de", "fr"), shared_map, raw_dir = "data/raw")
  expect_true(all(is_valid_project_name(names(panel))))
})

test_that("align_panel windows every series to a common start/end", {
  x <- koma::as_ets(stats::ts(1:40, start = c(2000, 1), frequency = 4),
                    series_type = "level", method = "diff_log")
  panel <- list(de_gdp = x, fr_gdp = x)
  out <- align_panel(panel, start = c(2005, 1), end = c(2008, 4))
  expect_equal(stats::start(out$de_gdp), c(2005, 1))
  expect_equal(stats::end(out$de_gdp), c(2008, 4))
})

test_that("align_panel rejects a panel with mixed frequencies", {
  q <- koma::as_ets(stats::ts(1:20, start = c(2000, 1), frequency = 4),
                    series_type = "level", method = "diff_log")
  m <- koma::as_ets(stats::ts(1:60, start = c(2000, 1), frequency = 12),
                    series_type = "level", method = "diff_log")
  expect_error(align_panel(list(de_gdp = q, fr_gdp = m),
                           start = c(2005, 1), end = c(2008, 4)))
})
