test_that("fetch_eamdqd returns a wide data.frame with a date column", {
  out <- fetch_eamdqd(vintage = "latest", use_cache = FALSE)
  expect_s3_class(out, "data.frame")
  expect_true("date" %in% names(out))
})

test_that("eamdqd_variable_map has the required columns", {
  m <- eamdqd_variable_map()
  expect_s3_class(m, "data.frame")
  expect_named(m, c("eamdqd_code", "project_name", "series_type", "method"))
})

test_that("eamdqd_variable_map only maps to valid project names", {
  m <- eamdqd_variable_map()
  expect_true(all(is_valid_project_name(m$project_name)))
})

test_that("extract_eamdqd_series returns a date/value data.frame", {
  raw <- fetch_eamdqd(use_cache = FALSE)
  out <- extract_eamdqd_series(raw, "IPI.M.DE")
  expect_s3_class(out, "data.frame")
  expect_named(out, c("date", "value"))
})
