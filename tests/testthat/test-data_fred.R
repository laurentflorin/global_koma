test_that("fred_api_key errors informatively when unset, without leaking a real key", {
  withr::local_envvar(FRED_API_KEY = NA)
  expect_error(fred_api_key(), "FRED_API_KEY")
})

test_that("fred_api_key returns the key from the environment", {
  withr::local_envvar(FRED_API_KEY = "dummy-test-key")
  expect_equal(fred_api_key(), "dummy-test-key")
})

test_that("fred_api_key is never included in a condition message", {
  withr::local_envvar(FRED_API_KEY = "super-secret-value")
  err <- tryCatch(fetch_fred_series("NOT_A_REAL_SERIES", use_cache = FALSE),
                  error = function(e) e)
  expect_false(grepl("super-secret-value", conditionMessage(err), fixed = TRUE))
})

test_that("fetch_fred_series returns a date/value data.frame", {
  withr::local_envvar(FRED_API_KEY = "dummy-test-key")
  out <- fetch_fred_series("DEUCPIALLQINMEI", use_cache = FALSE)
  expect_s3_class(out, "data.frame")
  expect_named(out, c("date", "value"))
})

test_that("fetch_fred_series_batch returns one data.frame per series id", {
  withr::local_envvar(FRED_API_KEY = "dummy-test-key")
  out <- fetch_fred_series_batch(c("A", "B"), use_cache = FALSE)
  expect_named(out, c("A", "B"))
})

test_that("fred_cache_path is scoped under data/cache/fred", {
  expect_match(fred_cache_path("DEUCPIALLQINMEI"), "^data/cache/fred/")
})
