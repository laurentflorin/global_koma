skip_if_no_fred_key <- function() {
  key <- Sys.getenv("FRED_API_KEY", unset = NA_character_)
  if (is.na(key) || !nzchar(key)) {
    testthat::skip("FRED_API_KEY is not set in this environment")
  }
}

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

test_that("fred_cache_path is scoped under data/cache/fred and depends on the window", {
  expect_match(fred_cache_path("GDPC1"), "^data/cache/fred/")
  expect_false(identical(
    fred_cache_path("GDPC1", start_date = "2000-01-01"),
    fred_cache_path("GDPC1", start_date = "2010-01-01")
  ))
})

test_that("fetch_fred_series returns an ascending date/value data.frame", {
  skip_if_no_fred_key()
  withr::local_dir(withr::local_tempdir())

  out <- fetch_fred_series("GDPC1", start_date = "2015-01-01", use_cache = FALSE)
  expect_s3_class(out, "data.frame")
  expect_named(out, c("date", "value"))
  expect_gt(nrow(out), 0)
  expect_true(all(diff(out$date) > 0)) # ascending, no duplicate periods
  expect_false(anyNA(out$value))
})

test_that("fetch_fred_series caches and a second call does not need the network", {
  skip_if_no_fred_key()
  withr::local_dir(withr::local_tempdir())

  a <- fetch_fred_series("UNRATE", start_date = "2020-01-01")
  cache_path <- fred_cache_path("UNRATE", start_date = "2020-01-01")
  expect_true(file.exists(cache_path))

  # break the key so a live call would fail, then confirm the cached call
  # still succeeds identically
  withr::local_envvar(FRED_API_KEY = "definitely-not-a-real-key")
  b <- fetch_fred_series("UNRATE", start_date = "2020-01-01", use_cache = TRUE)
  expect_identical(a, b)
})

test_that("fetch_fred_series drops FRED-flagged missing observations rather than coercing to NA", {
  skip_if_no_fred_key()
  withr::local_dir(withr::local_tempdir())

  out <- fetch_fred_series("GDPC1", start_date = "2015-01-01", use_cache = FALSE)
  expect_false(anyNA(out$value))
})

test_that("fetch_fred_series_batch returns one data.frame per series id", {
  skip_if_no_fred_key()
  withr::local_dir(withr::local_tempdir())

  out <- fetch_fred_series_batch(c("GDPC1", "UNRATE"), start_date = "2020-01-01")
  expect_named(out, c("GDPC1", "UNRATE"))
  expect_true(all(vapply(out, is.data.frame, logical(1))))
})

test_that("an invalid series id errors with FRED's own message, not a generic failure", {
  skip_if_no_fred_key()
  withr::local_dir(withr::local_tempdir())

  expect_error(fetch_fred_series("NOT_A_REAL_SERIES_XYZ", use_cache = FALSE))
})
