skip_if_offline_zenodo <- function() {
  ok <- tryCatch({
    httr2::request("https://zenodo.org/api/records/10514667/versions/latest") |>
      httr2::req_perform()
    TRUE
  }, error = function(e) FALSE)
  if (!ok) testthat::skip("Zenodo is not reachable from this test environment")
}

test_that("fetch_eamdqd downloads, caches, and manifests the latest vintage", {
  skip_if_offline_zenodo()
  skip_on_cran()
  withr::local_dir(withr::local_tempdir())

  eamdqd <- fetch_eamdqd(vintage = "latest")
  expect_s3_class(eamdqd, "eamdqd_vintage")
  expect_true(all(file.exists(eamdqd$xlsx)))
  expect_true(file.exists(eamdqd$codebook_pdf))
  expect_true(file.exists(eamdqd_manifest_path()))

  manifest <- jsonlite::read_json(eamdqd_manifest_path(), simplifyVector = FALSE)
  expect_true(eamdqd$vintage %in% names(manifest))
  expect_identical(manifest[[eamdqd$vintage]]$vintage, eamdqd$vintage)

  # second call must hit the cache: same paths, no re-download
  eamdqd2 <- fetch_eamdqd(vintage = eamdqd$vintage, use_cache = TRUE)
  expect_identical(eamdqd2$manifest$file_checksum_md5, eamdqd$manifest$file_checksum_md5)
})

test_that("eamdqd_codebook writes a reviewable CSV with the expected columns", {
  skip_if_offline_zenodo()
  skip_on_cran()
  withr::local_dir(withr::local_tempdir())

  eamdqd <- fetch_eamdqd(vintage = "latest")
  out_path <- withr::local_tempfile(fileext = ".csv")
  cb <- eamdqd_codebook(eamdqd, out_path = out_path, use_cache = FALSE)

  expect_true(file.exists(out_path))
  expect_named(cb, c(
    "code", "country", "name", "description", "unit", "frequency", "source",
    "sa", "sa_d", "aggregation", "tr_heavy", "tr_light", "tr_blt", "class", "vintage"
  ))
  expect_true(all(cb$country %in% c("EA", "AT", "BE", "DE", "EL", "ES", "FR", "IE", "IT", "NL", "PT")))
  expect_true(all(cb$tr_heavy %in% 1:6))
  expect_true(all(cb$tr_light %in% 1:6))
  expect_true(all(cb$tr_blt %in% 1:6))

  # use_cache = TRUE must read back the file rather than re-parsing
  cb2 <- eamdqd_codebook(eamdqd, out_path = out_path, use_cache = TRUE)
  expect_equal(nrow(cb2), nrow(cb))
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
  skip_if_offline_zenodo()
  skip_on_cran()
  withr::local_dir(withr::local_tempdir())

  raw <- fetch_eamdqd(use_cache = TRUE)
  out <- extract_eamdqd_series(raw, "IPI.M.DE")
  expect_s3_class(out, "data.frame")
  expect_named(out, c("date", "value"))
})

# --- eamdqd_aggregate_quarterly() ---------------------------------------
#
# NOTE: the original test plan called for "one test per transformation
# code" (TR1-TR6). Per CLAUDE.md's "Data transformation policy", this
# project never applies those codes -- everything stays in levels, and
# koma::as_ets(method = ) does any rate-of-change transform later. There
# is therefore no transform step left to test; these tests instead cover
# the treatment steps that do still apply to levels: quarterly
# aggregation, outlier treatment, and EM imputation.

test_that("quarterly aggregation means stock variables and sums flow variables", {
  x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12) # Jan..Dec 2020
  q_mean <- eamdqd_aggregate_quarterly(x, start = c(2020, 1), aggregation = 1)
  q_sum <- eamdqd_aggregate_quarterly(x, start = c(2020, 1), aggregation = 2)

  expect_equal(as.numeric(q_mean), c(2, 5, 8, 11))
  expect_equal(as.numeric(q_sum), c(6, 15, 24, 33))
  expect_equal(stats::frequency(q_mean), 4)
  expect_equal(stats::start(q_mean), c(2020, 1))
})

test_that("a quarter with a missing month is NA, not partially aggregated", {
  x <- c(1, 2, 3, 4, 5, NA, 7, 8, 9) # Q2's third month is missing
  q <- eamdqd_aggregate_quarterly(x, start = c(2020, 1), aggregation = 1)
  expect_equal(as.numeric(q), c(2, NA, 8))
})

test_that("a structurally partial boundary quarter is dropped, not NA'd", {
  x <- c(2, 3, 4, 5, 6, 7, 8, 9) # starts in Feb: Q1 2020 only has 2 of 3 months
  q <- eamdqd_aggregate_quarterly(x, start = c(2020, 2), aggregation = 1)
  expect_equal(stats::start(q), c(2020, 2))
  expect_equal(as.numeric(q), c(5, 8))
})

# --- eamdqd_treat_outliers() ---------------------------------------------

test_that("the outlier rule fires just past 10 IQR and not at 9 IQR", {
  # IQR is recomputed on the series *after* the test value is inserted, and
  # a single point can shift a 100-observation sample's own IQR by a
  # percent or two -- so this asserts the qualitative claim (an
  # observation just past 10x the series' IQR from the median fires; one
  # at 9x does not) using multipliers verified against the function itself
  # to sit cleanly on either side of that shifted boundary, rather than an
  # infinitesimal epsilon that the self-referential recomputation would
  # make flaky.
  set.seed(1)
  x <- rnorm(100, mean = 10, sd = 1)
  med <- stats::median(x)
  iqr <- stats::IQR(x)

  x_over <- x
  x_over[50] <- med + 11 * iqr
  out_over <- eamdqd_treat_outliers(x_over, c = 10)
  expect_true(out_over$outlier[50])

  x_under <- x
  x_under[50] <- med + 9 * iqr
  out_under <- eamdqd_treat_outliers(x_under, c = 10)
  expect_false(out_under$outlier[50])
})

test_that("an outlier is replaced by a local median, not dropped or left untouched", {
  set.seed(1)
  x <- rnorm(40, mean = 10, sd = 1)
  med <- stats::median(x)
  iqr <- stats::IQR(x)
  x[20] <- med + 50 * iqr

  out <- eamdqd_treat_outliers(x, c = 10, window = 5)
  expect_equal(out$n_outliers, 1L)
  expect_false(out$x[20] == x[20])
  expect_true(abs(out$x[20] - med) < iqr) # replacement is a plausible local value
  expect_equal(out$x[-20], x[-20]) # every other observation is untouched
})

test_that("a series with more than 20% flagged observations is left untouched", {
  x <- c(rep(0, 8), rep(1000, 3)) # a mostly-zero series: 3/11 = 27% would flag
  out <- eamdqd_treat_outliers(x, c = 10)
  expect_equal(out$n_outliers, 0L)
  expect_equal(out$x, x)
})

# --- eamdqd_em_impute() ---------------------------------------------------

test_that("EM imputation preserves every observed value exactly", {
  set.seed(2)
  Tn <- 60
  n <- 8
  common_factor <- cumsum(rnorm(Tn))
  X <- sapply(seq_len(n), function(j) 0.5 * common_factor + rnorm(Tn, sd = 0.3) + j)

  Xna <- X
  na_cells <- rbind(c(5, 3), c(10, 1), c(50, 8), c(30, 5))
  Xna[na_cells] <- NA

  imputed <- eamdqd_em_impute(Xna, q = 1, maxiter = 200, thresh = 1e-6)

  observed <- !is.na(Xna)
  expect_identical(imputed[observed], X[observed])
  expect_false(anyNA(imputed))
})

test_that("EM imputation recovers a shared factor reasonably well", {
  set.seed(3)
  Tn <- 80
  n <- 10
  common_factor <- cumsum(rnorm(Tn))
  X <- sapply(seq_len(n), function(j) common_factor + rnorm(Tn, sd = 0.2))

  Xna <- X
  Xna[40, 1] <- NA

  imputed <- eamdqd_em_impute(Xna, q = 1, maxiter = 200, thresh = 1e-6)
  expect_lt(abs(imputed[40, 1] - X[40, 1]), 1) # close to the true value, not just the column mean
  expect_gt(
    abs(mean(X[, 1]) - X[40, 1]),
    abs(imputed[40, 1] - X[40, 1])
  ) # closer than a naive mean-fill would be
})

test_that("EM imputation is a no-op when there are no missing values", {
  set.seed(4)
  X <- matrix(rnorm(50 * 5), 50, 5)
  imputed <- eamdqd_em_impute(X, q = 1)
  expect_identical(imputed, X)
})
