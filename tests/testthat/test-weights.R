# Every fetch function here caches its own responses under a relative
# "data/cache/..." path, which resolves against testthat's working
# directory during a test run (tests/testthat/, not the package root).
# One shared tempdir, created once for this whole file and torn down when
# it finishes, gives every test below a real on-disk cache to reuse
# without ever writing into the source tree.
.heavy_test_dir <- withr::local_tempdir(.local_envir = testthat::teardown_env())

skip_if_offline_ecb <- function() {
  ok <- tryCatch({
    ecb::get_data("EXR.D.USD.EUR.SP00.A")
    TRUE
  }, error = function(e) FALSE)
  if (!ok) testthat::skip("ECB Data Portal is not reachable from this test environment")
}

skip_if_offline_eurostat <- function() {
  ok <- tryCatch({
    eurostat::get_eurostat_toc()
    TRUE
  }, error = function(e) FALSE)
  if (!ok) testthat::skip("Eurostat is not reachable from this test environment")
}

skip_if_offline_imf <- function() {
  ok <- tryCatch({
    httr2::request("https://www.imf.org/external/datamapper/api/v1/NGDP_RPCH") |>
      httr2::req_perform()
    TRUE
  }, error = function(e) FALSE)
  if (!ok) testthat::skip("IMF DataMapper is not reachable from this test environment")
}

test_that("build_trade_weight_matrix rows sum to 1 and no country weights itself", {
  skip_if_offline_ecb()
  skip_on_cran()
  withr::local_dir(.heavy_test_dir)

  mat <- build_trade_weight_matrix(out_path = NULL)

  expect_equal(dim(mat), c(length(modelled_countries), length(modelled_countries) + 1))
  expect_setequal(rownames(mat), modelled_countries)
  expect_setequal(colnames(mat), c(modelled_countries, "row"))

  # every row sums to exactly 1
  expect_equal(unname(rowSums(mat)), rep(1, nrow(mat)), tolerance = 1e-8)

  # no country appears in its own trade weights
  for (cc in modelled_countries) {
    expect_equal(mat[cc, cc], 0)
  }

  expect_true(all(mat >= 0))
  expect_setequal(names(attr(mat, "source")), modelled_countries)
  expect_equal(attr(mat, "source")[["us"]], "reciprocal")
})

test_that("build_trade_weight_matrix writes a reviewable CSV with row/column names", {
  skip_if_offline_ecb()
  skip_on_cran()
  withr::local_dir(.heavy_test_dir)

  out_path <- withr::local_tempfile(fileext = ".csv")
  mat <- build_trade_weight_matrix(out_path = out_path)

  expect_true(file.exists(out_path))
  written <- utils::read.csv(out_path, row.names = 1, check.names = FALSE)
  expect_setequal(rownames(written), rownames(mat))
  expect_setequal(colnames(written), colnames(mat))
})

test_that("build_gdp_weight_matrix sums to 1 over the ten EA countries", {
  skip_if_offline_eurostat()
  skip_on_cran()
  withr::local_dir(.heavy_test_dir)

  w <- build_gdp_weight_matrix(out_path = NULL)

  expect_named(w, ea_countries, ignore.order = TRUE)
  expect_equal(sum(w), 1, tolerance = 1e-8)
  expect_true(all(w > 0))
})

test_that("build_gdp_weight_matrix writes a reviewable CSV", {
  skip_if_offline_eurostat()
  skip_on_cran()
  withr::local_dir(.heavy_test_dir)

  out_path <- withr::local_tempfile(fileext = ".csv")
  w <- build_gdp_weight_matrix(out_path = out_path)

  expect_true(file.exists(out_path))
  written <- utils::read.csv(out_path)
  expect_named(written, c("iso2", "weight"))
  expect_equal(sum(written$weight), 1, tolerance = 1e-8)
})

test_that("row_gdp_weights sums to 1 over the six named partners plus 'other'", {
  skip_if_offline_ecb()
  skip_on_cran()
  withr::local_dir(.heavy_test_dir)

  w <- row_gdp_weights()

  expect_named(w, c(row_partners, "other"), ignore.order = TRUE)
  expect_equal(sum(w), 1, tolerance = 1e-8)
  expect_true(all(w >= 0)) # verified non-negative after the currency_trans fix
})

test_that("build_row_gdp returns a quarterly koma_ts index starting at start_year Q1", {
  skip_if_offline_ecb()
  skip_if_offline_imf()
  skip_on_cran()
  withr::local_dir(.heavy_test_dir)

  w <- row_gdp_weights()
  s <- build_row_gdp(w, start_year = 2010, end_year = 2015)

  expect_true(koma::is_ets(s))
  expect_equal(stats::start(s), c(2010, 1))
  expect_equal(stats::frequency(s), 4)
  expect_equal(as.numeric(s)[1], 100)
  expect_false(anyNA(as.numeric(s)))
})

test_that("country_weights('gdp') and country_weights('trade') both sum to 1", {
  skip_if_offline_eurostat()
  skip_if_offline_ecb()
  skip_on_cran()
  withr::local_dir(.heavy_test_dir)

  w_gdp <- country_weights(c("de", "fr", "it", "es"), basis = "gdp")
  expect_named(w_gdp, c("de", "fr", "it", "es"))
  expect_equal(sum(w_gdp), 1, tolerance = 1e-8)

  w_trade <- country_weights(c("de", "fr", "it", "es"), basis = "trade")
  expect_named(w_trade, c("de", "fr", "it", "es"))
  expect_equal(sum(w_trade), 1, tolerance = 1e-8)
})

test_that("country_weights rejects an unknown basis", {
  expect_error(country_weights(c("de", "fr"), basis = "population"))
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

test_that("apply_weights errors when the panel is missing a weighted country's series", {
  x <- koma::as_ets(stats::ts(rep(1, 20), start = c(2010, 1), frequency = 4),
                    series_type = "rate", method = "none")
  panel <- list(de_gdp = x)
  expect_error(apply_weights(panel, "gdp", c(de = 0.5, fr = 0.5), scope = "ea"), "fr_gdp")
})

test_that("weighted_identity renders a valid koma identity equation", {
  eq <- weighted_identity("gdp", c(de = 0.6, fr = 0.4), scope = "ea")
  expect_equal(eq, "ea_gdp == 0.6*de_gdp + 0.4*fr_gdp")
})
