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
  skip_if_offline_zenodo()
  skip_on_cran()
  withr::local_dir(withr::local_tempdir())

  cb <- eamdqd_codebook(fetch_eamdqd(vintage = "latest"), use_cache = FALSE)
  m <- suppressWarnings(eamdqd_variable_map(cb, out_path = NULL))
  expect_s3_class(m, "data.frame")
  expect_true(all(c("eamdqd_code", "project_name", "series_type", "method") %in% names(m)))
})

test_that("eamdqd_variable_map only maps to valid project names", {
  skip_if_offline_zenodo()
  skip_on_cran()
  withr::local_dir(withr::local_tempdir())

  cb <- eamdqd_codebook(fetch_eamdqd(vintage = "latest"), use_cache = FALSE)
  m <- suppressWarnings(eamdqd_variable_map(cb, out_path = NULL))
  expect_true(all(is_valid_project_name(m$project_name)))
})

test_that("extract_eamdqd_series returns a date/value data.frame", {
  skip_if_offline_zenodo()
  skip_on_cran()
  withr::local_dir(withr::local_tempdir())

  raw <- fetch_eamdqd(use_cache = TRUE)
  out <- extract_eamdqd_series(raw, "GDP_DE")
  expect_s3_class(out, "data.frame")
  expect_named(out, c("date", "value"))
  expect_gt(nrow(out), 0)
  expect_false(anyNA(out$value)) # only observed periods are returned
})

test_that("extract_eamdqd_series rejects an unknown series name", {
  skip_if_offline_zenodo()
  skip_on_cran()
  withr::local_dir(withr::local_tempdir())

  raw <- fetch_eamdqd(use_cache = TRUE)
  expect_error(extract_eamdqd_series(raw, "NOT_A_SERIES"), "not found")
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

# --- eamdqd_transform(): one test per transformation code ----------------
#
# The codes are the DATASET's, not the FRED-MD numbering: 1 = log,
# 2 = Dlog, 3 = D2log, 4 = none, 5 = D, 6 = D2. Codes 2 and 4 are the two
# most dangerous to get wrong -- under the FRED-MD numbering quoted in the
# original brief, 1 would mean "none" and 2 "first difference", so a
# mis-port would silently log-difference series meant to be left alone and
# vice versa. Both are asserted explicitly below.

tr_fixture <- function() {
  matrix(c(1, 2, 4, 8), ncol = 1, dimnames = list(NULL, "s"))
}

test_that("TR code 1 is a scaled log", {
  x <- tr_fixture()
  expect_equal(as.numeric(eamdqd_transform(x, tr = 1, scale = 100)), 100 * log(c(1, 2, 4, 8)))
})

test_that("TR code 2 is a scaled log difference, not a plain difference", {
  x <- tr_fixture()
  got <- as.numeric(eamdqd_transform(x, tr = 2, scale = 100))
  expect_equal(got, c(NA, 100 * diff(log(c(1, 2, 4, 8)))))
  expect_false(isTRUE(all.equal(got[-1], diff(c(1, 2, 4, 8))))) # not a plain difference
})

test_that("TR code 3 is a scaled second log difference", {
  x <- tr_fixture()
  expect_equal(
    as.numeric(eamdqd_transform(x, tr = 3, scale = 100)),
    c(NA, NA, diff(100 * log(c(1, 2, 4, 8)), differences = 2))
  )
})

test_that("TR code 4 is no transformation at all", {
  x <- tr_fixture()
  expect_equal(as.numeric(eamdqd_transform(x, tr = 4)), c(1, 2, 4, 8))
})

test_that("TR code 5 is a plain first difference", {
  x <- tr_fixture()
  expect_equal(as.numeric(eamdqd_transform(x, tr = 5)), c(NA, 1, 2, 4))
})

test_that("TR code 6 is a plain second difference", {
  x <- tr_fixture()
  expect_equal(as.numeric(eamdqd_transform(x, tr = 6)), c(NA, NA, 1, 2))
})

test_that("the scale argument only affects the log-based codes", {
  x <- tr_fixture()
  expect_equal(as.numeric(eamdqd_transform(x, tr = 2, scale = 1)), c(NA, diff(log(c(1, 2, 4, 8)))))
  expect_equal(as.numeric(eamdqd_transform(x, tr = 5, scale = 1)),
               as.numeric(eamdqd_transform(x, tr = 5, scale = 100)))
})

test_that("a negative value under a log code demotes to the non-log code and warns", {
  x <- matrix(c(1, -2, 3, 4), ncol = 1, dimnames = list(NULL, "neg"))
  expect_warning(got <- eamdqd_transform(x, tr = 2), "negative values")
  expect_equal(as.numeric(got), c(NA, -3, 5, 1)) # demoted 2 -> 5, a plain difference
})

test_that("an out-of-range transformation code is rejected", {
  expect_error(eamdqd_transform(tr_fixture(), tr = 7), "1-6")
})

# --- frequency handling --------------------------------------------------

synthetic_sheet <- function() {
  time <- seq(as.Date("2020-01-01"), by = "month", length.out = 12)
  values <- cbind(
    q_stock = c(NA, NA, 100, NA, NA, 101, NA, NA, 102, NA, NA, 103),
    m_mean  = as.numeric(1:12),
    m_sum   = as.numeric(1:12)
  )
  info <- data.frame(
    code = c("QSTOCK", "MMEAN", "MSUM"),
    country = "XX",
    name = c("q_stock", "m_mean", "m_sum"),
    frequency = c("Q", "M", "M"),
    aggregation = c(1, 1, 2),
    tr_heavy = c(2, 2, 2), tr_light = c(2, 4, 2), tr_blt = c(2, 2, 2),
    class = c("R", "R", "N"),
    stringsAsFactors = FALSE
  )
  list(values = values, time = time, info = info)
}

test_that("a natively quarterly series survives the monthly grid intact", {
  # Regression guard: quarterly series are stored only in months 3/6/9/12,
  # so routing them through the monthly aggregator would see two NAs per
  # quarter and silently return an all-NA series.
  p <- eamdqd_to_frequency(synthetic_sheet(), frequency = "q")
  expect_equal(as.numeric(p$values[, "q_stock"]), c(100, 101, 102, 103))
  expect_equal(p$frequency, 4)
})

test_that("monthly series are aggregated by mean or sum per their Aggregation code", {
  p <- eamdqd_to_frequency(synthetic_sheet(), frequency = "q")
  expect_equal(as.numeric(p$values[, "m_mean"]), c(2, 5, 8, 11))
  expect_equal(as.numeric(p$values[, "m_sum"]), c(6, 15, 24, 33))
})

test_that("frequency = 'm' keeps only natively monthly series", {
  p <- eamdqd_to_frequency(synthetic_sheet(), frequency = "m")
  expect_equal(colnames(p$values), c("m_mean", "m_sum"))
  expect_equal(p$frequency, 12)
  expect_equal(nrow(p$values), 12)
})

# --- covid window --------------------------------------------------------

test_that("the covid window blanks calendar 2020-2021 for real series only", {
  values <- matrix(1, nrow = 12, ncol = 2, dimnames = list(NULL, c("real", "fin")))
  out <- eamdqd_covid_window(values, class = c("R", "F"),
                             start = c(2019, 1), frequency = 4)
  yr <- rep(2019:2021, each = 4)
  expect_true(all(is.na(out[yr %in% c(2020, 2021), "real"])))
  expect_true(all(!is.na(out[yr == 2019, "real"])))
  expect_true(all(!is.na(out[, "fin"]))) # financial series untouched
})

# --- caveat warnings -----------------------------------------------------

# Collect every warning a call emits, so each caveat can be asserted
# independently -- eamdqd_warn_caveats() always emits the 2020 one, which
# makes expect_no_warning() useless for the country-specific ones.
caveat_messages <- function(countries) {
  msgs <- character()
  withCallingHandlers(
    eamdqd_warn_caveats(countries),
    warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  msgs
}

test_that("the Irish 2015 break warning fires only for IE", {
  expect_true(any(grepl("Irish", caveat_messages(c("DE", "IE")))))
  expect_false(any(grepl("Irish", caveat_messages("DE"))))
})

test_that("the Greek coverage warning fires only for EL", {
  expect_true(any(grepl("Greek", caveat_messages(c("EL", "FR")))))
  expect_false(any(grepl("Greek", caveat_messages("FR"))))
})

test_that("the 2020 break warning always fires", {
  for (cc in list("DE", "EL", "IE", c("FR", "NL"))) {
    expect_true(any(grepl("2020", caveat_messages(cc))))
  }
})

# --- variable map --------------------------------------------------------

map_fixture <- function() {
  data.frame(
    code = c("GDP", "GDP", "TASS.SDB", "UNETOT", "THOURS"),
    country = c("DE", "EA", "DE", "DE", "DE"),
    name = c("GDP_DE", "GDP_EA", "TASS.SDB_DE", "UNETOT_DE", "THOURS_DE"),
    tr_heavy = c(2, 2, 2, 5, 3), tr_light = c(2, 2, 2, 4, 3), tr_blt = c(2, 2, 2, 4, 3),
    class = c("R", "R", "F", "R", "R"),
    stringsAsFactors = FALSE
  )
}

test_that("variable names follow the <iso2>_<concept> convention", {
  m <- suppressWarnings(eamdqd_variable_map(map_fixture(), out_path = NULL))
  expect_equal(m$project_name[m$eamdqd_code == "GDP_DE"], "de_gdp")
  expect_equal(m$project_name[m$eamdqd_code == "GDP_EA"], "ea_gdp")
  expect_true(all(is_valid_project_name(m$project_name)))
})

test_that("dots in EA-MD/QD codes are sanitised, since koma rejects them", {
  m <- suppressWarnings(eamdqd_variable_map(map_fixture(), out_path = NULL))
  expect_equal(m$project_name[m$eamdqd_code == "TASS.SDB_DE"], "de_tass_sdb")
  expect_false(any(grepl(".", m$project_name, fixed = TRUE)))
})

test_that("method is derived from the transformation code", {
  m <- suppressWarnings(eamdqd_variable_map(map_fixture(), tr_set = "light", out_path = NULL))
  expect_equal(m$method[m$eamdqd_code == "GDP_DE"], "diff_log")  # TR 2
  expect_equal(m$method[m$eamdqd_code == "UNETOT_DE"], "none")   # TR 4
  expect_true(all(m$series_type == "level"))
})

test_that("codes with no koma equivalent fall back to 'none' with a warning", {
  expect_warning(
    m <- eamdqd_variable_map(map_fixture(), tr_set = "light", out_path = NULL),
    "no koma equivalent"
  )
  expect_equal(m$method[m$eamdqd_code == "THOURS_DE"], "none") # TR 3
})

# --- eamdqd_panel() ------------------------------------------------------

test_that("covid_treatment without transform is an error, not a silent no-op", {
  fake <- structure(list(xlsx = c(DE = "does-not-exist.xlsx")), class = "eamdqd_vintage")
  expect_error(
    eamdqd_panel(fake, countries = "DE", covid_treatment = TRUE, transform = FALSE),
    "requires"
  )
})

test_that("eamdqd_panel returns koma_ts in levels, untransformed, by default", {
  skip_if_offline_zenodo()
  skip_on_cran()
  withr::local_dir(withr::local_tempdir())

  eamdqd <- fetch_eamdqd(vintage = "latest")
  p <- suppressWarnings(eamdqd_panel(eamdqd, countries = "DE", frequency = "q"))

  expect_true(all(vapply(p, koma::is_ets, logical(1))))
  expect_true(all(is_valid_project_name(names(p))))
  expect_true("de_gdp" %in% names(p))

  g <- p$de_gdp
  expect_identical(attr(g, "series_type"), "level")
  expect_identical(attr(g, "method"), "diff_log")
  expect_identical(attr(g, "country"), "DE")
  expect_identical(attr(g, "series_class"), "R") # not `class`, which is reserved
  expect_true(inherits(g, "koma_ts")) # attribute naming did not clobber the class
  expect_gt(stats::var(as.numeric(g), na.rm = TRUE), 0) # levels, not all-NA
})

test_that("transform = TRUE marks the panel so koma will not transform it twice", {
  skip_if_offline_zenodo()
  skip_on_cran()
  withr::local_dir(withr::local_tempdir())

  eamdqd <- fetch_eamdqd(vintage = "latest")
  p <- suppressWarnings(eamdqd_panel(eamdqd, countries = "IE", frequency = "q", transform = TRUE))

  g <- p$ie_gdp
  expect_identical(attr(g, "series_type"), "rate")
  expect_identical(attr(g, "method"), "none")
  # the guard that matters: koma's own rate() must be a no-op here
  expect_equal(as.numeric(koma::rate(g)), as.numeric(g))
  expect_false(anyNA(as.numeric(g))) # EM imputation ran
})
