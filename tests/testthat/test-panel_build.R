skip_if_no_fred_key <- function() {
  key <- Sys.getenv("FRED_API_KEY", unset = NA_character_)
  if (is.na(key) || !nzchar(key)) testthat::skip("FRED_API_KEY is not set in this environment")
}

skip_if_offline_zenodo <- function() {
  ok <- tryCatch({
    httr2::request("https://zenodo.org/api/records/10514667/versions/latest") |>
      httr2::req_perform()
    TRUE
  }, error = function(e) FALSE)
  if (!ok) testthat::skip("Zenodo is not reachable from this test environment")
}

# Building the full 11-country global panel hits five live sources
# (EA-MD/QD, FRED, ECB, Eurostat-as-fallback, IMF DataMapper). Every fetch
# function caches its own responses under a relative "data/cache/..." path,
# which resolves against testthat's working directory during a test run
# (tests/testthat/, not the package root -- confirmed empirically: without
# an explicit working directory, cache files land under
# tests/testthat/data/cache/, not the project's own data/cache/). One
# shared tempdir, created once for this whole file and torn down when it
# finishes, gives every test below a real on-disk cache to reuse without
# ever writing into the source tree.
.heavy_test_dir <- withr::local_tempdir(.local_envir = testthat::teardown_env())

# Memoised on top of that shared cache, so every test_that() below reuses
# one fetch instead of refetching per test.
.test_panel_cache <- new.env(parent = emptyenv())

test_panel <- function() {
  if (is.null(.test_panel_cache$panel)) {
    eamdqd <- fetch_eamdqd(vintage = "latest", use_cache = TRUE)
    weights <- row_gdp_weights()
    .test_panel_cache$panel <- build_global_panel(eamdqd = eamdqd, row_weights = weights)
  }
  .test_panel_cache$panel
}

test_that("build_country_panel('de') returns one koma_ts per target concept", {
  skip_if_offline_zenodo()
  skip_on_cran()
  withr::local_dir(.heavy_test_dir)

  eamdqd <- fetch_eamdqd(vintage = "latest", use_cache = TRUE)
  panel <- build_country_panel("de", eamdqd = eamdqd)

  expected <- paste0("de_", c(
    "gdp", "consumption", "investment", "government", "exports", "imports",
    "prices", "core_prices", "unemployment", "long_rate", "domestic_demand"
  ))
  expect_named(panel, expected, ignore.order = TRUE)
  expect_true(all(vapply(panel, koma::is_ets, logical(1))))
})

test_that("build_country_panel('us') returns one koma_ts per target concept", {
  skip_if_no_fred_key()
  withr::local_dir(.heavy_test_dir)

  panel <- build_country_panel("us")

  expected <- paste0("us_", c(
    "gdp", "consumption", "investment", "government", "exports", "imports",
    "prices", "core_prices", "unemployment", "long_rate", "domestic_demand"
  ))
  expect_named(panel, expected, ignore.order = TRUE)
  expect_true(all(vapply(panel, koma::is_ets, logical(1))))
})

test_that("build_country_panel rejects an unknown country and a missing eamdqd vintage", {
  expect_error(build_country_panel("xx"), "xx")
  expect_error(build_country_panel("de"), "eamdqd")
})

test_that("EA countries get no own policy rate or exchange rate variable", {
  skip_if_no_fred_key()
  skip_if_offline_zenodo()
  skip_on_cran()
  withr::local_dir(.heavy_test_dir)

  panel <- test_panel()
  names_ <- names(panel)

  # The brief is explicit: EA countries share ONE policy rate and have NO
  # own exchange rate -- e.g. "de_policy_rate"/"fr_exchange_rate" must not
  # exist. Heterogeneity enters only through <iso2>_long_rate.
  for (cc in ea_countries) {
    expect_false(country_var(cc, "policy_rate") %in% names_)
    expect_false(country_var(cc, "exchange_rate") %in% names_)
  }
  expect_true("ea_policy_rate" %in% names_)
  expect_true("us_policy_rate" %in% names_)
  expect_true("eur_usd" %in% names_)
  expect_false("ea_exchange_rate" %in% names_)
})

test_that("build_global_panel keys every series by a valid project name", {
  skip_if_no_fred_key()
  skip_if_offline_zenodo()
  skip_on_cran()
  withr::local_dir(.heavy_test_dir)

  panel <- test_panel()
  expect_true(all(is_valid_project_name(names(panel))))
  expect_true(all(vapply(panel, koma::is_ets, logical(1))))
})

test_that("align_panel finds a common sample starting around 2000Q1", {
  skip_if_no_fred_key()
  skip_if_offline_zenodo()
  skip_on_cran()
  withr::local_dir(.heavy_test_dir)

  aligned <- align_panel(test_panel())
  starts <- unique(t(sapply(aligned, stats::start)))
  ends <- unique(t(sapply(aligned, stats::end)))

  # One common start/end shared by every series in the panel.
  expect_equal(nrow(starts), 1)
  expect_equal(nrow(ends), 1)
  # Constrained by EA-MD/QD's own coverage; the brief expects "roughly
  # 2000Q1". Report the actual value on failure rather than hard-coding an
  # exact end (which moves forward every vintage).
  expect_equal(starts[1, ], c(2000, 1), info = paste("common start was", paste(starts[1, ], collapse = "Q")))
  expect_true(all(vapply(aligned, koma::is_ets, logical(1))))
})

test_that("align_panel windows every series to an explicit common start/end", {
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

test_that("the national accounts identity holds to a stated tolerance for every modelled country", {
  skip_if_no_fred_key()
  skip_if_offline_zenodo()
  skip_on_cran()
  withr::local_dir(.heavy_test_dir)

  panel <- test_panel()

  # gdp ~= consumption + investment + government + exports - imports, i.e.
  # gdp ~= domestic_demand + exports - imports. The residual is the
  # statistical discrepancy / inventory change that isn't in our target
  # variable set, so it is never exactly zero. Tolerances below are set
  # from the observed max relative error per country, with headroom, not
  # tightened to the exact fitted value:
  #  - the euro-area countries run 2-6% (Eurostat's own discrepancy line),
  #    except Ireland, whose real GDP is distorted by multinational
  #    profit-shifting/contract-manufacturing (the same 2015 level break
  #    documented in data_eamdqd.R) and needs a much wider allowance;
  #  - the US is much tighter (BEA's internally consistent chain-linked
  #    NIPA accounts).
  tolerance <- c(
    at = 0.05, be = 0.03, de = 0.05, gr = 0.08, es = 0.05, fr = 0.03,
    ie = 0.20, it = 0.03, nl = 0.05, pt = 0.03, us = 0.01
  )

  for (cc in modelled_countries) {
    gdp <- as.numeric(panel[[country_var(cc, "gdp")]])
    dd  <- as.numeric(panel[[country_var(cc, "domestic_demand")]])
    x   <- as.numeric(panel[[country_var(cc, "exports")]])
    m   <- as.numeric(panel[[country_var(cc, "imports")]])
    n <- min(length(gdp), length(dd), length(x), length(m))

    rel_err <- abs(gdp[seq_len(n)] - (dd[seq_len(n)] + x[seq_len(n)] - m[seq_len(n)])) / abs(gdp[seq_len(n)])
    expect_lt(max(rel_err, na.rm = TRUE), tolerance[[cc]])
  }
})

# --- internal gaps / attribute harmonisation (no network) ----------------

test_that("internal_gaps distinguishes an internal hole from a ragged edge", {
  mk <- function(v) koma::as_ets(stats::ts(v, start = c(2000, 1), frequency = 4),
                                 series_type = "level", method = "diff_log")
  panel <- list(
    hole    = mk(c(1, 2, NA, 4, 5)),            # internal -- koma cannot handle
    leading = mk(c(NA, NA, 3, 4, 5)),           # ragged edge -- koma fills it
    trailing = mk(c(1, 2, 3, NA, NA)),          # ragged edge
    clean   = mk(c(1, 2, 3, 4, 5))
  )
  gaps <- internal_gaps(panel)

  expect_named(gaps, "hole")
  expect_equal(gaps$hole, 2000.5)
})

test_that("fill_internal_gaps interpolates internal holes, warns, and leaves edges alone", {
  mk <- function(v) koma::as_ets(stats::ts(v, start = c(2000, 1), frequency = 4),
                                 series_type = "level", method = "diff_log",
                                 country = "DE")
  panel <- list(hole = mk(c(1, 2, NA, 4, 5)), trailing = mk(c(1, 2, 3, NA, NA)))

  expect_warning(filled <- fill_internal_gaps(panel), "interpolated")

  expect_equal(as.numeric(filled$hole), c(1, 2, 3, 4, 5))
  # ragged edge untouched -- koma conditions on it properly itself
  expect_true(all(is.na(as.numeric(filled$trailing)[4:5])))
  # koma attributes survive
  expect_equal(attr(filled$hole, "series_type"), "level")
  expect_equal(attr(filled$hole, "country"), "DE")
  expect_true(koma::is_ets(filled$hole))
})

test_that("fill_internal_gaps is a silent no-op on a clean panel", {
  mk <- function(v) koma::as_ets(stats::ts(v, start = c(2000, 1), frequency = 4),
                                 series_type = "level", method = "diff_log")
  panel <- list(a = mk(1:5), b = mk(2:6))
  expect_no_warning(out <- fill_internal_gaps(panel))
  expect_identical(out, panel)
})

test_that("harmonise_panel_attrs gives every series the same attribute names", {
  # koma's as_mets() aborts unless every series carries an identical set
  # of attribute names -- our panel naturally violates that.
  x <- koma::as_ets(stats::ts(1:8, start = c(2000, 1), frequency = 4),
                    series_type = "level", method = "diff_log",
                    country = "DE", eamdqd_code = "GDP_DE")
  y <- koma::as_ets(stats::ts(1:8, start = c(2000, 1), frequency = 4),
                    series_type = "level", method = "diff_log", source = "ecb")

  out <- harmonise_panel_attrs(list(de_gdp = x, oil_price = y))
  names_of <- function(z) sort(setdiff(names(attributes(z)), c("tsp", "class")))

  expect_equal(names_of(out$de_gdp), names_of(out$oil_price))
  # values preserved where they existed, NA where they did not
  expect_equal(attr(out$de_gdp, "eamdqd_code"), "GDP_DE")
  expect_true(is.na(attr(out$oil_price, "eamdqd_code")))
  expect_equal(attr(out$oil_price, "source"), "ecb")
  expect_true(all(vapply(out, koma::is_ets, logical(1))))
})

test_that("rate concepts are tagged series_type = 'rate', levels as 'level'", {
  expect_equal(concept_series_type[["long_rate"]], "rate")
  expect_equal(concept_series_type[["unemployment"]], "rate")
  expect_equal(concept_series_type[["gdp"]], "level")
  expect_equal(concept_series_type[["prices"]], "level")
  # every concept with a method must also have a series_type
  expect_setequal(names(concept_series_type), names(concept_method))
})

test_that("fred_spliced_dollar_index joins the two Fed indices without a level break", {
  skip_if_no_fred_key()
  skip_on_cran()
  withr::local_dir(.heavy_test_dir)

  x <- suppressMessages(fred_spliced_dollar_index())

  expect_equal(stats::frequency(x), 4)
  expect_false(anyNA(as.numeric(x)))
  # must span the estimation sample and run past the forecast horizon
  expect_lte(stats::tsp(x)[1], 2000)
  expect_gte(stats::tsp(x)[2], 2023.75)

  # the join must not introduce an artificial jump: the quarter-on-quarter
  # growth at the 2006Q1 splice point stays inside the series' own range
  growth <- abs(diff(log(as.numeric(x))))
  join <- which(abs(as.numeric(stats::time(x)) - 2006) < 0.01)
  expect_lt(growth[join - 1], max(growth))
})
