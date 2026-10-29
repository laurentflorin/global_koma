test_that("num_to_period inverts decimal time, including floating-point error", {
  expect_equal(num_to_period(2004.25, 4), c(2004, 2))
  expect_equal(num_to_period(stats::tsp(stats::ts(1:3, start = c(2010, 4), frequency = 4))[1], 4), c(2010, 4))
})

test_that("eurostat_to_ts orders the rows and dates the series by its first quarter", {
  d <- data.frame(time = as.Date(c("2020-07-01", "2020-04-01", "2020-10-01")), values = c(2, 1, 3))
  x <- eurostat_to_ts(d)
  expect_equal(stats::start(x), c(2020, 2))
  expect_equal(as.numeric(x), c(1, 2, 3))
  expect_error(eurostat_to_ts(d[0, ]), "no rows")
})

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
                                 series_type = "level", method = "diff_log")
  panel <- list(hole = mk(c(1, 2, NA, 4, 5)), trailing = mk(c(1, 2, 3, NA, NA)))
  expect_warning(out <- fill_internal_gaps(panel), "interpolated")
  expect_equal(as.numeric(out$hole), c(1, 2, 3, 4, 5))
  expect_equal(as.numeric(out$trailing), c(1, 2, 3, NA, NA))
  expect_equal(attr(out$hole, "method"), "diff_log")
  expect_identical(fill_internal_gaps(list(trailing = panel$trailing)), list(trailing = panel$trailing))
})

test_that("harmonise_panel_attrs gives every series the same attribute names", {
  # koma's as_mets() aborts unless every series carries an identical set
  # of attribute names.
  x <- koma::as_ets(stats::ts(1:8, start = c(2000, 1), frequency = 4),
                    series_type = "level", method = "diff_log",
                    country = "DE", source_key = "denaac1000")
  y <- koma::as_ets(stats::ts(1:8, start = c(2000, 1), frequency = 4),
                    series_type = "level", method = "diff_log", source = "ecb")

  out <- harmonise_panel_attrs(list(de_gdp = x, oil_price = y))
  names_of <- function(z) sort(setdiff(names(attributes(z)), c("tsp", "class")))

  expect_equal(names_of(out$de_gdp), names_of(out$oil_price))
  # values preserved where they existed, NA where they did not
  expect_equal(attr(out$de_gdp, "source_key"), "denaac1000")
  expect_true(is.na(attr(out$oil_price, "source_key")))
  expect_equal(attr(out$oil_price, "source"), "ecb")
  expect_true(all(vapply(out, koma::is_ets, logical(1))))
})

test_that("chain_weighted_index reproduces its identity in rate space", {
  panel <- synthetic_panel()
  weights <- c(fr_imports = 0.2, row_imports = 0.8)
  index <- chain_weighted_index(panel, weights)

  expect_true(koma::is_ets(index))
  expect_equal(attr(index, "series_type"), "level")
  expect_equal(attr(index, "method"), "diff_log")

  # The whole point: koma differences this before estimating, and the
  # differenced series must equal the identity's weighted average of the
  # components' growth rates.
  lhs <- as.numeric(koma::rate(index))
  rhs <- 0.2 * as.numeric(koma::rate(panel$fr_imports)) + 0.8 * as.numeric(koma::rate(panel$row_imports))
  expect_equal(lhs, rhs, tolerance = 1e-10)
})

test_that("chain_weighted_index differs materially from a level-space sum", {
  panel <- synthetic_panel()
  weights <- c(fr_imports = 0.2, row_imports = 0.8)

  rate_space <- as.numeric(koma::rate(chain_weighted_index(panel, weights)))
  level_space <- 100 * diff(log(
    0.2 * as.numeric(panel$fr_imports) + 0.8 * as.numeric(panel$row_imports)
  ))

  # Adding the levels does NOT satisfy the identity; this asserts the two
  # really are different so the distinction cannot silently regress.
  expect_gt(max(abs(rate_space - level_space)), 1e-6)
})

test_that("chain_weighted_index guards its inputs", {
  panel <- synthetic_panel()

  expect_error(chain_weighted_index(panel, c(ea_short_rate = 1)), "series_type")
  expect_error(chain_weighted_index(panel, c(no_such_gdp = 1)), "missing")
  expect_error(chain_weighted_index(panel, c(1, 2)), "named by variable")
  expect_error(chain_weighted_index(panel, numeric(0)), "named by variable")

  negative <- panel
  negative$row_imports[5] <- -1
  expect_error(chain_weighted_index(negative, c(row_imports = 1)), "non-positive")
})

test_that("chain_weighted_index preserves ragged edges but rejects internal gaps", {
  panel <- synthetic_panel()

  ragged <- panel
  ragged$row_imports[78:80] <- NA
  out <- chain_weighted_index(ragged, c(fr_imports = 0.2, row_imports = 0.8))
  # cumsum() would otherwise turn a trailing NA into an all-NA series
  expect_equal(sum(is.na(out)), 3)
  expect_false(anyNA(out[1:77]))

  gapped <- panel
  gapped$row_imports[40] <- NA
  expect_error(chain_weighted_index(gapped, c(fr_imports = 0.2, row_imports = 0.8)), "internal")
})

test_that("covid_dummy is a 0/1 indicator that koma will not transform", {
  panel <- synthetic_panel()
  d <- covid_dummy("covid_2005q2", panel$de_gdp)

  expect_true(koma::is_ets(d))
  expect_equal(attr(d, "series_type"), "rate")
  expect_equal(attr(d, "method"), "none")
  expect_equal(sum(d), 1)
  expect_equal(as.numeric(stats::time(d))[which(d == 1)], 2005.25)
  expect_equal(length(d), length(panel$de_gdp))

  expect_error(covid_dummy("nonsense", panel$de_gdp), "covid_2020q2")
  expect_error(covid_dummy("covid_1900q1", panel$de_gdp), "outside")
})

test_that("window_keeping_attrs keeps a koma_ts a koma_ts", {
  x <- synthetic_panel()$de_gdp
  w <- window_keeping_attrs(x, end = c(2010, 4))
  expect_true(koma::is_ets(w))
  expect_equal(stats::end(w), c(2010, 4))
  expect_equal(attr(w, "method"), "diff_log")
})
