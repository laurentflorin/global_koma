test_that("bloc codes are accepted by country_var but arbitrary strings are not", {
  expect_equal(country_var("reu", "gdp"), "reu_gdp")
  expect_equal(country_var(c("de", "reu"), "imports"), c("de_imports", "reu_imports"))
  expect_error(country_var("eur", "gdp"), "two-letter lowercase")
  expect_error(country_var("REU", "gdp"), "two-letter lowercase")
})

test_that("reu_members() is the modelled EA set less DE, FR and IT", {
  expect_setequal(reu_members(), setdiff(ea_countries, c("de", "fr", "it")))
  expect_setequal(stage2d_countries(), c("de", "fr", "it", "us", "cn", "reu"))
})

test_that("bloc_gdp_weights renormalises to exactly 1", {
  w <- c(at = 0.03, be = 0.04, de = 0.30, es = 0.10, fr = 0.20,
         gr = 0.02, ie = 0.04, it = 0.15, nl = 0.08, pt = 0.04)
  b <- bloc_gdp_weights(w, reu_members())
  expect_setequal(names(b), reu_members())
  expect_equal(sum(b), 1)
  # Relative order within the bloc is preserved by renormalisation.
  expect_gt(b[["es"]], b[["nl"]])
  expect_error(bloc_gdp_weights(w, c("xx")), "covers none")
})

# --------------------------------------------------------------------------
# Bloc aggregation
# --------------------------------------------------------------------------

synthetic_member_panel <- function(members = c("at", "be"), n = 40) {
  set.seed(11)
  out <- list()
  for (m in members) {
    for (concept in c("gdp", "consumption", "investment", "government",
                      "exports", "imports", "prices", "core_prices")) {
      out[[country_var(m, concept)]] <- koma::as_ets(
        stats::ts(100 * cumprod(1 + stats::rnorm(n, 0.005, 0.01)), start = c(2000, 1), frequency = 4),
        series_type = "level", method = "diff_log", country = toupper(m), source = "test"
      )
    }
    out[[country_var(m, "domestic_demand")]] <- out[[country_var(m, "consumption")]]
    for (concept in c("unemployment", "long_rate")) {
      out[[country_var(m, concept)]] <- koma::as_ets(
        stats::ts(3 + stats::rnorm(n, 0, 0.2), start = c(2000, 1), frequency = 4),
        series_type = "rate", method = "none", country = toupper(m), source = "test"
      )
    }
  }
  out
}

test_that("aggregate_bloc_panel chains level series and averages rate series", {
  panel <- synthetic_member_panel()
  w <- c(at = 0.4, be = 0.6)
  bloc <- aggregate_bloc_panel(panel, "reu", c("at", "be"), w)

  expect_setequal(names(bloc), country_var("reu", bloc_concepts()))
  expect_equal(attr(bloc$reu_gdp, "series_type"), "level")
  expect_equal(attr(bloc$reu_long_rate, "series_type"), "rate")
  expect_equal(attr(bloc$reu_long_rate, "method"), "none")

  # A rate is a weighted average of LEVELS -- averaging in rate space is the
  # same thing, since koma passes rate/none series through untouched.
  expected_rate <- 0.4 * as.numeric(panel$at_long_rate) + 0.6 * as.numeric(panel$be_long_rate)
  expect_equal(as.numeric(bloc$reu_long_rate), expected_rate, tolerance = 1e-12)

  # A level series is a weighted average of GROWTH RATES, integrated back --
  # so rate() of the bloc equals the weighted average of rate() of the members,
  # which is exactly what makes a koma identity over it hold.
  expect_equal(
    as.numeric(koma::rate(bloc$reu_gdp)),
    0.4 * as.numeric(koma::rate(panel$at_gdp)) + 0.6 * as.numeric(koma::rate(panel$be_gdp)),
    tolerance = 1e-10
  )
  # It is NOT the weighted average of the levels; that is the mistake this
  # construction exists to avoid.
  level_sum <- 0.4 * as.numeric(panel$at_gdp) + 0.6 * as.numeric(panel$be_gdp)
  expect_false(isTRUE(all.equal(as.numeric(bloc$reu_gdp), level_sum, tolerance = 1e-6)))
})

test_that("aggregate_bloc_panel rejects a bad code, missing weights or a short panel", {
  panel <- synthetic_member_panel()
  expect_error(aggregate_bloc_panel(panel, "xyz", c("at", "be"), c(at = 0.4, be = 0.6)),
               "not a declared bloc code")
  expect_error(aggregate_bloc_panel(panel, "reu", c("at", "be"), c(at = 1)),
               "no entry for")
  expect_error(aggregate_bloc_panel(panel, "reu", c("at", "be"), c(at = 0.4, be = 0.4)),
               "must sum to 1")
  expect_error(aggregate_bloc_panel(panel["at_gdp"], "reu", c("at", "be"), c(at = 0.4, be = 0.6)),
               "missing")
})

test_that("bloc_expenditure_shares averages members' shares, not index ratios", {
  panel <- synthetic_member_panel()
  w <- c(at = 0.4, be = 0.6)
  panel <- c(panel, aggregate_bloc_panel(panel, "reu", names(w), w))

  shares <- bloc_expenditure_shares(panel, "reu", names(w), w)
  at <- expenditure_shares(panel, "at")
  be <- expenditure_shares(panel, "be")

  expect_equal(
    unname(shares$gdp[["reu_exports"]]),
    round(0.4 * unname(at$gdp[["at_exports"]]) + 0.6 * unname(be$gdp[["be_exports"]]), 3),
    tolerance = 1e-3
  )
  # The naive route -- expenditure_shares() on the bloc's own base-100 index
  # series -- is a ratio of relative growth, not a share, and must differ.
  naive <- expenditure_shares(panel, "reu")
  expect_false(isTRUE(all.equal(naive$gdp[["reu_exports"]], shares$gdp[["reu_exports"]],
                                tolerance = 1e-6)))

  expect_null(bloc_expenditure_shares(panel, "reu", names(w), w, components = FALSE)$domestic_demand)
})

# --------------------------------------------------------------------------
# China's assembled panel (no network -- the arithmetic only)
# --------------------------------------------------------------------------

test_that("ts_combine and ts_ratio work on the common window", {
  a <- stats::ts(c(10, 20, 30, 40), start = c(2000, 1), frequency = 4)
  b <- stats::ts(c(1, 2, 4), start = c(2000, 2), frequency = 4)
  expect_equal(as.numeric(ts_ratio(a, b)), c(20, 15, 10))
  expect_equal(as.numeric(ts_combine(list(a, b), c(1, -1))), c(19, 28, 36))
  expect_equal(stats::start(ts_combine(list(a, b), c(1, -1))), c(2000, 2))
})

test_that("wb_period_to_start parses both GEM period flavours", {
  expect_equal(wb_period_to_start("2024Q3"), c(2024L, 3L))
  expect_equal(wb_period_to_start("2024M07"), c(2024L, 7L))
  expect_error(wb_period_to_start("2024-07"), "not a World Bank period label")
})

test_that("oecd_to_ts re-indexes on the period label rather than row order", {
  d <- data.frame(
    TIME_PERIOD = c("2020-Q3", "2020-Q1", "2020-Q4"),
    OBS_VALUE = c(3, 1, 4),
    stringsAsFactors = FALSE
  )
  out <- oecd_to_ts(d)
  expect_equal(stats::start(out), c(2020, 1))
  # 2020-Q2 is absent from the input and must come back NA, not shift Q3 up.
  expect_equal(as.numeric(out), c(1, NA, 3, 4))
  expect_error(oecd_to_ts(data.frame(TIME_PERIOD = "2020M01", OBS_VALUE = 1)),
               "Unexpected OECD period label")
})
