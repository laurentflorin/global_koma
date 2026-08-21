# A small synthetic panel with the accounting identities holding exactly,
# so share computations have a known answer. No network.
synthetic_country_panel <- function(iso2 = "de", n = 80, seed = 42) {
  set.seed(seed)
  mk <- function(v, series_type = "level", method = "diff_log") {
    koma::as_ets(stats::ts(v, start = c(2000, 1), frequency = 4),
                 series_type = series_type, method = method)
  }
  consumption <- 100 + cumsum(stats::rnorm(n, 1.0, 0.2))
  investment  <-  40 + cumsum(stats::rnorm(n, 0.4, 0.1))
  government  <-  60 + cumsum(stats::rnorm(n, 0.5, 0.1))
  exports     <-  90 + cumsum(stats::rnorm(n, 0.9, 0.2))
  imports     <-  80 + cumsum(stats::rnorm(n, 0.8, 0.2))
  domestic_demand <- consumption + investment + government
  gdp <- domestic_demand + exports - imports

  out <- list(
    consumption = mk(consumption), investment = mk(investment),
    government = mk(government), exports = mk(exports), imports = mk(imports),
    domestic_demand = mk(domestic_demand), gdp = mk(gdp),
    prices = mk(100 + cumsum(stats::rnorm(n, 0.4, 0.2))),
    core_prices = mk(100 + cumsum(stats::rnorm(n, 0.3, 0.2))),
    unemployment = mk(abs(7 + cumsum(stats::rnorm(n, 0, 0.1))), "rate", "none"),
    long_rate = mk(abs(3 + cumsum(stats::rnorm(n, 0, 0.1))), "rate", "none")
  )
  stats::setNames(out, country_var(iso2, names(out)))
}

synthetic_shared_panel <- function(n = 80, seed = 7) {
  set.seed(seed)
  mk <- function(v, series_type = "level", method = "diff_log") {
    koma::as_ets(stats::ts(v, start = c(2000, 1), frequency = 4),
                 series_type = series_type, method = method)
  }
  list(
    row_gdp          = mk(100 + cumsum(stats::rnorm(n, 0.8, 0.2))),
    eur_usd          = mk(1.1 + cumsum(stats::rnorm(n, 0, 0.01))),
    us_exchange_rate = mk(100 + cumsum(stats::rnorm(n, 0, 1))),
    oil_price        = mk(50 + cumsum(stats::rnorm(n, 0.3, 1))),
    ea_policy_rate   = mk(abs(2 + cumsum(stats::rnorm(n, 0, 0.1))), "rate", "none"),
    us_policy_rate   = mk(abs(2 + cumsum(stats::rnorm(n, 0, 0.1))), "rate", "none")
  )
}

fixed_shares <- function(iso2) {
  list(
    gdp = stats::setNames(
      c(0.96, 0.44, -0.40),
      country_var(iso2, c("domestic_demand", "exports", "imports"))
    ),
    domestic_demand = stats::setNames(
      c(0.50, 0.20, 0.30),
      country_var(iso2, c("consumption", "investment", "government"))
    )
  )
}

# --- expenditure_shares --------------------------------------------------

test_that("expenditure_shares returns C/I/G shares summing to exactly 1", {
  panel <- synthetic_country_panel("de")
  shares <- expenditure_shares(panel, "de")

  # domestic_demand IS consumption + investment + government, so the mean
  # of the period-by-period ratios sums to 1 identically.
  expect_equal(sum(shares$domestic_demand), 1)
  expect_named(shares, c("gdp", "domestic_demand"))
  expect_named(shares$domestic_demand,
               country_var("de", c("consumption", "investment", "government")))
})

test_that("expenditure_shares returns the imports share as negative", {
  shares <- expenditure_shares(synthetic_country_panel("de"), "de")
  expect_lt(shares$gdp[[country_var("de", "imports")]], 0)
  expect_gt(shares$gdp[[country_var("de", "domestic_demand")]], 0)
  expect_gt(shares$gdp[[country_var("de", "exports")]], 0)
})

test_that("expenditure_shares reproduces known shares on a hand-built panel", {
  mk <- function(v) koma::as_ets(stats::ts(rep(v, 20), start = c(2000, 1), frequency = 4),
                                 series_type = "level", method = "diff_log")
  panel <- list(
    de_consumption = mk(50), de_investment = mk(20), de_government = mk(30),
    de_domestic_demand = mk(100), de_exports = mk(40), de_imports = mk(30),
    de_gdp = mk(110)
  )
  shares <- expenditure_shares(panel, "de")

  expect_equal(unname(shares$domestic_demand), c(0.5, 0.2, 0.3))
  expect_equal(unname(shares$gdp), c(round(100 / 110, 3), round(40 / 110, 3), -round(30 / 110, 3)))
})

test_that("expenditure_shares honours the estimation window", {
  panel <- synthetic_country_panel("de")
  dates <- list(estimation = list(start = c(2000, 1), end = c(2009, 4)))
  expect_false(identical(
    expenditure_shares(panel, "de"),
    expenditure_shares(panel, "de", dates)
  ))
})

test_that("expenditure_shares errors informatively on a missing series", {
  panel <- synthetic_country_panel("de")
  panel[["de_government"]] <- NULL
  expect_error(expenditure_shares(panel, "de"), "de_government")
})

# --- stage1_spec / stage1_country_equations ------------------------------

test_that("stage1_country_equations returns a koma_seq", {
  sys_eq <- stage1_country_equations("de", stage1_spec("de", fixed_shares("de")))
  expect_true(koma::is_system_of_equations(sys_eq))
})

test_that("stage1_country_equations names every endogenous variable with the country prefix", {
  for (iso2 in modelled_countries) {
    sys_eq <- stage1_country_equations(iso2, stage1_spec(iso2, fixed_shares(iso2)))
    expect_true(all(startsWith(sys_eq$endogenous_variables, paste0(iso2, "_"))))
  }
})

test_that("EA countries share one policy rate and have no own exchange rate", {
  # The brief is explicit: no de_policy_rate, no fr_exchange_rate.
  # Heterogeneity enters only through <iso2>_long_rate.
  for (iso2 in ea_countries) {
    sys_eq <- stage1_country_equations(iso2, stage1_spec(iso2, fixed_shares(iso2)))
    all_vars <- c(sys_eq$endogenous_variables, sys_eq$exogenous_variables)

    expect_false(country_var(iso2, "policy_rate") %in% all_vars)
    expect_false(country_var(iso2, "exchange_rate") %in% all_vars)
    expect_true("ea_policy_rate" %in% sys_eq$exogenous_variables)
    expect_true("eur_usd" %in% sys_eq$exogenous_variables)
    expect_true(country_var(iso2, "long_rate") %in% sys_eq$endogenous_variables)
  }
})

test_that("the US block makes its policy rate endogenous and uses the broad dollar index", {
  sys_eq <- stage1_country_equations("us", stage1_spec("us", fixed_shares("us")))

  expect_true("us_policy_rate" %in% sys_eq$endogenous_variables)
  expect_false("us_policy_rate" %in% sys_eq$exogenous_variables)
  expect_true("us_exchange_rate" %in% sys_eq$exogenous_variables)
  expect_false("eur_usd" %in% sys_eq$exogenous_variables)
  expect_false("ea_policy_rate" %in% sys_eq$exogenous_variables)

  # one more endogenous variable than an EA country: the Taylor rule
  ea <- stage1_country_equations("de", stage1_spec("de", fixed_shares("de")))
  expect_equal(length(sys_eq$endogenous_variables),
               length(ea$endogenous_variables) + 1)
})

test_that("government is exogenous everywhere -- it has no equation of its own", {
  for (iso2 in modelled_countries) {
    sys_eq <- stage1_country_equations(iso2, stage1_spec(iso2, fixed_shares(iso2)))
    expect_true(country_var(iso2, "government") %in% sys_eq$exogenous_variables)
  }
})

test_that("exports are driven by row_gdp for every country", {
  for (iso2 in modelled_countries) {
    sys_eq <- stage1_country_equations(iso2, stage1_spec(iso2, fixed_shares(iso2)))
    expect_true("row_gdp" %in% sys_eq$exogenous_variables)
  }
})

test_that("the GDP identity keeps its negative imports weight through koma's parser", {
  # Regression test for the +-w*x corruption: koma parses
  # "gdp == 0.96*dd + 0.44*x + -0.4*m" without error but stores a spurious
  # extra weight. identity_equation() must emit "- 0.4*m" instead.
  sys_eq <- stage1_country_equations("de", stage1_spec("de", fixed_shares("de")))
  identity <- sys_eq$identities[[country_var("de", "gdp")]]

  expect_equal(length(identity$weights), length(identity$components))
  expect_equal(unname(unlist(identity$weights)), c(0.96, 0.44, -0.40))
})

test_that("every one of the 11 stage-1 systems is identified", {
  for (iso2 in modelled_countries) {
    sys_eq <- stage1_country_equations(iso2, stage1_spec(iso2, fixed_shares(iso2)))
    out <- check_identification(sys_eq)
    expect_true(all(out$order_condition), info = iso2)
    expect_true(all(out$rank_condition), info = iso2)
  }
})

test_that("stage1_spec rejects an unknown country", {
  expect_error(stage1_spec("xx", fixed_shares("xx")), "xx")
})

test_that("stage1_country_equations rejects a spec with no stochastic equation", {
  expect_error(
    stage1_country_equations("de", list(stochastic = list(), identities = list())),
    "stochastic"
  )
})

# --- stage1_dates --------------------------------------------------------

test_that("stage1_dates reads the start from the panel and sets a conditional-fill gap", {
  panel <- c(synthetic_country_panel("de"), synthetic_shared_panel())
  dates <- stage1_dates(panel)

  expect_equal(dates$estimation$start, c(2000, 1))
  expect_equal(dates$estimation$end, c(2019, 4))
  # forecast start is deliberately well after the estimation end, so koma
  # conditionally fills the COVID quarters rather than estimating on them
  expect_equal(dates$forecast$start, c(2023, 1))
})

test_that("stage1_dates rejects a mixed-frequency or empty panel", {
  q <- koma::as_ets(stats::ts(1:20, start = c(2000, 1), frequency = 4),
                    series_type = "level", method = "diff_log")
  m <- koma::as_ets(stats::ts(1:60, start = c(2000, 1), frequency = 12),
                    series_type = "level", method = "diff_log")
  expect_error(stage1_dates(list(a = q, b = m)), "frequenc")
  expect_error(stage1_dates(list()), "empty")
})

# --- fit_stage1 ----------------------------------------------------------

test_that("fit_stage1 errors informatively when the panel lacks a needed series", {
  panel <- synthetic_country_panel("de") # no shared variables at all
  dates <- stage1_dates(panel)
  expect_error(fit_stage1("de", panel, dates), "row_gdp")
})

test_that("fit_stage1 estimates a country and records its runtime", {
  skip_on_cran()
  panel <- c(synthetic_country_panel("de"), synthetic_shared_panel())
  dates <- list(
    estimation = list(start = c(2000, 1), end = c(2015, 4)),
    forecast = list(start = c(2018, 1), end = c(2018, 4))
  )

  fit <- fit_stage1("de", panel, dates, options = list(gibbs = list(ndraws = 200)))

  expect_s3_class(fit, "koma_estimate")
  expect_type(attr(fit, "runtime_s"), "double")
  expect_equal(attr(fit, "iso2"), "de")
  expect_setequal(names(fit$estimates), country_var("de", c(
    "consumption", "investment", "exports", "imports", "prices", "long_rate"
  )))
})

test_that("fit_stage1 truncates endogenous series but not exogenous ones", {
  skip_on_cran()
  panel <- c(synthetic_country_panel("de"), synthetic_shared_panel())
  dates <- list(
    estimation = list(start = c(2000, 1), end = c(2015, 4)),
    forecast = list(start = c(2018, 1), end = c(2018, 4))
  )
  fit <- fit_stage1("de", panel, dates, options = list(gibbs = list(ndraws = 200)))

  # endogenous stop at the estimation end; exogenous keep their full
  # sample, since they are what the conditional fill conditions on
  expect_equal(stats::end(fit$ts_data[[country_var("de", "gdp")]]), c(2015, 4))
  expect_equal(stats::end(fit$ts_data$row_gdp), stats::end(panel$row_gdp))
})

# --- stage1_summary ------------------------------------------------------

test_that("stage1_summary binds acceptance rates and runtimes across fits", {
  mk_fit <- function(iso2, rate) {
    fit <- list(estimates = stats::setNames(
      list(list(count_accepted = c(rep(1, rate), rep(0, 100 - rate))),
           list(count_accepted = rep(NA_real_, 100))),
      country_var(iso2, c("consumption", "exports"))
    ))
    class(fit) <- "koma_estimate"
    attr(fit, "runtime_s") <- 1.5
    fit
  }
  fits <- list(de = mk_fit("de", 40), fr = mk_fit("fr", 80))

  out <- stage1_summary(fits, print = FALSE)
  expect_named(out, c("country", "runtime_s", "equation", "acceptance_rate",
                      "has_mh_step", "flagged"))
  expect_setequal(out$country, c("de", "fr"))

  expect_false(out$flagged[out$equation == "de_consumption"])  # 40%, in band
  expect_true(out$flagged[out$equation == "fr_consumption"])   # 80%, out of band
  # no-MH-step equations are never flagged
  expect_false(any(out$flagged[!out$has_mh_step]))
})
