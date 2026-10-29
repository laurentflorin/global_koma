# Shared fixtures: a small synthetic panel and a two-country system written
# out as one koma equation string, the way scratch/vja_model_equations.R is.
# Lives in a helper-*.R file because testthat sources only those into every
# test file's environment.

synthetic_panel <- function(n = 80, seed = 42) {
  set.seed(seed)
  mk <- function(v, series_type = "level", method = "diff_log") {
    koma::as_ets(stats::ts(v, start = c(2000, 1), frequency = 4),
                 series_type = series_type, method = method)
  }
  walk <- function(base, drift, sd) base + cumsum(stats::rnorm(n, drift, sd))
  rate <- function(base, sd) abs(base + cumsum(stats::rnorm(n, 0, sd)))

  panel <- list()
  for (cc in c("de", "fr")) {
    consumption <- walk(100, 1.0, 0.2)
    investment <- walk(40, 0.4, 0.1)
    government <- walk(60, 0.5, 0.1)
    exports <- walk(90, 0.9, 0.2)
    imports <- walk(80, 0.8, 0.2)
    series <- list(
      consumption = mk(consumption), investment = mk(investment),
      government = mk(government), exports = mk(exports), imports = mk(imports),
      gdp = mk(consumption + investment + government + exports - imports),
      prices = mk(walk(100, 0.4, 0.2)),
      long_rate = mk(rate(3, 0.1), "rate", "none")
    )
    panel <- c(panel, stats::setNames(series, paste0(cc, "_", names(series))))
  }
  panel$row_imports <- mk(walk(100, 0.8, 0.2))
  panel$ea_short_rate <- mk(rate(2, 0.1), "rate", "none")
  panel$us_short_rate <- mk(rate(2, 0.1), "rate", "none")
  panel$covid_2009q1 <- covid_dummy("covid_2009q1", panel$de_gdp)
  panel
}

# Every stochastic equation before every identity; explicit weights on every
# identity term; "- 0.4*x", never "+ -0.4*x". The rest-of-world term keeps each
# foreign-demand identity's weights clear of summing over the system alone.
synthetic_equations <- "
de_consumption ~ constant + de_consumption.L(1) + de_long_rate
de_investment ~ constant + de_investment.L(1) + de_long_rate
de_exports ~ constant + de_foreign_demand + covid_2009q1
de_imports ~ constant + de_domestic_demand
de_prices ~ constant + de_prices.L(1)
de_long_rate ~ constant + de_long_rate.L(1) + ea_short_rate + us_short_rate
fr_consumption ~ constant + fr_consumption.L(1) + fr_long_rate
fr_investment ~ constant + fr_investment.L(1) + fr_long_rate
fr_exports ~ constant + fr_foreign_demand + covid_2009q1
fr_imports ~ constant + fr_domestic_demand
fr_prices ~ constant + fr_prices.L(1)
fr_long_rate ~ constant + fr_long_rate.L(1) + ea_short_rate + us_short_rate
ea_short_rate ~ constant + ea_short_rate.L(1) + ea_prices + ea_gdp
de_domestic_demand == 0.5*de_consumption + 0.2*de_investment + 0.3*de_government
de_gdp == 1*de_domestic_demand + 0.4*de_exports - 0.4*de_imports
de_foreign_demand == 0.6*fr_imports + 0.4*row_imports
fr_domestic_demand == 0.5*fr_consumption + 0.2*fr_investment + 0.3*fr_government
fr_gdp == 1*fr_domestic_demand + 0.4*fr_exports - 0.4*fr_imports
fr_foreign_demand == 0.6*de_imports + 0.4*row_imports
ea_gdp == 0.5*de_gdp + 0.5*fr_gdp
ea_prices == 0.5*de_prices + 0.5*fr_prices
"

synthetic_exogenous <- c("de_government", "fr_government", "row_imports", "us_short_rate", "covid_2009q1")

synthetic_dates <- function(estimation_end = c(2015, 4)) {
  list(
    estimation = list(start = c(2000, 1), end = estimation_end),
    forecast = list(start = c(2018, 1), end = c(2018, 4))
  )
}

# The system, and the panel with a series for every identity that has no
# observed counterpart (everything but GDP), chained from the identity's own
# weights so it holds exactly.
synthetic_system <- function(equations = synthetic_equations, n = 80, seed = 42) {
  sys_eq <- koma::system_of_equations(equations, exogenous_variables = synthetic_exogenous)
  panel <- synthetic_panel(n = n, seed = seed)
  for (nm in setdiff(names(sys_eq$identities), names(panel))) {
    id <- sys_eq$identities[[nm]]
    w <- stats::setNames(unlist(id$weights, use.names = FALSE), names(id$components))
    panel[[nm]] <- chain_weighted_index(panel, w)
  }
  list(sys_eq = sys_eq, panel = panel, dates = synthetic_dates())
}

# Estimated once per test run and draw count: the tests that need a fit only
# read it, and copies they modify do not touch the cache.
synthetic_fit <- local({
  cache <- list()
  function(ndraws = 100) {
    key <- as.character(ndraws)
    if (is.null(cache[[key]])) {
      s <- synthetic_system()
      s$fit <- fit_system(s$sys_eq, s$panel, s$dates, options = list(gibbs = list(ndraws = ndraws)))
      cache[[key]] <<- s
    }
    cache[[key]]
  }
})
