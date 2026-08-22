# Shared fixtures for tests that need a real (fast, synthetic, no-network)
# koma_estimate to exercise estimate/forecast-touching code. Lives in a
# helper-*.R file, not a plain test-*.R one, because testthat only guarantees
# a file is sourced into every test file's environment for helper-*.R --
# top-level definitions in a test-*.R file are not reliably visible from
# another test-*.R file (verified: they are not, under this project's actual
# test-running configuration).

diagnostics_synthetic_panel <- function(iso2 = "de", n = 80, seed = 42) {
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

  country <- stats::setNames(list(
    consumption = mk(consumption), investment = mk(investment),
    government = mk(government), exports = mk(exports), imports = mk(imports),
    domestic_demand = mk(domestic_demand), gdp = mk(gdp),
    prices = mk(100 + cumsum(stats::rnorm(n, 0.4, 0.2))),
    core_prices = mk(100 + cumsum(stats::rnorm(n, 0.3, 0.2))),
    unemployment = mk(abs(7 + cumsum(stats::rnorm(n, 0, 0.1))), "rate", "none"),
    long_rate = mk(abs(3 + cumsum(stats::rnorm(n, 0, 0.1))), "rate", "none")
  ), country_var(iso2, c(
    "consumption", "investment", "government", "exports", "imports",
    "domestic_demand", "gdp", "prices", "core_prices", "unemployment", "long_rate"
  )))

  shared <- list(
    row_gdp          = mk(100 + cumsum(stats::rnorm(n, 0.8, 0.2))),
    eur_usd          = mk(1.1 + cumsum(stats::rnorm(n, 0, 0.01))),
    us_exchange_rate = mk(100 + cumsum(stats::rnorm(n, 0, 1))),
    oil_price        = mk(50 + cumsum(stats::rnorm(n, 0.3, 1))),
    ea_policy_rate   = mk(abs(2 + cumsum(stats::rnorm(n, 0, 0.1))), "rate", "none"),
    us_policy_rate   = mk(abs(2 + cumsum(stats::rnorm(n, 0, 0.1))), "rate", "none")
  )

  c(country, shared)
}

diagnostics_synthetic_fit <- function(iso2 = "de", n = 80, ndraws = 200) {
  panel <- diagnostics_synthetic_panel(iso2, n = n)
  dates <- list(
    estimation = list(start = c(2000, 1), end = c(2015, 4)),
    forecast = list(start = c(2018, 1), end = c(2018, 4))
  )
  list(
    panel = panel, dates = dates,
    fit = fit_stage1(iso2, panel, dates, options = list(gibbs = list(ndraws = ndraws)))
  )
}
