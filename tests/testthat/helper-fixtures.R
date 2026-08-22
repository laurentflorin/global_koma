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

# The stage-3a concepts, added to a synthetic panel so the labour/price block
# can be built and its identities checked without touching the network. The
# levels are arbitrary but positive (chain_weighted_index() takes logs) and
# `wages` is deliberately built as a wage BILL divided by employment, mirroring
# derived_wage_rate(), so the ulc/real_income identities are exercised on a
# series constructed the same way the real one is.
stage3a_synthetic_panel <- function(iso2 = "de", n = 80, seed = 7) {
  panel <- diagnostics_synthetic_panel(iso2, n = n)
  set.seed(seed)
  mk <- function(v) {
    koma::as_ets(stats::ts(v, start = c(2000, 1), frequency = 4),
                 series_type = "level", method = "diff_log")
  }
  employment <- 40000 + cumsum(stats::rnorm(n, 20, 40))
  wage_bill <- 300 + cumsum(stats::rnorm(n, 2.0, 0.5))
  extra <- stats::setNames(list(
    employment = mk(employment),
    wages = mk(wage_bill / employment),
    nonenergy_prices = mk(100 + cumsum(stats::rnorm(n, 0.35, 0.2))),
    energy_prices = mk(100 + cumsum(stats::rnorm(n, 0.5, 1.5))),
    import_prices = mk(100 + cumsum(stats::rnorm(n, 0.3, 0.6))),
    export_prices = mk(100 + cumsum(stats::rnorm(n, 0.3, 0.5)))
  ), country_var(iso2, c(
    "employment", "wages", "nonenergy_prices", "energy_prices",
    "import_prices", "export_prices"
  )))
  # A partner, so the foreign-demand and foreign-price indices both have
  # something to average over.
  partner <- stats::setNames(
    list(
      mk(100 + cumsum(stats::rnorm(n, 0.3, 0.5))),
      mk(100 + cumsum(stats::rnorm(n, 0.8, 0.3)))
    ),
    country_var("fr", c("export_prices", "gdp"))
  )
  c(panel, extra, partner)
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
