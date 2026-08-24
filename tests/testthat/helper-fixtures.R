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

# A small two-country JOINT (stage-2-shaped) synthetic fit, for tests that
# need a system spanning more than one country -- score_all_countries()'s
# contract, and the stage-2 backtest recipe in R/scoring.R. Countries share
# one draw of each unprefixed variable (eur_usd, oil_price, ...), taken from
# the first country's panel; that has no economic meaning here, it only
# needs to be internally consistent and estimable.
diagnostics_synthetic_stage2_fit <- function(countries = c("de", "fr"), n = 80, ndraws = 100,
                                             estimation_end = c(2015, 4),
                                             forecast_start = c(2018, 1),
                                             forecast_end = c(2018, 4)) {
  panels <- lapply(seq_along(countries), function(i) {
    diagnostics_synthetic_panel(countries[i], n = n, seed = 42 + i)
  })
  panel <- panels[[1]]
  for (p in panels[-1]) panel <- c(panel, p[setdiff(names(p), names(panel))])

  # Deliberately NOT reciprocal-sums-to-1: stage2_linkage_weights() folds
  # whatever is left over into a "row_gdp" residual identity term, and koma's
  # construct_posterior() aborts ("posterior beta matrix has zeros at
  # different indices...") if that residual's weight comes out at EXACTLY
  # zero, which a perfectly-reciprocal small closed system produces. Real
  # bilateral trade weights never sum to 1 across a handful of countries, so
  # this only bites a hand-built synthetic fixture -- keep the off-diagonal
  # below 1 so row_gdp stays genuinely nonzero.
  n_c <- length(countries)
  trade_weights <- matrix((1 - diag(n_c)) / (n_c - 1) * 0.7, n_c, n_c, dimnames = list(countries, countries))
  gdp_weights <- stats::setNames(rep(1 / n_c, n_c), countries)

  lw <- stage2_linkage_weights(countries, trade_weights, gdp_weights)
  dates <- list(
    estimation = list(start = c(2000, 1), end = estimation_end),
    forecast = list(start = forecast_start, end = forecast_end)
  )
  shares <- stats::setNames(
    lapply(countries, function(cc) expenditure_shares(panel, cc, dates)),
    countries
  )
  spec <- stage2_spec(countries, shares, lw)
  sys_eq <- build_stage2_system(spec)
  stage2_panel <- build_stage2_panel(panel, lw)

  list(
    panel = stage2_panel, raw_panel = panel, dates = dates, sys_eq = sys_eq,
    linkage_weights = lw, trade_weights = trade_weights, gdp_weights = gdp_weights,
    fit = fit_stage2(sys_eq, stage2_panel, dates, options = list(gibbs = list(ndraws = ndraws)))
  )
}

# A two-country stage-2 system built straight from a stage2_options() list, so
# a test can compare the STRUCTURE of a stage-2b system against a stage-2c one
# without estimating either. Deliberately mirrors test-stage2_system.R's own
# fixtures (uneven trade weights, a "row" residual well clear of zero) rather
# than sharing them, because those live in that file and are not visible here.
reachability_sys_eq <- function(opts = stage2_options(), countries = c("de", "fr")) {
  n <- length(countries)
  trade_weights <- matrix(
    0, n, n + 1,
    dimnames = list(countries, c(countries, "row"))
  )
  for (i in seq_len(n)) {
    others <- setdiff(seq_len(n), i)
    trade_weights[i, others] <- 0.25
    trade_weights[i, "row"] <- 1 - 0.25 * length(others)
  }
  gdp_weights <- stats::setNames(rep(1 / n, n), countries)

  lw <- stage2_linkage_weights(countries, trade_weights, gdp_weights)
  shares <- stats::setNames(lapply(countries, function(cc) {
    list(
      gdp = stats::setNames(
        c(0.9, 0.4, -0.3),
        country_var(cc, c("domestic_demand", "exports", "imports"))
      ),
      domestic_demand = stats::setNames(
        c(0.6, 0.2, 0.2),
        country_var(cc, c("consumption", "investment", "government"))
      )
    )
  }), countries)

  build_stage2_system(stage2_spec(countries, shares, lw, opts = opts))
}
