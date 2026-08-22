# --- derived_wage_rate ---------------------------------------------------

test_that("derived_wage_rate divides the wage bill by employment", {
  bill <- stats::ts(c(100, 102, 104), start = c(2000, 1), frequency = 4)
  emp <- stats::ts(c(10, 10.2, 10.4), start = c(2000, 1), frequency = 4)
  w <- derived_wage_rate(bill, emp, "de")
  expect_equal(as.numeric(w), c(10, 10, 10))
  expect_equal(attr(w, "series_type"), "level")
  expect_equal(attr(w, "method"), "diff_log")
  expect_equal(attr(w, "source"), "derived")
})

test_that("derived_wage_rate windows to the overlap rather than recycling", {
  # EA-MD/QD's WS runs a quarter shorter than TEMP in the current vintage;
  # silently recycling the shorter one would misdate every observation.
  bill <- stats::ts(c(100, 102, 104), start = c(2000, 1), frequency = 4)
  emp <- stats::ts(c(10, 10, 10, 10, 10), start = c(2000, 1), frequency = 4)
  w <- derived_wage_rate(bill, emp, "de")
  expect_length(w, 3)
  expect_equal(stats::end(w), c(2000, 3))
})

test_that("derived_wage_rate aborts rather than returning a partial series", {
  emp <- stats::ts(c(10, 10), start = c(2000, 1), frequency = 4)
  expect_error(derived_wage_rate(NULL, emp, "de"), "WS")
})

# --- resolve_stage3a_concepts --------------------------------------------

test_that("resolve_stage3a_concepts handles FALSE, TRUE and a subset", {
  expect_equal(resolve_stage3a_concepts(FALSE), character())
  # TRUE means every extended concept, stage 3a and 3b alike.
  expect_setequal(resolve_stage3a_concepts(TRUE), c(stage3a_concepts, stage3b_concepts))
  expect_equal(resolve_stage3a_concepts("export_prices"), "export_prices")
  expect_error(resolve_stage3a_concepts("nonsense_prices"), "Unknown extended concept")
})

test_that("resolve_stage3a_concepts refuses netborrowing without govdebt", {
  # netborrowing is the first difference of govdebt; asking for it alone would
  # fail later with a confusing NULL rather than here.
  expect_error(resolve_stage3a_concepts("netborrowing"), "derived from")
  expect_equal(resolve_stage3a_concepts(c("govdebt", "netborrowing")),
               c("govdebt", "netborrowing"))
})

# --- align_panel(extend = ) ----------------------------------------------

test_that("align_panel pads a short series instead of truncating the panel", {
  mk <- function(v, end) koma::as_ets(
    stats::ts(v, end = end, frequency = 4), series_type = "level", method = "diff_log"
  )
  panel <- list(
    long = mk(1:12, c(2003, 4)),
    short = mk(1:10, c(2003, 2))  # two quarters behind
  )
  # Default: the short series drags the whole panel back with it.
  truncated <- align_panel(panel)
  expect_equal(stats::end(truncated$long), c(2003, 2))

  # With an explicit end and extend, the established window survives and the
  # short series simply carries a ragged edge.
  padded <- align_panel(panel, start = c(2001, 1), end = c(2003, 4), extend = TRUE)
  expect_equal(stats::end(padded$long), c(2003, 4))
  expect_equal(stats::end(padded$short), c(2003, 4))
  expect_equal(sum(is.na(as.numeric(padded$short))), 2)
})

# --- hicp weights validation in labour_block ------------------------------

test_that("labour_block refuses weights that do not partition the basket", {
  expect_error(
    labour_block("de", c(nonenergy_prices = 0.85, energy_prices = 0.10)),
    "sum to 1"
  )
  expect_error(
    labour_block("de", c(core_prices = 0.9, energy_prices = 0.1)),
    "must be named"
  )
})

# --- the block's equations ------------------------------------------------

test_that("labour_block emits the stage-3a equations with the right shape", {
  b <- labour_block("de", c(nonenergy_prices = 0.9, energy_prices = 0.1))
  eqs <- build_system_equations(list(stochastic = b$stochastic, identities = b$identities))

  expect_true("de_wages ~ de_unemployment + de_prices + de_wages.L(1)" %in% eqs)
  expect_true(
    "de_nonenergy_prices ~ de_ulc + de_import_prices + de_unemployment + de_nonenergy_prices.L(1)" %in% eqs
  )
  # Negative identity weights must render as "- 1*x", never "+ -1*x": koma
  # parses the latter without error but stores the weights wrong.
  expect_true("de_productivity == 1*de_gdp - 1*de_employment" %in% eqs)
  expect_true("de_ulc == 1*de_wages - 1*de_productivity" %in% eqs)
  expect_true("de_real_income == 1*de_wages + 1*de_employment - 1*de_prices" %in% eqs)
  expect_false(any(grepl("+ -", eqs, fixed = TRUE)))

  # foreign_prices is exogenous in phase A: no identity unless weights are given.
  expect_false("de_foreign_prices" %in% names(b$identities))
  b2 <- labour_block("de", c(nonenergy_prices = 0.9, energy_prices = 0.1),
                     foreign_price_weights = c(fr_export_prices = 1))
  expect_true("de_foreign_prices" %in% names(b2$identities))
})

test_that("a labour country loses its stochastic price equation and gains price terms", {
  shares <- list(
    gdp = c(de_domestic_demand = 0.9, de_exports = 0.4, de_imports = -0.3),
    domestic_demand = c(de_consumption = 0.6, de_investment = 0.2, de_government = 0.2)
  )
  fw <- c(fr_gdp = 0.3, row_gdp = 0.7)

  plain <- country_block("de", shares, fw)
  expect_true("de_prices" %in% names(plain$stochastic))

  opts <- stage2_options(labour_countries = "de",
                         hicp_weights = list(de = c(nonenergy_prices = 0.9, energy_prices = 0.1)))
  labour <- country_block("de", shares, fw, opts)
  # prices becomes an identity in labour_block(), so it must not also be a
  # stochastic equation here -- build_system() aborts on a duplicated LHS.
  expect_false("de_prices" %in% names(labour$stochastic))
  expect_true("de_real_income" %in% labour$stochastic$de_consumption$terms)
  expect_true(all(c("de_export_prices", "de_foreign_prices") %in% labour$stochastic$de_exports$terms))
})

test_that("the imports equation gains no price term, even for a labour country", {
  # Tried four ways and rejected: contemporaneously the import price takes the
  # wrong sign and collapses the domestic-demand elasticity; lagged it restores
  # the elasticity but is indistinguishable from zero. Imports must therefore
  # look exactly as they do without the labour block.
  shares <- list(
    gdp = c(de_domestic_demand = 0.9, de_exports = 0.4, de_imports = -0.3),
    domestic_demand = c(de_consumption = 0.6, de_investment = 0.2, de_government = 0.2)
  )
  fw <- c(fr_gdp = 0.3, row_gdp = 0.7)
  opts <- stage2_options(labour_countries = "de",
                         hicp_weights = list(de = c(nonenergy_prices = 0.9, energy_prices = 0.1)))

  plain <- country_block("de", shares, fw)
  labour <- country_block("de", shares, fw, opts)

  expect_false("de_import_prices" %in% labour$stochastic$de_imports$terms)
  expect_equal(labour$stochastic$de_imports$terms, plain$stochastic$de_imports$terms)
  # and no sign rule is left dangling for a term that is not there
  expect_false("imports_fall_in_own_price" %in%
                 vapply(stage3a_sign_rules("de"), function(r) r$check, character(1)))
})

test_that("stage2_options rejects a country in both block lists", {
  expect_error(
    stage2_options(labour_countries = "de", export_price_countries = "de",
                   hicp_weights = list(de = c(nonenergy_prices = 0.9, energy_prices = 0.1))),
    "both"
  )
})

test_that("stage2_options requires weights for every labour country", {
  expect_error(stage2_options(labour_countries = "de"), "no entry for")
})

# --- foreign_price_weights ------------------------------------------------

test_that("foreign_price_weights drops row_gdp and renormalises to 1", {
  lw <- list(foreign_demand = list(
    de = c(fr_gdp = 0.2, it_gdp = 0.1, row_gdp = 0.7)
  ))
  w <- foreign_price_weights(lw, "de")
  expect_named(w, c("fr_export_prices", "it_export_prices"))
  expect_equal(sum(w), 1)
  expect_equal(unname(w[["fr_export_prices"]]), 2 / 3, tolerance = 1e-3)
  # The dropped rest-of-world weight is recorded, not discarded silently --
  # for Germany it is over half the trade weight.
  expect_equal(attr(w, "row_weight_dropped"), 0.7)
})

# --- identity exactness on constructed series -----------------------------

test_that("build_stage2_panel constructs every stage-3a identity exactly", {
  panel <- stage3a_synthetic_panel("de")
  lw <- list(
    foreign_demand = list(de = c(fr_gdp = 0.4, row_gdp = 0.6)),
    ea = c(de = 1)
  )
  hw <- c(nonenergy_prices = 0.9, energy_prices = 0.1)
  sp <- build_stage2_panel(panel, lw, labour_countries = "de", hicp_weights = list(de = hw))

  # Compare in RATE space -- the space koma enforces identities in. Use the ts
  # that koma::rate() returns rather than re-dating it: rate() drops the first
  # observation and any trailing NA, so forcing an end date misaligns the
  # series by a quarter and makes an exact identity look broken.
  maxdiff <- function(lhs, rhs, w) {
    m <- stats::na.omit(do.call(cbind, lapply(c(lhs, rhs), function(n) koma::rate(sp[[n]]))))
    max(abs(m[, 1] - as.numeric(m[, -1, drop = FALSE] %*% w)))
  }
  expect_lt(maxdiff("de_productivity", c("de_gdp", "de_employment"), c(1, -1)), 1e-10)
  expect_lt(maxdiff("de_ulc", c("de_wages", "de_productivity"), c(1, -1)), 1e-10)
  expect_lt(maxdiff("de_prices", c("de_nonenergy_prices", "de_energy_prices"), c(0.9, 0.1)), 1e-10)
  expect_lt(
    maxdiff("de_real_income", c("de_wages", "de_employment", "de_prices"), c(1, 1, -1)), 1e-10
  )
  expect_lt(maxdiff("de_foreign_prices", "fr_export_prices", 1), 1e-10)
})

test_that("build_stage2_panel builds ea_prices from the constructed price series", {
  # A labour country's `prices` is redefined as the sub-index aggregate, so the
  # EA aggregate must be built from that, not from the observed HICP it replaced.
  panel <- stage3a_synthetic_panel("de")
  lw <- list(foreign_demand = list(de = c(fr_gdp = 0.4, row_gdp = 0.6)), ea = c(de = 1))
  hw <- c(nonenergy_prices = 0.9, energy_prices = 0.1)
  sp <- build_stage2_panel(panel, lw, labour_countries = "de", hicp_weights = list(de = hw))
  m <- stats::na.omit(cbind(koma::rate(sp$ea_prices), koma::rate(sp$de_prices)))
  expect_lt(max(abs(m[, 1] - m[, 2])), 1e-10)
  # and that is NOT the observed series it replaced
  m2 <- stats::na.omit(cbind(koma::rate(sp$de_prices), koma::rate(panel$de_prices)))
  expect_gt(max(abs(m2[, 1] - m2[, 2])), 1e-6)
})

# --- sign rules -----------------------------------------------------------

test_that("sign_checks stays three rows without the labour block", {
  ct <- data.frame(
    equation = c("de_consumption", "de_imports", "de_long_rate"),
    term = c("de_gdp", "de_domestic_demand", "ea_policy_rate"),
    estimate = c(0.5, 0.8, 0.2), stringsAsFactors = FALSE
  )
  expect_equal(nrow(sign_checks(ct, "de")), 3)
  expect_true(all(sign_checks(ct, "de")$ok))
})

test_that("sign_checks flags a wrong-signed wage Phillips curve", {
  base <- data.frame(
    equation = c("de_consumption", "de_imports", "de_long_rate"),
    term = c("de_gdp", "de_domestic_demand", "ea_policy_rate"),
    estimate = c(0.5, 0.8, 0.2), stringsAsFactors = FALSE
  )
  good <- rbind(base, data.frame(equation = "de_wages", term = "de_unemployment",
                                 estimate = -0.3, stringsAsFactors = FALSE))
  bad <- rbind(base, data.frame(equation = "de_wages", term = "de_unemployment",
                                estimate = 0.3, stringsAsFactors = FALSE))
  g <- sign_checks(good, "de", labour = TRUE)
  b <- sign_checks(bad, "de", labour = TRUE)
  expect_true(g$ok[g$check == "wage_phillips_curve_negative"])
  expect_false(b$ok[b$check == "wage_phillips_curve_negative"])
})

test_that("a sign check that cannot be evaluated is a failure, not a pass", {
  ct <- data.frame(equation = character(0), term = character(0),
                   estimate = numeric(0), stringsAsFactors = FALSE)
  out <- sign_checks(ct, "de", labour = TRUE)
  expect_true(all(is.na(out$estimate)))
  expect_false(any(out$ok))
})

test_that("the euro-appreciation rules expect a negative sign", {
  # eur_usd is quoted USD per EUR, so a rise is a euro appreciation and must
  # LOWER euro-denominated import and energy prices. Easy to get backwards.
  rules <- stage3a_sign_rules("de")
  fx_rules <- Filter(function(r) r$term == "eur_usd", rules)
  expect_length(fx_rules, 2)
  expect_true(all(vapply(fx_rules, function(r) r$test(-0.2), logical(1))))
  expect_false(any(vapply(fx_rules, function(r) r$test(0.2), logical(1))))
})

# --- lag stability --------------------------------------------------------

test_that("check_lag_stability flags only own lags at or above the threshold", {
  ct <- data.frame(
    equation = c("de_wages", "de_employment", "de_long_rate", "de_wages"),
    term = c("de_wages.L(1)", "de_employment.L(1)", "de_long_rate.L(1)", "de_prices"),
    estimate = c(1.02, 0.5, 0.94, 0.3),
    ci_high = c(1.2, 0.6, 1.0, 0.4), stringsAsFactors = FALSE
  )
  out <- check_lag_stability(ct)
  expect_equal(nrow(out), 3)  # the non-lag term is excluded
  expect_true(out$flagged[out$equation == "de_wages"])
  expect_false(out$flagged[out$equation == "de_employment"])
  expect_equal(out$equation[1], "de_wages")  # ordered most-persistent first
})

# --- stage 3b: lagged identities and the construct_phi prefix trap ----------

test_that("stage2_exogenous_variables strips lag suffixes from identity components", {
  # A stock-flow accumulation identity carries a LAGGED component. Left raw, it
  # would be declared exogenous and koma's exact-set validate_completeness()
  # aborts with "Redundant exogenous variables detected".
  spec <- list(
    stochastic = list(
      de_netborrowing = list(terms = c("de_gdp", "de_netborrowing"),
                             lags = list(de_netborrowing = "1")),
      de_gdp = list(terms = c("oil_price", "de_gdp"), lags = list(de_gdp = "1"))
    ),
    identities = list(
      de_govdebt = stats::setNames(c(1, 1), c("de_govdebt.L(1)", "de_netborrowing"))
    )
  )
  exo <- stage2_exogenous_variables(spec)
  expect_false("de_govdebt.L(1)" %in% exo)
  expect_equal(exo, "oil_price")

  # and the system actually builds, with the lag counted in k
  sys <- build_stage2_system(spec)
  expect_true("de_govdebt.L(1)" %in% sys$total_exogenous_variables)
  expect_true("de_govdebt==1*de_govdebt.L(1)+1*de_netborrowing" %in% sys$equations)
})

test_that("an accumulation identity keeps explicit weights, never character(0)", {
  # koma stores implicit weights as character(0) -- the same silent corruption
  # CLAUDE.md records for `+ -0.4*x`. identity_equation() must emit "1*x".
  eq <- identity_equation("de_govdebt",
                          list(`de_govdebt.L(1)` = 1, de_netborrowing = 1))
  expect_equal(eq, "de_govdebt == 1*de_govdebt.L(1) + 1*de_netborrowing")
  sys <- koma::system_of_equations(
    equations = c("de_netborrowing ~ de_gdp + de_netborrowing.L(1)",
                  "de_gdp ~ oil_price + de_gdp.L(1)", eq),
    exogenous_variables = "oil_price"
  )
  # The failure mode is an EMPTY weight (character(0)), not a wrongly-typed
  # one: koma stores implicit weights as character(0) and then silently
  # mis-specifies the identity. Assert both components carry a real weight of 1.
  wts <- sys$identities$de_govdebt$weights
  expect_length(wts, 2)
  expect_false(any(vapply(wts, function(w) length(w) == 0, logical(1))))
  expect_equal(unname(as.numeric(unlist(wts))), c(1, 1))
})

test_that("stage2_preflight catches a lagged-name prefix collision", {
  # koma's construct_phi() prefix-matches lag columns, so `de_debt` would match
  # both de_debt.L(1) and de_debt_ratio.L(1) and silently mis-map the companion
  # matrix. Nothing downstream errors, so the preflight has to catch it.
  panel <- stage3a_synthetic_panel("de")
  mk <- function(v) koma::as_ets(stats::ts(v, start = c(2000, 1), frequency = 4),
                                 series_type = "level", method = "diff_log")
  n <- length(panel$de_gdp)
  panel$de_debt <- mk(100 + cumsum(stats::rnorm(n, 0.4, 0.2)))
  panel$de_debt_ratio <- mk(60 + cumsum(stats::rnorm(n, 0.1, 0.2)))

  colliding <- koma::system_of_equations(
    equations = c("de_debt ~ de_gdp + de_debt.L(1)",
                  "de_debt_ratio ~ de_gdp + de_debt_ratio.L(1)",
                  "de_gdp ~ oil_price + de_gdp.L(1)"),
    exogenous_variables = "oil_price"
  )
  out <- stage2_preflight(colliding, panel, seeds = 1,
                          dates = list(estimation = list(start = c(2000, 1), end = c(2015, 4)),
                                       forecast = list(start = c(2016, 1), end = c(2016, 4))))
  row <- out[out$check == "no lagged endogenous name prefixes another", ]
  expect_false(row$ok)
  expect_match(row$detail, "de_debt")
})

test_that("build_stage2_panel constructs the external block's identity series", {
  # Defining an identity without constructing its LHS series is caught by
  # stage2_preflight()'s "every variable has a panel series" check -- but only
  # if the construction exists to be tested. Both are pure price differences.
  panel <- stage3a_synthetic_panel("de")
  n <- length(panel$de_gdp)
  set.seed(11)
  mk <- function(v) koma::as_ets(stats::ts(v, start = c(2000, 1), frequency = 4),
                                 series_type = "level", method = "diff_log")
  panel$de_foreign_prices <- mk(100 + cumsum(stats::rnorm(n, 0.3, 0.4)))
  lw <- list(foreign_demand = list(de = c(fr_gdp = 0.4, row_gdp = 0.6)), ea = c(de = 1))
  hw <- c(nonenergy_prices = 0.9, energy_prices = 0.1)

  sp <- build_stage2_panel(panel, lw, labour_countries = "de",
                           hicp_weights = list(de = hw), external_countries = "de")
  expect_true(all(c("de_competitiveness", "de_terms_of_trade") %in% names(sp)))

  maxdiff <- function(lhs, rhs, w) {
    m <- stats::na.omit(do.call(cbind, lapply(c(lhs, rhs), function(x) koma::rate(sp[[x]]))))
    max(abs(m[, 1] - as.numeric(m[, -1, drop = FALSE] %*% w)))
  }
  expect_lt(maxdiff("de_competitiveness",
                    c("de_export_prices", "de_foreign_prices"), c(1, -1)), 1e-10)
  expect_lt(maxdiff("de_terms_of_trade",
                    c("de_export_prices", "de_import_prices"), c(1, -1)), 1e-10)

  # and nothing is built for a country that has no external block
  plain <- build_stage2_panel(panel, lw, labour_countries = "de", hicp_weights = list(de = hw))
  expect_false("de_competitiveness" %in% names(plain))
})

test_that("the stage-3b blocks require the labour block underneath them", {
  expect_error(
    stage2_options(labour_countries = character(), external_countries = "de"),
    "not"
  )
  expect_error(
    stage2_options(labour_countries = "de", fiscal_countries = "fr",
                   hicp_weights = list(de = c(nonenergy_prices = 0.9, energy_prices = 0.1))),
    "not"
  )
})
