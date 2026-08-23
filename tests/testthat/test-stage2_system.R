# --- fixtures ---------------------------------------------------------

# A two-country trade matrix in the same shape build_trade_weight_matrix()
# returns: rows = reporter, columns = partners + "row", rows summing to 1.
stage2_test_trade_weights <- function() {
  m <- matrix(
    c(
      0.00, 0.20, 0.80,
      0.30, 0.00, 0.70
    ),
    nrow = 2, byrow = TRUE,
    dimnames = list(c("de", "fr"), c("de", "fr", "row"))
  )
  m
}

stage2_test_gdp_weights <- function() c(de = 0.6, fr = 0.3, it = 0.1)

# Same shape for an arbitrary country set, with deliberately uneven weights
# (0.035, 0.065, 0.095, ...) so a threshold has something to bite on.
stage2_test_trade_weights_n <- function(countries) {
  n <- length(countries)
  m <- matrix(0, n, n + 1, dimnames = list(countries, c(countries, "row")))
  for (i in seq_len(n)) {
    others <- setdiff(seq_len(n), i)
    w <- seq_along(others) * 0.03 + 0.005
    m[i, others] <- w
    m[i, "row"] <- 1 - sum(w)
  }
  m
}

stage2_test_shares <- function(countries = c("de", "fr")) {
  stats::setNames(lapply(countries, function(cc) {
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
}

stage2_test_spec <- function(countries = c("de", "fr")) {
  lw <- stage2_linkage_weights(countries, stage2_test_trade_weights(), stage2_test_gdp_weights())
  stage2_spec(countries, stage2_test_shares(countries), lw)
}

# A synthetic two-country panel, mirroring the fixture style in
# test-diagnostics.R. Levels only, so chain_weighted_index() is valid.
stage2_test_panel <- function(n = 80, seed = 7) {
  set.seed(seed)
  mk <- function(v, series_type = "level", method = "diff_log") {
    koma::as_ets(stats::ts(v, start = c(2000, 1), frequency = 4),
      series_type = series_type, method = method
    )
  }
  walk <- function(base, drift, sd) base + cumsum(stats::rnorm(n, drift, sd))

  panel <- list()
  for (cc in c("de", "fr")) {
    consumption <- walk(100, 1.0, 0.2)
    investment <- walk(40, 0.4, 0.1)
    government <- walk(60, 0.5, 0.1)
    exports <- walk(90, 0.9, 0.2)
    imports <- walk(80, 0.8, 0.2)
    dd <- consumption + investment + government
    series <- list(
      consumption = mk(consumption), investment = mk(investment),
      government = mk(government), exports = mk(exports), imports = mk(imports),
      domestic_demand = mk(dd), gdp = mk(dd + exports - imports),
      prices = mk(walk(100, 0.4, 0.2)),
      long_rate = mk(abs(3 + cumsum(stats::rnorm(n, 0, 0.1))), "rate", "none")
    )
    panel <- c(panel, stats::setNames(series, country_var(cc, names(series))))
  }
  c(panel, list(
    row_gdp = mk(walk(100, 0.8, 0.2)),
    eur_usd = mk(1.1 + cumsum(stats::rnorm(n, 0, 0.01))),
    oil_price = mk(abs(50 + cumsum(stats::rnorm(n, 0.3, 1)))),
    ea_policy_rate = mk(abs(2 + cumsum(stats::rnorm(n, 0, 0.1))), "rate", "none")
  ))
}

# --- stage2_linkage_weights -------------------------------------------

test_that("stage2_linkage_weights lumps unmodelled partners into row_gdp", {
  lw <- stage2_linkage_weights(c("de", "fr"), stage2_test_trade_weights(), stage2_test_gdp_weights())

  expect_named(lw$foreign_demand$de, c("fr_gdp", "row_gdp"))
  expect_equal(unname(lw$foreign_demand$de[["fr_gdp"]]), 0.2)
  # France's own W_trade weight is kept as-is; the rest rides on row_gdp so
  # the weights still sum to 1.
  expect_equal(unname(lw$foreign_demand$de[["row_gdp"]]), 0.8)
  expect_equal(sum(lw$foreign_demand$de), 1)
  expect_equal(sum(lw$foreign_demand$fr), 1)
  expect_named(lw$foreign_demand$fr, c("de_gdp", "row_gdp"))
})

test_that("stage2_linkage_weights renormalises GDP weights over the subset", {
  lw <- stage2_linkage_weights(c("de", "fr"), stage2_test_trade_weights(), stage2_test_gdp_weights())
  # 0.6 and 0.3 out of the full set, renormalised over just the two.
  expect_equal(unname(lw$ea[["de"]]), round(0.6 / 0.9, 3))
  expect_equal(unname(lw$ea[["fr"]]), round(0.3 / 0.9, 3))
  expect_equal(sum(lw$ea), 1, tolerance = 1e-3)
})

test_that("stage2_linkage_weights rejects an unusable country set", {
  tw <- stage2_test_trade_weights()
  gw <- stage2_test_gdp_weights()
  expect_error(stage2_linkage_weights("de", tw, gw), "at least two")
  expect_error(stage2_linkage_weights(c("de", "zz"), tw, gw), "no row")
  expect_error(stage2_linkage_weights(c("de", "it"), tw, gw), "no row")
})

# --- build_system_equations / stage2_exogenous_variables --------------

test_that("build_system_equations emits every identity after every stochastic equation", {
  eqs <- build_system_equations(stage2_test_spec())
  is_identity <- grepl("==", eqs, fixed = TRUE)

  # koma assumes this positionally in model_identification() and
  # estimate_sem(); an identity in the middle is silently mis-estimated.
  expect_false(any(diff(which(!is_identity)) > 1))
  expect_true(all(which(is_identity) > max(which(!is_identity))))
  expect_equal(sum(is_identity), 8)
  expect_equal(sum(!is_identity), 13)
})

test_that("build_system_equations wires the cross-country linkage", {
  eqs <- build_system_equations(stage2_test_spec())

  # exports are driven by foreign demand, not the exogenous row_gdp
  expect_true("de_exports ~ de_foreign_demand + de_exports.L(1)" %in% eqs)
  expect_false(any(grepl("^de_exports ~ row_gdp", eqs)))
  # foreign demand is built out of the OTHER country's endogenous gdp
  expect_true("de_foreign_demand == 0.2*fr_gdp + 0.8*row_gdp" %in% eqs)
  expect_true("fr_foreign_demand == 0.3*de_gdp + 0.7*row_gdp" %in% eqs)
  # the policy rate has its own equation, and reaches investment via long_rate
  expect_true("ea_policy_rate ~ ea_prices + ea_gdp + ea_policy_rate.L(1)" %in% eqs)
  expect_true("de_investment ~ de_gdp + de_long_rate + de_investment.L(1)" %in% eqs)
})

test_that("build_system_equations appends per-equation tau settings", {
  eqs <- build_system_equations(stage2_test_spec(), tau = c(de_consumption = 2.2))
  expect_true("de_consumption ~ de_gdp + de_consumption.L(1) [tau = 2.2]" %in% eqs)
  expect_error(build_system_equations(stage2_test_spec(), tau = c(nope = 2)), "not a stochastic equation")
})

test_that("stage2_exogenous_variables derives the exact set koma requires", {
  ex <- stage2_exogenous_variables(stage2_test_spec())

  # koma's validate_completeness() is an exact-set check: a superset aborts
  # ("Redundant exogenous variables detected") just as a subset does.
  expect_setequal(ex, c("eur_usd", "oil_price", "row_gdp", "de_government", "fr_government"))
  # ea_policy_rate is endogenous in stage 2 -- that is the crucial change
  expect_false("ea_policy_rate" %in% ex)
  # so are the aggregates and foreign demand
  expect_false(any(c("ea_gdp", "ea_prices", "de_foreign_demand") %in% ex))
})

# --- build_stage2_system ----------------------------------------------

test_that("build_stage2_system returns an identified koma_seq", {
  sys_eq <- build_stage2_system(stage2_test_spec())

  expect_true(koma::is_system_of_equations(sys_eq))
  expect_length(sys_eq$endogenous_variables, 21)
  expect_length(sys_eq$stochastic_equations, 13)
  expect_length(sys_eq$identities, 8)
  expect_true("ea_policy_rate" %in% sys_eq$stochastic_equations)

  # model_identification() fills free coefficients with rnorm draws, so one
  # pass proves little -- repeat it.
  for (s in 1:5) {
    set.seed(s)
    expect_true(koma::model_identification(
      sys_eq$character_gamma_matrix, sys_eq$character_beta_matrix, sys_eq$identities
    ))
  }
})

test_that("the contemporaneous cross-country cycle is present", {
  sys_eq <- build_stage2_system(stage2_test_spec())
  g <- sys_eq$character_gamma_matrix
  v <- sys_eq$endogenous_variables
  adjacency <- g != "0" & g != "1"
  diag(adjacency) <- FALSE

  reachable <- function(from) {
    seen <- from
    repeat {
      nxt <- unique(c(seen, v[which(adjacency[v %in% seen, , drop = FALSE], arr.ind = TRUE)[, 2]]))
      if (setequal(nxt, seen)) break
      seen <- nxt
    }
    seen
  }

  # de_gdp -> fr_foreign_demand -> fr_exports -> fr_gdp -> de_foreign_demand
  # -> de_exports -> de_gdp
  expect_true("fr_gdp" %in% reachable("de_gdp"))
  expect_true("de_gdp" %in% reachable("fr_gdp"))
  # and the policy rate now reaches the real side (via long_rate -> investment)
  expect_true("de_gdp" %in% reachable("ea_policy_rate"))
})

# --- chain_weighted_index ---------------------------------------------

test_that("chain_weighted_index reproduces its identity in rate space", {
  panel <- stage2_test_panel()
  weights <- c(fr_gdp = 0.2, row_gdp = 0.8)
  index <- chain_weighted_index(panel, weights)

  expect_true(koma::is_ets(index))
  expect_equal(attr(index, "series_type"), "level")
  expect_equal(attr(index, "method"), "diff_log")

  # The whole point: koma differences this before estimating, and the
  # differenced series must equal the identity's weighted average of the
  # components' growth rates.
  lhs <- as.numeric(koma::rate(index))
  rhs <- 0.2 * as.numeric(koma::rate(panel$fr_gdp)) + 0.8 * as.numeric(koma::rate(panel$row_gdp))
  expect_equal(lhs, rhs, tolerance = 1e-10)
})

test_that("chain_weighted_index differs materially from a level-space sum", {
  panel <- stage2_test_panel()
  weights <- c(fr_gdp = 0.2, row_gdp = 0.8)

  rate_space <- as.numeric(koma::rate(chain_weighted_index(panel, weights)))
  level_space <- 100 * diff(log(
    0.2 * as.numeric(panel$fr_gdp) + 0.8 * as.numeric(panel$row_gdp)
  ))

  # apply_weights()-style level aggregation does NOT satisfy the identity;
  # this asserts the two really are different so the distinction cannot
  # silently regress into a no-op.
  expect_gt(max(abs(rate_space - level_space)), 1e-6)
})

test_that("chain_weighted_index guards its inputs", {
  panel <- stage2_test_panel()

  expect_error(chain_weighted_index(panel, c(ea_policy_rate = 1)), "series_type")
  expect_error(chain_weighted_index(panel, c(no_such_gdp = 1)), "missing")
  expect_error(chain_weighted_index(panel, c(1, 2)), "named by variable")
  expect_error(chain_weighted_index(panel, numeric(0)), "named by variable")

  negative <- panel
  negative$row_gdp[5] <- -1
  expect_error(chain_weighted_index(negative, c(row_gdp = 1)), "non-positive")
})

test_that("chain_weighted_index preserves ragged edges but rejects internal gaps", {
  panel <- stage2_test_panel()

  ragged <- panel
  ragged$row_gdp[78:80] <- NA
  out <- chain_weighted_index(ragged, c(fr_gdp = 0.2, row_gdp = 0.8))
  # cumsum() would otherwise turn a trailing NA into an all-NA series
  expect_equal(sum(is.na(out)), 3)
  expect_false(anyNA(out[1:77]))

  gapped <- panel
  gapped$row_gdp[40] <- NA
  expect_error(chain_weighted_index(gapped, c(fr_gdp = 0.2, row_gdp = 0.8)), "internal")
})

# --- build_stage2_panel / stage2_preflight ----------------------------

test_that("build_stage2_panel supplies a series for every endogenous variable", {
  panel <- stage2_test_panel()
  lw <- stage2_linkage_weights(c("de", "fr"), stage2_test_trade_weights(), stage2_test_gdp_weights())
  sys_eq <- build_stage2_system(stage2_test_spec())

  # koma requires a series for identity-defined variables too, and only
  # says so from inside estimate().
  expect_false(all(sys_eq$endogenous_variables %in% names(panel)))
  augmented <- build_stage2_panel(panel, lw)
  expect_true(all(sys_eq$endogenous_variables %in% names(augmented)))
  expect_setequal(
    setdiff(names(augmented), names(panel)),
    c("de_foreign_demand", "fr_foreign_demand", "ea_gdp", "ea_prices")
  )
})

test_that("stage2_preflight passes a well-formed system and catches a missing series", {
  panel <- stage2_test_panel()
  lw <- stage2_linkage_weights(c("de", "fr"), stage2_test_trade_weights(), stage2_test_gdp_weights())
  sys_eq <- build_stage2_system(stage2_test_spec())

  ok <- stage2_preflight(sys_eq, build_stage2_panel(panel, lw), seeds = 1:3,
                         dates = list(estimation = list(start = c(2000, 1), end = c(2015, 4))))
  expect_true(all(ok$ok))
  expect_true("identities declared last" %in% ok$check)

  # without `dates` the k < T row cannot be evaluated and reports NA rather
  # than guessing -- callers must treat that as unchecked, not as a pass
  unchecked <- stage2_preflight(sys_eq, build_stage2_panel(panel, lw), seeds = 1:2)
  expect_true(is.na(unchecked$ok[unchecked$check == "k < T (x'x invertible, Wishart df > 0)"]))
  expect_true(all(unchecked$ok[unchecked$check != "k < T (x'x invertible, Wishart df > 0)"]))

  bad <- stage2_preflight(sys_eq, panel, seeds = 1:3)
  expect_false(bad$ok[bad$check == "every variable has a panel series"])
  # it reports every problem rather than aborting at the first
  expect_s3_class(bad, "data.frame")
})

test_that("stage2_preflight rejects a non-koma_seq", {
  expect_error(stage2_preflight(list(), list()), "koma_seq")
})

# --- fit_stage2 -------------------------------------------------------

test_that("fit_stage2 names the missing derived series rather than failing inside koma", {
  panel <- stage2_test_panel()
  sys_eq <- build_stage2_system(stage2_test_spec())
  dates <- list(
    estimation = list(start = c(2000, 1), end = c(2015, 4)),
    forecast = list(start = c(2018, 1), end = c(2018, 4))
  )
  expect_error(fit_stage2(sys_eq, panel, dates), "de_foreign_demand")
})

test_that("fit_stage2 estimates the linked system end to end", {
  skip_on_cran()
  panel <- stage2_test_panel()
  lw <- stage2_linkage_weights(c("de", "fr"), stage2_test_trade_weights(), stage2_test_gdp_weights())
  sys_eq <- build_stage2_system(stage2_test_spec())
  dates <- list(
    estimation = list(start = c(2000, 1), end = c(2015, 4)),
    forecast = list(start = c(2018, 1), end = c(2018, 4))
  )

  fit <- fit_stage2(sys_eq, build_stage2_panel(panel, lw), dates,
    options = list(gibbs = list(ndraws = 200))
  )

  expect_s3_class(fit, "koma_estimate")
  expect_named(fit$estimates, sys_eq$stochastic_equations)
  expect_length(fit$estimates, 13)
  expect_true(is.numeric(attr(fit, "runtime_s")))

  # equations with no contemporaneous endogenous regressor have no
  # Metropolis step and must not be flagged
  acceptance <- check_acceptance_rates(fit)
  expect_false(acceptance$has_mh_step[acceptance$equation == "de_prices"])
  expect_false(acceptance$flagged[acceptance$equation == "de_prices"])
})

# --- stage 2b: build_system() and the block architecture ---------------

test_that("build_system assembles blocks with identities last", {
  spec <- stage2_test_spec()
  sys_eq <- build_system(
    countries = c("de", "fr"),
    blocks = list(whole = spec),
    weights = list()
  )
  expect_true(koma::is_system_of_equations(sys_eq))
  expect_length(sys_eq$endogenous_variables, 21)
  is_identity <- grepl("==", sys_eq$equations, fixed = TRUE)
  expect_true(all(which(is_identity) > max(which(!is_identity))))
})

test_that("build_system resolves a block supplied as a function", {
  spec <- stage2_test_spec()
  called_with <- NULL
  as_function <- function(countries, weights) {
    called_with <<- list(countries = countries, weights = weights)
    spec
  }
  sys_eq <- build_system(c("de", "fr"), list(f = as_function), list(w = 1))
  expect_equal(called_with$countries, c("de", "fr"))
  expect_equal(called_with$weights, list(w = 1))
  expect_length(sys_eq$endogenous_variables, 21)
})

test_that("build_system names the blocks that collide", {
  spec <- stage2_test_spec()
  clash <- list(stochastic = list(), identities = list(ea_gdp = c(de_gdp = 1)))
  expect_error(
    build_system(c("de", "fr"), list(main = spec, extra = clash), list()),
    "ea_gdp"
  )
  expect_error(build_system(character(), list(), list()), "empty")
  expect_error(build_system(character(), list(bad = 42), list()), "stochastic")
})

test_that("a new block extends the system without touching existing code", {
  # The stage-3 path: contribute an identity block and nothing else changes.
  spec <- stage2_test_spec()
  world_block <- function(countries, weights) {
    list(
      stochastic = list(),
      identities = list(world_gdp = c(de_gdp = 0.7, fr_gdp = 0.3))
    )
  }
  base <- build_system(c("de", "fr"), list(main = spec), list())
  extended <- build_system(c("de", "fr"), list(main = spec, world = world_block), list())

  expect_length(extended$endogenous_variables, length(base$endogenous_variables) + 1)
  expect_true("world_gdp" %in% names(extended$identities))
  expect_true(all(base$stochastic_equations %in% extended$stochastic_equations))
  # still ordered correctly with the new identity appended
  is_identity <- grepl("==", extended$equations, fixed = TRUE)
  expect_true(all(which(is_identity) > max(which(!is_identity))))
})

# --- stage 2b block constructors --------------------------------------

test_that("country_block drops government and renormalises when asked", {
  shares <- stage2_test_shares("de")$de
  fw <- c(fr_gdp = 0.2, row_gdp = 0.8)

  with_gov <- country_block("de", shares, fw, stage2_options(include_government = TRUE))
  expect_named(with_gov$identities$de_domestic_demand,
               c("de_consumption", "de_investment", "de_government"))

  without <- country_block("de", shares, fw, stage2_options(include_government = FALSE))
  expect_named(without$identities$de_domestic_demand, c("de_consumption", "de_investment"))
  # renormalised: raw 0.6/0.2 would sum to 0.8 and under-predict dd growth
  expect_equal(sum(without$identities$de_domestic_demand), 1, tolerance = 1e-3)
  expect_equal(unname(without$identities$de_domestic_demand[["de_consumption"]]), 0.75)
})

test_that("country_block adds extra regressors to real-side equations only", {
  shares <- stage2_test_shares("de")$de
  b <- country_block("de", shares, c(fr_gdp = 1),
                     stage2_options(extra_regressors = c("covid_2020q2")))
  real <- c("de_consumption", "de_investment", "de_exports", "de_imports", "de_prices")
  for (eq in real) expect_true("covid_2020q2" %in% b$stochastic[[eq]]$terms)
  # rates show no mechanical COVID break, so they are deliberately excluded
  expect_false("covid_2020q2" %in% b$stochastic$de_long_rate$terms)
})

test_that("country_block gives the US a policy rule and its own policy rate", {
  shares <- stage2_test_shares("us")$us
  b <- country_block("us", shares, c(de_gdp = 1), stage2_options(policy_rule = TRUE))
  expect_true("us_policy_rate" %in% names(b$stochastic))
  expect_true("us_policy_rate" %in% b$stochastic$us_long_rate$terms)
  expect_false("ea_policy_rate" %in% b$stochastic$us_long_rate$terms)

  without <- country_block("us", shares, c(de_gdp = 1), stage2_options(policy_rule = FALSE))
  expect_false("us_policy_rate" %in% names(without$stochastic))
  expect_true("ea_policy_rate" %in% without$stochastic$us_long_rate$terms)
})

test_that("country_block honours an fx override without breaking the default", {
  shares <- stage2_test_shares("us")$us
  default <- country_block("us", shares, c(de_gdp = 1), stage2_options())
  expect_true("eur_usd" %in% default$stochastic$us_prices$terms)

  overridden <- country_block("us", shares, c(de_gdp = 1),
                              stage2_options(fx = c(us = "us_exchange_rate")))
  expect_true("us_exchange_rate" %in% overridden$stochastic$us_prices$terms)
  expect_false("eur_usd" %in% overridden$stochastic$us_prices$terms)
})

test_that("there is exactly one ea_policy_rate however many countries", {
  for (n in c(2, 4)) {
    cs <- c("de", "fr", "it", "es")[seq_len(n)]
    lw <- stage2_linkage_weights(cs, stage2_test_trade_weights_n(cs), stage2_test_gdp_weights())
    spec <- stage2_spec(cs, stage2_test_shares(cs), lw)
    expect_equal(sum(names(spec$stochastic) == "ea_policy_rate"), 1)
  }
})

# --- linkage weight options -------------------------------------------

test_that("stage2_linkage_weights keeps names when there is one partner", {
  # matrix indexing drops names at length 1; the two-country pilot is exactly
  # that case and used to silently produce NA weights
  lw <- stage2_linkage_weights(c("de", "fr"), stage2_test_trade_weights(), stage2_test_gdp_weights())
  expect_false(anyNA(lw$foreign_demand$de))
  expect_named(lw$foreign_demand$de, c("fr_gdp", "row_gdp"))
})

test_that("stage2_linkage_weights folds sub-threshold partners into row", {
  cs <- c("de", "fr", "it", "es")
  tw <- stage2_test_trade_weights_n(cs)
  lw_all <- stage2_linkage_weights(cs, tw, stage2_test_gdp_weights(), threshold = 0)
  lw_cut <- stage2_linkage_weights(cs, tw, stage2_test_gdp_weights(), threshold = 0.05)

  expect_gt(length(lw_all$foreign_demand$de), length(lw_cut$foreign_demand$de))
  # folded, not dropped: still a proper weighted average
  expect_equal(sum(lw_cut$foreign_demand$de), 1, tolerance = 1e-3)
  expect_equal(sum(lw_all$foreign_demand$de), 1, tolerance = 1e-3)
})

test_that("ireland_proxy swaps ie_gdp for ie_consumption in partners only", {
  cs <- c("de", "ie")
  tw <- stage2_test_trade_weights_n(cs)
  base <- stage2_linkage_weights(cs, tw, c(de = 0.6, ie = 0.4))
  prox <- stage2_linkage_weights(cs, tw, c(de = 0.6, ie = 0.4), ireland_proxy = TRUE)

  expect_true("ie_gdp" %in% names(base$foreign_demand$de))
  expect_true("ie_consumption" %in% names(prox$foreign_demand$de))
  expect_false("ie_gdp" %in% names(prox$foreign_demand$de))
  # Ireland's own foreign demand is untouched
  expect_equal(names(base$foreign_demand$ie), names(prox$foreign_demand$ie))
})

test_that("ea weights cover euro-area members only", {
  lw <- stage2_linkage_weights(c("de", "fr", "us"),
                               stage2_test_trade_weights_n(c("de", "fr", "us")),
                               c(de = 0.6, fr = 0.3))
  expect_setequal(lw$ea_members, c("de", "fr"))
  expect_false("us" %in% names(lw$ea))
  expect_equal(sum(lw$ea), 1, tolerance = 1e-3)
})

# --- COVID dummies -----------------------------------------------------

test_that("covid_dummy is a 0/1 indicator that koma will not transform", {
  panel <- stage2_test_panel()
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

test_that("build_stage2_panel adds requested dummies", {
  panel <- stage2_test_panel()
  lw <- stage2_linkage_weights(c("de", "fr"), stage2_test_trade_weights(), stage2_test_gdp_weights())
  out <- build_stage2_panel(panel, lw, dummies = c("covid_2005q2", "covid_2005q3"))
  expect_true(all(c("covid_2005q2", "covid_2005q3") %in% names(out)))
  expect_equal(sum(out$covid_2005q2), 1)
})

test_that("stage2b_dummies_through only includes dummies at or before the origin", {
  expect_equal(stage2b_dummies_through(c(2019, 4)), character(0))
  expect_equal(stage2b_dummies_through(c(2020, 1)), "covid_2020q1")
  expect_equal(stage2b_dummies_through(c(2020, 2)), c("covid_2020q1", "covid_2020q2"))
  expect_equal(stage2b_dummies_through(c(2021, 1)), c("covid_2020q1", "covid_2020q2", "covid_2020q3"))
  expect_equal(stage2b_dummies_through(c(2024, 4)), stage2b_dummies())
})

# --- the k < T guard ---------------------------------------------------

test_that("stage2_preflight fails loudly when k >= T", {
  panel <- stage2_test_panel()
  lw <- stage2_linkage_weights(c("de", "fr"), stage2_test_trade_weights(), stage2_test_gdp_weights())
  sys_eq <- build_stage2_system(stage2_test_spec())
  augmented <- build_stage2_panel(panel, lw)

  roomy <- stage2_preflight(sys_eq, augmented, seeds = 1:2,
                            dates = list(estimation = list(start = c(2000, 1), end = c(2015, 4))))
  expect_true(roomy$ok[roomy$check == "k < T (x'x invertible, Wishart df > 0)"])

  # a window too short for k: koma would otherwise fail only deep inside
  # estimate(), with "computationally singular" and a negative Wishart df
  cramped <- stage2_preflight(sys_eq, augmented, seeds = 1:2,
                              dates = list(estimation = list(start = c(2000, 1), end = c(2002, 4))))
  expect_false(cramped$ok[cramped$check == "k < T (x'x invertible, Wishart df > 0)"])
})

test_that("estimation_length matches what koma actually reports", {
  panel <- stage2_test_panel()
  # 2000Q1-2019Q4 is 80 quarters; koma reports T = 78 after the diff_log
  # transform and the L(1) lag each cost one period.
  expect_equal(
    estimation_length(panel, list(estimation = list(start = c(2000, 1), end = c(2019, 4)))),
    78
  )
  expect_equal(
    estimation_length(panel, list(estimation = list(start = c(2000, 1), end = c(2024, 4)))),
    98
  )
})

# --- runtime projection ------------------------------------------------

test_that("project_stage2_runtime recovers a known exponent", {
  bench <- data.frame(stochastic = c(10, 20, 40, 80), seconds = 2 * c(10, 20, 40, 80)^1.5)
  proj <- project_stage2_runtime(bench, stochastic = 160)

  expect_equal(proj$exponent, 1.5, tolerance = 1e-6)
  expect_equal(proj$projected_seconds, 2 * 160^1.5, tolerance = 1e-4)
  expect_error(project_stage2_runtime(bench[1, ], 160), "at least two")
})
