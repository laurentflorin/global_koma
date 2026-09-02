test_that("stage2_policy_rule_countries keeps TRUE meaning the US alone", {
  expect_equal(stage2_policy_rule_countries(stage2_options()), character())
  expect_equal(stage2_policy_rule_countries(stage2_options(policy_rule = TRUE)), "us")
  expect_equal(
    stage2_policy_rule_countries(stage2_options(policy_rule = c("us", "cn"))),
    c("us", "cn")
  )
})

test_that("policy_rate_map is unchanged for stage 2b/2c and extended for 2d", {
  expect_equal(policy_rate_map(stage2b_config()$opts), c(us = "us_policy_rate"))
  expect_equal(policy_rate_map(stage2c_config()$opts), c(us = "us_policy_rate"))
  expect_equal(
    policy_rate_map(stage2_options(policy_rule = c("us", "cn"))),
    c(us = "us_policy_rate", cn = "cn_policy_rate")
  )
  # A country with no rule falls back to the shared euro-area rate, which is
  # what makes the spread identity and the constructed spread series agree.
  expect_equal(spread_policy_rate("de", policy_rate_map(stage2c_config()$opts)), "ea_policy_rate")
  expect_equal(spread_policy_rate("cn", policy_rate_map(stage2d_config()$opts)), "cn_policy_rate")
})

test_that("a generalised policy rule builds the same US block stage 2b had", {
  shares <- list(
    gdp = c(us_domestic_demand = 1, us_exports = 0.1, us_imports = -0.15),
    domestic_demand = c(us_consumption = 0.7, us_investment = 0.2, us_government = 0.1)
  )
  fw <- c(de_gdp = 0.4, row_gdp = 0.6)
  old <- country_block("us", shares, fw, stage2_options(policy_rule = TRUE))
  new <- country_block("us", shares, fw, stage2_options(policy_rule = "us"))
  expect_equal(old, new)
  expect_true("us_policy_rate" %in% names(old$stochastic))
  expect_equal(old$stochastic$us_long_rate$terms,
               c("us_prices", "us_policy_rate", "us_gdp", "us_long_rate"))
})

# --------------------------------------------------------------------------
# Merged domestic demand
# --------------------------------------------------------------------------

cn_shares <- function() {
  list(
    gdp = c(cn_domestic_demand = 0.958, cn_exports = 0.243, cn_imports = -0.201),
    domestic_demand = NULL
  )
}

test_that("merged demand replaces two equations with one and drops the identity", {
  fw <- c(de_imports = 0.03, row_gdp = 0.97)
  opts <- stage2_options(
    include_government = FALSE, policy_rule = "cn",
    phillips_countries = "cn", spread_countries = "cn",
    merged_demand_countries = "cn"
  )
  block <- country_block("cn", cn_shares(), fw, opts)

  expect_false(any(c("cn_consumption", "cn_investment") %in% names(block$stochastic)))
  expect_true("cn_domestic_demand" %in% names(block$stochastic))
  # Domestic demand is now ESTIMATED, so it must not also be defined by an
  # identity -- build_system() aborts on a duplicated left-hand side.
  expect_false("cn_domestic_demand" %in% names(block$identities))
  expect_setequal(names(block$identities), c("cn_gdp", "cn_foreign_demand", "cn_long_rate"))

  # The rate term survives the merge: it is the country's only monetary channel.
  expect_true("cn_long_rate" %in% block$stochastic$cn_domestic_demand$terms)
  expect_equal(names(block$stochastic$cn_domestic_demand$lags), "cn_domestic_demand")
  # Imports still load domestic demand; nothing downstream notices the change.
  expect_true("cn_domestic_demand" %in% block$stochastic$cn_imports$terms)
  # Spread identity is taken against China's OWN policy rate, not the ECB's.
  expect_setequal(names(block$identities$cn_long_rate), c("cn_spread", "cn_policy_rate"))
})

test_that("a non-merged country still needs its domestic-demand split", {
  fw <- c(de_imports = 0.03, row_gdp = 0.97)
  expect_error(
    country_block("cn", cn_shares(), fw, stage2_options(include_government = FALSE)),
    "carries no.*domestic_demand"
  )
})

test_that("merged demand leaves every other country's block untouched", {
  shares <- list(
    gdp = c(de_domestic_demand = 0.93, de_exports = 0.41, de_imports = -0.35),
    domestic_demand = c(de_consumption = 0.7, de_investment = 0.2, de_government = 0.1)
  )
  fw <- c(fr_imports = 0.1, row_gdp = 0.9)
  base <- stage2c_config()$opts
  with_merge <- stage2_options(
    include_government = base$include_government,
    extra_regressors = base$extra_regressors, policy_rule = base$policy_rule,
    fx = base$fx, phillips_countries = base$phillips_countries,
    consumption_rate_countries = base$consumption_rate_countries,
    spread_countries = base$spread_countries, merged_demand_countries = "cn"
  )
  expect_equal(country_block("de", shares, fw, base), country_block("de", shares, fw, with_merge))
})

# --------------------------------------------------------------------------
# Configuration
# --------------------------------------------------------------------------

test_that("stage2d_config keeps every stage-2c refinement and adds nothing else", {
  cfg <- stage2d_config()
  expect_setequal(cfg$refinements, setdiff(stage2c_refinements(), "import_content"))
  expect_equal(cfg$demand_concept, "imports")
  expect_setequal(cfg$opts$phillips_countries, stage2d_countries())
  expect_setequal(cfg$opts$spread_countries, stage2d_countries())
  expect_equal(cfg$opts$merged_demand_countries, "cn")
  expect_equal(cfg$blocs, list(reu = reu_members()))
  expect_false(cfg$opts$include_government)
})

test_that("stage2d_dates starts where China's export volumes do", {
  d <- stage2d_dates()
  expect_equal(d$estimation$start, c(2005, 1))
  expect_equal(d$estimation$end, c(2024, 4))
  expect_true(d$forecast$start[1] > d$estimation$end[1])
})

test_that("stage2_options rejects an undeclared bloc in bloc_weights", {
  expect_error(stage2_options(bloc_weights = list(xyz = c(at = 1))), "not.*bloc")
  expect_silent(stage2_options(bloc_weights = list(reu = c(at = 1))))
})

test_that("stage2_shares aborts rather than silently mis-weighting a bloc", {
  panel <- list()
  expect_error(
    stage2_shares("reu", panel, NULL, stage2_options()),
    "carries no.*bloc_weights"
  )
})

test_that("stage2d_config wires bloc weights through when given gdp_weights", {
  w <- c(at = 0.03, be = 0.04, de = 0.30, es = 0.10, fr = 0.20,
         gr = 0.02, ie = 0.04, it = 0.15, nl = 0.08, pt = 0.04)
  cfg <- stage2d_config(w)
  expect_setequal(names(cfg$opts$bloc_weights$reu), reu_members())
  expect_equal(sum(cfg$opts$bloc_weights$reu), 1)
  expect_equal(stage2d_config()$opts$bloc_weights, list())
})

# --------------------------------------------------------------------------
# Trade-weight collapse
# --------------------------------------------------------------------------

fake_trade_weights <- function() {
  countries <- c("de", "fr", "at", "nl", "us")
  cols <- c(countries, "row")
  m <- matrix(0, length(countries), length(cols), dimnames = list(countries, cols))
  m["de", ] <- c(0.00, 0.10, 0.06, 0.08, 0.12, 0.64)
  m["fr", ] <- c(0.14, 0.00, 0.02, 0.05, 0.09, 0.70)
  m["at", ] <- c(0.30, 0.05, 0.00, 0.04, 0.06, 0.55)
  m["nl", ] <- c(0.20, 0.07, 0.03, 0.00, 0.10, 0.60)
  m["us", ] <- c(0.05, 0.04, 0.01, 0.02, 0.00, 0.88)
  attr(m, "source") <- stats::setNames(c(rep("direct", 4), "dots"), countries)
  m
}

test_that("collapse_trade_weights sums the column and renormalises the row", {
  W <- fake_trade_weights()
  blocs <- list(reu = c("at", "nl"))
  bw <- list(reu = c(at = 0.25, nl = 0.75))
  out <- collapse_trade_weights(W, blocs, bw)

  expect_setequal(rownames(out), c("de", "fr", "us", "reu"))
  expect_equal(unname(rowSums(out)), rep(1, 4), tolerance = 1e-12)

  # Column: a non-member's weight on the bloc is the plain sum of its weights
  # on the members, and its other cells are untouched.
  expect_equal(unname(out["de", "reu"]), 0.06 + 0.08)
  expect_equal(unname(out["de", "fr"]), 0.10)
  expect_equal(unname(out["de", "row"]), 0.64)

  # Row: members' rows averaged, intra-bloc trade dropped, then renormalised.
  raw <- 0.25 * W["at", ] + 0.75 * W["nl", ]
  intra <- raw[["at"]] + raw[["nl"]]
  expect_equal(unname(attr(out, "intra_bloc")[["reu"]]), intra)
  expect_equal(unname(out["reu", "de"]), unname(raw[["de"]] / (1 - intra)), tolerance = 1e-12)
  expect_equal(unname(out["reu", "reu"]), 0)

  expect_equal(unname(attr(out, "source")[["reu"]]), "bloc average")
  expect_equal(unname(attr(out, "source")[["us"]]), "dots")
})

test_that("collapse_gdp_weights sums a bloc's members and keeps the total", {
  w <- c(at = 0.03, be = 0.04, de = 0.30, es = 0.10, fr = 0.20,
         gr = 0.02, ie = 0.04, it = 0.15, nl = 0.08, pt = 0.04)
  out <- collapse_gdp_weights(w, list(reu = reu_members()))
  expect_setequal(names(out), c("de", "fr", "it", "reu"))
  expect_equal(sum(out), sum(w))
  expect_equal(unname(out[["reu"]]), sum(w[reu_members()]))
})

test_that("collapse_trade_weights rejects overlapping blocs and unknown members", {
  W <- fake_trade_weights()
  expect_error(
    collapse_trade_weights(W, list(a = c("at"), b = c("at")), list(a = c(at = 1), b = c(at = 1))),
    "more than one bloc"
  )
  expect_error(
    collapse_trade_weights(W, list(reu = c("at", "xx")), list(reu = c(at = 0.5, xx = 0.5))),
    "no row for bloc member"
  )
})

test_that("build_trade_weight_matrix refuses a reporter in both reporter sets", {
  expect_error(
    build_trade_weight_matrix(c("de", "us"), reciprocal_reporters = "us",
                              dots_reporters = "us", out_path = NULL),
    "in both"
  )
})

# --------------------------------------------------------------------------
# row_gdp must stop containing China once China is modelled
# --------------------------------------------------------------------------

test_that("build_row_gdp only uses partners the weight vector still carries", {
  weights <- c(gb = 0.3, ch = 0.2, jp = 0.2, pl = 0.15, se = 0.15)
  expect_false("cn" %in% names(weights))
  expect_error(build_row_gdp(c(other = 1)), "names none of the")
})

# --------------------------------------------------------------------------
# Sign rules
# --------------------------------------------------------------------------

test_that("merged demand restates the two consumption rules instead of failing them", {
  coefs <- data.frame(
    equation = c("cn_domestic_demand", "cn_domestic_demand", "cn_imports", "cn_prices", "cn_spread"),
    term = c("cn_gdp", "cn_long_rate", "cn_domestic_demand", "cn_gdp", "cn_prices"),
    estimate = c(0.6, -0.2, 0.8, 0.3, 0.1),
    stringsAsFactors = FALSE
  )
  merged <- sign_checks(coefs, "cn", stage2c = stage2d_config()$refinements, merged_demand = TRUE)
  expect_true(all(merged$ok))
  expect_true("absorption_positive" %in% merged$check)
  expect_true("demand_rate_channel_negative" %in% merged$check)
  expect_false(any(grepl("^mpc_", merged$check)))

  # Without the flag the same fit reports two failures against equations the
  # system deliberately does not contain.
  unflagged <- sign_checks(coefs, "cn", stage2c = stage2d_config()$refinements)
  expect_false(all(unflagged$ok))
  expect_true(all(is.na(unflagged$estimate[unflagged$check %in%
    c("mpc_in_0_1", "consumption_rate_channel_negative")])))
})
