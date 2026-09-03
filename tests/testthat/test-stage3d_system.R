test_that("bloc_concepts is opt-in about the labour set", {
  base <- bloc_concepts()
  full <- bloc_concepts(stage3a = TRUE)
  expect_false(any(c("wages", "employment", "nonenergy_prices") %in% base))
  expect_true(all(c("wages", "employment", "energy_prices", "nonenergy_prices",
                    "import_prices", "export_prices") %in% full))
  expect_true(all(base %in% full))
})

test_that("bloc_hicp_weights averages members and sums to exactly 1", {
  w <- list(
    at = c(nonenergy_prices = 0.92, energy_prices = 0.08),
    nl = c(nonenergy_prices = 0.90, energy_prices = 0.10)
  )
  out <- bloc_hicp_weights(w, c(at = 0.25, nl = 0.75))
  expect_setequal(names(out), c("nonenergy_prices", "energy_prices"))
  expect_equal(sum(out), 1)
  expect_equal(unname(out[["energy_prices"]]), 0.25 * 0.08 + 0.75 * 0.10, tolerance = 1e-12)
  expect_equal(attr(out, "members"), c("at", "nl"))
  expect_error(bloc_hicp_weights(w, c(at = 0.5, xx = 0.5)), "no entry for")
})

test_that("bloc_hicp_weights renormalises away a rounding residual", {
  # labour_block() aborts unless the split sums to 1 -- seven members' rounded
  # weights can leave a 1e-4 crumb, which is not a missing component.
  w <- list(a = c(nonenergy_prices = 0.9, energy_prices = 0.0999),
            b = c(nonenergy_prices = 0.9, energy_prices = 0.0999))
  out <- bloc_hicp_weights(w, c(a = 0.5, b = 0.5))
  expect_equal(sum(out), 1)
})

# --------------------------------------------------------------------------
# The affordability frontier
# --------------------------------------------------------------------------

test_that("stage3d_frontier reproduces the k arithmetic and the df >= 4 floor", {
  f <- stage3d_frontier(base_k = 46, t_obs = 78, block_cost = 7,
                        entities = c(de = 1, reu = 7, fr = 1, it = 1))
  expect_equal(nrow(f), 5)
  expect_equal(f$k, c(46, 53, 60, 67, 74))
  expect_equal(f$df, 78 - c(46, 53, 60, 67, 74))
  # A bloc covers seven economies for one entity's worth of columns -- that is
  # the whole point, and it must show up in the coverage column.
  expect_equal(f$economies_covered, c(0, 1, 8, 9, 10))
  expect_true(all(f$feasible))

  # df >= 4 is the floor, not df > 0 (evaluation.qmd, 92 estimations).
  tight <- stage3d_frontier(base_k = 46, t_obs = 52)
  expect_false(any(tight$feasible[tight$df < 4]))
  expect_true(all(tight$feasible[tight$df >= 4]))
})

test_that("stage3d_config keeps the stage-2d core and adds the labour block", {
  w <- c(at = 0.03, be = 0.04, de = 0.30, es = 0.10, fr = 0.20,
         gr = 0.02, ie = 0.04, it = 0.15, nl = 0.08, pt = 0.04)
  hicp <- list(de = c(nonenergy_prices = 0.89, energy_prices = 0.11),
               reu = c(nonenergy_prices = 0.90, energy_prices = 0.10))
  cfg <- stage3d_config(w, hicp_weights = hicp, labour_countries = c("de", "reu"))

  expect_equal(cfg$labour_countries, c("de", "reu"))
  expect_equal(cfg$opts$labour_countries, c("de", "reu"))
  # The stage-2d core is untouched: same entities, same refinements, same merge.
  expect_setequal(cfg$opts$spread_countries, stage2d_countries())
  expect_equal(cfg$opts$merged_demand_countries, "cn")
  expect_setequal(cfg$refinements, setdiff(stage2c_refinements(), "import_content"))
  expect_setequal(names(cfg$opts$bloc_weights$reu), reu_members())
})

test_that("stage3d_config refuses entities that cannot carry the block", {
  w <- c(at = 0.03, be = 0.04, de = 0.30, es = 0.10, fr = 0.20,
         gr = 0.02, ie = 0.04, it = 0.15, nl = 0.08, pt = 0.04)
  hicp <- list(de = c(nonenergy_prices = 0.89, energy_prices = 0.11))
  # China has no wage, employment or HICP-split data; the US has none of the
  # extended concepts on FRED.
  expect_error(stage3d_config(w, hicp_weights = hicp, labour_countries = c("de", "cn")),
               "cannot carry the labour block")
  expect_error(stage3d_config(w, hicp_weights = hicp, labour_countries = c("de", "us")),
               "cannot carry the labour block")
  expect_error(stage3d_config(w, hicp_weights = hicp, labour_countries = "xx"),
               "not in the stage-2d entity set")
})

# --------------------------------------------------------------------------
# Bugs the regional-core rollout exposed
# --------------------------------------------------------------------------

test_that("foreign_price_weights resolves a partner on any demand basis", {
  # A foreign_demand identity loads partner GDP (2a/2b), partner consumption
  # (ireland_proxy) or partner IMPORTS (2c/2d). A fixed `_(gdp|consumption)$`
  # strip left `fr_imports` intact and then aborted inside country_var().
  lw <- list(foreign_demand = list(
    gdp   = c(fr_gdp = 0.3, us_gdp = 0.2, row_gdp = 0.5),
    imp   = c(fr_imports = 0.3, reu_imports = 0.2, row_gdp = 0.5),
    proxy = c(ie_consumption = 0.4, row_gdp = 0.6)
  ))
  expect_setequal(names(foreign_price_weights(lw, "gdp")),
                  c("fr_export_prices", "us_export_prices"))
  expect_setequal(names(foreign_price_weights(lw, "imp")),
                  c("fr_export_prices", "reu_export_prices"))
  expect_setequal(names(foreign_price_weights(lw, "proxy")), "ie_export_prices")

  # The renormalised weights and the recorded dropped residual are unchanged.
  w <- foreign_price_weights(lw, "gdp")
  expect_equal(sum(w), 1)
  expect_equal(attr(w, "row_weight_dropped"), 0.5)
})

test_that("identity_equation renders enough digits for koma to match the series", {
  # koma parses the STRING while the identity's LHS series is built from the
  # numeric weight, so a weight with more than 7 significant digits made the
  # two disagree -- silently, since koma has no identity-consistency check.
  eq <- identity_equation("p", list(a = 0.90056321, b = 0.09943679))
  expect_true(grepl("0.90056321", eq, fixed = TRUE))
  expect_true(grepl("0.09943679", eq, fixed = TRUE))

  # A weight that is already round must render exactly as before, so no
  # existing equation string changes.
  expect_equal(identity_equation("de_gdp", list(de_domestic_demand = 0.937,
                                                de_exports = 0.382,
                                                de_imports = -0.33)),
               "de_gdp == 0.937*de_domestic_demand + 0.382*de_exports - 0.33*de_imports")
  expect_equal(identity_equation("x", list(a = 1, b = 1)), "x == 1*a + 1*b")
})

test_that("sign_checks drops the stage-2c Phillips rule for a labour country", {
  # Under the labour block `<iso2>_prices` is an identity, so there is no
  # `prices ~ gdp` coefficient. The claim is restated as
  # price_phillips_curve_negative on nonenergy_prices.
  coefs <- data.frame(
    equation = c("de_prices", "de_nonenergy_prices"),
    term = c("de_gdp", "de_unemployment"),
    estimate = c(NA_real_, -0.2), stringsAsFactors = FALSE
  )
  with_labour <- sign_checks(coefs, "de", stage2c = stage2c_refinements(), labour = TRUE)
  expect_false("phillips_curve_positive" %in% with_labour$check)
  expect_true("price_phillips_curve_negative" %in% with_labour$check)

  without <- sign_checks(coefs, "de", stage2c = stage2c_refinements())
  expect_true("phillips_curve_positive" %in% without$check)
})
