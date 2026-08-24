# Fixtures diagnostics_synthetic_panel()/diagnostics_synthetic_fit() live in
# helper-fixtures.R (shared with test-spillovers.R).

test_that("check_acceptance_rates flags equations outside the target band", {
  fit <- list(
    estimates = list(
      de_c = list(count_accepted = rep(c(1, 0), 50)),   # 50%, in band
      de_i = list(count_accepted = rep(NA_real_, 100))  # no MH step
    ),
    sys_eq = list(character_gamma_matrix = matrix("0", 2, 2,
      dimnames = list(c("de_c", "de_i"), c("de_c", "de_i"))))
  )
  class(fit) <- "koma_estimate"
  out <- check_acceptance_rates(fit)
  expect_s3_class(out, "data.frame")
  expect_named(out, c("equation", "has_mh_step", "acceptance_rate", "flagged"))

  expect_equal(out$acceptance_rate[out$equation == "de_c"], 0.5)
  expect_true(out$has_mh_step[out$equation == "de_c"])
  expect_false(out$flagged[out$equation == "de_c"])
})

test_that("check_acceptance_rates reports an all-NA equation as having no MH step", {
  # Equations with no contemporaneous endogenous regressor have no
  # Metropolis step: count_accepted is NA throughout. They must never be
  # flagged -- koma excludes them from its own warning for the same reason.
  fit <- list(estimates = list(de_i = list(count_accepted = rep(NA_real_, 100))))
  class(fit) <- "koma_estimate"
  out <- check_acceptance_rates(fit)

  expect_false(out$has_mh_step)
  expect_true(is.na(out$acceptance_rate))
  expect_false(out$flagged)
})

test_that("check_acceptance_rates flags rates on either side of the band", {
  fit <- list(estimates = list(
    too_low  = list(count_accepted = c(rep(1, 10), rep(0, 90))),  # 10%
    ok       = list(count_accepted = c(rep(1, 40), rep(0, 60))),  # 40%
    too_high = list(count_accepted = c(rep(1, 80), rep(0, 20)))   # 80%
  ))
  class(fit) <- "koma_estimate"
  out <- check_acceptance_rates(fit)

  expect_true(out$flagged[out$equation == "too_low"])
  expect_false(out$flagged[out$equation == "ok"])
  expect_true(out$flagged[out$equation == "too_high"])
})

test_that("check_acceptance_rates defaults to koma's own 20-60% band", {
  # koma:::get_default_acceptance_prob() returns c(0.2, 0.6). koma's
  # `equations` vignette prose says 30-60%; the code is authoritative.
  expect_equal(formals(check_acceptance_rates)$band, quote(c(0.2, 0.6)))
  expect_equal(koma:::get_default_acceptance_prob()$acceptance_prob, c(0.2, 0.6))
})

test_that("check_acceptance_rates rejects a malformed band and an empty fit", {
  fit <- list(estimates = list(a = list(count_accepted = c(1, 0))))
  class(fit) <- "koma_estimate"
  expect_error(check_acceptance_rates(fit, band = c(0.6, 0.2)), "increasing")

  empty <- structure(list(estimates = list()), class = "koma_estimate")
  expect_error(check_acceptance_rates(empty), "estimates")
})

test_that("check_identification reports per-equation order/rank conditions", {
  sys_eq <- koma::system_of_equations(c(
    "consumption ~ gdp + consumption.L(1)",
    "investment ~ investment.L(1)",
    "gdp == 0.6*consumption + 0.4*investment"
  ))
  out <- check_identification(sys_eq)

  expect_s3_class(out, "data.frame")
  expect_named(out, c("equation", "order_condition", "rank_condition"))
  expect_setequal(out$equation, c("consumption", "investment"))
  expect_true(all(out$order_condition))
})

test_that("check_identification rejects a non-koma_seq", {
  expect_error(check_identification(list()), "koma_seq")
})

test_that("diagnostics_grid returns one plot per variable and kind", {
  skip_on_cran()
  fx <- diagnostics_synthetic_fit()
  plots <- diagnostics_grid(fx$fit, "de", c("consumption", "investment"), kind = "trace")
  expect_named(plots, c("de_consumption", "de_investment"))
  expect_true(all(vapply(plots, inherits, logical(1), "ggplot")))
})

test_that("coefficient_table returns a 90% interval by default with the expected columns", {
  skip_on_cran()
  fx <- diagnostics_synthetic_fit()
  ct <- coefficient_table(fx$fit)

  expect_named(ct, c("equation", "term", "estimate", "ci_low", "ci_high"))
  expect_true(all(ct$ci_low <= ct$estimate))
  expect_true(all(ct$estimate <= ct$ci_high))
  expect_true(all(country_var("de", c("consumption", "investment", "imports", "prices", "long_rate")) %in% ct$equation))
})

test_that("sign_checks flags a violation without correcting it", {
  ok <- data.frame(
    equation = c("de_consumption", "de_imports", "de_long_rate"),
    term = c("de_gdp", "de_domestic_demand", "ea_policy_rate"),
    estimate = c(0.5, 0.2, 0.1), ci_low = c(0.3, 0.1, 0.0), ci_high = c(0.7, 0.3, 0.2),
    stringsAsFactors = FALSE
  )
  out <- sign_checks(ok, "de")
  expect_true(all(out$ok))

  bad <- ok
  bad$estimate <- c(1.5, -0.2, -0.1) # MPC > 1, negative import elasticity, negative rate loading
  out_bad <- sign_checks(bad, "de")
  expect_false(any(out_bad$ok))
  # the violating estimate is reported as-is, not clamped or corrected
  expect_equal(out_bad$estimate[out_bad$check == "mpc_in_0_1"], 1.5)
})

test_that("sign_checks reports NA (not an error) when a term is absent", {
  empty <- data.frame(equation = character(0), term = character(0), estimate = numeric(0),
                      ci_low = numeric(0), ci_high = numeric(0), stringsAsFactors = FALSE)
  out <- sign_checks(empty, "de")
  expect_true(all(is.na(out$estimate)))
  expect_false(any(out$ok))
})

test_that("sign_checks uses us_policy_rate for the US and ea_policy_rate elsewhere", {
  ok <- data.frame(
    equation = c("us_long_rate"), term = c("us_policy_rate"),
    estimate = 0.1, ci_low = 0, ci_high = 0.2, stringsAsFactors = FALSE
  )
  out <- sign_checks(ok, "us")
  expect_equal(out$term[out$check == "long_rate_loads_on_policy_rate"], "us_policy_rate")

  out_de <- sign_checks(ok, "de")
  expect_equal(out_de$term[out_de$check == "long_rate_loads_on_policy_rate"], "ea_policy_rate")
})

test_that("rmse_in_sample returns a non-negative RMSE per stochastic equation", {
  skip_on_cran()
  fx <- diagnostics_synthetic_fit()
  out <- rmse_in_sample(fx$fit)

  expect_named(out, c("equation", "rmse", "nobs"))
  expect_setequal(out$equation, names(fx$fit$estimates))
  expect_true(all(out$rmse >= 0))
  expect_true(all(out$nobs > 0))
})

test_that("check_running_mean_stability finds no flags on a converged synthetic fit", {
  skip_on_cran()
  fx <- diagnostics_synthetic_fit(ndraws = 500)
  out <- check_running_mean_stability(fx$fit)

  expect_named(out, c("equation", "param", "coef", "drift", "mcse", "flagged"))
  expect_true(mean(out$flagged) < 0.2) # a handful of false positives at z_crit=2 is expected, not zero
})

test_that("shock_exogenous_level leaves history untouched and rescales only the forecast window", {
  skip_on_cran()
  fx <- diagnostics_synthetic_fit()
  shocked <- shock_exogenous_level(fx$fit, fx$panel, "oil_price", fx$dates, function(x) x * 1.5)

  hist_before <- stats::window(fx$fit$ts_data$oil_price, end = fx$dates$estimation$end)
  hist_after <- stats::window(shocked$ts_data$oil_price, end = fx$dates$estimation$end)
  expect_equal(as.numeric(hist_after), as.numeric(hist_before))

  fc_before <- stats::window(fx$fit$ts_data$oil_price, start = fx$dates$forecast$start, end = fx$dates$forecast$end)
  fc_after <- stats::window(shocked$ts_data$oil_price, start = fx$dates$forecast$start, end = fx$dates$forecast$end)
  expect_false(isTRUE(all.equal(as.numeric(fc_before), as.numeric(fc_after))))
})

test_that("conditional_forecast_check reports a positive prices response to the oil scenario", {
  skip_on_cran()
  fx <- diagnostics_synthetic_fit()
  out <- conditional_forecast_check(fx$fit, "de", fx$panel, scenario = "oil")

  expect_named(out, c(
    "country", "scenario", "description", "target", "horizon",
    "baseline", "scenario_value", "diff", "expected_sign", "sign_ok"
  ))
  expect_equal(unique(out$target), "de_prices")
  expect_equal(unique(out$expected_sign), "positive")
})

test_that("conditional_forecast_check finds zero GDP effect from the policy scenario -- no channel exists", {
  skip_on_cran()
  # de_long_rate is a dead end in the stage-1 template: nothing downstream
  # depends on it, so shocking ea_policy_rate cannot move de_gdp. This is
  # a structural property of the template, not noise -- assert it exactly.
  fx <- diagnostics_synthetic_fit()
  out <- conditional_forecast_check(fx$fit, "de", fx$panel, scenario = "policy")

  expect_true(all(out$diff == 0))
  expect_false(any(out$sign_ok))
})

test_that("conditional_forecast_check uses restrictions for the US's endogenous policy rate", {
  skip_on_cran()
  fx <- diagnostics_synthetic_fit(iso2 = "us")
  expect_true("us_policy_rate" %in% fx$fit$sys_eq$endogenous_variables)

  out <- conditional_forecast_check(fx$fit, "us", fx$panel, scenario = "policy")
  expect_equal(unique(out$target), "us_gdp")
})

test_that("forecast() is not deterministic by default, which is exactly why the sign check uses approximate = TRUE", {
  skip_on_cran()
  fx <- diagnostics_synthetic_fit()
  f1 <- koma::forecast(fx$fit, dates = fx$dates)
  f2 <- koma::forecast(fx$fit, dates = fx$dates)
  expect_false(isTRUE(all.equal(as.numeric(f1$mean$de_gdp), as.numeric(f2$mean$de_gdp))))

  a1 <- koma::forecast(fx$fit, dates = fx$dates, options = list(approximate = TRUE))
  a2 <- koma::forecast(fx$fit, dates = fx$dates, options = list(approximate = TRUE))
  expect_equal(as.numeric(a1$mean$de_gdp), as.numeric(a2$mean$de_gdp))
})

test_that("tune_tau converges or reports it did not, within max_iter", {
  skip_on_cran()
  panel <- diagnostics_synthetic_panel("de")
  dates <- list(
    estimation = list(start = c(2000, 1), end = c(2015, 4)),
    forecast = list(start = c(2018, 1), end = c(2018, 4))
  )
  res <- tune_tau("de", panel, dates, max_iter = 2, options = list(gibbs = list(ndraws = 200)))

  expect_s3_class(res$fit, "koma_estimate")
  expect_true(is.logical(res$converged))
  expect_true(max(res$history$iteration) <= 2)
  # every equation's final-iteration tau is recorded, defaulting to 1.1
  final <- res$history[res$history$iteration == max(res$history$iteration), ]
  expect_true(all(final$tau > 0))
})

test_that("tune_tau_all runs every requested country", {
  skip_on_cran()
  panel <- c(diagnostics_synthetic_panel("de"), diagnostics_synthetic_panel("fr")[
    setdiff(names(diagnostics_synthetic_panel("fr")), names(diagnostics_synthetic_panel("de")))
  ])
  dates <- list(
    estimation = list(start = c(2000, 1), end = c(2015, 4)),
    forecast = list(start = c(2018, 1), end = c(2018, 4))
  )
  res <- tune_tau_all(c("de", "fr"), panel, dates, max_iter = 1,
                      options = list(gibbs = list(ndraws = 150)), parallel = FALSE)
  expect_named(res, c("de", "fr"))
  expect_true(all(vapply(res, function(r) inherits(r$fit, "koma_estimate"), logical(1))))
})

test_that("pseudo_oos_rmse returns one row per origin and honours the horizon", {
  skip_on_cran()
  panel <- diagnostics_synthetic_panel("de", n = 100)
  out <- suppressWarnings(pseudo_oos_rmse(
    "de", panel, eval_start = c(2018, 1), eval_end = c(2019, 4), horizon = 2,
    options = list(gibbs = list(ndraws = 150))
  ))
  expect_gt(nrow(out), 0)
  expect_true(all(names(diagnostics_synthetic_fit()$fit$estimates) %in% names(out) |
                 grepl("^de_", names(out))))
  expect_equal(attr(out, "iso2"), "de")
})

test_that("diagnostics_grid skips a concept with no stochastic equation, with a warning", {
  skip_on_cran()
  fx <- diagnostics_synthetic_fit()
  # "unemployment" is not a stage-1 stochastic equation at all (unlike
  # "prices", which is always present -- just sometimes without a
  # Metropolis step -- and so is never something diagnostics_grid skips).
  expect_warning(
    plots <- diagnostics_grid(fx$fit, "de", c("consumption", "unemployment"), kind = "trace"),
    "unemployment"
  )
  expect_named(plots, "de_consumption")
  expect_s3_class(plots$de_consumption, "ggplot")
})

test_that("save_stage1_plots writes one PNG per equation and kind, per country", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  fx <- diagnostics_synthetic_fit()
  out_dir <- withr::local_tempdir()

  written <- save_stage1_plots(list(de = fx$fit), out_dir = out_dir, kind = "trace")
  expect_true(length(written) > 0)
  expect_true(all(file.exists(written)))
  expect_true(all(grepl("^trace_de_", basename(written))))
})

# --- contemporaneous reachability --------------------------------------

test_that("stage 2b's policy rate reaches GDP but no price variable", {
  sys_eq <- reachability_sys_eq()
  reached <- contemporaneous_reachability(sys_eq, "ea_policy_rate")

  # The stage-1/2a finding, reproduced structurally: the rate moves the real
  # side through long_rate -> investment, ...
  expect_true("de_gdp" %in% reached)
  expect_true("fr_gdp" %in% reached)
  # ... and stops there. The price equation has no contemporaneous endogenous
  # regressor at all, so the Taylor rule responds to an inflation rate nothing
  # it does can influence.
  expect_length(grep("prices$", reached), 0)
  # The rule's OUTPUT term is nonetheless closed -- the rate returns to itself
  # through ea_gdp. It is specifically the inflation term that is an open loop,
  # which is why "the policy rate is endogenous" is not the same claim.
  expect_true("ea_gdp" %in% reached)
  expect_true("ea_policy_rate" %in% reached)
})

test_that("stage 2c's refinements close the monetary loop", {
  cc <- c("de", "fr")
  sys_eq <- reachability_sys_eq(stage2_options(
    phillips_countries = cc, consumption_rate_countries = cc,
    import_content_countries = cc, spread_countries = cc
  ))
  reached <- contemporaneous_reachability(sys_eq, "ea_policy_rate")

  expect_true(all(c("de_prices", "fr_prices", "ea_prices") %in% reached))
  # A cycle brings the policy rate back to itself -- that IS the closed loop.
  expect_true("ea_policy_rate" %in% reached)
})

test_that("contemporaneous_reachability rejects a variable outside the system", {
  expect_error(
    contemporaneous_reachability(reachability_sys_eq(), "de_wages"),
    "not endogenous"
  )
})

# --- the monetary loop path --------------------------------------------

test_that("monetary_loop names all six links and refuses the US", {
  path <- monetary_loop("de", consumption_to_gdp = 0.54, prices_to_ea_prices = 0.3)

  expect_length(path, 6)
  expect_equal(path[[1]], 1)
  expect_equal(path[["consumption <- long_rate"]],
               list(equation = "de_consumption", term = "de_long_rate"))
  expect_equal(path[["prices <- gdp (Phillips curve)"]],
               list(equation = "de_prices", term = "de_gdp"))
  expect_equal(path[["ea_prices <- prices (identity)"]], 0.3)
  expect_equal(path[["ea_policy_rate <- ea_prices (Taylor rule)"]],
               list(equation = "ea_policy_rate", term = "ea_prices"))

  expect_error(monetary_loop("us", 0.5, 0.2), "us_policy_rate")
})

# --- stage-2c sign rules are per-refinement ----------------------------

test_that("a dropped stage-2c refinement contributes no rule rather than a failure", {
  # The recommended stage-2c configuration drops refinement 5 (import content).
  # With `stage2c = TRUE` its rule is still applied, finds no `de_exports` term
  # in the imports equation, and scores NA/FALSE -- eleven deliberate absences
  # read as eleven failures across the system.
  coefs <- data.frame(
    equation = c("de_consumption", "de_imports", "de_prices", "de_spread"),
    term = c("de_gdp", "de_domestic_demand", "de_gdp", "de_prices"),
    estimate = c(0.5, 0.4, 0.1, 0.2), stringsAsFactors = FALSE
  )

  all_five <- sign_checks(coefs, "de", stage2c = TRUE)
  expect_true("import_content_positive" %in% all_five$check)
  expect_true(is.na(all_five$estimate[all_five$check == "import_content_positive"]))
  expect_false(all_five$ok[all_five$check == "import_content_positive"])

  dropped <- sign_checks(coefs, "de",
                         stage2c = c("phillips", "consumption_rate", "spread"))
  expect_false("import_content_positive" %in% dropped$check)
  expect_equal(nrow(dropped), nrow(all_five) - 1)
  # The rules that remain are unchanged, and the spread refinement still
  # suppresses the imposed long-rate pass-through check.
  expect_false("long_rate_loads_on_policy_rate" %in% dropped$check)
  expect_true(all(c("phillips_curve_positive", "consumption_rate_channel_negative",
                    "spread_loads_on_prices_positive") %in% dropped$check))
})

test_that("without the spread refinement the long-rate pass-through check stays", {
  coefs <- data.frame(equation = "de_long_rate", term = "ea_policy_rate",
                      estimate = 0.05, stringsAsFactors = FALSE)
  res <- sign_checks(coefs, "de", stage2c = c("phillips"))
  expect_true("long_rate_loads_on_policy_rate" %in% res$check)
  expect_true(res$ok[res$check == "long_rate_loads_on_policy_rate"])
})

test_that("sign_checks rejects an unknown stage-2c refinement", {
  coefs <- data.frame(equation = "de_prices", term = "de_gdp", estimate = 0.1,
                      stringsAsFactors = FALSE)
  expect_error(sign_checks(coefs, "de", stage2c = "phillips_curve"), "Unknown")
})
