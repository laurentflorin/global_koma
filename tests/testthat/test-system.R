test_that("preflight_system passes a well-formed system and catches a missing series", {
  s <- synthetic_system()
  ok <- preflight_system(s$sys_eq, s$panel, seeds = 1:3, dates = s$dates)
  expect_true(all(ok$ok))
  expect_true("identities declared last" %in% ok$check)

  # without `dates` the k < T row cannot be evaluated and reports NA rather
  # than guessing -- callers must treat that as unchecked, not as a pass
  unchecked <- preflight_system(s$sys_eq, s$panel, seeds = 1:2)
  k_row <- unchecked$check == "k < T (x'x invertible, Wishart df > 0)"
  expect_true(is.na(unchecked$ok[k_row]))
  expect_true(all(unchecked$ok[!k_row]))

  short <- s$panel
  short$de_foreign_demand <- NULL
  bad <- preflight_system(s$sys_eq, short, seeds = 1:3)
  expect_false(bad$ok[bad$check == "every variable has a panel series"])
  expect_match(bad$detail[bad$check == "every variable has a panel series"], "de_foreign_demand")
  # it reports every problem rather than aborting at the first
  expect_s3_class(bad, "data.frame")
})

test_that("preflight_system rejects a non-koma_seq", {
  expect_error(preflight_system(list(), list()), "koma_seq")
})

test_that("preflight_system fails loudly when k >= T", {
  s <- synthetic_system()
  k_row <- "k < T (x'x invertible, Wishart df > 0)"
  roomy <- preflight_system(s$sys_eq, s$panel, seeds = 1:2, dates = s$dates)
  expect_true(roomy$ok[roomy$check == k_row])

  # a window too short for k: koma would otherwise fail only deep inside
  # estimate(), with "computationally singular" and a negative Wishart df
  cramped <- preflight_system(s$sys_eq, s$panel, seeds = 1:2,
                              dates = list(estimation = list(start = c(2000, 1), end = c(2002, 4))))
  expect_false(cramped$ok[cramped$check == k_row])
})

test_that("preflight_system catches a lagged-name prefix collision", {
  # koma's construct_phi() prefix-matches lag columns, so `de_debt` would match
  # both de_debt.L(1) and de_debt_ratio.L(1) and silently mis-map the companion
  # matrix. Nothing downstream errors, so the preflight has to catch it.
  panel <- synthetic_panel()
  mk <- function(v) koma::as_ets(stats::ts(v, start = c(2000, 1), frequency = 4),
                                 series_type = "level", method = "diff_log")
  n <- length(panel$de_gdp)
  panel$de_debt <- mk(100 + cumsum(stats::rnorm(n, 0.4, 0.2)))
  panel$de_debt_ratio <- mk(60 + cumsum(stats::rnorm(n, 0.1, 0.2)))

  colliding <- koma::system_of_equations(
    equations = c("de_debt ~ de_gdp + de_debt.L(1)",
                  "de_debt_ratio ~ de_gdp + de_debt_ratio.L(1)",
                  "de_gdp ~ row_imports + de_gdp.L(1)"),
    exogenous_variables = "row_imports"
  )
  out <- preflight_system(colliding, panel, seeds = 1, dates = synthetic_dates())
  row <- out[out$check == "no lagged endogenous name prefixes another", ]
  expect_false(row$ok)
  expect_match(row$detail, "de_debt")
})

test_that("estimation_length matches what koma actually reports", {
  panel <- synthetic_panel()
  # 2000Q1-2019Q4 is 80 quarters; koma reports T = 78 after the diff_log
  # transform and the L(1) lag each cost one period.
  expect_equal(
    estimation_length(panel, list(estimation = list(start = c(2000, 1), end = c(2019, 4)))),
    78
  )
  expect_equal(
    estimation_length(panel, list(estimation = list(start = c(2000, 1), end = c(2015, 4)))),
    62
  )
})

test_that("identity_consistency reproduces an exact identity and catches a broken one", {
  s <- synthetic_system()
  res <- identity_consistency(s$sys_eq, s$panel)

  # foreign demand is chained in rate space and must reproduce exactly.
  fd <- res[res$identity == "de_foreign_demand", ]
  expect_equal(fd$space, "rate")
  expect_lt(fd$max_abs_error, 1e-10)
  expect_true(fd$ok)

  # GDP is observed and inexact by construction: reported, never flagged, so
  # a real statistical discrepancy cannot fail the pre-flight.
  gdp <- res[res$identity == "de_gdp", ]
  expect_false(gdp$exact_expected)
  expect_true(gdp$ok)

  # Build foreign demand the wrong way, by adding levels, and confirm it is
  # caught.
  broken <- s$panel
  broken$de_foreign_demand <- koma::as_ets(
    0.6 * s$panel$fr_imports + 0.4 * s$panel$row_imports,
    series_type = "level", method = "diff_log"
  )
  bad <- identity_consistency(s$sys_eq, broken)
  bad_fd <- bad[bad$identity == "de_foreign_demand", ]
  expect_gt(bad_fd$max_abs_error, 1e-6)
  expect_false(bad_fd$ok)

  # and the pre-flight surfaces it rather than passing the system through
  pf <- preflight_system(s$sys_eq, broken, seeds = 1:2, dates = s$dates)
  row <- pf[pf$check == "exact identities hold in the panel", ]
  expect_false(row$ok)
  expect_match(row$detail, "de_foreign_demand")
})

test_that("identity_consistency checks a rate identity as a literal sum of levels", {
  panel <- synthetic_panel()
  spread <- koma::as_ets(panel$de_long_rate - panel$ea_short_rate, series_type = "rate", method = "none")
  panel$de_spread <- spread
  sys_eq <- koma::system_of_equations(c(
    "de_spread ~ constant + de_spread.L(1) + us_short_rate",
    "ea_short_rate ~ constant + ea_short_rate.L(1) + us_short_rate",
    "de_long_rate == 1*de_spread + 1*ea_short_rate"
  ), exogenous_variables = "us_short_rate")

  res <- identity_consistency(sys_eq, panel)
  expect_equal(res$space, "level")
  expect_lt(res$max_abs_error, 1e-12)

  panel$de_spread <- koma::as_ets(spread + 0.1, series_type = "rate", method = "none")
  expect_false(identity_consistency(sys_eq, panel)$ok)
})

test_that("fit_system names a missing identity series rather than failing inside koma", {
  s <- synthetic_system()
  short <- s$panel
  short$de_foreign_demand <- NULL
  expect_error(fit_system(s$sys_eq, short, s$dates), "de_foreign_demand")
})

test_that("fit_system refuses an internal gap and points at fill_internal_gaps()", {
  s <- synthetic_system()
  s$panel$row_imports[30] <- NA
  expect_error(fit_system(s$sys_eq, s$panel, s$dates), "fill_internal_gaps")
})

test_that("fit_system estimates the system end to end", {
  skip_on_cran()
  s <- synthetic_fit(ndraws = 200)

  expect_s3_class(s$fit, "koma_estimate")
  expect_named(s$fit$estimates, s$sys_eq$stochastic_equations)
  expect_length(s$fit$estimates, 13)
  expect_true(is.numeric(attr(s$fit, "runtime_s")))
  # the endogenous series are truncated to the estimation end; the exogenous
  # ones are not
  expect_lte(stats::end(s$fit$ts_data$de_gdp)[1], 2015)
  expect_equal(stats::end(s$fit$ts_data$row_imports), c(2019, 4))

  # equations with no contemporaneous endogenous regressor have no
  # Metropolis step and must not be flagged
  acceptance <- check_acceptance_rates(s$fit)
  expect_false(acceptance$has_mh_step[acceptance$equation == "de_prices"])
  expect_false(acceptance$flagged[acceptance$equation == "de_prices"])
})

test_that("warn_if_blas_threaded stays quiet when BLAS is pinned", {
  withr::local_envvar(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1")
  expect_false(warn_if_blas_threaded(4))
})

test_that("parallel_strategy forks only where forking is safe", {
  expect_true(parallel_strategy() %in% c("future::multicore", "future::multisession"))
  if (.Platform$OS.type == "windows" || Sys.info()[["sysname"]] == "Darwin") {
    expect_equal(parallel_strategy(), "future::multisession")
  }
})
