#!/usr/bin/env Rscript
# How koma estimation time scales, and how tau tunes the MCMC acceptance rate.
#
# Run with:  scratch/Rrun scratch/koma_scaling.R
# Results are transcribed into docs/koma-api.md.

suppressPackageStartupMessages(library(koma))
set.seed(11)

cat(R.version.string, "| koma", as.character(packageVersion("koma")),
    "| cores", parallel::detectCores(), "\n\n")

quiet <- function(expr) {
  suppressMessages(suppressWarnings(
    utils::capture.output(val <- force(expr), type = "output")))
  val
}
el <- function(expr) system.time(quiet(expr))[["elapsed"]]

# ------------------------------------------------- the Switzerland system ----
soe_equations <- "consumption ~ gdp + consumption.L(1),
investment ~ investment.L(1),
exports ~ world_gdp + exports.L(1),
imports ~ domestic_demand + imports.L(1),
prices ~ exchange_rate + oil_price + prices.L(1),
interest_rate ~ prices + interest_rate_germany + prices.L(1),
gdp == 0.6*consumption + 0.6*domestic_demand + 0.5*exports - 0.4*imports,
domestic_demand == 0.6*consumption + 0.4*investment"
soe_exog <- c("world_gdp", "interest_rate_germany", "exchange_rate", "oil_price")
soe_dates <- list(estimation = list(start = c(1996, 1), end = c(2019, 4)),
                  forecast   = list(start = c(2020, 1), end = c(2020, 4)))

soe_data <- function() {
  data("small_open_economy", envir = environment())
  s <- names(small_open_economy)
  s <- s[!s %in% c("interest_rate", "interest_rate_germany")]
  d <- lapply(s, function(x) as_ets(small_open_economy[[x]],
                                    series_type = "level", method = "diff_log"))
  names(d) <- s
  d$interest_rate <- as_ets(small_open_economy$interest_rate,
                            series_type = "rate", method = "none")
  d$interest_rate_germany <- as_ets(small_open_economy$interest_rate_germany,
                                    series_type = "rate", method = "none")
  d
}
acc_of <- function(fit) {
  a <- vapply(fit$estimates, function(z) mean(z$count_accepted, na.rm = TRUE),
              numeric(1))
  a[!is.nan(a)]
}

# ================================================== A. ndraws scaling ========
cat("== A. ndraws sweep (Switzerland model, 6 stochastic eq, sequential) ==\n")
future::plan("future::sequential")
d <- soe_data()
sq <- system_of_equations(soe_equations, soe_exog)
A <- do.call(rbind, lapply(c(250, 500, 1000, 2000, 4000), function(nd) {
  t <- el(estimate(ts_data = d, sys_eq = sq, dates = soe_dates,
                   options = list(gibbs = list(ndraws = nd))))
  data.frame(ndraws = nd, elapsed_s = round(t, 2), ms_per_draw = round(1000 * t / nd, 3))
}))
print(A, row.names = FALSE)

# ================================= B. equation-count scaling (synthetic) =====
cat("\n== B. equation-count sweep (synthetic recursive systems, ndraws = 1000) ==\n")
cat("   each equation: y_i ~ x + y_i.L(1) + y_{i+1}  (so all but the last have\n",
    "  one contemporaneous endogenous regressor and therefore an MH step)\n", sep = "")

make_system <- function(n, tobs = 200) {
  eqs <- vapply(seq_len(n), function(i) {
    if (i < n) sprintf("y%d ~ x + y%d.L(1) + y%d", i, i, i + 1)
    else       sprintf("y%d ~ x + y%d.L(1)", i, i)
  }, character(1))
  sq <- system_of_equations(paste(eqs, collapse = ","), "x")
  mk <- function() as_ets(stats::ts(stats::rnorm(tobs, 0.5, 1),
                                    start = c(1970, 1), frequency = 4),
                          series_type = "rate", method = "none")
  dat <- replicate(n, mk(), simplify = FALSE)
  names(dat) <- paste0("y", seq_len(n))
  dat$x <- mk()
  list(sys_eq = sq, ts_data = dat,
       dates = list(estimation = list(start = c(1971, 1), end = c(2015, 4)),
                    forecast   = list(start = c(2016, 1), end = c(2016, 4))))
}

B <- do.call(rbind, lapply(c(3, 6, 12, 24), function(n) {
  m <- make_system(n)
  future::plan("future::sequential")
  t_seq <- el(estimate(ts_data = m$ts_data, sys_eq = m$sys_eq, dates = m$dates,
                       options = list(gibbs = list(ndraws = 1000))))
  future::plan("future::multicore", workers = parallelly::availableCores(omit = 1))
  t_par <- el(estimate(ts_data = m$ts_data, sys_eq = m$sys_eq, dates = m$dates,
                       options = list(gibbs = list(ndraws = 1000))))
  future::plan("future::sequential")
  data.frame(n_equations = n,
             sequential_s = round(t_seq, 2),
             per_equation_s = round(t_seq / n, 2),
             multicore_s = round(t_par, 2),
             speedup = round(t_seq / t_par, 2))
}))
print(B, row.names = FALSE)

# =========================================== C. tau tuning on one equation ===
cat("\n== C. tau sweep on the Switzerland `imports` equation ==\n")
cat("   default tau = 1.1 gives imports 61.0%, just above the 20-60% band\n")
C <- do.call(rbind, lapply(c(0.5, 1.1, 2, 4, 8, 16), function(tau) {
  eq <- sub("imports ~ domestic_demand \\+ imports.L\\(1\\)",
            sprintf("imports ~ domestic_demand + imports.L(1) [tau = %g]", tau),
            soe_equations)
  sq2 <- system_of_equations(eq, soe_exog)
  fit <- quiet(estimate(ts_data = d, sys_eq = sq2, dates = soe_dates,
                        options = list(gibbs = list(ndraws = 1000))))
  a <- acc_of(fit)
  data.frame(tau_on_imports = tau,
             imports = sprintf("%.1f%%", a[["imports"]] * 100),
             consumption = sprintf("%.1f%%", a[["consumption"]] * 100),
             interest_rate = sprintf("%.1f%%", a[["interest_rate"]] * 100))
}))
print(C, row.names = FALSE)

cat("\n== C2. global tau sweep (options$gibbs$tau, all equations) ==\n")
C2 <- do.call(rbind, lapply(c(0.5, 1.1, 2, 4, 8, 16), function(tau) {
  fit <- quiet(estimate(ts_data = d, sys_eq = sq, dates = soe_dates,
                        options = list(gibbs = list(ndraws = 1000, tau = tau))))
  a <- acc_of(fit)
  data.frame(tau = tau,
             consumption = sprintf("%.1f%%", a[["consumption"]] * 100),
             imports = sprintf("%.1f%%", a[["imports"]] * 100),
             interest_rate = sprintf("%.1f%%", a[["interest_rate"]] * 100))
}))
print(C2, row.names = FALSE)

cat("\nDone.\n")
