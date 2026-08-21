#!/usr/bin/env Rscript
# End-to-end reproduction of vignette("small_open_economy")
# ("Estimating Small Macro Model for Switzerland", vignettes/koma-small-macro-model.Rmd).
#
# Run with:  scratch/Rrun scratch/vignette_repro.R
#
# Set KOMA_PARALLEL=1 to run estimation/forecast under future::multicore.
# Recorded results live in docs/koma-api.md; see the "Runtime" section there.

suppressPackageStartupMessages(library(koma))

parallel_mode <- nzchar(Sys.getenv("KOMA_PARALLEL"))
if (parallel_mode) {
  workers <- parallelly::availableCores(omit = 1)
  future::plan("future::multicore", workers = workers)
  cat("## future::multicore with", workers, "workers\n")
} else {
  cat("## sequential\n")
}
cat(R.version.string, "| koma", as.character(packageVersion("koma")),
    "| cores", parallel::detectCores(), "\n\n")

set.seed(11)
timings <- list()
stamp <- function(label, expr) {
  t <- system.time(val <- force(expr))
  timings[[label]] <<- t
  cat("\n>>> ", label, ": ", sprintf("%.1f s elapsed", t[["elapsed"]]), "\n\n", sep = "")
  val
}

# --------------------------------------------------------- 1. the system ----
equations <- "consumption ~ gdp + consumption.L(1),
investment ~ investment.L(1),
exports ~ world_gdp + exports.L(1),
imports ~ domestic_demand + imports.L(1),
prices ~ exchange_rate + oil_price + prices.L(1),
interest_rate ~ prices + interest_rate_germany + prices.L(1),
gdp == 0.6*consumption + 0.6*domestic_demand + 0.5*exports - 0.4*imports,
domestic_demand == 0.6*consumption + 0.4*investment"

exogenous_variables <- c("world_gdp", "interest_rate_germany",
                         "exchange_rate", "oil_price")

sys_eq <- system_of_equations(equations, exogenous_variables)
print(sys_eq)

dates <- list(
  estimation = list(start = c(1996, 1), end = c(2019, 4)),
  forecast   = list(start = c(2023, 1), end = c(2023, 4))
)

# ------------------------------------------------------------- 2. data ------
data("small_open_economy")
series <- names(small_open_economy)
series <- series[!series %in% c("interest_rate", "interest_rate_germany")]

ts_data <- lapply(series, function(x) {
  as_ets(small_open_economy[[x]], series_type = "level", method = "diff_log")
})
names(ts_data) <- series
ts_data$interest_rate <- as_ets(
  small_open_economy$interest_rate, series_type = "rate", method = "none")
ts_data$interest_rate_germany <- as_ets(
  small_open_economy$interest_rate_germany, series_type = "rate", method = "none")

cat("\n## data span\n")
print(vapply(small_open_economy, function(x) {
  paste(paste(stats::start(x), collapse = "Q"), "-",
        paste(stats::end(x), collapse = "Q"), "| freq", stats::frequency(x))
}, character(1)))

# endogenous series must stop at the last observed quarter of the vignette
ts_data[sys_eq$endogenous_variables] <-
  lapply(sys_eq$endogenous_variables, function(x) {
    stats::window(ts_data[[x]], end = c(2019, 4))
  })

# --------------------------------------------------------- 3. estimate ------
estimates <- stamp("estimate()", estimate(
  ts_data = ts_data, sys_eq = sys_eq, dates = dates))

print(estimates)
print(summary(estimates))
print(summary(estimates, variables = "investment"))

# ------------------------------------------- 4. MCMC acceptance rates -------
cat("\n## MCMC acceptance rates (per stochastic equation)\n")
acc <- vapply(estimates$estimates,
              function(z) mean(z$count_accepted, na.rm = TRUE), numeric(1))
n_gamma <- vapply(sys_eq$stochastic_equations, function(v) {
  sum(grepl("gamma", sys_eq$character_gamma_matrix[, v]))
}, numeric(1))
print(data.frame(
  equation             = names(acc),
  contemp_endog_regr   = n_gamma[names(acc)],
  acceptance_rate      = ifelse(is.nan(acc), NA, sprintf("%.1f%%", acc * 100)),
  row.names            = NULL
))
cat("(NA = equation has no contemporaneous endogenous regressor, so no",
    "Metropolis step exists; see R/mh_within_gibbs_algorithm.R:107)\n")

cat("\n## gibbs settings actually used\n")
str(estimates$gibbs_specifications[[1]])

# --------------------------------------------------- 5. unconditional fc ----
forecasts <- stamp("forecast() unconditional", forecast(estimates, dates = dates))
print(forecasts)
cat("\n## gdp forecast, growth rate then level\n")
print(rate(forecasts$mean$gdp))
print(level(forecasts$mean$gdp))
print(summary(forecasts, variables = "gdp"))

# ----------------------------------------------------- 6. conditional fc ----
cat("\n## single hard restriction: prices = 0.5 at horizon 1\n")
fc_cond <- stamp("forecast() 1 restriction", forecast(
  estimates, dates = dates,
  restrictions = list(prices = list(value = 0.5, horizon = 1))))
cat("prices, unconditional vs conditional:\n")
print(rbind(unconditional = as.numeric(forecasts$mean$prices),
            conditional   = as.numeric(fc_cond$mean$prices)))

cat("\n## multiple variables x multiple horizons at once\n")
fc_multi <- stamp("forecast() 6 restrictions", forecast(
  estimates, dates = dates,
  restrictions = list(
    prices        = list(horizon = 1:4, value = c(0.5, 0.4, 0.3, 0.2)),
    interest_rate = list(horizon = c(1, 4), value = c(1.0, 1.5))
  )))
cat("prices:\n");        print(as.numeric(fc_multi$mean$prices))
cat("interest_rate:\n"); print(as.numeric(fc_multi$mean$interest_rate))
cat("(restrictions are exact equality constraints: check the values above",
    "reproduce the requested path)\n")

cat("\n## eigen vs projection conditional-innovation method\n")
fc_eigen <- stamp("forecast() eigen method", forecast(
  estimates, dates = dates,
  restrictions = list(prices = list(horizon = 1:2, value = c(0.5, 0.4))),
  options = list(conditional_innov_method = "eigen")))
print(as.numeric(fc_eigen$mean$prices))

# ------------------------------------------------------------ 7. summary ----
cat("\n\n## TIMINGS (elapsed seconds)\n")
print(data.frame(
  step    = names(timings),
  elapsed = sprintf("%.1f", vapply(timings, function(t) t[["elapsed"]], numeric(1))),
  row.names = NULL
))
cat("\nDone.\n")
