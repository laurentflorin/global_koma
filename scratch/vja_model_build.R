# Estimate and forecast the VJA model (scratch/vja_model_equations.R).
#
# Run from the project root, with BLAS pinned to one thread (CLAUDE.md):
#
#   OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 Rscript scratch/vja_model_build.R
#
# With VJA_PREFLIGHT_ONLY=1 in the environment it stops after step 4, which
# takes seconds rather than the estimation's minutes.
#
# Steps, each reported as it runs:
# 1. Build the panel (scratch/vja_model_panel.R) and parse the equations.
# 2. Evaluate the identity weights the way koma does (the 2025 ratios their
#    expressions name) and check that the share-type ones sum to one.
# 3. Build the identity series that have no observed counterpart, from those
#    weights, so those identities hold exactly.
# 4. Pre-flight: everything koma assumes and does not check, plus k < T.
# 5. Estimate, kept in data/outputs/fit.rds. That fit is reused only if
#    the equations, exogenous variables and dates are unchanged; delete it
#    after changing the data.
# 6. Acceptance rates, own-lag stability, expected signs, and whether each
#    short rate reaches prices and GDP.
# 7. An eight-quarter forecast from 2026Q3, with the share of explosive draws.
#
# Results go to data/outputs/: fit.rds, panel.rds, checks.rds, forecast.rds.

suppressMessages(devtools::load_all(".", quiet = TRUE))
source(file.path("scratch", "vja_model_panel.R"))
source(file.path("scratch", "vja_model_equations.R"))

out_dir <- file.path("data", "outputs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
workers <- 4

# --- 1. panel and equations -------------------------------------------------
raw <- vja_read_raw()
weight_year <- floor(vja_dates$dynamic_weights$end[1]) - 1
panel <- vja_build_panel(raw, suppressMessages(vja_eurostat_na()), weight_year = weight_year)
cli::cli_inform(c("Panel: {length(panel)} series.", stats::setNames(attr(panel, "notes"), rep("i", length(attr(panel, "notes"))))))
sys_eq <- koma::system_of_equations(vja_equations, exogenous_variables = vja_exogenous)
cli::cli_inform("Equations: {length(sys_eq$stochastic_equations)} stochastic, {length(sys_eq$identities)} identities, {length(sys_eq$weight_variables)} series named in weights.")

# --- 2 and 3. identity weights and series -------------------------------------
# `sys_eq` keeps the expressions and is what koma estimates; `sys_eq_w` carries
# the numbers they evaluate to, for the checks below that need numbers.
built <- vja_add_identity_series(panel, sys_eq, vja_dates)
panel <- built$panel
sys_eq_w <- built$sys_eq
weights <- do.call(rbind, lapply(names(sys_eq_w$identities), function(nm) {
  id <- sys_eq_w$identities[[nm]]
  data.frame(identity = nm, term = names(id$components), weight = unlist(id$weights, use.names = FALSE),
             expression = unlist(sys_eq$identities[[nm]]$weights, use.names = FALSE))
}))
# Every identity but GDP is a weighted average, so its weights should sum to
# one: exactly where the denominator is the sum of the numerators (domestic
# demand, foreign demand, ea_gdp, ea_prices), up to chain-linking for the
# trade totals. GDP's fall short by inventories and the discrepancy.
sums <- tapply(weights$weight, weights$identity, sum)
average <- !grepl("_gdp$", names(sums)) | names(sums) == "ea_gdp"
off <- sums[average & abs(sums - 1) > 0.02]
if (length(off) > 0) {
  cli::cli_warn(c("Identity weights that should sum to one do not:", stats::setNames(sprintf("%s: %.3f", names(off), off), rep("x", length(off)))))
}
cli::cli_inform(c(
  "Identity weights evaluated on {weight_year} data: {nrow(weights)} terms.",
  "i" = "Weighted averages sum to {sprintf('%.3f', min(sums[average]))}-{sprintf('%.3f', max(sums[average]))}; GDP weights to {sprintf('%.3f', min(sums[!average]))}-{sprintf('%.3f', max(sums[!average]))}."
))

# --- 4. pre-flight --------------------------------------------------------------
# GDP and the trade totals are published series and hold only up to
# inventories, the discrepancy and chain-linking; everything else was built
# from the identity's own weights and must hold exactly.
observed_identities <- "^(de|fr|it|reu|us|gb|jp)_(gdp|exports|imports)$"
preflight <- preflight_system(sys_eq_w, panel, dates = vja_dates, inexact = observed_identities)
consistency <- identity_consistency(sys_eq_w, panel, inexact = observed_identities)
print(preflight, right = FALSE, row.names = FALSE)
if (!all(preflight$ok %in% TRUE)) {
  cli::cli_abort("Pre-flight failed; see the table above.")
}
observed_gap <- consistency[!consistency$exact_expected, ]
observed_gap <- observed_gap[order(-observed_gap$max_abs_error), ]
cli::cli_inform("Observed identities, worst quarterly growth gap: {paste(sprintf('%s %.1f', observed_gap$identity, observed_gap$max_abs_error), collapse = ', ')}pp.")
if (nzchar(Sys.getenv("VJA_PREFLIGHT_ONLY"))) {
  cli::cli_inform("VJA_PREFLIGHT_ONLY is set: stopping before estimation.")
  quit(save = "no")
}

# --- 5. estimate -------------------------------------------------------------------
fit_path <- file.path(out_dir, "fit.rds")
spec_now <- list(equations = vja_equations, exogenous = vja_exogenous, dates = vja_dates)
fit <- if (file.exists(fit_path)) readRDS(fit_path) else NULL
if (is.null(fit) || !identical(attr(fit, "vja_spec"), spec_now)) {
  fit <- fit_system(sys_eq, panel, vja_dates, workers = workers)
  attr(fit, "vja_spec") <- spec_now
  saveRDS(fit, fit_path)
  cli::cli_inform("Estimated in {round(attr(fit, 'runtime_s') / 60, 1)} min; saved {.file {fit_path}}.")
} else {
  cli::cli_inform("cached: {.file {fit_path}}")
}
# koma evaluated the weights itself; they must be the ones the identity series
# were built from.
fit_weights <- unlist(lapply(fit$sys_eq$identities, `[[`, "weights"))
koma_gap <- max(abs(as.numeric(fit_weights) - unlist(lapply(sys_eq_w$identities, `[[`, "weights"))))
if (!is.finite(koma_gap) || koma_gap > 1e-12) {
  cli::cli_abort("koma's estimated weights differ from the ones the identity series were built from (gap {koma_gap}).")
}
saveRDS(panel, file.path(out_dir, "panel.rds"))

# --- 6. checks ---------------------------------------------------------------------
acceptance <- check_acceptance_rates(fit)
flagged <- acceptance[acceptance$flagged %in% TRUE, ]
if (nrow(flagged) > 0) {
  # The doubling rule tune_tau_system() uses: raise tau above the band, lower
  # it below. Written into the equations file by hand, as `[tau = x]`.
  current <- vapply(flagged$equation, function(eq) {
    setting <- sys_eq$equation_settings[[eq]]$tau
    if (is.null(setting)) 1.1 else setting
  }, numeric(1))
  suggested <- ifelse(flagged$acceptance_rate > 0.6, current * 2, current / 2)
  cli::cli_warn(c(
    "{nrow(flagged)} equation{?s} outside the 20-60% acceptance band. Suggested settings:",
    stats::setNames(sprintf("%s: %.0f%% -> [tau = %.3g]", flagged$equation, 100 * flagged$acceptance_rate, suggested), rep("x", nrow(flagged)))
  ))
}
coefs <- coefficient_table(fit)
lags <- check_lag_stability(coefs)

# Expected signs, checked on the posterior mean. `strong` additionally asks
# the 90% interval to exclude zero.
sign_rule <- function(equation, term, sign) data.frame(equation = equation, term = term, sign = sign)
neer_of <- function(cc) if (cc %in% vja_ea_members) "ea_neer" else paste0(cc, "_neer")
rules <- do.call(rbind, c(
  lapply(vja_full, function(cc) rbind(
    sign_rule(paste0(cc, "_consumption"), paste0(cc, "_long_rate"), -1),
    sign_rule(paste0(cc, "_investment"), paste0(cc, "_long_rate"), -1),
    sign_rule(paste0(cc, "_goods_exports"), paste0(cc, "_foreign_demand"), 1),
    sign_rule(paste0(cc, "_goods_exports"), neer_of(cc), -1),
    sign_rule(paste0(cc, "_services_exports"), paste0(cc, "_foreign_demand"), 1),
    sign_rule(paste0(cc, "_goods_imports"), paste0(cc, "_domestic_demand"), 1),
    sign_rule(paste0(cc, "_services_imports"), paste0(cc, "_domestic_demand"), 1)
  )),
  lapply(setdiff(vja_full, "jp"), function(cc) sign_rule(paste0(cc, "_prices"), neer_of(cc), -1)),
  lapply(c("ea", "us", "gb"), function(cc) rbind(
    sign_rule(paste0(cc, "_short_rate"), paste0(cc, "_prices"), 1),
    sign_rule(paste0(cc, "_short_rate"), paste0(cc, "_gdp"), 1)
  )),
  list(sign_rule("jp_short_rate", "jp_gdp", 1)),
  lapply(vja_full, function(cc) {
    short <- if (cc %in% vja_ea_members) "ea_short_rate" else paste0(cc, "_short_rate")
    sign_rule(paste0(cc, "_long_rate"), short, 1)
  }),
  lapply(c("ea", "us", "gb", "jp"), function(cc) sign_rule(paste0(cc, "_neer"), paste0(cc, "_short_rate"), 1)),
  lapply(c("cn", "in"), function(cc) rbind(
    sign_rule(paste0(cc, "_gdp"), paste0(cc, "_exports"), 1),
    sign_rule(paste0(cc, "_exports"), paste0(cc, "_foreign_demand"), 1),
    sign_rule(paste0(cc, "_imports"), paste0(cc, "_gdp"), 1)
  ))
))
signs <- merge(rules, coefs, by = c("equation", "term"), all.x = TRUE, sort = FALSE)
signs$ok <- sign(signs$estimate) == signs$sign
signs$strong <- signs$ok & sign(signs$ci_low) == sign(signs$ci_high)
cli::cli_inform("Expected signs: {sum(signs$ok, na.rm = TRUE)} of {nrow(signs)} right, {sum(signs$strong, na.rm = TRUE)} with a 90% interval excluding zero.")
wrong <- signs[!signs$ok %in% TRUE, ]
if (nrow(wrong) > 0) {
  print(wrong[, c("equation", "term", "sign", "estimate", "ci_low", "ci_high")], row.names = FALSE, digits = 3)
}
cli::cli_inform("Largest own lag: {lags$equation[1]} {sprintf('%.3f', lags$estimate[1])}; {sum(lags$flagged)} at or above 1.")

# Does each short rate reach prices and GDP within the quarter?
reach <- do.call(rbind, lapply(c("ea_short_rate", "us_short_rate", "gb_short_rate", "jp_short_rate"), function(r) {
  hit <- contemporaneous_reachability(sys_eq, r)
  data.frame(from = r, reached = length(hit), prices = sum(grepl("_prices$", hit)),
             gdp = sum(grepl("_gdp$", hit)))
}))
print(reach, row.names = FALSE)

saveRDS(list(preflight = preflight, consistency = consistency, weights = weights,
             acceptance = acceptance, coefficients = coefs, lags = lags,
             signs = signs, reach = reach),
        file.path(out_dir, "checks.rds"))

# --- 7. forecast ---------------------------------------------------------------------
fc <- forecast_system(fit, panel, horizon = 8)
saveRDS(fc, file.path(out_dir, "forecast.rds"))
explosive <- forecast_explosive_table(fc$paths)
cli::cli_inform("Explosive draws at horizon 8: {sprintf('%.2f', mean(explosive$h_last))} over all variables, {sprintf('%.2f', mean(explosive$h_last[grepl('_gdp$', explosive$variable)]))} for GDP.")
gdp <- fc$paths[fc$paths$kind == "forecast" & fc$paths$variable %in% paste0(vja_entities, "_gdp"), ]
print(stats::reshape(gdp[, c("variable", "time", "rate_median")], idvar = "time", timevar = "variable", direction = "wide"),
      row.names = FALSE, digits = 2)
