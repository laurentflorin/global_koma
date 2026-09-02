# Build every artefact `reports/stage2d_regional_core.qmd` reads.
#
# Run from the project root, with BLAS pinned:
#
#   OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 Rscript scratch/stage2d_build.R
#
# Everything lands under data/cache/stage2d/ (git-ignored). The estimation
# steps are the expensive part -- roughly 95s for one fit and four times that
# for the tau tuning loop -- so each is cached and skipped if present. Delete
# the file to force a re-run.

devtools::load_all(".", quiet = TRUE)

out_dir <- file.path("data", "cache", "stage2d")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
save_rds <- function(x, name) saveRDS(x, file.path(out_dir, paste0(name, ".rds")))
cached <- function(name, expr) {
  path <- file.path(out_dir, paste0(name, ".rds"))
  if (file.exists(path)) {
    cli::cli_inform("cached: {name}")
    return(readRDS(path))
  }
  value <- force(expr)
  saveRDS(value, path)
  value
}

# --- inputs ---------------------------------------------------------------
panel0 <- targets::tar_read(panel)
gdp_weights <- targets::tar_read(gdp_weights)

cfg <- stage2d_config(gdp_weights)
dates <- stage2d_dates()
countries <- stage2d_countries()

W <- cached("trade_weights", suppressWarnings(build_stage2d_trade_weights(gdp_weights)))
row_weights <- cached("row_weights", row_gdp_weights(exclude = c("us", "cn")))
row_weights_2b <- cached("row_weights_2b", row_gdp_weights())

# --- panel ----------------------------------------------------------------
panel <- cached("panel_base", {
  p <- add_stage2d_countries(panel0, gdp_weights, dates = dates)
  p[["row_gdp"]] <- align_panel(
    list(row_gdp = build_row_gdp(row_weights)),
    start = stats::start(p[[1]]), end = stats::end(p[[1]]), extend = TRUE
  )[[1]]
  harmonise_panel_attrs(p)
})
save_rds(attr(build_cn_panel(dates = dates), "cn_shares"), "cn_shares")
save_rds(bloc_gdp_weights(gdp_weights, reu_members()), "bloc_weights")

gw <- collapse_gdp_weights(gdp_weights, cfg$blocs)
lw <- stage2_linkage_weights(countries, W, gw, threshold = cfg$threshold,
                             demand_concept = cfg$demand_concept)
save_rds(lw, "linkage_weights")
save_rds(gw, "gdp_weights")

shares <- stage2_shares(countries, panel, dates, cfg$opts)
save_rds(shares, "shares")
spec <- stage2_spec(countries, shares, lw, opts = cfg$opts)
sys_eq <- build_stage2_system(spec)
save_rds(spec, "spec")
save_rds(sys_eq, "sys_eq")

stage_panel <- build_stage2_panel(panel, lw, dummies = cfg$dummies,
                                  spread_countries = cfg$spread_countries,
                                  policy_rate = policy_rate_map(cfg$opts))
save_rds(stage_panel, "panel")

save_rds(stage2_preflight(sys_eq, stage_panel, dates = dates), "preflight")
save_rds(identity_consistency(sys_eq, stage_panel), "identity_check")

k <- length(sys_eq$total_exogenous_variables)
t_obs <- estimation_length(stage_panel, dates)
save_rds(
  data.frame(
    entities = length(countries),
    stochastic = length(sys_eq$stochastic_equations),
    identities = length(sys_eq$identities),
    equations = length(sys_eq$endogenous_variables),
    exogenous = length(sys_eq$exogenous_variables),
    k = k, t_obs = t_obs, df = t_obs - k,
    stringsAsFactors = FALSE
  ),
  "shape"
)

# --- estimation -----------------------------------------------------------
untuned <- cached("core", fit_stage2(sys_eq, stage_panel, dates, workers = 6))
save_rds(check_acceptance_rates(untuned), "acceptance_untuned")

tuned <- cached("tuned", tune_tau_system(spec, stage_panel, dates, workers = 6))
fit <- tuned$fit
save_rds(tuned$history, "tuning_history")
save_rds(tuned$tau, "tau")
save_rds(check_acceptance_rates(fit), "acceptance")

coefs <- coefficient_table(fit)
save_rds(coefs, "coefs")
save_rds(check_lag_stability(coefs), "lag_stability")
save_rds(rmse_in_sample(fit), "insample")

save_rds(
  do.call(rbind, lapply(countries, function(cc) {
    s <- sign_checks(coefs, cc, stage2c = cfg$refinements,
                     merged_demand = cc %in% cfg$merged_demand_countries)
    s$iso2 <- cc
    s
  })),
  "signs"
)

save_rds(
  stats::setNames(
    lapply(c("ea_policy_rate", "us_policy_rate", "cn_policy_rate"),
           function(v) contemporaneous_reachability(sys_eq, v)),
    c("ea_policy_rate", "us_policy_rate", "cn_policy_rate")
  ),
  "reachability"
)

# The implied domestic-demand elasticity of every country that HAS the split,
# which is what the merged China equation has to be judged against.
save_rds(
  do.call(rbind, lapply(setdiff(countries, cfg$merged_demand_countries), function(cc) {
    w <- spec$identities[[country_var(cc, "domestic_demand")]]
    g <- function(eq, term) {
      row <- coefs[coefs$equation == eq & coefs$term == term, ]
      if (nrow(row) == 0) NA_real_ else row$estimate[1]
    }
    consumption <- g(country_var(cc, "consumption"), country_var(cc, "gdp"))
    investment <- g(country_var(cc, "investment"), country_var(cc, "gdp"))
    data.frame(
      iso2 = cc, consumption = consumption, investment = investment,
      implied = unname(w[[country_var(cc, "consumption")]]) * consumption +
        unname(w[[country_var(cc, "investment")]]) * investment,
      stringsAsFactors = FALSE
    )
  })),
  "implied_demand_elasticity"
)

cli::cli_inform("Stage 2d artefacts written to {.file {out_dir}}.")
