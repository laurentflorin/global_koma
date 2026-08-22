# Multi-country Bayesian macro model pipeline.
#
# Run with `targets::tar_make()`. See CLAUDE.md for the repo map and the
# stage 1 / stage 2 / stage 3 vocabulary this pipeline follows.
#
# Data ingestion (data_fred.R / data_eamdqd.R / panel_build.R /
# weights.R) and stage 1 (stage1_models.R) are implemented. Stage 2,
# stage 3 and scoring are still stubs, so a bare tar_make() errors -- by
# design, not a regression.
#
# NOTE ON WHICH STUB IT STOPS AT: targets is free to schedule any target
# whose dependencies are met, so it does *not* fail in stage order. In
# particular `model_blocks` (stage 3) depends only on the `blocks`
# constant below, so it is dispatched almost immediately and errors
# before stage 1 has even started. To exercise the implemented part of
# the pipeline, ask for it by name:
#
#   targets::tar_make(names = "stage1_diagnostics")
#
# which runs data -> panel -> stage 1 and prints the acceptance-rate
# report (verified: ~1m40s end to end, 11 fits in ~1m20s).

library(targets)
library(tarchetypes)

# Load this project's own R/ functions (not yet an installed package).
devtools::load_all(".", quiet = TRUE)

tar_option_set(
  packages = c("koma", "cli", "rlang"),
  format = "rds"
)

# --- configuration -----------------------------------------------------

# `modelled_countries` (the ten EA countries plus "us") and `ea_countries`
# come from R/panel_build.R.
countries <- modelled_countries
blocks <- list(ea = ea_countries)
shared_concepts <- c("gdp")
truly_exogenous <- c("oil_price")

# Stage 2a is the two-country pilot of the linkage mechanism (see
# R/stage2_system.R). It is deliberately DE + FR only: the point is to prove
# that trade-weighted foreign demand, endogenous ea_policy_rate and the
# cross-country contemporaneous cycle all work inside one
# system_of_equations() before scaling to all eleven.
pilot_countries <- c("de", "fr")

# --- pipeline ------------------------------------------------------------

list(
  # -- raw data --
  tar_target(eamdqd, fetch_eamdqd(vintage = "latest")),
  tar_target(row_weights, row_gdp_weights()),

  # -- aggregation weights (see weights.R); both write reviewable CSVs --
  tar_target(trade_weights, build_trade_weight_matrix(countries)),
  tar_target(gdp_weights, build_gdp_weight_matrix(ea_countries)),

  # -- per-country panels, one target per country (see panel_build.R) --
  tar_map(
    values = list(iso2 = countries),
    names = "iso2",
    tar_target(country_panel, build_country_panel(iso2, eamdqd = eamdqd))
  ),

  # -- full multi-country panel, aligned to its common sample --
  tar_target(
    global_panel,
    build_global_panel(countries, eamdqd = eamdqd, row_weights = row_weights)
  ),
  # NOTE: align to the panel's own widest common window, NOT to the
  # estimation window. Exogenous series must extend past the forecast
  # start for koma to conditionally fill 2020-2022; truncating them to
  # the estimation end here would break that. fit_stage1() truncates the
  # *endogenous* series itself.
  # fill_internal_gaps() resolves holes koma cannot estimate on (it
  # warns, naming each one); ragged edges are left for koma itself.
  tar_target(panel, fill_internal_gaps(align_panel(global_panel))),
  tar_target(estimation_dates, stage1_dates(panel)),

  # -- stage 1: per-country satellite models (see stage1_models.R) --
  tar_target(
    stage1_fits,
    fit_stage1_all(countries, panel, estimation_dates)
  ),
  tar_target(stage1_diagnostics, stage1_summary(stage1_fits)),

  # -- stage 2a: two-country linked pilot (see stage2_system.R) --
  # Trade-weighted foreign demand makes each country's exports depend on the
  # other's endogenous GDP, so the whole thing is one koma system rather
  # than two. Scaling to `countries` is stage 2b.
  tar_target(
    stage2a_linkage_weights,
    stage2_linkage_weights(pilot_countries, trade_weights, gdp_weights)
  ),
  tar_target(
    stage2a_spec,
    stage2_spec(
      pilot_countries,
      stats::setNames(
        lapply(pilot_countries, function(cc) expenditure_shares(panel, cc, estimation_dates)),
        pilot_countries
      ),
      stage2a_linkage_weights
    )
  ),
  tar_target(stage2a_sys_eq, build_stage2_system(stage2a_spec)),
  # koma requires a series for identity-defined variables too, and only says
  # so from inside estimate(); these four have no observed counterpart.
  tar_target(stage2a_panel, build_stage2_panel(panel, stage2a_linkage_weights)),
  tar_target(
    stage2a_preflight,
    stage2_preflight(stage2a_sys_eq, stage2a_panel, dates = estimation_dates)
  ),
  tar_target(
    stage2a_fit,
    fit_stage2(stage2a_sys_eq, stage2a_panel, estimation_dates)
  ),
  tar_target(stage2a_acceptance, check_acceptance_rates(stage2a_fit)),

  # -- stage 2b: the full eleven-country system (see stage2_system.R) --
  # Same mechanism, all eleven economies, 68 stochastic equations and 35
  # identities in one koma system. It needs its OWN dates: the stage-1/2a
  # window leaves only 78 observations against k = 76, and with government
  # retained the system is not estimable at all (koma projects every equation
  # on the full k-column x_matrix each draw, so k >= T is a hard failure).
  # stage2b_config() carries the departures that buy the room back.
  tar_target(stage2b_dates_target, stage2b_dates(panel)),
  tar_target(stage2b_cfg, stage2b_config()),
  tar_target(
    stage2b_linkage_weights,
    stage2_linkage_weights(countries, trade_weights, gdp_weights,
                           threshold = stage2b_cfg$threshold,
                           ireland_proxy = stage2b_cfg$ireland_proxy)
  ),
  tar_target(
    stage2b_spec,
    stage2_spec(
      countries,
      stats::setNames(
        lapply(countries, function(cc) expenditure_shares(panel, cc, stage2b_dates_target)),
        countries
      ),
      stage2b_linkage_weights,
      opts = stage2b_cfg$opts
    )
  ),
  tar_target(stage2b_sys_eq, build_stage2_system(stage2b_spec)),
  tar_target(
    stage2b_panel,
    build_stage2_panel(panel, stage2b_linkage_weights, dummies = stage2b_cfg$dummies)
  ),
  tar_target(
    stage2b_preflight,
    stage2_preflight(stage2b_sys_eq, stage2b_panel, dates = stage2b_dates_target)
  ),
  # NOTE: run this with OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 in the
  # environment. This R links a pthread OpenBLAS, so each forked worker
  # otherwise claims every core -- measured load average 116 on 16 cores and a
  # >30x slowdown. See warn_if_blas_threaded().
  tar_target(
    stage2b_fit,
    fit_stage2(stage2b_sys_eq, stage2b_panel, stage2b_dates_target, workers = 8)
  ),
  tar_target(stage2b_acceptance, check_acceptance_rates(stage2b_fit)),
  tar_target(stage2b_stability, check_running_mean_stability(stage2b_fit)),

  # -- cross-country spillovers (see spillovers.R) --
  # koma has no impulse-response helper: spillovers are the DIFFERENCE
  # between two conditional forecasts, paired via common random numbers
  # (same set.seed() immediately before each forecast() call) so the
  # difference isolates the shock rather than koma's well-documented
  # call-to-call forecast noise. This reproduces the STRUCTURE of the
  # spillover analysis against `stage2b_fit` (the untuned system); the
  # numbers actually reported in reports/stage2_spillovers.qmd come from the
  # tau-tuned cache (`data/cache/stage2b/full_tuned.rds`), the same
  # "final" system stage2b_full_system.qmd's own verdict is based on.
  tar_target(spillover_horizon, 8),
  tar_target(spillover_seed, 20240101),
  tar_target(
    spillover_extension,
    extend_forecast_horizon(stage2b_fit, stage2b_panel, quarters = spillover_horizon)
  ),
  tar_target(
    spillover_baseline_forecast,
    {
      set.seed(spillover_seed)
      fc <- koma::forecast(spillover_extension$fit, dates = spillover_extension$dates,
                           options = list(approximate = FALSE, probs = c(0.05, 0.95)))
      attr(fc, "scenario_diff_seed") <- spillover_seed
      fc
    }
  ),
  # tar_combine() needs the actual tar_map() list object in its `...`, not a
  # bare target-name symbol, so this is captured in a variable rather than
  # inlined -- `<-` as a list() argument both binds it (for tar_combine()
  # below, evaluated next in the same list() call) and returns the value
  # (which targets flattens into the pipeline like any nested tar_map()
  # output, the same way country_panel's tar_map() above does).
  demand_shock_targets <- tar_map(
    values = list(iso2 = countries),
    names = "iso2",
    tar_target(
      spillover_demand_diff,
      scenario_diff(
        spillover_extension$fit,
        restrictions = gdp_demand_shock(spillover_baseline_forecast, iso2, size = 1),
        horizon = spillover_horizon, seed = spillover_seed,
        baseline_forecast = spillover_baseline_forecast
      )
    )
  ),
  tar_combine(
    spillover_demand_diffs, demand_shock_targets,
    command = stats::setNames(list(!!!.x), countries)
  ),
  tar_target(
    spillover_monetary_diff,
    scenario_diff(
      spillover_extension$fit,
      restrictions = policy_rate_shock(spillover_baseline_forecast, "ea_policy_rate", size = 1, quarters = 4),
      horizon = spillover_horizon, seed = spillover_seed,
      baseline_forecast = spillover_baseline_forecast
    )
  ),
  tar_target(
    spillover_oil_fit,
    oil_price_shock(spillover_extension$fit, spillover_extension$panel, spillover_extension$dates, size = 1.5)
  ),
  tar_target(
    spillover_oil_diff,
    scenario_diff(
      spillover_extension$fit, restrictions = NULL, horizon = spillover_horizon,
      scenario_fit = spillover_oil_fit, seed = spillover_seed,
      baseline_forecast = spillover_baseline_forecast
    )
  ),
  tar_target(spillover_matrix_result, spillover_matrix(spillover_demand_diffs, countries)),
  tar_target(
    spillover_sanity,
    spillover_sanity_checks(spillover_matrix_result, spillover_monetary_diff, countries, stage2b_linkage_weights)
  ),

  # -- stage 3: regional/global aggregation blocks (see stage3_blocks.R) --
  tar_target(model_blocks, define_blocks(blocks)),
  tar_target(
    block_aggregates,
    aggregate_blocks(panel, model_blocks, shared_concepts,
                     weights = list(ea = gdp_weights))
  ),

  # -- diagnostics and scoring (see diagnostics.R, scoring.R) --
  tar_target(
    scores,
    score_all_countries(stage2b_fit, countries, shared_concepts,
                        stage2b_dates_target, horizon = 4)
  ),
  tar_target(model_leaderboard, leaderboard(scores, by = "concept"))
)
