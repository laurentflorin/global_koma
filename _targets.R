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

  # -- stage 2: joint multi-country system (see stage2_system.R) --
  tar_target(
    stage2_sys_eq,
    build_stage2_system(countries, country_specs = list(),
                        shared_concepts, weights = list(gdp = gdp_weights),
                        truly_exogenous)
  ),
  tar_target(
    stage2_fit,
    fit_stage2(stage2_sys_eq, panel, estimation_dates, stage1_fits)
  ),

  # -- stage 3: regional/global aggregation blocks (see stage3_blocks.R) --
  tar_target(model_blocks, define_blocks(blocks)),
  tar_target(
    block_aggregates,
    aggregate_blocks(panel, model_blocks, shared_concepts,
                     weights = list(ea = gdp_weights))
  ),

  # -- diagnostics and scoring (see diagnostics.R, scoring.R) --
  tar_target(acceptance_rates, check_acceptance_rates(stage2_fit)),
  tar_target(
    scores,
    score_all_countries(stage2_fit, countries, shared_concepts,
                        estimation_dates, horizon = 4)
  ),
  tar_target(model_leaderboard, leaderboard(scores, by = "concept"))
)
