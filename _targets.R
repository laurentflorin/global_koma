# Multi-country Bayesian macro model pipeline.
#
# Run with `targets::tar_make()`. See CLAUDE.md for the repo map and the
# stage 1 / stage 2 / stage 3 vocabulary this pipeline follows.
#
# No data is fetched by any target yet -- data_fred.R / data_eamdqd.R /
# panel_build.R are stubs (see their roxygen docs and failing tests under
# tests/testthat/). tar_make() will error until those are implemented;
# tar_manifest()/tar_visnetwork() can still be used to inspect the plan.

library(targets)
library(tarchetypes)

# Load this project's own R/ functions (not yet an installed package).
devtools::load_all(".", quiet = TRUE)

tar_option_set(
  packages = c("koma", "cli", "rlang"),
  format = "rds"
)

# --- configuration -----------------------------------------------------

countries <- c("de", "fr", "it", "es", "us")
blocks <- list(ea = c("de", "fr", "it", "es"))
shared_concepts <- c("gdp")
truly_exogenous <- c("oil_price")

estimation_dates <- list(
  estimation = list(start = c(1999, 1), end = c(2019, 4)),
  forecast   = list(start = c(2020, 1), end = c(2020, 4))
)

# --- pipeline ------------------------------------------------------------

list(
  # -- raw data (stubs; see data_fred.R / data_eamdqd.R) --
  tar_target(fred_series_map, data.frame()),
  tar_target(eamdqd_map, eamdqd_variable_map()),

  # -- per-country panels, one target per country (see panel_build.R) --
  tar_map(
    values = list(iso2 = countries),
    names = "iso2",
    tar_target(country_panel, build_country_panel(iso2, fred_series_map))
  ),

  # -- shared/global panel + full aligned panel (see panel_build.R) --
  tar_target(
    global_panel,
    build_global_panel(countries, fred_series_map)
  ),
  tar_target(
    panel,
    align_panel(global_panel, estimation_dates$estimation$start,
               estimation_dates$estimation$end)
  ),

  # -- aggregation weights (see weights.R) --
  tar_target(gdp_weights, country_weights(countries, basis = "gdp", year = 2019)),

  # -- stage 1: per-country satellite models (see stage1_models.R) --
  tar_target(
    stage1_fits,
    fit_stage1_all(countries, panel, estimation_dates)
  ),

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
