# Shared helpers for the eight-quarter forecast section every stage report
# carries. Presentational only -- the forecasting itself lives in
# R/forecasts.R and the artefacts are built by scratch/forecasts_build.R.
# Sourced directly by each report's load chunk, the same way
# reports/_equation_overview_helpers.R is.
#
# The section is deliberately identical in structure across stages -- fan
# chart, median table, explosive-draw table, anchoring note -- so that the
# eight stages can be read against each other. What differs between them is
# only which entities exist and whether the forecast window is in the past.

#' Load one stage's forecast artefact.
forecast_artefact <- function(stage) {
  path <- file.path("data", "cache", "forecasts", paste0(stage, ".rds"))
  if (!file.exists(path)) {
    stop("Missing ", path, " -- run scratch/forecasts_build.R first.", call. = FALSE)
  }
  readRDS(path)
}

#' `<entity>_<concept>` names that actually exist in a forecast's paths.
#'
#' Reports name the entities they care about; a stage that does not carry one
#' (China outside stage 2d, `reu` outside stage 2d) simply drops out rather
#' than erroring, so one helper serves every report.
fc_vars <- function(paths, entities, concept) {
  wanted <- paste0(entities, "_", concept)
  wanted[wanted %in% unique(paths$variable)]
}

#' Facet labels: `de_gdp` -> `DE`, so a GDP panel reads as a country grid.
fc_entity_labels <- function(vars) {
  stats::setNames(toupper(sub("_.*$", "", vars)), vars)
}

#' Facet labels: `de_gdp` -> `DE GDP`, for a mixed-concept panel.
fc_full_labels <- function(vars) {
  stats::setNames(
    paste(toupper(sub("_.*$", "", vars)), gsub("_", " ", sub("^[a-z]+_", "", vars))),
    vars
  )
}

#' The one-line summary of what a stage's forecast horizon actually covers.
#'
#' Stages 1 and 2a forecast from 2023Q1 into observed data; stages 2b onward
#' forecast from 2025Q1 into a window that is mostly still ahead. A reader
#' comparing two of these sections needs to be told which is which, every
#' time, rather than inferring it from the axis.
fc_window_note <- function(fc) {
  origin <- sprintf("%dQ%d", fc$origin[1], fc$origin[2])
  last <- quarter_label(max(fc$paths$time))
  n_actual_q <- length(unique(fc$paths$time[
    fc$paths$kind == "forecast" & !is.na(fc$paths$actual_rate)
  ]))
  sprintf(
    "%d quarters from **%s** to **%s**, %d posterior draws. %s",
    fc$horizon, origin, last, fc$n_draws,
    if (n_actual_q >= fc$horizon) {
      "The whole window is already observed, so this is an out-of-sample check rather than a projection."
    } else if (n_actual_q > 0) {
      sprintf("Actuals exist for the first %d quarter%s; the rest is genuinely ahead of the data.",
              n_actual_q, if (n_actual_q == 1) "" else "s")
    } else {
      "No actuals exist yet anywhere in the window."
    }
  )
}

#' The anchoring caveat, stated with this stage's own numbers.
#'
#' koma anchors a level path on the last value in the fit's `ts_data`, which
#' for a stage that conditionally fills (1 and 2a) is a *filled* value rather
#' than an observed one. Returns NULL when every anker is observed, so the
#' stages where this does not apply say nothing rather than a reassurance.
fc_anker_note <- function(fc, tolerance = 0.5) {
  a <- fc$anker
  if (is.null(a) || nrow(a) == 0) return(NULL)
  worst <- a[which.max(abs(a$gap_pct)), ]
  if (abs(worst$gap_pct) < tolerance) {
    return(sprintf(
      paste("Every level path is anchored on an **observed** value (largest anchor",
            "discrepancy %.2f%%, on `%s`), so the level and rate charts are both",
            "directly comparable to the actuals."),
      abs(worst$gap_pct), worst$variable
    ))
  }
  sprintf(
    paste("**The level paths are anchored on conditionally-filled values, not observed ones.**",
          "koma takes the anchor from the end of the fit's own `ts_data`, and this stage fills",
          "%s before forecasting. The worst discrepancy is `%s`, anchored at %.4g against an",
          "observed %.4g -- **%.1f%% out** -- and the median across all variables is %.1f%%.",
          "A level gap at horizon 1 is therefore inherited from the fill, not produced by the",
          "forecast. **Read the rate chart, which is unaffected.**"),
    unique(a$anker_time)[1], worst$variable, worst$anker, worst$observed,
    worst$gap_pct, stats::median(abs(a$gap_pct))
  )
}

#' Explosive-draw summary sentence for a set of variables.
fc_explosive_note <- function(fc, vars = NULL) {
  e <- forecast_explosive_table(fc$paths, vars)
  sprintf(
    paste("Across the variables shown, **%.1f%%** of draws are discarded as explosive at horizon 1,",
          "rising to **%.1f%%** at horizon %d (worst: `%s` at %.1f%%)."),
    100 * mean(e$h1), 100 * mean(e$h_last), fc$horizon,
    e$variable[1], 100 * e$h_last[1]
  )
}

#' The median-path table, transposed so quarters are rows.
fc_median_table <- function(fc, vars, space = "rate", digits = 2) {
  tab <- forecast_table(fc$paths, vars, space = space, digits = digits)
  names(tab) <- c("quarter", toupper(sub("_.*$", "", vars)))
  tab
}

#' Median forecast against the realised path, where one exists.
#'
#' Only meaningful for a stage whose window is already observed; returns NULL
#' otherwise so the report can drop the block entirely.
fc_vs_actual_table <- function(fc, vars, space = "rate", digits = 2) {
  med <- if (space == "rate") "rate_median" else "level_median"
  act <- if (space == "rate") "actual_rate" else "actual_level"
  d <- fc$paths[fc$paths$kind == "forecast" & fc$paths$variable %in% vars, ]
  if (all(is.na(d[[act]]))) return(NULL)
  out <- data.frame(quarter = quarter_label(sort(unique(d$time))), stringsAsFactors = FALSE)
  for (v in vars) {
    dv <- d[d$variable == v, ]
    i <- match(sort(unique(d$time)), dv$time)
    out[[paste0(toupper(sub("_.*$", "", v)), " fc")]] <- round(dv[[med]][i], digits)
    out[[paste0(toupper(sub("_.*$", "", v)), " act")]] <- round(dv[[act]][i], digits)
  }
  out
}

# ---------------------------------------------------------------------------
# Cross-stage comparison helpers.
#
# Everything above summarises ONE stage's forecast. The overview report needs
# to put several stages beside each other, which raises two problems that do
# not exist within a stage, and both are handled here rather than in the .qmd:
#
#   1. The stages do not carry the same variables. Comparing an error averaged
#      over stage 2b's eleven countries against one averaged over stage 2d's
#      six is a comparison of country mixes, not of models. `fc_common_vars()`
#      intersects, so every stage is scored on exactly the same series.
#   2. The stages do not share a forecast origin. Stages 1 and 2a run from
#      2023Q1, everything from 2b on from 2025Q1, so their horizons cover
#      different quarters of history and their errors are not on the same
#      scale. Nothing here pools across that boundary; the report shows the
#      two groups separately and says why.
# ---------------------------------------------------------------------------

#' Variables every artefact carries AND agrees on, optionally filtered.
#'
#' A thin wrapper on `common_forecast_variables()`, which does the real work
#' and carries the reasoning: intersecting the variable sets is not enough,
#' because two stages can carry the same NAME for different series --
#' `<iso2>_prices` is observed HICP in stage 2b and a chain-weighted identity
#' under the labour block. The package function drops those and reports what it
#' dropped in a `dropped` attribute.
#'
#' `concepts` narrows to the concepts worth averaging over -- growth rates of
#' real volumes and prices, not policy rates, whose errors are in percentage
#' points and would dominate any average they were included in.
fc_common_vars <- function(fc_list, concepts = NULL) {
  common_forecast_variables(fc_list, concepts)
}

#' Mean absolute error by stage and horizon, on a shared variable set.
#'
#' One row per stage x horizon. `n` is the number of variable-quarters behind
#' each cell and is returned rather than hidden, because a horizon where only
#' some stages have an outturn would otherwise look like a difference between
#' models.
fc_error_by_horizon <- function(fc_list, vars, space = "rate") {
  rows <- lapply(names(fc_list), function(s) {
    e <- forecast_error_table(fc_list[[s]]$paths, vars, space = space)
    if (nrow(e) == 0) return(NULL)
    do.call(rbind, lapply(split(e, e$horizon), function(z) data.frame(
      stage = s, label = fc_list[[s]]$label, horizon = z$horizon[1],
      mae = mean(z$abs_error), rmse = sqrt(mean(z$error^2)),
      bias = mean(z$error), n = nrow(z), stringsAsFactors = FALSE
    )))
  })
  out <- do.call(rbind, Filter(Negate(is.null), rows))
  rownames(out) <- NULL
  out
}

#' One variable's fan across several stages, as a single `paths` frame.
#'
#' [plot_forecast_fan()] facets on `variable`, so a cross-stage chart is made
#' by relabelling each stage's rows with a stage-specific key and stacking
#' them. Returns the frame and the labeller together so the caller cannot pair
#' the wrong two.
fc_stage_compare <- function(fc_list, var, stages = names(fc_list)) {
  stages <- stages[vapply(stages, function(s) var %in% fc_list[[s]]$paths$variable, logical(1))]
  d <- do.call(rbind, lapply(stages, function(s) {
    x <- fc_list[[s]]$paths[fc_list[[s]]$paths$variable == var, ]
    x$variable <- paste0(s, "::", var)
    x
  }))
  keys <- paste0(stages, "::", var)
  labs <- vapply(stages, function(s) sub(" \\(.*$", "", fc_list[[s]]$label), character(1))
  list(paths = d, vars = keys, labeller = stats::setNames(unname(labs), keys))
}
