# Shared helpers for the eight-quarter forecast section every stage report
# carries. Presentational only -- the forecasting itself lives in
# R/forecasts.R and the artefacts are built by scratch/forecasts_build.R.
# Sourced directly by each report's load chunk, the same way
# reports/_equation_overview_helpers.R is.
#
# The section is deliberately identical in structure across stages -- fan
# chart, median table, explosive-draw table, anchoring note -- so that the
# seven stages can be read against each other. What differs between them is
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
