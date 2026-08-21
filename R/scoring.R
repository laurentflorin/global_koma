# Out-of-sample scoring, across countries and forecast variants.

#' Score one country's forecast against actuals
#'
#' Wraps `koma::model_evaluation()` for a single country/concept and
#' reshapes its output into this project's scoring schema.
#'
#' @param fit A `koma::koma_estimate` object.
#' @param iso2 Two-letter lowercase ISO country code.
#' @param concepts Character vector of concepts to score, e.g.
#'   `c("gdp", "prices")`.
#' @param dates koma `dates` list with a `forecast` range to evaluate over.
#' @param horizon Integer forecast horizon (see `?koma::model_evaluation`).
#'
#' @return A `data.frame` with columns `iso2`, `concept`, `horizon`,
#'   `rmse`.
#' @export
score_country_forecast <- function(fit, iso2, concepts, dates, horizon) {
  stop("not implemented", call. = FALSE)
}

#' Score every country in a fitted system
#'
#' @param fit A `koma::koma_estimate` object for the joint system.
#' @param countries Character vector of ISO-2 country codes.
#' @param concepts Character vector of concepts to score for every
#'   country.
#' @param dates koma `dates` list with a `forecast` range to evaluate over.
#' @param horizon Integer forecast horizon.
#'
#' @return A `data.frame`, the row-bound output of
#'   [score_country_forecast()] across `countries`.
#' @export
score_all_countries <- function(fit, countries, concepts, dates, horizon) {
  stop("not implemented", call. = FALSE)
}

#' Rank model variants by score
#'
#' @param scores A `data.frame` as returned by [score_all_countries()],
#'   with an added `variant` column identifying which model/spec produced
#'   each row.
#' @param by Column(s) to average `rmse` over before ranking, e.g.
#'   `c("concept")` for a per-concept leaderboard.
#'
#' @return A `data.frame` with columns `variant`, `by` columns, `mean_rmse`,
#'   `rank`, sorted best-first.
#' @export
leaderboard <- function(scores, by = "concept") {
  stop("not implemented", call. = FALSE)
}
