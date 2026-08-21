# Aggregation weights for building shared (ea_ / world_) variables from
# country-level series, and for the stage 3 regional/global blocks.

#' Compute country aggregation weights
#'
#' Computes the weights used to aggregate country-level series into a
#' shared euro-area or world series, on the requested basis.
#'
#' @param countries Character vector of ISO-2 country codes to weight.
#' @param basis One of `"gdp"` (nominal GDP shares) or `"trade"`
#'   (bilateral trade shares).
#' @param year Reference year for the weights (weights are typically fixed
#'   at a base year rather than time-varying, unlike koma's own dynamic
#'   identity weights).
#'
#' @return A named numeric vector, one weight per element of `countries`,
#'   summing to 1.
#' @export
country_weights <- function(countries, basis = c("gdp", "trade"), year) {
  stop("not implemented", call. = FALSE)
}

#' Apply weights to build a shared series from country series
#'
#' @param panel A named list of `koma_ts` objects (see [build_global_panel()]).
#' @param concept The concept to aggregate, e.g. `"gdp"` (looks up
#'   `<iso2>_gdp` for each country in `weights`).
#' @param weights Named numeric vector as returned by [country_weights()].
#' @param scope `"ea"` or `"world"`; determines the output variable's
#'   prefix via [shared_var()].
#'
#' @return A single `koma_ts`, the weighted aggregate.
#' @export
apply_weights <- function(panel, concept, weights, scope = c("ea", "world")) {
  stop("not implemented", call. = FALSE)
}

#' Build a koma identity equation from country weights
#'
#' Convenience wrapper around [identity_equation()] that turns a
#' [country_weights()] vector into the `(weight)*component` terms of an
#' aggregation identity, e.g. `ea_gdp == 0.3*de_gdp + 0.2*fr_gdp + ...`.
#'
#' @param concept The concept being aggregated, e.g. `"gdp"`.
#' @param weights Named numeric vector as returned by [country_weights()].
#' @param scope `"ea"` or `"world"`.
#'
#' @return A single equation string.
#' @export
weighted_identity <- function(concept, weights, scope = c("ea", "world")) {
  stop("not implemented", call. = FALSE)
}
