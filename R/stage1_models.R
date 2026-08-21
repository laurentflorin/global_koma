# Stage 1: per-country satellite models.
#
# Each country is estimated as its own small koma system (e.g. a
# small-open-economy block like the koma "small_open_economy" vignette),
# taking shared/world variables as exogenous. Stage 1 fits are later reused
# in stage 2 as a starting point via koma::estimate(..., estimates =).

#' Build one country's stage-1 equation set
#'
#' Returns the character vector of stochastic and identity equations for a
#' single country's satellite model, using [stochastic_equation()] and
#' [identity_equation()] with `<iso2>_`-prefixed variable names.
#'
#' @param iso2 Two-letter lowercase ISO country code.
#' @param spec A list describing the country's equations (dependent
#'   variables, regressors, lags) -- shape TBD once the model spec is
#'   finalised.
#'
#' @return A `koma::koma_seq` object.
#' @export
stage1_country_equations <- function(iso2, spec) {
  stop("not implemented", call. = FALSE)
}

#' Fit one country's stage-1 model
#'
#' @param iso2 Two-letter lowercase ISO country code.
#' @param panel Named list of `koma_ts` for this country plus its
#'   exogenous (shared) regressors, as built by [build_global_panel()].
#' @param dates koma `dates` list (`estimation`, `forecast`, ...).
#' @param options Passed through to `koma::estimate(options = )`.
#'
#' @return A `koma::koma_estimate` object.
#' @export
fit_stage1 <- function(iso2, panel, dates, options = list()) {
  stop("not implemented", call. = FALSE)
}

#' Fit stage 1 for every country
#'
#' @param countries Character vector of ISO-2 country codes.
#' @param panel Named list of `koma_ts`, the full multi-country panel.
#' @param dates koma `dates` list.
#' @param options Passed through to [fit_stage1()].
#'
#' @return A named list of `koma_estimate` objects, one per country.
#' @export
fit_stage1_all <- function(countries, panel, dates, options = list()) {
  stop("not implemented", call. = FALSE)
}
