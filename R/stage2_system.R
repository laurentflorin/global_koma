# Stage 2: the joint multi-country system.
#
# Combines every country's stage-1 equations with the shared/aggregate
# equations into a single koma::system_of_equations() call, so
# cross-country simultaneity (via ea_/world_ variables) is estimated
# jointly rather than country-by-country.

#' Assemble the joint multi-country equation string
#'
#' Concatenates every country's stage-1 equations (see
#' [stage1_country_equations()]) with the shared aggregation identities
#' (see [weighted_identity()]) into one equation string for
#' `koma::system_of_equations()`.
#'
#' @param countries Character vector of ISO-2 country codes.
#' @param country_specs Named list of per-country stage-1 specs, keyed by
#'   `iso2` (see [stage1_country_equations()]).
#' @param shared_concepts Character vector of concepts to aggregate into
#'   `ea_`/`world_` variables (see [weighted_identity()]).
#' @param weights Named list of [country_weights()] vectors, one per
#'   `shared_concepts` element.
#'
#' @return A single character vector of equation strings.
#' @export
build_system_equations <- function(countries, country_specs, shared_concepts,
                                   weights) {
  stop("not implemented", call. = FALSE)
}

#' Determine the joint system's exogenous variables
#'
#' Every variable referenced on an equation's RHS that is not itself an
#' endogenous (country or shared) variable in the system must be declared
#' exogenous to `koma::system_of_equations()`. This computes that set.
#'
#' @param countries Character vector of ISO-2 country codes.
#' @param truly_exogenous Character vector of variables with no equation of
#'   their own anywhere in the system (e.g. `"oil_price"`, `"vix"`).
#'
#' @return A character vector of exogenous variable names.
#' @export
stage2_exogenous_variables <- function(countries, truly_exogenous) {
  stop("not implemented", call. = FALSE)
}

#' Build the joint multi-country `koma_seq`
#'
#' @inheritParams build_system_equations
#' @param truly_exogenous Passed to [stage2_exogenous_variables()].
#'
#' @return A `koma::koma_seq` object for the full system.
#' @export
build_stage2_system <- function(countries, country_specs, shared_concepts,
                                weights, truly_exogenous) {
  stop("not implemented", call. = FALSE)
}

#' Fit the joint multi-country system
#'
#' Estimates the full system, optionally warm-started from stage-1 fits via
#' `koma::estimate(..., estimates = )`.
#'
#' @param sys_eq A `koma_seq` as built by [build_stage2_system()].
#' @param panel Named list of `koma_ts`, the full multi-country panel.
#' @param dates koma `dates` list.
#' @param stage1_fits Optional named list of stage-1 `koma_estimate`
#'   objects (see [fit_stage1_all()]) to warm-start from.
#' @param options Passed through to `koma::estimate(options = )`.
#'
#' @return A `koma::koma_estimate` object for the joint system.
#' @export
fit_stage2 <- function(sys_eq, panel, dates, stage1_fits = NULL, options = list()) {
  stop("not implemented", call. = FALSE)
}
