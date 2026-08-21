# Wrappers around koma's MCMC diagnostics, applied project-wide (across
# every country in a fitted stage-1 or stage-2 model) rather than one
# equation at a time.

#' Summarise MCMC acceptance rates across a whole fit
#'
#' Wraps koma's per-equation acceptance-rate logic (see
#' `?koma::estimate`, "What tau does" in `docs/koma-api.md`) across every
#' stochastic equation in a fit, flagging any outside the target band.
#'
#' @param fit A `koma::koma_estimate` object.
#' @param band Numeric length-2 vector, the acceptable acceptance-rate
#'   range. Default `c(0.2, 0.6)`, matching koma's own default.
#'
#' @return A `data.frame` with columns `equation`, `has_mh_step`,
#'   `acceptance_rate`, `flagged`.
#' @export
check_acceptance_rates <- function(fit, band = c(0.2, 0.6)) {
  stop("not implemented", call. = FALSE)
}

#' Diagnostic plots for a set of variables across countries
#'
#' Wraps `koma::trace_plot()`, `koma::acf_plot()`, and
#' `koma::running_mean_plot()` for every `<iso2>_<concept>` combination
#' implied by `countries` x `concepts`, returning one plot per
#' variable/kind rather than requiring the caller to loop.
#'
#' @param fit A `koma::koma_estimate` object.
#' @param countries Character vector of ISO-2 country codes.
#' @param concepts Character vector of concepts, e.g. `c("gdp", "prices")`.
#' @param kind One of `"trace"`, `"acf"`, `"running_mean"`.
#'
#' @return A named list of `ggplot` objects, keyed by variable name.
#' @export
diagnostics_grid <- function(fit, countries, concepts, kind = c("trace", "acf", "running_mean")) {
  stop("not implemented", call. = FALSE)
}

#' Identification pre-check for a multi-country system
#'
#' Runs `koma::model_identification()` and reports failures per equation
#' with the offending country/concept, rather than koma's single
#' system-wide abort.
#'
#' @param sys_eq A `koma::koma_seq` object.
#'
#' @return A `data.frame` with columns `equation`, `order_condition`,
#'   `rank_condition`.
#' @export
check_identification <- function(sys_eq) {
  stop("not implemented", call. = FALSE)
}
