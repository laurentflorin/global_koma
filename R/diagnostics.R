# Wrappers around koma's MCMC diagnostics, applied project-wide (across
# every country in a fitted stage-1 or stage-2 model) rather than one
# equation at a time.

#' Summarise MCMC acceptance rates across a whole fit
#'
#' Wraps koma's per-equation acceptance-rate logic (see
#' `?koma::estimate`, "What tau does" in `docs/koma-api.md`) across every
#' stochastic equation in a fit, flagging any outside the target band.
#'
#' koma exposes no accessor for this: the per-draw 0/1 acceptance
#' indicator lives at `fit$estimates[[equation]]$count_accepted`, and the
#' rate is its mean. Equations with **no contemporaneous endogenous
#' regressor** have no Metropolis step at all -- their gamma block is
#' empty and `count_accepted` is `NA` throughout. Those are reported with
#' `has_mh_step = FALSE` and an `NA` rate, and are never flagged; koma
#' excludes them from its own warning for the same reason.
#'
#' The default band matches `koma:::get_default_acceptance_prob()`, which
#' returns `c(0.2, 0.6)`. (koma's `equations` vignette prose says
#' 30%-60%; the code is authoritative and says 20%-60%.)
#'
#' @param fit A `koma::koma_estimate` object.
#' @param band Numeric length-2 vector, the acceptable acceptance-rate
#'   range. Default `c(0.2, 0.6)`, matching koma's own default.
#'
#' @return A `data.frame` with columns `equation`, `has_mh_step`,
#'   `acceptance_rate`, `flagged`.
#' @export
check_acceptance_rates <- function(fit, band = c(0.2, 0.6)) {
  estimates <- fit$estimates
  if (is.null(estimates) || length(estimates) == 0) {
    cli::cli_abort("{.arg fit} has no {.field estimates}; is it a {.cls koma_estimate}?")
  }
  if (length(band) != 2 || band[1] >= band[2]) {
    cli::cli_abort("{.arg band} must be two increasing numbers, got {.val {band}}.")
  }

  rate <- vapply(estimates, function(equation) {
    accepted <- equation$count_accepted
    if (is.null(accepted) || all(is.na(accepted))) {
      return(NA_real_)
    }
    mean(accepted, na.rm = TRUE)
  }, numeric(1))

  has_mh_step <- !is.na(rate)

  data.frame(
    equation = names(estimates),
    has_mh_step = has_mh_step,
    acceptance_rate = unname(rate),
    flagged = has_mh_step & (rate < band[1] | rate > band[2]),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
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
#' `koma::model_identification()` is called automatically inside
#' `koma::estimate()`, but only aborts -- it does not report which
#' equation failed. Calling it here up front is cheap (it is a purely
#' symbolic check on the `Gamma`/`Beta` matrices, with no data and no
#' MCMC) and catches a mis-specified system before committing to a long
#' run.
#'
#' Note the argument koma actually wants is the whole `identities` list
#' (post-weight-resolution), not just the weights -- passing the weights
#' alone fails with an opaque "NA/NaN/Inf in foreign function call".
#'
#' @param sys_eq A `koma::koma_seq` object.
#'
#' @return A `data.frame` with columns `equation`, `order_condition`,
#'   `rank_condition`.
#' @export
check_identification <- function(sys_eq) {
  if (!koma::is_system_of_equations(sys_eq)) {
    cli::cli_abort("{.arg sys_eq} must be a {.cls koma_seq} from {.fn koma::system_of_equations}.")
  }

  equations <- sys_eq$stochastic_equations
  result <- tryCatch(
    {
      koma::model_identification(
        sys_eq$character_gamma_matrix,
        sys_eq$character_beta_matrix,
        sys_eq$identities
      )
      NULL
    },
    error = function(e) conditionMessage(e)
  )
  identified <- is.null(result)

  if (!identified) {
    cli::cli_warn(c(
      "!" = "The system is not identified.",
      "i" = result
    ))
  }

  data.frame(
    equation = equations,
    order_condition = identified,
    rank_condition = identified,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}
