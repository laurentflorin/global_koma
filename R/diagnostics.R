# Posterior diagnostics for a fitted system.

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

#' Posterior coefficient table for a fitted system
#'
#' One row per (equation, term), with the posterior mean and a credible
#' interval. Works via `summary(fit, use_texreg = FALSE)`, which falls
#' back to a plain list even when `texreg` is not installed (verified) --
#' so this needs no reporting package beyond base R.
#'
#' @param fit A `koma::koma_estimate` object.
#' @param ci_low,ci_up Lower/upper quantile, in percent. Default `5`/`95`,
#'   i.e. a 90% interval.
#'
#' @return A `data.frame` with columns `equation`, `term`, `estimate`,
#'   `ci_low`, `ci_high`.
#' @export
coefficient_table <- function(fit, ci_low = 5, ci_up = 95) {
  s <- suppressMessages(summary(fit, ci_low = ci_low, ci_up = ci_up, use_texreg = FALSE))

  rows <- lapply(s$stats, function(eq) {
    data.frame(
      equation = eq$model.name, term = eq$coef.names,
      estimate = unname(eq$coef), ci_low = unname(eq$ci.low), ci_high = unname(eq$ci.up),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Own-lag stability of every equation
#'
#' An equation whose own-lag coefficient reaches 1 in absolute value has a
#' unit or explosive root, and koma's Gibbs sampler places **no stationarity
#' constraint on draws**, so it will happily return them. Few degrees of
#' freedom make this more likely. When a coefficient next to the own lag goes
#' to zero, check whether the lag went to a unit root and absorbed it.
#'
#' @param coef_table A `data.frame` from [coefficient_table()].
#' @param threshold Absolute value at or above which an own lag is flagged.
#'
#' @return A `data.frame` with columns `equation`, `term`, `estimate`,
#'   `ci_high`, `flagged`, ordered most-persistent first.
#' @export
check_lag_stability <- function(coef_table, threshold = 1) {
  lags <- coef_table[grepl("\\.L\\(1\\)$", coef_table$term), ]
  own <- lags[sub("\\.L\\(1\\)$", "", lags$term) == lags$equation, ]
  if (nrow(own) == 0) {
    return(data.frame(
      equation = character(0), term = character(0), estimate = numeric(0),
      ci_high = numeric(0), flagged = logical(0), stringsAsFactors = FALSE
    ))
  }
  out <- data.frame(
    equation = own$equation, term = own$term, estimate = own$estimate,
    ci_high = own$ci_high, flagged = abs(own$estimate) >= threshold,
    stringsAsFactors = FALSE
  )
  out[order(-abs(out$estimate)), ]
}

#' Which endogenous variables a variable reaches within the quarter
#'
#' Breadth-first search over `sys_eq$character_gamma_matrix`, the symbolic
#' matrix of **contemporaneous** endogenous regressors: entry `[i, j]` is
#' non-`"0"` when variable `i` appears on the right-hand side of equation `j`.
#' A path through it is a within-quarter transmission channel, identity links
#' included -- koma renders an identity weight as a `-thetaJ_I` symbol, not as
#' a literal number, so the only entries that have to be excluded are the
#' diagonal `"1"`s, which are each equation's own left-hand side rather than a
#' regressor.
#'
#' **Why this is worth measuring rather than assuming.** Making a variable
#' endogenous does not give it a transmission channel. A Taylor rule whose
#' rate reaches no price equation responds to an inflation rate nothing it
#' does can influence: an open loop that looks closed in the equation listing.
#'
#' The search starts from the equations `from` *enters*, so `from` appears in
#' the result only when a cycle brings it back to itself. Self-reachability
#' says *a* loop is closed, not *which* one: count what is reached (prices,
#' GDP) rather than whether the start point comes back.
#'
#' Reachability is a statement about *structure*, not about magnitude. A path
#' whose links are estimated near zero is reachable and inert.
#'
#' @param sys_eq A `koma::system_of_equations()` result.
#' @param from One or more endogenous variable names to start from.
#'
#' @return A sorted character vector of the endogenous variables reachable
#'   from `from` through contemporaneous links.
#' @export
contemporaneous_reachability <- function(sys_eq, from) {
  g <- sys_eq$character_gamma_matrix
  unknown <- setdiff(from, colnames(g))
  if (length(unknown) > 0) {
    cli::cli_abort("{.val {unknown}} {?is/are} not endogenous in this system.")
  }

  adjacency <- g != "0"
  diag(adjacency) <- FALSE

  reached <- character()
  frontier <- from
  while (length(frontier) > 0) {
    rows <- adjacency[rownames(g) %in% frontier, , drop = FALSE]
    nxt <- setdiff(colnames(g)[apply(rows, 2, any)], reached)
    reached <- c(reached, nxt)
    frontier <- nxt
  }
  sort(reached)
}

