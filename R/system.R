# Checking and estimating a koma system: what koma assumes and does not check,
# and the estimate() call with the panel prepared the way it needs.

#' Pre-flight checks on a system before committing to estimation
#'
#' Answers, up front and with a readable message, the questions koma either
#' answers too late or does not answer at all:
#'
#' - **every identity component resolves** to an endogenous, exogenous or
#'   weight variable;
#' - **no variable is defined twice**;
#' - **every variable has a series in `panel`**, identities and weight
#'   variables included. koma checks this only inside `estimate()`, after
#'   building the design matrices;
#' - **no required series has an internal `NA`**, which koma cannot estimate
#'   on;
#' - **every identity comes after every stochastic equation**. koma assumes
#'   this *positionally* (`model_identification()` and `estimate_sem()` index
#'   the first `n_endogenous - n_identities` columns as the stochastic ones),
#'   and declaring an identity earlier makes it check and estimate the wrong
#'   columns with no error;
#' - **no lagged endogenous name is a prefix of another** (see the comment in
#'   the code);
#' - **the system is identified**. `koma::model_identification()` fills the
#'   free coefficients with `rnorm` draws, so a single pass proves very
#'   little; this repeats it across `seeds` and reports how many passed;
#' - **`k < T`**, when `dates` is given;
#' - **every identity meant to be exact holds in the panel**, via
#'   [identity_consistency()]. koma never checks this.
#'
#' An identity with injected weights (`(a/b)*x`) has to be checked with the
#' numbers koma evaluates them to: pass a `sys_eq` whose identities carry
#' those, from `koma:::get_seq_weights()`.
#'
#' @param sys_eq A `koma::koma_seq`.
#' @param panel Named list of `koma_ts`.
#' @param seeds Integer vector of RNG seeds to repeat the identification
#'   check under.
#' @param dates koma `dates` list. Without it the `k < T` check reports `NA`.
#' @param inexact Passed to [identity_consistency()]: the identities that hold
#'   only approximately and are reported but not flagged.
#'
#' @return A `data.frame` with columns `check`, `ok`, `detail`. Never aborts:
#'   the point is to report every problem at once rather than stop at the
#'   first.
#' @export
preflight_system <- function(sys_eq, panel, seeds = 1:10, dates = NULL, inexact = "_gdp$") {
  if (!koma::is_system_of_equations(sys_eq)) {
    cli::cli_abort("{.arg sys_eq} must be a {.cls koma_seq} from {.fn koma::system_of_equations}.")
  }

  endogenous <- sys_eq$endogenous_variables
  declared <- c(endogenous, sys_eq$exogenous_variables, sys_eq$weight_variables, "constant")

  # Every base (lag-stripped) identity component must resolve. Note it is
  # `components` that is keyed by variable name -- `weights` is keyed by
  # koma's internal theta symbols ("theta16_17"), which would never match.
  rhs_names <- unique(unlist(lapply(sys_eq$identities, function(id) names(id$components)), use.names = FALSE))
  unresolved <- setdiff(sub("\\.L\\(.*", "", rhs_names %||% character(0)), declared)

  duplicated_endogenous <- endogenous[duplicated(endogenous)]

  needed <- c(endogenous, sys_eq$exogenous_variables, sys_eq$weight_variables)
  missing_series <- setdiff(needed, names(panel))

  gaps <- if (length(missing_series) == 0) internal_gaps(panel[needed]) else list()

  identified <- vapply(seeds, function(s) {
    set.seed(s)
    tryCatch(
      {
        koma::model_identification(
          sys_eq$character_gamma_matrix, sys_eq$character_beta_matrix, sys_eq$identities
        )
        TRUE
      },
      error = function(e) FALSE
    )
  }, logical(1))

  n_stochastic <- length(sys_eq$stochastic_equations)
  identities_last <- setequal(
    utils::tail(endogenous, length(sys_eq$identities)),
    names(sys_eq$identities)
  )

  # koma's construct_phi() locates an endogenous variable's lag columns with an
  # UNANCHORED prefix match, `grepl(paste0("^", name), exogenous_variables)`.
  # If one lagged endogenous name is a string prefix of another -- `de_debt`
  # and `de_debt_ratio`, say -- the shorter name matches both lag columns, the
  # inner loop runs twice for the same lag order, and the second pass
  # OVERWRITES the first's entry, pointing Phi at the wrong row of B. Silently:
  # no error, no warning, just a wrong companion matrix and therefore wrong
  # forecasts. Nothing downstream would catch it, so catch it here.
  lag_columns <- grep("\\.L\\(", sys_eq$total_exogenous_variables, value = TRUE)
  colliding <- Filter(
    function(v) sum(startsWith(lag_columns, v)) > 1,
    endogenous
  )
  names_ok <- length(colliding) == 0

  # koma projects EVERY equation on the FULL k-column x_matrix on EVERY draw
  # (construct_pi_hat_0 / construct_theta_hat_j both do
  # Matrix::solve(t(x) %*% x)), and draws Omega from riwish(T - k, .). So
  # k >= T is an unconditional double failure -- "system is computationally
  # singular" plus "v must be >= dimension of S in rwish()" -- and it happens
  # only after koma has built the design matrices. Catch it here instead.
  k <- length(sys_eq$total_exogenous_variables)
  n_obs <- if (is.null(dates)) NA_integer_ else estimation_length(panel, dates)
  df_residual <- if (is.na(n_obs)) NA_integer_ else n_obs - k
  df_ok <- if (is.na(df_residual)) NA else df_residual > 0

  # The arithmetic koma never does. Only meaningful once every series is
  # present, so it is skipped rather than reported when the panel is short.
  consistency <- if (length(missing_series) == 0) {
    identity_consistency(sys_eq, panel, inexact = inexact)
  } else {
    identity_consistency(sys_eq, panel, inexact = inexact)[0, ]
  }
  checked <- consistency[consistency$exact_expected, ]
  broken <- checked[!checked$ok, ]
  identities_hold <- if (nrow(checked) == 0) NA else nrow(broken) == 0

  data.frame(
    check = c(
      "identity RHS variables all resolve",
      "no variable defined twice",
      "every variable has a panel series",
      "no internal NAs in required series",
      "identities declared last",
      "no lagged endogenous name prefixes another",
      "system identified",
      "k < T (x'x invertible, Wishart df > 0)",
      "exact identities hold in the panel"
    ),
    ok = c(
      length(unresolved) == 0,
      length(duplicated_endogenous) == 0,
      length(missing_series) == 0,
      length(gaps) == 0,
      identities_last,
      names_ok,
      all(identified),
      df_ok,
      identities_hold
    ),
    detail = c(
      if (length(unresolved) == 0) "-" else paste(unresolved, collapse = ", "),
      if (length(duplicated_endogenous) == 0) "-" else paste(duplicated_endogenous, collapse = ", "),
      if (length(missing_series) == 0) {
        paste0(length(needed), " variables present")
      } else {
        paste("missing:", paste(missing_series, collapse = ", "))
      },
      if (length(gaps) == 0) "-" else paste(names(gaps), collapse = ", "),
      paste0(n_stochastic, " stochastic then ", length(sys_eq$identities), " identities"),
      if (names_ok) "-" else paste0("koma construct_phi() would mis-map: ", paste(colliding, collapse = ", ")),
      paste0(sum(identified), "/", length(seeds), " seeds pass order + rank"),
      if (is.na(df_residual)) {
        paste0("k = ", k, " (pass `dates` to check against T)")
      } else {
        paste0("k = ", k, ", T = ", n_obs, ", df = ", df_residual)
      },
      if (nrow(checked) == 0) {
        "no identity checkable against the panel"
      } else if (nrow(broken) == 0) {
        paste0(nrow(checked), " checked, worst |error| ",
               format(max(checked$max_abs_error), digits = 3, scientific = TRUE))
      } else {
        paste0(nrow(broken), " of ", nrow(checked), " violated: ",
               paste(sprintf("%s (%.3g)", broken$identity, broken$max_abs_error),
                     collapse = ", "))
      }
    ),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

#' Number of observations koma will actually estimate on
#'
#' The estimation window less the periods koma drops to build the first lag.
#' Used by [preflight_system()] to check `k < T` before committing to a run.
#'
#' @param panel Named list of `koma_ts`.
#' @param dates koma `dates` list.
#'
#' @return Integer count of usable observations.
#' @keywords internal
estimation_length <- function(panel, dates) {
  frequency <- stats::frequency(panel[[1]])
  to_index <- function(yq) yq[1] * frequency + (yq[2] - 1)
  start <- to_index(dates$estimation$start)
  end <- to_index(dates$estimation$end)
  # Two periods go, not one: `rate()` costs the first observation of every
  # diff_log series, and the L(1) lag costs one more. Verified: 2000Q1-2019Q4
  # is 80 quarters, and koma reports "Estimation start moved to 2000 Q3" with
  # T = 78.
  as.integer(end - start + 1 - 2)
}

#' How badly each identity fails to hold in the panel
#'
#' koma has **no identity-consistency check**. It parses an identity, stores
#' its weights, and enforces it during estimation, but never verifies that
#' the series it is handed actually satisfy it. A `+ -0.4*x` that stores the
#' wrong weights, or a left-hand side built by adding levels, goes through
#' silently. This function does the arithmetic koma does not.
#'
#' **Which space the comparison happens in matters.** koma estimates on
#' growth rates, so an identity over `level`/`diff_log` series is a statement
#' about `diff(log())` and is checked with [koma::rate()]. An identity over
#' `rate`/`none` series (interest rates, spreads) is a literal linear
#' combination of levels, since level and rate space coincide when
#' `method = "none"`. Mixing the two is not checkable and is skipped, as is an
#' identity with a lagged component.
#'
#' **Not every identity is meant to be exact.** An identity whose left-hand
#' side is a published series (GDP, which holds only up to inventories and the
#' statistical discrepancy) is reported with its error but never flagged;
#' `inexact` is the pattern that decides.
#'
#' The weights must be numbers. For injected weights (`(a/b)*x`), pass a
#' system whose identities carry what koma evaluates them to.
#'
#' @param sys_eq A `koma::koma_seq`.
#' @param panel Named list of `koma_ts`.
#' @param tolerance Maximum absolute error, in the identity's own units, an
#'   identity may show before it is flagged.
#' @param inexact Regular expression matching identity names that are inexact
#'   by construction and must not be flagged.
#'
#' @return A `data.frame` with one row per checkable identity: `identity`,
#'   `space`, `max_abs_error`, `exact_expected`, `ok`.
#' @export
identity_consistency <- function(sys_eq, panel,
                                 tolerance = 1e-8,
                                 inexact = "_gdp$") {
  rows <- lapply(names(sys_eq$identities), function(nm) {
    id <- sys_eq$identities[[nm]]
    terms <- names(id$components)
    # A lagged component (the stock-flow accumulation idiom) would need the
    # series shifted before differencing; out of scope, so skip rather than
    # report a spurious failure.
    if (any(grepl("\\.L\\(", terms)) || is.null(panel[[nm]]) ||
        !all(terms %in% names(panel))) {
      return(NULL)
    }
    # koma keys `weights` by its own theta symbols and `components` by variable
    # name; the two line up positionally.
    w <- stats::setNames(unlist(id$weights, use.names = FALSE), terms)

    series <- panel[c(nm, terms)]
    types <- vapply(series, function(x) attr(x, "series_type") %||% NA_character_, character(1))
    methods <- vapply(series, function(x) attr(x, "method") %||% NA_character_, character(1))
    literal <- all(types == "rate", na.rm = TRUE) && all(methods == "none", na.rm = TRUE)
    if (!literal && !all(types == "level", na.rm = TRUE)) return(NULL)

    space <- if (literal) "level" else "rate"
    cmp <- if (literal) series else lapply(series, koma::rate)
    m <- stats::na.omit(do.call(cbind, cmp))
    if (nrow(m) == 0) return(NULL)

    err <- max(abs(as.numeric(m[, 1]) - as.numeric(m[, -1, drop = FALSE] %*% w[terms])))
    exact <- !grepl(inexact, nm)
    data.frame(
      identity = nm, space = space, max_abs_error = err,
      exact_expected = exact, ok = !exact || err <= tolerance,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(out)) {
    return(data.frame(identity = character(), space = character(),
                      max_abs_error = numeric(), exact_expected = logical(),
                      ok = logical(), stringsAsFactors = FALSE))
  }
  rownames(out) <- NULL
  out[order(-out$max_abs_error), ]
}

#' Estimate a system
#'
#' `koma::estimate()` with the panel prepared the way it needs: subset to the
#' series the system names (endogenous, exogenous and weight variables),
#' refuse internal `NA`s with a message that names them, harmonise attributes
#' for `as_mets()`, and truncate the **endogenous** series to the estimation
#' end. The exogenous series keep their full length, which is what lets koma
#' conditionally fill any gap up to `dates$forecast$start`.
#'
#' **Never warm-start a changed system with `koma::estimate(estimates = )`.**
#' koma decides what to re-estimate by comparing only the symbolic **`B`**
#' matrices; the `Gamma` block is never compared. A changed contemporaneous
#' regressor is `Gamma`-only, so a warm start silently keeps stale draws of
#' the wrong dimension for exactly the equations that changed. This always
#' estimates cold.
#'
#' @param sys_eq A `koma::koma_seq`.
#' @param panel Named list of `koma_ts`, including a series for every identity.
#' @param dates koma `dates` list.
#' @param options Passed through to `koma::estimate(options = )`.
#' @param workers Number of `future` workers to spread the per-equation
#'   sampling across. `NULL` (the default) leaves koma sequential. Pin BLAS to
#'   one thread when using it (see [warn_if_blas_threaded()]).
#'
#' @return A `koma::koma_estimate` object, carrying `runtime_s` and `workers`
#'   attributes.
#' @export
fit_system <- function(sys_eq, panel, dates, options = list(), workers = NULL) {
  needed <- c(sys_eq$endogenous_variables, sys_eq$exogenous_variables, sys_eq$weight_variables)
  missing <- setdiff(needed, names(panel))
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "{.arg panel} is missing {.val {missing}}, required by the system.",
      "i" = "Identity-defined variables need a series too; see {.fn chain_weighted_index}."
    ))
  }

  gaps <- internal_gaps(panel[needed])
  if (length(gaps) > 0) {
    cli::cli_abort(c(
      "!" = "{.val {names(gaps)}} {?has/have} internal {.val NA}s, which koma cannot estimate on.",
      "i" = "Run {.fn fill_internal_gaps} on the panel first -- it interpolates them and warns."
    ))
  }

  ts_data <- harmonise_panel_attrs(panel[needed])
  ts_data[sys_eq$endogenous_variables] <- lapply(
    sys_eq$endogenous_variables,
    function(name) window_keeping_attrs(ts_data[[name]], end = dates$estimation$end)
  )

  # koma fans out one future per stochastic equation but never sets a plan
  # itself, so the default is fully sequential. With dozens of equations this
  # is the single biggest runtime lever, so set the plan here and restore it
  # on exit.
  if (!is.null(workers) && workers > 1) {
    warn_if_blas_threaded(workers)
    old_plan <- future::plan(parallel_strategy(), workers = workers)
    on.exit(future::plan(old_plan), add = TRUE)
    cli::cli_inform("Estimating {length(sys_eq$stochastic_equations)} equation{?s} on {workers} worker{?s}.")
  }

  started <- Sys.time()
  fit <- koma::estimate(ts_data, sys_eq, dates, options = options)
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))

  attr(fit, "runtime_s") <- elapsed
  attr(fit, "workers") <- workers %||% 1L
  fit
}

#' Pick a `future::plan()` strategy for this platform
#'
#' Apple's Accelerate BLAS is not fork-safe and segfaults inside koma's
#' `eigen()` call, and Windows cannot fork at all; both need `multisession`.
#' See `docs/koma-api.md` §3.
#'
#' @return `"future::multicore"` or `"future::multisession"`.
#' @keywords internal
parallel_strategy <- function() {
  can_fork <- .Platform$OS.type != "windows" && Sys.info()[["sysname"]] != "Darwin"
  if (can_fork) "future::multicore" else "future::multisession"
}

#' Warn when a threaded BLAS will fight the per-equation workers
#'
#' This project's R links against a **pthread** OpenBLAS
#' (`libopenblasp-*.so`). Each process it runs in claims every core for its own
#' BLAS thread pool, and `future::multicore` forks inherit that. Running `w`
#' workers therefore spawns roughly `w x ncores` threads onto `ncores` cores.
#'
#' Measured on this machine: 8 workers against a 25-equation system drove the
#' load average to **116 on 16 cores**, with each worker burning ~180% CPU and
#' the step failing to finish in twenty minutes -- against ~1 minute for the
#' same work done properly.
#'
#' The fix is to pin BLAS to one thread per process **before R starts**, so the
#' parallelism is purely across equations:
#'
#' ```sh
#' OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 Rscript ...
#' ```
#'
#' It has to be the environment rather than `Sys.setenv()` inside the session,
#' because OpenBLAS sizes its pool when the library first initialises. Pinning
#' costs nothing here: koma's matrices are at most `k x k`, far too small for
#' threaded BLAS to pay for its own synchronisation.
#'
#' @param workers Number of workers about to be started.
#'
#' @return Invisibly `TRUE` if a warning was issued.
#' @keywords internal
warn_if_blas_threaded <- function(workers) {
  pinned <- any(vapply(
    c("OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS"),
    function(v) identical(Sys.getenv(v), "1"),
    logical(1)
  ))
  if (pinned) {
    return(invisible(FALSE))
  }
  threaded <- grepl("openblasp|libmkl|libblis", utils::sessionInfo()$BLAS %||% "", ignore.case = TRUE)
  if (!threaded) {
    return(invisible(FALSE))
  }

  cli::cli_warn(c(
    "!" = "A threaded BLAS is active and {.arg workers} is {workers}: each forked
           worker will claim every core for its own thread pool.",
    "i" = "Re-run with {.code OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1} set in the
           environment before R starts.",
    "i" = "Left unpinned this oversubscribes badly -- measured load average 116 on
           16 cores, and a step that should take a minute did not finish in twenty."
  ))
  invisible(TRUE)
}
