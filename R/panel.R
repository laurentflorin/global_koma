# Panel helpers: turning raw series into the named list of koma_ts that
# koma::estimate() takes, and building series that exist only as identities.

#' Decimal time to `c(year, period)`
#'
#' The inverse of `year + (period - 1) / frequency`, robust to the floating
#' point error `stats::tsp()` carries.
#'
#' @param t Decimal time, e.g. `2004.25`.
#' @param frequency Periods per year.
#'
#' @return Integer-valued `c(year, period)`.
#' @keywords internal
num_to_period <- function(t, frequency) {
  year <- floor(t + .Machine$double.eps)
  period <- round((t - year) * frequency) + 1L
  c(year, period)
}

#' Convert a `get_eurostat()` result to a plain `ts`
#'
#' @param d A data frame from `eurostat::get_eurostat(time_format = "date")`,
#'   one series, with columns `time` and `values`. It must have no missing
#'   periods: values are placed by position from the first date.
#' @param frequency Periods per year.
#'
#' @return A `ts`.
#' @keywords internal
eurostat_to_ts <- function(d, frequency = 4) {
  if (nrow(d) == 0) {
    cli::cli_abort("Eurostat query returned no rows.")
  }
  d <- d[order(d$time), ]
  start_year <- as.integer(format(d$time[1], "%Y"))
  start_period <- if (frequency == 4) {
    (as.integer(format(d$time[1], "%m")) - 1L) %/% 3L + 1L
  } else {
    as.integer(format(d$time[1], "%m"))
  }
  stats::ts(d$values, start = c(start_year, start_period), frequency = frequency)
}

#' The koma-specific attributes of a koma_ts
#'
#' @param x A `koma_ts`.
#'
#' @return A named list: `series_type`, `method` and anything else set with
#'   `koma::as_ets()`, without the `ts` attributes.
#' @keywords internal
get_custom_attrs <- function(x) {
  keep <- setdiff(names(attributes(x)), c("tsp", "class", "dim", "dimnames", "names"))
  attrs <- attributes(x)[keep]
  attrs
}

#' Give every series in a panel the same set of attributes
#'
#' `koma::estimate()` runs its `ts_data` through `as_mets()`, which
#' **requires every series to carry an identical set of attribute names**
#' and aborts with "Provide the same attributes for each series in your
#' list" otherwise. This takes the union of the attribute names present
#' anywhere in `panel` and fills the gaps with `NA`. `ets_attributes` is
#' dropped and left to koma, which maintains it itself.
#'
#' @param panel A named list of `koma_ts`.
#'
#' @return The same list, every element carrying the same attribute names.
#' @export
harmonise_panel_attrs <- function(panel) {
  custom <- lapply(panel, get_custom_attrs)
  attr_names <- setdiff(unique(unlist(lapply(custom, names))), "ets_attributes")

  out <- lapply(seq_along(panel), function(i) {
    attrs <- custom[[i]]
    attrs[["ets_attributes"]] <- NULL
    for (missing_name in setdiff(attr_names, names(attrs))) {
      attrs[[missing_name]] <- NA
    }
    do.call(koma::as_ets, c(list(panel[[i]]), attrs[attr_names]))
  })
  stats::setNames(out, names(panel))
}

#' Report which series in a panel have internal (non-edge) `NA`s
#'
#' Distinguishes an **internal** gap -- an `NA` with observed values on
#' both sides -- from a leading or trailing `NA`, which is a ragged edge.
#' The distinction matters because koma fills ragged edges itself
#' (`fill_ragged_edge()`/`conditional_fill()`), but has no facility for a
#' hole in the middle of a series and fails with an opaque
#' "time series contains internal NAs" from deep inside `level()`.
#'
#' @param panel A named list of `koma_ts`.
#'
#' @return A named list, one element per affected series, each a numeric
#'   vector of the affected times. Empty if the panel is clean.
#' @export
internal_gaps <- function(panel) {
  gaps <- lapply(panel, function(x) {
    values <- as.numeric(x)
    observed <- which(!is.na(values))
    if (length(observed) == 0) {
      return(numeric(0))
    }
    span <- seq(min(observed), max(observed))
    as.numeric(stats::time(x))[span[is.na(values[span])]]
  })
  gaps[vapply(gaps, length, integer(1)) > 0]
}

#' Interpolate internal gaps, and say so
#'
#' Fills every internal `NA` (see [internal_gaps()]) by linear interpolation
#' and warns, naming each series and period: an interpolated observation is
#' invented data, and koma cannot estimate on a gap at all. Ragged edges are
#' left alone; koma fills those itself.
#'
#' @param panel A named list of `koma_ts`.
#'
#' @return The panel with its internal gaps filled.
#' @export
fill_internal_gaps <- function(panel) {
  gaps <- internal_gaps(panel)
  if (length(gaps) == 0) {
    return(panel)
  }

  described <- vapply(names(gaps), function(name) {
    paste0(name, " (", paste(sprintf("%.2f", gaps[[name]]), collapse = ", "), ")")
  }, character(1))
  cli::cli_warn(c(
    "!" = "Linearly interpolated {sum(lengths(gaps))} internal gap{?s} in {length(gaps)} series: {.val {unname(described)}}.",
    "i" = "koma cannot estimate on a series with an internal {.val NA}; these values are interpolated, not observed."
  ))

  for (name in names(gaps)) {
    x <- panel[[name]]
    values <- as.numeric(x)
    observed <- which(!is.na(values))
    span <- seq(min(observed), max(observed))
    values[span] <- stats::approx(observed, values[observed], xout = span)$y

    attrs <- get_custom_attrs(x)
    attrs[["ets_attributes"]] <- NULL
    filled <- stats::ts(values, start = stats::start(x), frequency = stats::frequency(x))
    panel[[name]] <- do.call(koma::as_ets, c(list(filled), attrs))
  }

  panel
}

#' Window a koma_ts, restoring its koma attributes
#'
#' So a `koma_ts` never silently degrades to a plain `ts`, which would make
#' `koma::estimate()` block on an interactive prompt (`docs/koma-api.md`
#' gotcha 1).
#'
#' @param x A `koma_ts`.
#' @param ... Passed to `stats::window()`.
#'
#' @return The windowed `koma_ts`.
#' @keywords internal
window_keeping_attrs <- function(x, ...) {
  attrs <- get_custom_attrs(x)
  do.call(koma::as_ets, c(list(stats::window(x, ...)), attrs))
}

#' Build a level index whose growth rate is a weighted average of others
#'
#' The series to use as the left-hand side of a koma **identity** that has no
#' observed counterpart (foreign demand, domestic demand, a regional
#' aggregate).
#'
#' koma estimates on growth rates: a series tagged `method = "diff_log"` is
#' converted by `rate()` to `100*diff(log(x))` before it reaches the sampler,
#' so an identity like `ea_gdp == 0.6*de_gdp + 0.4*fr_gdp` is a statement
#' about *growth rates*. Adding the levels instead would not satisfy it,
#' because `log(0.6*a + 0.4*b) != 0.6*log(a) + 0.4*log(b)`, and koma has no
#' identity-consistency check that would notice (see [identity_consistency()]).
#'
#' So this builds the weighted average in growth space and integrates back:
#' `g = sum_i w_i * diff(log(x_i))`, then `index = base * exp(cumsum(g))`.
#' koma's `level()` inverts `diff_log` as `exp(cumsum(x/100))*100`
#' (`docs/koma-api.md` §1), so `rate()` of the result reproduces `100*g`
#' exactly and the identity holds to machine precision.
#'
#' Weights are **not** renormalised: an identity's weights are a
#' specification choice, and rescaling them here would desync the data from
#' the equation. A set that does not sum to 1 gives an index that is not a
#' weighted average; `+1`/`-1` weights give exact differences.
#'
#' Ragged edges are preserved: leading/trailing periods where any component
#' is `NA` come back `NA` rather than poisoning the whole chain through
#' `cumsum()`. An *internal* `NA` is an error, since a chained index cannot
#' bridge one -- use [fill_internal_gaps()] first.
#'
#' @param panel A named list of `koma_ts` objects.
#' @param weights Named numeric vector keyed by **full variable name**, e.g.
#'   `c(de_gdp = 0.6, fr_gdp = 0.4)`.
#' @param base Index value at the first computable observation.
#'
#' @return A single `koma_ts` with `series_type = "level"` and
#'   `method = "diff_log"`, spanning the components' common window.
#' @export
chain_weighted_index <- function(panel, weights, base = 100) {
  if (length(weights) == 0 || is.null(names(weights)) || any(!nzchar(names(weights)))) {
    cli::cli_abort("{.arg weights} must be a non-empty vector named by variable.")
  }
  series_list <- lapply(names(weights), function(v) panel[[v]])
  missing <- names(weights)[vapply(series_list, is.null, logical(1))]
  if (length(missing) > 0) {
    cli::cli_abort("{.arg panel} is missing {.val {missing}}.")
  }

  # diff_log is meaningless on a series that is already a rate, and on a
  # non-positive one. Both are caller errors worth naming.
  types <- vapply(series_list, function(x) attr(x, "series_type") %||% NA_character_, character(1))
  not_level <- names(weights)[!identical(NA_character_, types) & types != "level"]
  if (length(not_level) > 0) {
    cli::cli_abort(c(
      "{.fn chain_weighted_index} needs {.field series_type} {.val level} components.",
      "x" = "{.val {not_level}} {?is/are} not."
    ))
  }
  non_positive <- names(weights)[vapply(series_list, function(x) any(x <= 0, na.rm = TRUE), logical(1))]
  if (length(non_positive) > 0) {
    cli::cli_abort("{.val {non_positive}} {?has/have} non-positive values; {.fn log} is undefined.")
  }

  freqs <- unique(vapply(series_list, stats::frequency, numeric(1)))
  if (length(freqs) != 1) {
    cli::cli_abort("All series being chained must share one frequency.")
  }
  common_start <- max(vapply(series_list, function(x) stats::tsp(x)[1], numeric(1)))
  common_end <- min(vapply(series_list, function(x) stats::tsp(x)[2], numeric(1)))
  if (common_start > common_end) {
    cli::cli_abort("{.arg weights} names series with no overlapping window.")
  }

  log_levels <- lapply(series_list, function(x) {
    log(as.numeric(stats::window(x, start = common_start, end = common_end)))
  })
  n <- length(log_levels[[1]])
  growth <- Reduce(`+`, Map(function(l, w) diff(l) * w, log_levels, as.numeric(weights)))

  observed <- which(!is.na(growth))
  if (length(observed) == 0) {
    cli::cli_abort("Every period is {.val NA} for at least one component; nothing to chain.")
  }
  first <- min(observed)
  last <- max(observed)
  if (anyNA(growth[first:last])) {
    cli::cli_abort(c(
      "A component has an internal {.val NA}; a chained index cannot bridge one.",
      "i" = "Run {.fn fill_internal_gaps} on {.arg panel} first."
    ))
  }

  index <- rep(NA_real_, n)
  index[first] <- base
  index[seq(first + 1L, last + 1L)] <- base * exp(cumsum(growth[first:last]))

  koma::as_ets(
    stats::ts(index, start = num_to_period(common_start, freqs), frequency = freqs),
    series_type = "level",
    method = "diff_log"
  )
}

#' Build a 0/1 quarter dummy shaped like the rest of the panel
#'
#' Named `covid_<year>q<quarter>`, e.g. `"covid_2020q2"`, and returned over
#' the same span and frequency as `template` so it lines up with everything
#' else and extends as zeros through the forecast window.
#'
#' Tagged `series_type = "rate", method = "none"` so koma's `rate()` passes it
#' through untouched: a dummy must reach the sampler as the 0/1 indicator it
#' is, not be differenced.
#'
#' @param name Dummy name, `covid_<year>q<quarter>`.
#' @param template A `koma_ts` whose span and frequency to copy.
#'
#' @return A `koma_ts` of zeros with a single 1.
#' @export
covid_dummy <- function(name, template) {
  parsed <- regmatches(name, regexec("^covid_([0-9]{4})q([1-4])$", name))[[1]]
  if (length(parsed) != 3) {
    cli::cli_abort("{.val {name}} is not of the form {.val covid_2020q2}.")
  }
  year <- as.integer(parsed[2])
  quarter <- as.integer(parsed[3])

  frequency <- stats::frequency(template)
  times <- stats::time(template)
  target <- year + (quarter - 1) / frequency
  hit <- which(abs(as.numeric(times) - target) < 1e-6)
  if (length(hit) == 0) {
    cli::cli_abort("{.val {name}} falls outside the panel's span.")
  }

  values <- rep(0, length(times))
  values[hit] <- 1
  koma::as_ets(
    stats::ts(values, start = stats::start(template), frequency = frequency),
    series_type = "rate", method = "none"
  )
}
