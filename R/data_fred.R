# FRED (Federal Reserve Economic Data) fetch layer.
#
# The API key lives in .Renviron as FRED_API_KEY. It must never be
# committed, logged, or printed -- see CLAUDE.md.

#' Read the FRED API key from the environment
#'
#' Reads `FRED_API_KEY` from the environment (populated from `.Renviron` by
#' R at session start). Aborts with an informative, key-free message if it
#' is unset. The key itself is never included in any message, log, or
#' returned value's `print()`/`format()` representation.
#'
#' @return The API key as a single string, invisibly.
#' @export
fred_api_key <- function() {
  stop("not implemented", call. = FALSE)
}

#' Fetch a single FRED series
#'
#' Downloads one series from the FRED `series/observations` endpoint and
#' caches the raw JSON response under `data/cache/`. Does not transform the
#' series into a `koma_ts` -- see `panel_build.R` for that step.
#'
#' @param series_id FRED series identifier, e.g. `"CPALTT01DEQ657N"`.
#' @param start_date Optional `Date` or `"YYYY-MM-DD"` string.
#' @param end_date Optional `Date` or `"YYYY-MM-DD"` string.
#' @param use_cache Logical; if `TRUE` (default) and a cached response
#'   exists under `data/cache/`, skip the network call.
#'
#' @return A `data.frame` with columns `date` and `value`.
#' @export
fetch_fred_series <- function(series_id, start_date = NULL, end_date = NULL,
                              use_cache = TRUE) {
  stop("not implemented", call. = FALSE)
}

#' Fetch multiple FRED series
#'
#' Vectorised wrapper around [fetch_fred_series()].
#'
#' @param series_ids Character vector of FRED series identifiers.
#' @param ... Passed to [fetch_fred_series()].
#'
#' @return A named list of `data.frame`s, one per `series_ids` element.
#' @export
fetch_fred_series_batch <- function(series_ids, ...) {
  stop("not implemented", call. = FALSE)
}

#' Cache file path for a FRED series
#'
#' @param series_id FRED series identifier.
#'
#' @return A path under `data/cache/fred/`.
#' @keywords internal
fred_cache_path <- function(series_id) {
  stop("not implemented", call. = FALSE)
}
