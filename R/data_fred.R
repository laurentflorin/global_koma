# FRED (Federal Reserve Economic Data) fetch layer.
#
# The API key lives in .Renviron as FRED_API_KEY. It must never be
# committed, logged, or printed -- see CLAUDE.md.
#
# FRED series are not vintage-stamped the way EA-MD/QD is (see
# data_eamdqd.R): a given series_id is a live, continuously revised
# endpoint, not a dated release. So there is no vintage manifest here --
# each series is cached individually under data/cache/fred/, keyed by
# series_id (and by the start/end window requested, since a narrower
# window is a different query), and re-fetched whenever `use_cache =
# FALSE` or no cache file exists.

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
  key <- Sys.getenv("FRED_API_KEY", unset = NA_character_)
  if (is.na(key) || !nzchar(key)) {
    cli::cli_abort(c(
      "!" = "{.envvar FRED_API_KEY} is not set.",
      "i" = "Copy {.file .Renviron.example} to {.file .Renviron} and set {.envvar FRED_API_KEY} to a key from {.url https://fred.stlouisfed.org/docs/api/api_key.html}.",
      "i" = "{.file .Renviron} is git-ignored; never commit a key literal anywhere in the repo -- see CLAUDE.md."
    ))
  }
  invisible(key)
}

#' Cache file path for a FRED series
#'
#' @param series_id FRED series identifier.
#' @param start_date,end_date As passed to [fetch_fred_series()]; folded
#'   into the cache key because a narrower window is a different query.
#'
#' @return A path under `data/cache/fred/`.
#' @keywords internal
fred_cache_path <- function(series_id, start_date = NULL, end_date = NULL) {
  tag <- paste(series_id, start_date %||% "NA", end_date %||% "NA", sep = "_")
  file.path("data", "cache", "fred", paste0(tag, ".json"))
}

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Fetch a single FRED series
#'
#' Downloads one series from the FRED `series/observations` endpoint and
#' caches the raw JSON response under `data/cache/fred/`. Does not
#' transform the series into a `koma_ts` -- see `panel_build.R` for that
#' step.
#'
#' @param series_id FRED series identifier, e.g. `"GDPC1"`.
#' @param start_date Optional `Date` or `"YYYY-MM-DD"` string.
#' @param end_date Optional `Date` or `"YYYY-MM-DD"` string.
#' @param use_cache Logical; if `TRUE` (default) and a cached response
#'   exists under `data/cache/`, skip the network call.
#'
#' @return A `data.frame` with columns `date` and `value`, ascending by
#'   date. Observations FRED marks missing (`"."`) are dropped, not
#'   coerced to `NA` silently mixed in with real gaps -- callers see only
#'   observed periods, matching [extract_eamdqd_series()]'s contract.
#' @export
fetch_fred_series <- function(series_id, start_date = NULL, end_date = NULL,
                              use_cache = TRUE) {
  stopifnot(is.character(series_id), length(series_id) == 1L)
  cache_path <- fred_cache_path(series_id, start_date, end_date)

  if (use_cache && file.exists(cache_path)) {
    body <- jsonlite::read_json(cache_path, simplifyVector = FALSE)
  } else {
    key <- fred_api_key()
    req <- httr2::request("https://api.stlouisfed.org/fred/series/observations") |>
      httr2::req_url_query(
        series_id = series_id,
        api_key = key,
        file_type = "json",
        observation_start = start_date,
        observation_end = end_date
      ) |>
      httr2::req_error(body = function(resp) {
        # FRED's error body may itself echo query params; strip anything
        # that could be the key rather than trust it never appears.
        msg <- tryCatch(httr2::resp_body_json(resp)$error_message, error = function(e) NULL)
        if (is.null(msg)) return("request failed")
        gsub(key, "<redacted>", msg, fixed = TRUE)
      })

    resp <- httr2::req_perform(req)
    body <- httr2::resp_body_json(resp, simplifyVector = FALSE)

    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(body, cache_path, auto_unbox = TRUE, pretty = TRUE)
  }

  obs <- body$observations
  if (length(obs) == 0) {
    cli::cli_abort("FRED series {.val {series_id}} returned no observations.")
  }

  dates <- as.Date(vapply(obs, function(o) o$date, character(1)))
  values <- vapply(obs, function(o) o$value, character(1))
  keep <- values != "."
  if (!all(keep)) {
    cli::cli_inform("{.val {series_id}}: dropping {sum(!keep)} FRED-flagged missing observation{?s} ({.val .}).")
  }

  out <- data.frame(date = dates[keep], value = as.numeric(values[keep]), stringsAsFactors = FALSE)
  out[order(out$date), ]
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
  stats::setNames(
    lapply(series_ids, fetch_fred_series, ...),
    series_ids
  )
}
