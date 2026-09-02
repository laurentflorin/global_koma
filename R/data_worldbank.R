# World Bank Global Economic Monitor (GEM) fetch layer.
#
# GEM is the closest analogue this project has found to EA-MD/QD for an
# economy outside the euro area and the US: a curated, cross-country,
# seasonally-adjusted macro panel at monthly and quarterly frequency, kept
# current (the 2026-03-31 vintage runs to 2025Q4 / 2026M02), served by a
# free, keyless, stable API. It is the primary source for China, which is
# why the stage-2d country set can exist at all -- see `stage2d_config()`
# and CLAUDE.md's "Stage 2d" section for what it does and does not cover.
#
# GEM lives in DataBank source 15, so every query carries `source=15`. That
# is load-bearing: the same indicator ids do not exist in the default WDI
# source, and omitting it returns an empty result rather than an error.
#
# Per CLAUDE.md's data-transformation policy everything here is returned in
# LEVELS. GEM does publish `*ZGY` percent-change variants of several
# indicators; they are deliberately not used.

#' Directory used to cache World Bank API responses
#'
#' Mirrors [eurostat_cache_dir()] and `fred_cache_path()`. Git-ignored via
#' `data/cache/*`.
#' @keywords internal
worldbank_cache_dir <- function() {
  path <- file.path("data", "cache", "worldbank")
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

#' Fetch one World Bank Global Economic Monitor series
#'
#' Downloads a single (indicator, country, frequency) GEM series and caches
#' the parsed observations under `data/cache/worldbank/`.
#'
#' **Frequency is part of the query, not a property of the indicator.** GEM
#' stores several indicators at both monthly and quarterly frequency and some
#' at only one: China's `CPTOTSAXN` (CPI) is monthly-only and comes back
#' *empty*, not missing, if asked for quarterly, while `NYGDPMKTPSAKN` (real
#' GDP) is quarterly-only. An empty response is therefore an error worth
#' naming here rather than an `NA` column three layers down.
#'
#' The API paginates at `per_page`; this follows every page, because a
#' 1995-2026 monthly series is ~375 observations and the default page size is
#' 50.
#'
#' @param indicator GEM indicator id, e.g. `"NYGDPMKTPSAKN"`.
#' @param iso3 ISO-3 country code, e.g. `"CHN"`.
#' @param frequency `"Q"` or `"M"`.
#' @param start_year,end_year Bounds on the requested window.
#' @param use_cache Logical; if `TRUE` (default) and a cached response exists,
#'   skip the network call.
#'
#' @return A `data.frame` with columns `period` (e.g. `"2024Q3"`) and
#'   `value`, ascending by period, with missing observations dropped.
#' @export
fetch_wb_gem <- function(indicator, iso3, frequency = c("Q", "M"),
                         start_year = 1990, end_year = as.integer(format(Sys.Date(), "%Y")) + 1L,
                         use_cache = TRUE) {
  frequency <- match.arg(frequency)
  stopifnot(is.character(indicator), length(indicator) == 1L)

  cache_path <- file.path(
    worldbank_cache_dir(),
    sprintf("gem_%s_%s_%s_%d_%d.rds", indicator, iso3, frequency, start_year, end_year)
  )
  if (use_cache && file.exists(cache_path)) {
    return(readRDS(cache_path))
  }

  date_range <- if (identical(frequency, "Q")) {
    sprintf("%dQ1:%dQ4", start_year, end_year)
  } else {
    sprintf("%dM01:%dM12", start_year, end_year)
  }

  collect <- function(page) {
    req <- httr2::request("https://api.worldbank.org/v2") |>
      httr2::req_url_path_append("country", iso3, "indicator", indicator) |>
      httr2::req_url_query(
        source = "15", format = "json", per_page = "1000",
        date = date_range, page = as.character(page)
      )
    httr2::resp_body_json(httr2::req_perform(req), simplifyVector = FALSE)
  }

  body <- collect(1)
  # The v2 API answers with a two-element array: [metadata, observations].
  # A malformed query answers with a one-element array carrying `message`,
  # which is why this is checked rather than indexed blindly.
  if (length(body) < 2 || is.null(body[[2]])) {
    cli::cli_abort(c(
      "World Bank GEM returned no data block for {.val {indicator}} / {.val {iso3}}.",
      "i" = "Response: {.val {utils::head(unlist(body), 3)}}"
    ))
  }
  observations <- body[[2]]
  pages <- body[[1]]$pages %||% 1L
  for (page in seq_len(pages)[-1]) {
    observations <- c(observations, collect(page)[[2]])
  }

  keep <- !vapply(observations, function(o) is.null(o$value), logical(1))
  if (!any(keep)) {
    cli::cli_abort(c(
      "World Bank GEM has no {frequency} observations for {.val {indicator}} / {.val {iso3}}.",
      "i" = "GEM stores some indicators at only one frequency; an empty answer is what the wrong one returns."
    ))
  }
  observations <- observations[keep]

  out <- data.frame(
    period = vapply(observations, function(o) o$date, character(1)),
    value = vapply(observations, function(o) as.numeric(o$value), numeric(1)),
    stringsAsFactors = FALSE
  )
  out <- out[order(out$period), ]
  rownames(out) <- NULL

  saveRDS(out, cache_path)
  out
}

#' Convert a GEM period label to a `ts` start
#'
#' GEM writes `"2024Q3"` for quarters and `"2024M07"` for months.
#' @keywords internal
wb_period_to_start <- function(period) {
  parsed <- regmatches(period, regexec("^([0-9]{4})(Q|M)([0-9]{1,2})$", period))[[1]]
  if (length(parsed) != 4) {
    cli::cli_abort("{.val {period}} is not a World Bank period label ({.val 2024Q3} / {.val 2024M07}).")
  }
  c(as.integer(parsed[2]), as.integer(parsed[4]))
}

#' Fetch a GEM series as a quarterly `ts`
#'
#' Wraps [fetch_wb_gem()] and, for a monthly indicator, aggregates to
#' quarterly with [eamdqd_aggregate_quarterly()] -- the same aggregator the
#' EA-MD/QD and ECB paths use, so a monthly price index reaches the panel the
#' same way `<iso2>_prices` does for a euro-area country.
#'
#' GEM series can have internal gaps (a country skipping a month). Those are
#' left in place as `NA` rather than interpolated here: [internal_gaps()] and
#' [fill_internal_gaps()] are where this project decides about invented
#' observations, and they warn when they act.
#'
#' @param indicator GEM indicator id.
#' @param iso3 ISO-3 country code.
#' @param frequency `"Q"` (taken as-is) or `"M"` (aggregated to quarterly).
#' @param aggregation `1` for a period average (a price index, a rate) or
#'   `2` for a sum (a flow), matching [eamdqd_aggregate_quarterly()].
#' @param ... Passed to [fetch_wb_gem()].
#'
#' @return A quarterly `stats::ts`.
#' @export
wb_gem_quarterly <- function(indicator, iso3, frequency = c("Q", "M"),
                             aggregation = 1, ...) {
  frequency <- match.arg(frequency)
  d <- fetch_wb_gem(indicator, iso3, frequency = frequency, ...)
  start <- wb_period_to_start(d$period[1])

  if (identical(frequency, "Q")) {
    # Re-index rather than trusting row order to be gap-free: a missing
    # quarter in the middle must become an NA, not silently shift every later
    # observation a quarter earlier.
    idx <- vapply(d$period, function(p) {
      s <- wb_period_to_start(p)
      (s[1] - start[1]) * 4L + (s[2] - start[2]) + 1L
    }, integer(1))
    values <- rep(NA_real_, max(idx))
    values[idx] <- d$value
    return(stats::ts(values, start = start, frequency = 4))
  }

  idx <- vapply(d$period, function(p) {
    s <- wb_period_to_start(p)
    (s[1] - start[1]) * 12L + (s[2] - start[2]) + 1L
  }, integer(1))
  values <- rep(NA_real_, max(idx))
  values[idx] <- d$value
  eamdqd_aggregate_quarterly(values, start = start, aggregation = aggregation)
}
