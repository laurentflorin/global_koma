# OECD SDMX fetch layer.
#
# Used for exactly one thing today: China's short- and long-term interest
# rates, from the Short-Term Economic Statistics financial-market dataflow
# (`OECD.SDD.STES,DSD_STES@DF_FINMARK`). China is a non-member "key partner"
# in that dataflow, which is why it has interest rates there but no
# expenditure-side national accounts anywhere in the OECD estate -- see
# `build_cn_panel()` and CLAUDE.md's "Stage 2d" section.
#
# The public endpoint takes no key and is served as SDMX-CSV, which is far
# easier to parse correctly than SDMX-ML and (unlike the JSON flavour) puts
# every dimension in a named column.

#' Directory used to cache OECD SDMX responses
#' @keywords internal
oecd_cache_dir <- function() {
  path <- file.path("data", "cache", "oecd")
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

#' Fetch one OECD SDMX query as a data frame
#'
#' **The number of key positions is checked by the server and is not
#' negotiable.** `DSD_STES@DF_FINMARK` takes nine dot-separated positions and
#' `DSD_NAMAIN1@DF_QNA` takes thirteen; supplying the wrong count returns HTTP
#' 422 with "Not enough key values in query", not an empty result. Pass the
#' full key including its trailing empty positions.
#'
#' @param dataflow Full dataflow reference, e.g.
#'   `"OECD.SDD.STES,DSD_STES@DF_FINMARK,"`.
#' @param key Dot-separated series key, e.g. `"CHN.Q.IRLT......"`.
#' @param start_period,end_period Optional SDMX period bounds
#'   (`"2000-Q1"`).
#' @param use_cache Logical; if `TRUE` (default) and a cached response exists
#'   under `data/cache/oecd/`, skip the network call.
#'
#' @return A `data.frame` of the SDMX-CSV response, one row per observation.
#' @export
fetch_oecd_sdmx <- function(dataflow, key, start_period = NULL, end_period = NULL,
                            use_cache = TRUE) {
  tag <- gsub("[^A-Za-z0-9]+", "_", paste(dataflow, key, start_period %||% "NA", end_period %||% "NA"))
  cache_path <- file.path(oecd_cache_dir(), paste0(substr(tag, 1, 180), ".rds"))
  if (use_cache && file.exists(cache_path)) {
    return(readRDS(cache_path))
  }

  req <- httr2::request("https://sdmx.oecd.org/public/rest/data") |>
    httr2::req_url_path_append(dataflow, key) |>
    httr2::req_url_query(
      startPeriod = start_period, endPeriod = end_period, format = "csvfile"
    )
  text <- httr2::resp_body_string(httr2::req_perform(req))
  out <- utils::read.csv(text = text, stringsAsFactors = FALSE)
  if (nrow(out) == 0) {
    cli::cli_abort("OECD query {.val {key}} on {.val {dataflow}} returned no observations.")
  }

  saveRDS(out, cache_path)
  out
}

#' Fetch one OECD short-term-statistics interest rate as a quarterly `ts`
#'
#' `DSD_STES@DF_FINMARK` measures, for a given reference area:
#'
#' | `measure` | meaning |
#' |---|---|
#' | `IRSTCI` | immediate / overnight interbank rate -- the policy-rate proxy |
#' | `IR3TIB` | three-month interbank rate |
#' | `IRLT` | long-term (usually 10-year) government bond yield |
#'
#' **Verified spans for China (2026-09 vintage)**: `IRSTCI` 1990Q1-2025Q2,
#' `IR3TIB` 1997Q3-2026Q2, `IRLT` **2014Q1**-2026Q2. That last one is the
#' constraint behind [cn_long_rate_series()]: China's actual 10-year yield
#' does not reach back far enough for any estimation window this project
#' uses, so the long rate is spliced rather than taken raw.
#'
#' @param ref_area OECD reference area code (ISO-3, e.g. `"CHN"`).
#' @param measure One of the measure codes above.
#' @param ... Passed to [fetch_oecd_sdmx()].
#'
#' @return A quarterly `stats::ts` in percent per annum.
#' @export
oecd_finmark_rate <- function(ref_area, measure, ...) {
  d <- fetch_oecd_sdmx(
    "OECD.SDD.STES,DSD_STES@DF_FINMARK,",
    sprintf("%s.Q.%s.......", ref_area, measure),
    ...
  )
  d <- d[d$MEASURE == measure, ]
  if (nrow(d) == 0) {
    cli::cli_abort("OECD {.val {measure}} is not published for {.val {ref_area}}.")
  }
  oecd_to_ts(d)
}

#' Convert an SDMX-CSV data frame with `TIME_PERIOD`/`OBS_VALUE` to a `ts`
#'
#' Re-indexes on the period label rather than trusting row order: the OECD
#' returns observations in an arbitrary order, and a series with a gap would
#' otherwise silently shift every later observation.
#' @keywords internal
oecd_to_ts <- function(d) {
  parsed <- regmatches(d$TIME_PERIOD, regexec("^([0-9]{4})-Q([1-4])$", d$TIME_PERIOD))
  bad <- d$TIME_PERIOD[lengths(parsed) != 3]
  if (length(bad) > 0) {
    cli::cli_abort("Unexpected OECD period label{?s}: {.val {unique(bad)}}; expected {.val 2024-Q1}.")
  }
  year <- vapply(parsed, function(p) as.integer(p[2]), integer(1))
  quarter <- vapply(parsed, function(p) as.integer(p[3]), integer(1))

  index <- year * 4L + (quarter - 1L)
  start_index <- min(index)
  values <- rep(NA_real_, max(index) - start_index + 1L)
  values[index - start_index + 1L] <- as.numeric(d$OBS_VALUE)

  stats::ts(values, start = c(start_index %/% 4L, start_index %% 4L + 1L), frequency = 4)
}
