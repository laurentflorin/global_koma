# DBnomics fetch layer, used for IMF Direction of Trade Statistics.
#
# WHY A MIRROR. This project prefers primary sources and every other fetcher
# here talks to one. DOTS is the exception: the IMF's legacy SDMX endpoint
# (dataservices.imf.org) no longer responds at all, and its replacement
# (api.imf.org) does not currently serve the DOT dataflow -- verified
# 2026-09, `IMF.STA,DOT` returns "No such dataflow found". DBnomics mirrors
# it, keyless and stable, and preserves the upstream series identifiers
# verbatim, so `A.US.TXG_FOB_USD.DE` here is the same key it is at the IMF.
# If the IMF restores a working endpoint, only fetch_dbnomics_series() has to
# change; the identifiers callers pass do not.

#' Directory used to cache DBnomics responses
#' @keywords internal
dbnomics_cache_dir <- function() {
  path <- file.path("data", "cache", "dbnomics")
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

#' Fetch one DBnomics series
#'
#' @param provider Provider code, e.g. `"IMF"`.
#' @param dataset Dataset code, e.g. `"DOT"`.
#' @param series Series code in the provider's own notation, e.g.
#'   `"A.US.TXG_FOB_USD.DE"`.
#' @param use_cache Logical; if `TRUE` (default) and a cached response exists,
#'   skip the network call.
#'
#' @return A `data.frame` with columns `period` and `value`, ascending, with
#'   missing observations dropped. `NULL` if the series does not exist --
#'   which is a real answer for a bilateral trade pair, not an error: DOTS has
#'   no series for country pairs that never traded, and a caller asking for a
#'   matrix of them needs to distinguish "no trade" from "request failed".
#' @export
fetch_dbnomics_series <- function(provider, dataset, series, use_cache = TRUE) {
  cache_path <- file.path(
    dbnomics_cache_dir(),
    paste0(gsub("[^A-Za-z0-9._-]+", "_", paste(provider, dataset, series, sep = "_")), ".rds")
  )
  if (use_cache && file.exists(cache_path)) {
    return(readRDS(cache_path))
  }

  req <- httr2::request("https://api.db.nomics.world/v22/series") |>
    httr2::req_url_path_append(provider, dataset, series) |>
    httr2::req_url_query(observations = "true")
  body <- httr2::resp_body_json(httr2::req_perform(req), simplifyVector = FALSE)

  docs <- body$series$docs
  out <- if (length(docs) == 0) {
    NULL
  } else {
    periods <- unlist(docs[[1]]$period, use.names = FALSE)
    # DBnomics writes a missing observation as the STRING "NA", not as JSON
    # null, so a naive as.numeric() would produce a warning and an NA that
    # looks like a parse failure. Filter on the raw value first.
    raw <- docs[[1]]$value
    keep <- !vapply(raw, function(v) is.null(v) || identical(v, "NA"), logical(1))
    if (!any(keep)) {
      NULL
    } else {
      data.frame(
        period = periods[keep],
        value = as.numeric(unlist(raw[keep], use.names = FALSE)),
        stringsAsFactors = FALSE
      )
    }
  }

  saveRDS(out, cache_path)
  out
}

#' Bilateral trade weights for one reporter, from IMF DOTS
#'
#' The overall (import plus export) share of a reporter's **total merchandise
#' trade** that is with each partner, averaged over the most recent `window`
#' years, with the remainder carried as a `"row"` residual:
#'
#' ```
#' w_ij = mean_t (X_ijt + M_ijt) / (X_i,world,t + M_i,world,t)
#' ```
#'
#' **This is what [build_trade_weight_matrix()]'s reciprocal approximation was
#' standing in for, and it is strictly better where it is available.** The ECB
#' WTS dataflow computes weights *for euro-area reporters only*, so the US row
#' -- and, from stage 2d, China's -- had to be built from the reciprocal of
#' each euro-area country's own weight on that country, then renormalised.
#' Renormalising is where it goes wrong: it forces the modelled partners to
#' account for the reporter's entire trade. The stage-2b matrix on disk gives
#' the United States a rest-of-world weight of **4.6%** as a result, and
#' Ireland a 24.7% share of US trade -- both plainly false, and both recorded
#' in CLAUDE.md as known-unreliable. DOTS observes the same quantity directly,
#' world total included, so the residual is real.
#'
#' Two differences from ECB WTS worth stating, since the two now coexist in one
#' matrix. DOTS is **goods only**, where WTS covers manufactured products *and
#' services* (`TRD_PRODUCT = TMS`); and DOTS is a plain trade share where WTS
#' is a double-weighted effective-exchange-rate weight that also accounts for
#' third-market competition. So a `dots` row and a `direct` row are not the
#' same statistic. They are close enough to sit in one matrix -- both are
#' "share of this country's trade that is with that country", each row sums to
#' 1, and the `source` attribute records which is which -- but a comparison of
#' one row against another should say so.
#'
#' @param reporter Reporter ISO-2 code, lowercase (`"us"`, `"cn"`).
#' @param partners Partner ISO-2 codes, lowercase.
#' @param window Number of most recent annual observations to average.
#' @param end_year Latest year to use. DOTS revises, and the most recent year
#'   is often partial for some partners; defaults to two years back.
#'
#' @return A named numeric vector over `partners` plus `"row"`, summing to 1.
#' @export
imf_dots_weights <- function(reporter, partners, window = 3,
                             end_year = as.integer(format(Sys.Date(), "%Y")) - 2L) {
  reporter <- toupper(reporter)
  years <- as.character(seq(end_year - window + 1L, end_year))

  flow_total <- function(counterpart) {
    parts <- lapply(c("TXG_FOB_USD", "TMG_CIF_USD"), function(indicator) {
      d <- fetch_dbnomics_series(
        "IMF", "DOT", sprintf("A.%s.%s.%s", reporter, indicator, counterpart)
      )
      if (is.null(d)) {
        return(stats::setNames(rep(0, length(years)), years))
      }
      values <- stats::setNames(d$value, d$period)[years]
      values[is.na(values)] <- 0
      stats::setNames(as.numeric(values), years)
    })
    parts[[1]] + parts[[2]]
  }

  world <- flow_total("W00")
  if (all(world == 0)) {
    cli::cli_abort(c(
      "IMF DOTS has no world total for reporter {.val {reporter}} over {.val {years}}.",
      "i" = "Without it a share cannot be formed; check the reporter code and {.arg end_year}."
    ))
  }

  weights <- vapply(partners, function(p) {
    mean(flow_total(toupper(p)) / world)
  }, numeric(1))
  names(weights) <- partners

  residual <- 1 - sum(weights)
  if (residual < 0) {
    cli::cli_abort(c(
      "DOTS partner shares for {.val {reporter}} sum to {.val {round(sum(weights), 3)}}, above 1.",
      "i" = "That should be impossible against the {.val W00} world total; the vintage may mix definitions."
    ))
  }
  c(weights, row = residual)
}
