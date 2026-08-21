# Euro Area Monthly/Quarterly Database (EA-MD/QD) fetch and mapping layer.
#
# EA-MD/QD (Barigozzi, Conti & Luciani) is the euro-area analogue of
# FRED-MD/QD: a standardised, vintage-stamped panel of euro-area and
# member-state series. We use it for series FRED does not carry directly
# for euro-area member states.

#' Fetch an EA-MD/QD vintage
#'
#' Downloads one vintage of the EA-MD/QD dataset and caches the raw file
#' under `data/cache/`.
#'
#' @param vintage `"latest"` (default) or a vintage date string
#'   (`"YYYY-MM"`) as published by the EA-MD/QD maintainers.
#' @param use_cache Logical; if `TRUE` (default) and a cached vintage
#'   exists under `data/cache/`, skip the network call.
#'
#' @return A `data.frame` in the EA-MD/QD wide format (one date column plus
#'   one column per raw series code).
#' @export
fetch_eamdqd <- function(vintage = "latest", use_cache = TRUE) {
  stop("not implemented", call. = FALSE)
}

#' Map of EA-MD/QD codes to project variable names
#'
#' Returns the translation table from raw EA-MD/QD series codes to this
#' project's `<iso2>_<concept>` / `ea_<concept>` naming convention (see
#' `equations.R` and CLAUDE.md).
#'
#' @return A `data.frame` with columns `eamdqd_code`, `project_name`,
#'   `series_type` (`"level"` or `"rate"`), `method` (koma `rate()`/`level()`
#'   method, e.g. `"diff_log"`).
#' @export
eamdqd_variable_map <- function() {
  stop("not implemented", call. = FALSE)
}

#' Extract one mapped series from a raw EA-MD/QD vintage
#'
#' @param eamdqd_data Output of [fetch_eamdqd()].
#' @param eamdqd_code Raw EA-MD/QD series code to extract.
#'
#' @return A `data.frame` with columns `date` and `value`.
#' @export
extract_eamdqd_series <- function(eamdqd_data, eamdqd_code) {
  stop("not implemented", call. = FALSE)
}
