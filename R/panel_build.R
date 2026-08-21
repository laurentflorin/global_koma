# Assemble raw per-source series into koma_ts panels keyed by project
# variable name (see equations.R for the naming convention).

#' Build one country's panel
#'
#' Combines the raw FRED and EA-MD/QD series belonging to a single country
#' into a named list of `koma::koma_ts` objects, keyed by
#' `<iso2>_<concept>` names.
#'
#' @param iso2 Two-letter lowercase ISO country code.
#' @param series_map A `data.frame` describing which raw source and code
#'   maps to which concept for this country (source, code, concept,
#'   series_type, method).
#' @param raw_dir Directory holding cached raw series (default
#'   `"data/raw"`).
#'
#' @return A named list of `koma_ts` objects.
#' @export
build_country_panel <- function(iso2, series_map, raw_dir = "data/raw") {
  stop("not implemented", call. = FALSE)
}

#' Build the full multi-country panel
#'
#' Combines [build_country_panel()] output across all countries with the
#' shared (`ea_` / `world_` / unprefixed) series into one `ts_data` list
#' suitable for `koma::estimate()`.
#'
#' @param countries Character vector of ISO-2 country codes.
#' @param shared_series_map A `data.frame` describing the shared series
#'   (source, code, concept, scope, series_type, method).
#' @param raw_dir Directory holding cached raw series (default
#'   `"data/raw"`).
#'
#' @return A named list of `koma_ts` objects, validated with
#'   [is_valid_project_name()].
#' @export
build_global_panel <- function(countries, shared_series_map, raw_dir = "data/raw") {
  stop("not implemented", call. = FALSE)
}

#' Align a panel to a common frequency and sample
#'
#' Thin wrapper that windows every series in a panel to a common start/end
#' and asserts a single shared frequency, mirroring what
#' `koma::estimate()` requires.
#'
#' @param panel A named list of `koma_ts` objects.
#' @param start,end `c(year, period)` bounds.
#'
#' @return The windowed panel, same names as `panel`.
#' @export
align_panel <- function(panel, start, end) {
  stop("not implemented", call. = FALSE)
}
