# Euro Area Monthly/Quarterly Database (EA-MD/QD) fetch, codebook, and
# transformation layer.
#
# EA-MD/QD (Barigozzi, Lissona & Tonni, 2024) is the euro-area analogue of
# FRED-MD/QD: a vintage-stamped panel of euro-area and member-state series,
# published monthly on Zenodo (concept DOI 10.5281/zenodo.10514667) and
# mirrored at https://www.barigozzi.eu/EA.html. We use it for series FRED
# does not carry directly for euro-area member states.
#
# Each vintage ships as one ZIP containing: one Excel file per economy (EA
# aggregate + AT, BE, DE, EL, ES, FR, IE, IT, NL, PT), each with a `data`
# sheet (the raw unbalanced panel) and an `info` sheet (per-series
# metadata: Name, per-country row ID, Frequency, Source, SA, SA_d,
# Aggregation, three transformation-code sets TR1/TR2/TR3, Class);
# `_data_description.pdf` (the codebook: series descriptions, units, and
# the "light" transformation codes, by series and by country);
# `_ReadME.pdf` (methodology); and `routine_data.m`/`routine_data.py`
# (the reference implementations we are porting).
#
# IMPORTANT: the `info` sheet's `ID` column is a per-country row sequence
# number (1, 2, 3, ... for whatever series that country happens to carry),
# NOT a stable cross-country/cross-source key. Matching a series across the
# codebook PDF and the per-country `info` sheets must be done on the
# alphabetic series code (`Name` with its trailing `_<COUNTRY>` stripped),
# not on that numeric ID -- see `eamdqd_read_info_sheets()`.

eamdqd_zenodo_concept_id <- "10514667"

#' Directory used to cache EA-MD/QD downloads
#' @keywords internal
eamdqd_cache_dir <- function() {
  file.path("data", "cache", "eamdqd")
}

#' Path to the fetch manifest recording every vintage retrieved so far
#' @keywords internal
eamdqd_manifest_path <- function() {
  file.path(eamdqd_cache_dir(), "manifest.json")
}

#' Resolve the latest EA-MD/QD release record from Zenodo
#'
#' Follows the stable concept-DOI record (10.5281/zenodo.10514667) to
#' whichever specific version is currently newest.
#'
#' @return The parsed Zenodo record (a list), as returned by the Zenodo
#'   REST API.
#' @keywords internal
eamdqd_zenodo_latest_record <- function() {
  url <- sprintf("https://zenodo.org/api/records/%s/versions/latest", eamdqd_zenodo_concept_id)
  resp <- httr2::request(url) |>
    httr2::req_user_agent("globalkoma (https://github.com/) data_eamdqd.R") |>
    httr2::req_perform()
  httr2::resp_body_json(resp)
}

#' Resolve a specific EA-MD/QD vintage record from Zenodo
#'
#' @param vintage `"latest"`, or a version label as published by Zenodo
#'   (e.g. `"07.2026"`).
#'
#' @return The parsed Zenodo record (a list) for the requested vintage.
#' @keywords internal
eamdqd_zenodo_find_record <- function(vintage) {
  latest <- eamdqd_zenodo_latest_record()
  if (identical(vintage, "latest")) {
    return(latest)
  }

  page <- 1L
  max_pages <- 20L
  repeat {
    url <- sprintf("https://zenodo.org/api/records/%s/versions", latest$id)
    resp <- httr2::request(url) |>
      httr2::req_url_query(size = 25, page = page) |>
      httr2::req_user_agent("globalkoma (https://github.com/) data_eamdqd.R") |>
      httr2::req_perform()
    body <- httr2::resp_body_json(resp)
    hits <- body$hits$hits
    if (length(hits) == 0) break
    for (h in hits) {
      if (identical(h$metadata$version, vintage)) {
        return(h)
      }
    }
    page <- page + 1L
    if (page > max_pages) break
  }

  cli::cli_abort(c(
    "!" = "EA-MD/QD vintage {.val {vintage}} was not found on Zenodo.",
    "i" = "Use {.val latest}, or see https://zenodo.org/records/{latest$id}/versions for available labels."
  ))
}

#' Read and update the EA-MD/QD fetch manifest
#'
#' @param entry A list describing one fetched vintage, keyed by `vintage`.
#'
#' @return Invisibly, the full updated manifest (a list of vintage entries).
#' @keywords internal
eamdqd_manifest_upsert <- function(entry) {
  path <- eamdqd_manifest_path()
  manifest <- if (file.exists(path)) {
    jsonlite::read_json(path, simplifyVector = FALSE)
  } else {
    list()
  }
  manifest[[entry$vintage]] <- entry

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(manifest, path, auto_unbox = TRUE, pretty = TRUE)

  invisible(manifest)
}

#' Fetch an EA-MD/QD vintage
#'
#' Downloads one vintage of the EA-MD/QD dataset from Zenodo, caches the ZIP
#' and its extracted contents under `data/cache/eamdqd/`, and records the
#' vintage (Zenodo record id/DOI, publication date, file checksum, download
#' timestamp) in `data/cache/eamdqd/manifest.json` so results are
#' reproducible: re-running with the same `vintage` (including `"latest"`
#' resolved to a specific record at fetch time) always returns the exact
#' same files.
#'
#' This function only downloads and unpacks; it does not read or transform
#' any series. See [eamdqd_codebook()] for the series/transformation-code
#' table, and `vignette("panel_build")`-equivalent functions in
#' `panel_build.R` for turning the per-country Excel files into `koma_ts`
#' panels.
#'
#' @param vintage `"latest"` (default), or a Zenodo version label (e.g.
#'   `"07.2026"`) to pin a specific past vintage.
#' @param use_cache Logical; if `TRUE` (default) and a cached ZIP with a
#'   matching checksum already exists, skip the network call.
#'
#' @return An object of class `eamdqd_vintage`: a list with `vintage`
#'   (resolved version label), `manifest` (the entry just written), `root`
#'   (path to the extracted vintage folder), `xlsx` (named vector of
#'   per-country Excel file paths, named by ISO-2/`"EA"`), `codebook_pdf`,
#'   and `readme_pdf`.
#' @export
fetch_eamdqd <- function(vintage = "latest", use_cache = TRUE) {
  record <- eamdqd_zenodo_find_record(vintage)
  version <- record$metadata$version
  files <- record$files
  if (length(files) != 1) {
    cli::cli_abort("Expected exactly one file in EA-MD/QD vintage {.val {version}}, found {length(files)}.")
  }
  file_info <- files[[1]]
  zip_name <- file_info$key
  expected_md5 <- sub("^md5:", "", file_info$checksum)

  vintage_dir <- file.path(eamdqd_cache_dir(), version)
  dir.create(vintage_dir, recursive = TRUE, showWarnings = FALSE)
  zip_path <- file.path(vintage_dir, zip_name)

  needs_download <- TRUE
  if (use_cache && file.exists(zip_path)) {
    actual_md5 <- unname(tools::md5sum(zip_path))
    needs_download <- !identical(actual_md5, expected_md5)
  }

  if (needs_download) {
    cli::cli_inform("Downloading EA-MD/QD vintage {.val {version}} ({file_info$size} bytes)...")
    httr2::request(file_info$links$self) |>
      httr2::req_user_agent("globalkoma (https://github.com/) data_eamdqd.R") |>
      httr2::req_perform(path = zip_path)

    actual_md5 <- unname(tools::md5sum(zip_path))
    if (!identical(actual_md5, expected_md5)) {
      cli::cli_abort(c(
        "!" = "Downloaded EA-MD/QD ZIP for vintage {.val {version}} failed checksum verification.",
        "x" = "expected md5 {.val {expected_md5}}, got {.val {actual_md5}}.",
        "i" = "The download may be corrupt or the Zenodo record may have changed; delete {.file {zip_path}} and retry."
      ))
    }
  }

  extract_dir <- file.path(vintage_dir, "extracted")
  if (!dir.exists(extract_dir) || length(list.files(extract_dir, recursive = TRUE)) == 0) {
    utils::unzip(zip_path, exdir = extract_dir)
  }
  root_candidates <- list.dirs(extract_dir, recursive = FALSE)
  if (length(root_candidates) != 1) {
    cli::cli_abort("Expected exactly one top-level folder in the EA-MD/QD ZIP, found {length(root_candidates)}.")
  }
  root <- root_candidates[[1]]

  countries <- c("EA", "AT", "BE", "DE", "EL", "ES", "FR", "IE", "IT", "NL", "PT")
  xlsx_paths <- stats::setNames(file.path(root, paste0(countries, "data.xlsx")), countries)
  missing <- xlsx_paths[!file.exists(xlsx_paths)]
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "!" = "Missing expected per-country Excel file(s) in EA-MD/QD vintage {.val {version}}:",
      "x" = paste(missing, collapse = ", ")
    ))
  }

  manifest_entry <- list(
    vintage = version,
    zenodo_record_id = record$id,
    zenodo_doi = record$doi,
    zenodo_concept_doi = record$conceptdoi %||% NA_character_,
    publication_date = record$metadata$publication_date,
    file_name = zip_name,
    file_size_bytes = file_info$size,
    file_checksum_md5 = expected_md5,
    fetched_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    zip_path = zip_path,
    extract_dir = root
  )
  eamdqd_manifest_upsert(manifest_entry)

  structure(
    list(
      vintage = version,
      manifest = manifest_entry,
      root = root,
      xlsx = xlsx_paths,
      codebook_pdf = file.path(root, "_data_description.pdf"),
      readme_pdf = file.path(root, "_ReadME.pdf")
    ),
    class = "eamdqd_vintage"
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Parse the "Data Description and Transformation by country" table
#'
#' Extracts Table 2 of `_data_description.pdf` (series code, description,
#' unit, seasonal adjustment, frequency, source, class, and the "light"
#' transformation code per country) using `pdftools`' layout-preserving
#' text extraction, which -- unlike naive text extraction of this
#' particular PDF -- keeps inter-word spacing intact.
#'
#' Each data row ends in exactly 11 single-character country codes (one of
#' `1`-`6` or `-`, for EA, AT, BE, DE, EL, ES, FR, IE, IT, NL, PT in that
#' fixed order); everything between the row's numeric index and that block
#' is parsed positionally from the right: the last 4 whitespace-separated
#' tokens are Unit/SA/Frequency/Source/Class... (see body) with the
#' remainder joined back into the free-text description.
#'
#' @param pdf_path Path to `_data_description.pdf`.
#'
#' @return A `data.frame` with one row per series (not per series-country):
#'   `n` (the PDF's own numeric index, table order -- NOT comparable across
#'   files, see the module-level note), `id` (alphabetic series code),
#'   `description`, `unit`, `sa`, `frequency`, `source`, `class`, and one
#'   `tr_light_<COUNTRY>` column per country holding the light-scheme
#'   transformation code (or `NA` where the series is unavailable for that
#'   country).
#' @keywords internal
eamdqd_parse_codebook_pdf <- function(pdf_path) {
  pages <- pdftools::pdf_text(pdf_path)
  country_cols <- c("EA", "AT", "BE", "DE", "EL", "ES", "FR", "IE", "IT", "NL", "PT")

  table_pages <- Filter(function(p) grepl("Table 2:", p, fixed = TRUE), pages)
  if (length(table_pages) == 0) {
    cli::cli_abort("Could not find \"Table 2\" in {.file {pdf_path}}; the codebook PDF layout may have changed.")
  }

  row_re <- paste0(
    "^\\s*([0-9]+)\\s+(\\S+)\\s+(.*\\S)\\s+",
    paste(rep("([0-9-])", length(country_cols)), collapse = "\\s+"),
    "\\s*$"
  )

  rows <- list()
  for (page_text in table_pages) {
    lines <- strsplit(page_text, "\n")[[1]]
    for (ln in lines) {
      if (!grepl(row_re, ln, perl = TRUE)) next
      m <- regmatches(ln, regexec(row_re, ln, perl = TRUE))[[1]]
      rest_tokens <- strsplit(trimws(m[4]), "\\s+")[[1]]
      n_rest <- length(rest_tokens)
      if (n_rest < 5) next # malformed row; skip rather than guess

      rows[[length(rows) + 1]] <- c(
        list(
          n = as.integer(m[2]),
          id = m[3],
          description = paste(rest_tokens[seq_len(n_rest - 5)], collapse = " "),
          unit = rest_tokens[n_rest - 4],
          sa = rest_tokens[n_rest - 3],
          frequency = rest_tokens[n_rest - 2],
          source = rest_tokens[n_rest - 1],
          class = rest_tokens[n_rest]
        ),
        stats::setNames(as.list(m[5:(4 + length(country_cols))]), paste0("tr_light_", country_cols))
      )
    }
  }

  if (length(rows) == 0) {
    cli::cli_abort("Parsed zero rows from Table 2 in {.file {pdf_path}}; the codebook PDF layout may have changed.")
  }

  out <- do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))
  tr_cols <- paste0("tr_light_", country_cols)
  out[tr_cols] <- lapply(out[tr_cols], function(x) suppressWarnings(as.integer(x)))
  out
}

#' Read and combine every country's `info` sheet
#'
#' Ports the metadata half of what `routine_data.py`/`routine_data.m` read
#' per country (`info <- pd.read_excel(f"{sheet}data.xlsx", sheet="info")`)
#' into one long table across all countries. See the module-level note on
#' why matching is done on the alphabetic code, not the sheet's own `ID`
#' column.
#'
#' @param xlsx_paths Named vector of per-country Excel paths (as returned
#'   by [fetch_eamdqd()]$xlsx).
#'
#' @return A `data.frame`, one row per (series, country), with columns
#'   `code` (alphabetic series code), `country`, `name` (raw `Name` cell,
#'   e.g. `"GDP_EA"`), `frequency`, `source`, `sa`, `sa_d`, `aggregation`,
#'   `tr_heavy` (TR1), `tr_light` (TR2), `tr_blt` (TR3), `class`.
#' @keywords internal
eamdqd_read_info_sheets <- function(xlsx_paths) {
  countries <- names(xlsx_paths)
  info <- lapply(countries, function(cc) {
    df <- as.data.frame(readxl::read_excel(xlsx_paths[[cc]], sheet = "info"))
    df$country <- cc
    suffix_re <- paste0("_", cc, "$")
    df$code <- ifelse(grepl(suffix_re, df$Name), sub(suffix_re, "", df$Name), df$Name)
    df
  })
  out <- do.call(rbind, info)

  data.frame(
    code = out$code,
    country = out$country,
    name = out$Name,
    frequency = out$Frequency,
    source = out$Source,
    sa = out$SA,
    sa_d = out$SA_d,
    aggregation = out$Aggregation,
    tr_heavy = out$TR1,
    tr_light = out$TR2,
    tr_blt = out$TR3,
    class = out$Class,
    stringsAsFactors = FALSE
  )
}

#' Build the EA-MD/QD codebook
#'
#' Combines every country's `info` sheet (the authoritative source for
#' per-series, per-country transformation codes -- `tr_heavy`/`tr_light`/
#' `tr_blt`, corresponding to the "heavy", "light", and "BLT" transformation
#' sets described in `_ReadME.pdf`) with the series descriptions and units
#' parsed from `_data_description.pdf` (which the `info` sheets do not
#' carry), and writes the result to a CSV so the series list and
#' transformation-code mapping can be reviewed before it is used for
#' anything downstream (see [eamdqd_variable_map()]).
#'
#' A handful of series codes appear in the `info` sheets but not in the PDF
#' table (or vice versa) -- typically `_EACC` euro-area "changing
#' composition" variants, or a spelling difference between the PDF and the
#' Excel `Name` column (e.g. `GCFC` vs `GFCF`). These rows are kept with
#' `description`/`unit` left `NA` rather than dropped, and a warning lists
#' the affected codes -- silently dropping or guess-matching them would
#' hide a real mismatch that a human should look at.
#'
#' @param eamdqd An `eamdqd_vintage` object, as returned by
#'   [fetch_eamdqd()].
#' @param out_path Where to write the codebook CSV. Defaults to
#'   `data/raw/eamdqd_codebook.csv`.
#' @param use_cache Logical; if `TRUE` (default) and `out_path` already
#'   exists, read and return it instead of re-parsing the PDF and Excel
#'   files.
#'
#' @return The codebook `data.frame`, invisibly. One row per (series,
#'   country); see [eamdqd_read_info_sheets()] and
#'   [eamdqd_parse_codebook_pdf()] for column definitions.
#' @export
eamdqd_codebook <- function(eamdqd,
                            out_path = file.path("data", "raw", "eamdqd_codebook.csv"),
                            use_cache = TRUE) {
  stopifnot(inherits(eamdqd, "eamdqd_vintage"))

  codebook_col_classes <- c(
    code = "character", country = "character", name = "character",
    description = "character", unit = "character", frequency = "character",
    source = "character", sa = "character", sa_d = "integer",
    aggregation = "integer", tr_heavy = "integer", tr_light = "integer",
    tr_blt = "integer", class = "character", vintage = "character"
  )

  if (use_cache && file.exists(out_path)) {
    return(invisible(utils::read.csv(
      out_path, stringsAsFactors = FALSE, colClasses = codebook_col_classes
    )))
  }

  info <- eamdqd_read_info_sheets(eamdqd$xlsx)
  desc <- eamdqd_parse_codebook_pdf(eamdqd$codebook_pdf)

  codebook <- merge(
    info, desc[, c("id", "description", "unit")],
    by.x = "code", by.y = "id", all.x = TRUE
  )
  codebook$vintage <- eamdqd$vintage
  codebook <- codebook[order(codebook$code, codebook$country), ]

  unmatched <- unique(codebook$code[is.na(codebook$description)])
  if (length(unmatched) > 0) {
    cli::cli_warn(c(
      "!" = "{length(unmatched)} series code{?s} in the {.val {eamdqd$vintage}} info sheets have no matching description in the codebook PDF:",
      "x" = paste(unmatched, collapse = ", "),
      "i" = "Their {.field description}/{.field unit} are left NA. This usually means a naming difference between the PDF and the Excel {.field Name} column (e.g. an _EACC variant, or a spelling difference) -- check {.file {eamdqd$codebook_pdf}} by hand before mapping these."
    ))
  }

  col_order <- c(
    "code", "country", "name", "description", "unit", "frequency", "source",
    "sa", "sa_d", "aggregation", "tr_heavy", "tr_light", "tr_blt", "class", "vintage"
  )
  codebook <- codebook[, col_order]

  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(codebook, out_path, row.names = FALSE)

  invisible(codebook)
}

#' Map of EA-MD/QD codes to project variable names
#'
#' Returns the translation table from raw EA-MD/QD series codes to this
#' project's `<iso2>_<concept>` / `ea_<concept>` naming convention (see
#' `equations.R` and CLAUDE.md). Built on top of [eamdqd_codebook()] --
#' the raw series list and transformation codes -- once the mapping from
#' EA-MD/QD codes to project concepts has been reviewed and confirmed.
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

# --------------------------------------------------------------------------
# Data treatment: quarterly aggregation, outlier treatment, EM imputation.
#
# Ported from `aggregate()`, `remove_outliers()`, and `EMimputation()` /
# `princfact()` / `BaiNg()` in `routine_data.py` (equivalently
# `routine_data.m`), with one deliberate change from the reference
# implementation: per project policy (see CLAUDE.md, "Data transformation
# policy"), we never apply `EA_transform()` / the TR1/TR2/TR3
# stationarity-transform codes. Everything here operates on, and returns,
# levels; `koma::as_ets(..., method = )` is responsible for any
# rate-of-change transform, later, at the point of estimation.
#
# Outlier treatment also deliberately diverges from the reference
# implementation: `remove_outliers()` there just sets outliers to NA and
# leaves them for the same EM imputation step used for missing values. We
# instead replace an outlier with the median of a local window around it
# (see `eamdqd_treat_outliers()`), so an outlier's fate does not depend on
# how many *other* series happen to be missing at the same time -- it is
# the simpler, more transparent choice for values already flagged as
# implausible, and it is what was specified for this project.
# --------------------------------------------------------------------------

#' Aggregate a monthly series to quarterly frequency
#'
#' Ports the quarterly-aggregation rule from `aggregate()` in
#' `routine_data.py`: a quarter's value is the mean (`aggregation = 1`,
#' stock/rate variables) or sum (`aggregation = 2`, flow variables) of its
#' three monthly observations. A quarter is only produced when all three of
#' its months exist in `x`; a quarter with a missing month is `NA`
#' (interior ragged edge) rather than partially aggregated, and a quarter
#' at the start/end of `x` that does not have all three months present in
#' the input at all is dropped entirely (structural boundary, not missing
#' data).
#'
#' @param x Numeric vector of monthly observations, in levels.
#' @param start `c(year, month)` for `x[1]`.
#' @param aggregation `1` (mean, default) or `2` (sum), matching the
#'   EA-MD/QD `info` sheet's `Aggregation` column.
#'
#' @return A quarterly `ts` (`frequency = 4`).
#' @keywords internal
eamdqd_aggregate_quarterly <- function(x, start, aggregation = 1) {
  stopifnot(aggregation %in% c(1, 2))
  n <- length(x)
  if (n == 0) {
    return(stats::ts(numeric(0), start = start[1], frequency = 4))
  }

  month_index <- (start[1] * 12L + (start[2] - 1L)) + (seq_len(n) - 1L)
  quarter_index <- month_index %/% 3L # unique, increasing key per quarter

  quarters <- unique(quarter_index)
  n_months_in_quarter <- table(quarter_index)[as.character(quarters)]

  qvals <- vapply(quarters, function(qi) {
    vals <- x[quarter_index == qi]
    if (length(vals) != 3L || anyNA(vals)) {
      return(NA_real_)
    }
    if (aggregation == 1) mean(vals) else sum(vals)
  }, numeric(1))

  keep <- n_months_in_quarter == 3L # drop structurally-partial boundary quarters
  quarters <- quarters[keep]
  qvals <- qvals[keep]

  if (length(quarters) == 0) {
    return(stats::ts(numeric(0), start = start[1], frequency = 4))
  }

  q_start_year <- quarters[1] %/% 4L
  q_start_qtr <- quarters[1] %% 4L + 1L
  stats::ts(qvals, start = c(q_start_year, q_start_qtr), frequency = 4)
}

#' Flag and replace outliers with a local median
#'
#' An observation is an outlier when it is more than `c` interquartile
#' ranges from the series' median: `abs(x - median(x)) > c * IQR(x)`
#' (matching the threshold in `remove_outliers()` in `routine_data.py`,
#' default `c = 10`). Flagged observations are replaced with the median of
#' the `window` non-outlier observations centred on them (falling back to
#' whatever is available near the start/end of the series). Series where
#' more than 20% of observations are flagged are left untouched -- as in
#' the reference implementation, this guards against series with many
#' legitimate zeros or a narrow IQR producing "artificial" outliers.
#'
#' @param x Numeric vector, in levels.
#' @param c Threshold multiplier on the IQR. Default `10`.
#' @param window Number of observations on each side of a flagged point to
#'   use for the replacement median. Default `5`.
#'
#' @return A list with `x` (the treated vector), `outlier` (logical vector,
#'   `TRUE` where an observation was flagged and replaced), and `n_outliers`.
#' @keywords internal
eamdqd_treat_outliers <- function(x, c = 10, window = 5) {
  med <- stats::median(x, na.rm = TRUE)
  q <- stats::quantile(x, probs = c(0.25, 0.75), na.rm = TRUE, type = 7, names = FALSE)
  iqr <- q[2] - q[1]

  outlier <- !is.na(x) & abs(x - med) > c * iqr

  if (mean(outlier, na.rm = TRUE) > 0.2) {
    outlier[] <- FALSE
  }

  out <- x
  for (i in which(outlier)) {
    lo <- max(1L, i - window)
    hi <- min(length(x), i + window)
    neighbourhood <- x[lo:hi]
    neighbourhood <- neighbourhood[!outlier[lo:hi]]
    out[i] <- stats::median(neighbourhood, na.rm = TRUE)
  }

  list(x = out, outlier = outlier, n_outliers = sum(outlier))
}

#' Select the number of factors via the Bai & Ng (2002) IC2 criterion
#'
#' @param X Standardised `T x N` numeric matrix, no missing values.
#' @param qmax Maximum number of factors to consider.
#'
#' @return Integer, the selected number of factors (`>= 1`).
#' @keywords internal
eamdqd_bai_ng <- function(X, qmax) {
  n <- ncol(X)
  t <- nrow(X)
  qmax <- max(1L, min(qmax, n - 1L, t - 1L))

  ct <- ((n + t) / (n * t)) * log(min(n, t)) * seq_len(qmax)

  eig <- eigen(stats::cov(X) * (t - 1) / t, symmetric = TRUE)
  v <- eig$vectors
  fhat_full <- sqrt(n) * v # loadings, N x qmax (method-2 normalisation)

  ic <- numeric(qmax + 1L)
  for (qq in qmax:1) {
    lhat <- fhat_full[, seq_len(qq), drop = FALSE]
    fhat <- X %*% lhat / n
    chat <- fhat %*% t(lhat)
    ehat <- X - chat
    sigma <- mean(colSums(ehat^2) / t)
    ic[qq] <- log(sigma) + ct[qq]
  }
  sigma0 <- mean(colSums(X^2) / t)
  ic[qmax + 1L] <- log(sigma0)

  q <- which.min(ic)
  if (q > qmax) q <- 0L
  max(q, 1L)
}

#' Estimate a static factor model's common component
#'
#' Method-2-normalised principal-components estimator (the branch of
#' `princfact()` in `routine_data.py` actually used by `EMimputation()`):
#' loadings `C = sqrt(N) * eigenvectors(cov(X))`, factors `F = X C / N`,
#' common component `chi = F C'`.
#'
#' @param X `T x N` numeric matrix, standardised, no missing values.
#' @param q Number of factors.
#'
#' @return A list with `factors` (`T x q`), `loadings` (`N x q`), and
#'   `chi` (`T x N`, the common component).
#' @keywords internal
eamdqd_princomp <- function(X, q) {
  n <- ncol(X)
  eig <- eigen(stats::cov(X) * (nrow(X) - 1) / nrow(X), symmetric = TRUE)
  loadings <- sqrt(n) * eig$vectors[, seq_len(q), drop = FALSE]
  factors <- X %*% loadings / n
  chi <- factors %*% t(loadings)
  list(factors = factors, loadings = loadings, chi = chi)
}

#' Impute missing values with the Stock & Watson (2002) EM algorithm
#'
#' Ports `EMimputation()`/`princfact()`/`BaiNg()` from `routine_data.py`
#' (the same algorithm used by McCracken & Ng, 2016, for FRED-MD): missing
#' values are seeded with each series' unconditional mean, a static factor
#' model is fit by principal components, missing values are replaced by
#' the model's common component, and the model is re-fit on the updated
#' data; this repeats until the common component stops changing (relative
#' squared change below `thresh`) or `maxiter` is reached.
#'
#' **Observed values are never touched.** Only entries that are `NA` in
#' the input `X` are ever written to by the imputation step; this is
#' checked, not merely intended -- see the equality test in
#' `tests/testthat/test-data_eamdqd.R`.
#'
#' @param X `T x N` numeric matrix, in levels, with `NA` for missing
#'   values.
#' @param q Number of factors, or `99` (default) to select via
#'   [eamdqd_bai_ng()].
#' @param maxiter Maximum EM iterations. Default `1000`.
#' @param thresh Convergence threshold on the relative change in the
#'   common component. Default `1e-5`.
#'
#' @return `X` with every `NA` entry replaced by its imputed value; observed
#'   entries are returned unchanged.
#' @keywords internal
eamdqd_em_impute <- function(X, q = 99, maxiter = 1000, thresh = 1e-5) {
  X <- as.matrix(X)
  ind_na <- is.na(X)
  if (!any(ind_na)) {
    return(X)
  }

  col_mean <- colMeans(X, na.rm = TRUE)
  for (j in seq_len(ncol(X))) {
    X[ind_na[, j], j] <- col_mean[j]
  }

  standardise <- function(m) {
    mx <- colMeans(m)
    sx <- apply(m, 2, stats::sd)
    list(z = scale(m, center = mx, scale = sx), mx = mx, sx = sx)
  }

  st <- standardise(X)
  q_use <- if (identical(q, 99)) eamdqd_bai_ng(st$z, qmax = 15) else q
  pc <- eamdqd_princomp(st$z, q_use)
  chi0 <- pc$chi

  err <- Inf
  iter <- 0L
  while (err > thresh && iter < maxiter) {
    for (j in seq_len(ncol(X))) {
      idx <- ind_na[, j]
      if (any(idx)) {
        X[idx, j] <- chi0[idx, j] * st$sx[j] + st$mx[j]
      }
    }

    st <- standardise(X)
    q_use <- if (identical(q, 99)) eamdqd_bai_ng(st$z, qmax = 15) else q
    pc <- eamdqd_princomp(st$z, q_use)
    chi1 <- pc$chi

    err <- sum((chi1 - chi0)^2) / sum(chi0^2)
    chi0 <- chi1
    iter <- iter + 1L
  }

  X
}
