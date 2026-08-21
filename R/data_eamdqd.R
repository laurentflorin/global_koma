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
#' The mapping is **mechanical**, not hand-curated: EA-MD/QD names are
#' already `<CODE>_<COUNTRY>`, which maps onto this project's
#' `<iso2>_<concept>` convention by lowercasing and swapping the order
#' (`GDP_DE` -> `de_gdp`; `GDP_EA` -> `ea_gdp`, which is already the `ea_`
#' shared-variable prefix). Dots in EA-MD/QD codes (`TASS.SDB`,
#' `GGLB.LLN`) become underscores, since koma rejects `.` in variable
#' names (see `docs/koma-api.md` §8 Q2). Every generated name is checked
#' with [is_valid_project_name()].
#'
#' `method` is derived from the series' transformation code so that koma
#' applies, once, at estimation time, the transform the dataset's own
#' authors judged appropriate:
#'
#' | `TR` code | meaning | koma `method` |
#' |---|---|---|
#' | 1 | `log(x)` | `"diff_log"` |
#' | 2 | `Δlog(x)` | `"diff_log"` |
#' | 4 | `x` (none) | `"none"` |
#' | 3, 5, 6 | `Δ²log(x)`, `Δx`, `Δ²x` | `"none"` + warning |
#'
#' Codes 3/5/6 have no koma equivalent (koma offers `"percentage"`,
#' `"diff_log"`, `"none"`, or a custom expression -- there is no plain
#' first-difference or second-difference method). They fall back to
#' `"none"` and are listed in a warning rather than silently assigned,
#' because picking a substitute is a per-series judgement call.
#'
#' @param codebook A codebook `data.frame` as returned by
#'   [eamdqd_codebook()].
#' @param tr_set Which transformation-code set to derive `method` from:
#'   `"light"` (default, the reference implementation's own default),
#'   `"heavy"`, or `"blt"`.
#' @param out_path Where to write the mapping CSV for review. Defaults to
#'   `data/raw/eamdqd_variable_map.csv`. Pass `NULL` to skip writing.
#'
#' @return A `data.frame` with columns `eamdqd_code`, `project_name`,
#'   `series_type` (`"level"` or `"rate"`), `method` (koma `rate()`/`level()`
#'   method, e.g. `"diff_log"`), plus `country`, `tr_code` and `class` for
#'   traceability back to the codebook.
#' @export
eamdqd_variable_map <- function(codebook,
                                tr_set = c("light", "heavy", "blt"),
                                out_path = file.path("data", "raw", "eamdqd_variable_map.csv")) {
  tr_set <- match.arg(tr_set)
  tr_col <- c(light = "tr_light", heavy = "tr_heavy", blt = "tr_blt")[[tr_set]]
  tr <- codebook[[tr_col]]

  project_name <- tolower(paste0(codebook$country, "_", codebook$code))
  project_name <- gsub(".", "_", project_name, fixed = TRUE)

  method <- ifelse(tr %in% c(1, 2), "diff_log", "none")

  no_equivalent <- tr %in% c(3, 5, 6)
  if (any(no_equivalent)) {
    affected <- sort(unique(paste0(codebook$code[no_equivalent], " (TR", tr[no_equivalent], ")")))
    cli::cli_warn(c(
      "!" = "{length(affected)} series use{?s/} a transformation code with no koma equivalent; {.field method} set to {.val none}.",
      "x" = paste(utils::head(affected, 12), collapse = ", "),
      "i" = if (length(affected) > 12) "...and {length(affected) - 12} more." else NULL,
      "i" = "koma offers {.val percentage}, {.val diff_log}, {.val none}, or a custom expression -- there is no plain first- or second-difference method. Assign these by hand before using them in a model."
    ))
  }

  invalid <- !is_valid_project_name(project_name)
  if (any(invalid)) {
    cli::cli_abort(c(
      "!" = "Generated project names that violate the naming convention:",
      "x" = paste(utils::head(unique(project_name[invalid]), 10), collapse = ", ")
    ))
  }

  out <- data.frame(
    eamdqd_code = codebook$name,
    project_name = project_name,
    series_type = "level",
    method = method,
    country = codebook$country,
    tr_code = tr,
    class = codebook$class,
    stringsAsFactors = FALSE
  )
  out <- out[order(out$project_name), ]
  rownames(out) <- NULL

  if (!is.null(out_path)) {
    dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(out, out_path, row.names = FALSE)
  }

  out
}

#' Extract one raw series from an EA-MD/QD vintage
#'
#' Low-level accessor: pulls a single series straight out of the relevant
#' country's `data` sheet, at the sheet's own monthly grid, with no
#' aggregation, transformation, or imputation applied. For a usable
#' analysis panel, see [eamdqd_panel()].
#'
#' @param eamdqd_data An `eamdqd_vintage` object, as returned by
#'   [fetch_eamdqd()].
#' @param eamdqd_code Raw EA-MD/QD series name, e.g. `"GDP_DE"`.
#'
#' @return A `data.frame` with columns `date` (`Date`) and `value`
#'   (numeric), covering only the periods where the series is observed.
#' @export
extract_eamdqd_series <- function(eamdqd_data, eamdqd_code) {
  stopifnot(inherits(eamdqd_data, "eamdqd_vintage"))
  stopifnot(is.character(eamdqd_code), length(eamdqd_code) == 1L)

  for (cc in names(eamdqd_data$xlsx)) {
    sheet <- eamdqd_read_data_sheet(eamdqd_data$xlsx[[cc]], cc)
    if (eamdqd_code %in% colnames(sheet$values)) {
      v <- sheet$values[, eamdqd_code]
      keep <- !is.na(v)
      return(data.frame(
        date = as.Date(sheet$time[keep]),
        value = as.numeric(v[keep]),
        stringsAsFactors = FALSE
      ))
    }
  }

  cli::cli_abort(c(
    "!" = "Series {.val {eamdqd_code}} was not found in EA-MD/QD vintage {.val {eamdqd_data$vintage}}.",
    "i" = "Series names are {.code <CODE>_<COUNTRY>}, e.g. {.val GDP_DE}. See {.file data/raw/eamdqd_codebook.csv}."
  ))
}

# --------------------------------------------------------------------------
# Data treatment: quarterly aggregation, outlier treatment, EM imputation.
#
# Ported from `aggregate()`, `remove_outliers()`, and `EMimputation()` /
# `princfact()` / `BaiNg()` in `routine_data.py` (equivalently
# `routine_data.m`).
#
# Per project policy (see CLAUDE.md, "Data transformation policy"),
# `eamdqd_panel()` defaults to `transform = FALSE` and returns levels;
# `koma::as_ets(..., method = )` is responsible for any rate-of-change
# transform, later, at the point of estimation. Outlier treatment and EM
# imputation therefore run *only* under `transform = TRUE`, since both
# assume stationary data.
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

# --------------------------------------------------------------------------
# Reading the `data` sheets, frequency handling, transformation codes,
# and the assembled analysis panel.
#
# LAYOUT NOTE: the `data` sheet is a *monthly* grid (one row per month),
# not two frequency blocks. Series recorded at quarterly frequency carry
# their observation in the **last month of the quarter** (months 3, 6, 9,
# 12) and are `NA` in the other two. The sheet's column order matches the
# `info` sheet's row order exactly, so transformation codes align
# positionally. Both facts are relied on below and asserted at read time.
# --------------------------------------------------------------------------

#' Read one country's `data` sheet
#'
#' @param xlsx_path Path to a `<COUNTRY>data.xlsx` file.
#' @param country ISO-2 code (or `"EA"`) for that file.
#'
#' @return A list with `values` (numeric `T x N` matrix, columns named by
#'   the raw EA-MD/QD series name), `time` (`Date` vector of length `T`,
#'   monthly), and `info` (the per-series metadata rows, in the same order
#'   as the columns of `values`).
#' @keywords internal
eamdqd_read_data_sheet <- function(xlsx_path, country) {
  raw <- as.data.frame(readxl::read_excel(xlsx_path, sheet = "data"))
  info <- eamdqd_read_info_sheets(stats::setNames(xlsx_path, country))

  time_col <- names(raw)[1]
  series_names <- names(raw)[-1]

  if (!identical(series_names, info$name)) {
    cli::cli_abort(c(
      "!" = "The {.file {basename(xlsx_path)}} {.field data} sheet's columns do not line up with its {.field info} sheet.",
      "i" = "This port relies on those two being in the same order; the vintage's layout may have changed."
    ))
  }

  values <- as.matrix(raw[, -1, drop = FALSE])
  storage.mode(values) <- "double"
  colnames(values) <- series_names

  list(
    values = values,
    time = as.Date(raw[[time_col]]),
    info = info
  )
}

#' Pull a native quarterly series out of the monthly grid
#'
#' Quarterly series are stored with their value in the last month of the
#' quarter and `NA` in the other two, so they must be *extracted*, not
#' aggregated -- running them through [eamdqd_aggregate_quarterly()] would
#' see two `NA`s in every quarter and return an all-`NA` series.
#'
#' @param x Numeric vector on the monthly grid.
#' @param time `Date` vector of the same length.
#'
#' @return A quarterly `ts` (`frequency = 4`).
#' @keywords internal
eamdqd_extract_quarterly <- function(x, time) {
  month <- as.integer(format(time, "%m"))
  year <- as.integer(format(time, "%Y"))
  quarter <- (month - 1L) %/% 3L + 1L

  keep <- month %% 3L == 0L # last month of each quarter
  vals <- x[keep]
  yy <- year[keep]
  qq <- quarter[keep]

  if (length(vals) == 0) {
    return(stats::ts(numeric(0), start = c(year[1], 1), frequency = 4))
  }
  stats::ts(vals, start = c(yy[1], qq[1]), frequency = 4)
}

#' Put a country's panel onto the requested frequency
#'
#' `frequency = "q"` reproduces the reference implementation's default
#' `QM` mode: every series ends up quarterly, with monthly series
#' aggregated (mean for `Aggregation == 1`, sum for `Aggregation == 2`)
#' and natively quarterly series extracted from the monthly grid.
#' `frequency = "m"` keeps only the natively monthly series.
#'
#' @param sheet Output of [eamdqd_read_data_sheet()].
#' @param frequency `"q"` or `"m"`.
#'
#' @return A list with `values` (`T x N` matrix), `info` (metadata rows for
#'   the retained series), `start` (`c(year, period)`) and `frequency`
#'   (`4` or `12`).
#' @keywords internal
eamdqd_to_frequency <- function(sheet, frequency = c("q", "m")) {
  frequency <- match.arg(frequency)
  info <- sheet$info
  time <- sheet$time

  if (frequency == "m") {
    keep <- info$frequency == "M"
    if (!any(keep)) {
      cli::cli_abort("No monthly series available for this country.")
    }
    return(list(
      values = sheet$values[, keep, drop = FALSE],
      info = info[keep, , drop = FALSE],
      start = c(as.integer(format(time[1], "%Y")), as.integer(format(time[1], "%m"))),
      frequency = 12
    ))
  }

  month1 <- as.integer(format(time[1], "%m"))
  year1 <- as.integer(format(time[1], "%Y"))

  cols <- lapply(seq_len(ncol(sheet$values)), function(j) {
    x <- sheet$values[, j]
    if (info$frequency[j] == "Q") {
      eamdqd_extract_quarterly(x, time)
    } else {
      eamdqd_aggregate_quarterly(x, start = c(year1, month1),
                                 aggregation = info$aggregation[j])
    }
  })

  starts <- vapply(cols, function(z) stats::tsp(z)[1], numeric(1))
  ends <- vapply(cols, function(z) stats::tsp(z)[2], numeric(1))
  common_start <- min(starts)
  common_end <- max(ends)

  values <- vapply(cols, function(z) {
    as.numeric(stats::window(z, start = common_start, end = common_end, extend = TRUE))
  }, numeric(round((common_end - common_start) * 4) + 1L))
  values <- as.matrix(values)
  colnames(values) <- info$name

  list(
    values = values,
    info = info,
    start = c(common_start %/% 1, round((common_start %% 1) * 4) + 1L),
    frequency = 4
  )
}

#' Apply the EA-MD/QD transformation codes
#'
#' Ports `EA_transform()` from `routine_data.py`. **The codes are the
#' dataset's own, which are not the FRED-MD numbering:**
#'
#' | code | operation |
#' |---|---|
#' | 1 | `scale * log(x)` |
#' | 2 | `scale * Δlog(x)` |
#' | 3 | `scale * Δ²log(x)` |
#' | 4 | `x` (no transformation) |
#' | 5 | `Δx` |
#' | 6 | `Δ²x` |
#'
#' Note in particular that `4`, not `1`, means "no transformation", and
#' that `2` is a log difference rather than a plain first difference.
#'
#' Observations lost to differencing become `NA` at the head of the series.
#' Series carrying negative values under a log code (1/2/3) are demoted to
#' the corresponding non-log code (4/5/6) with a warning, matching the
#' reference implementation's guard -- but warned rather than printed, so
#' it cannot be missed in a non-interactive run.
#'
#' @param values Numeric `T x N` matrix, in levels.
#' @param tr Integer vector of length `N` of transformation codes.
#' @param scale Multiplier applied to the log-based codes (1/2/3).
#'   Defaults to `100`, matching the published codebook (`100 x log(x)`)
#'   and koma's own `diff_log`. The shipped reference code instead
#'   defaults to `c = 1`, i.e. no scaling.
#'
#' @return A numeric `T x N` matrix of transformed series.
#' @keywords internal
eamdqd_transform <- function(values, tr, scale = 100) {
  stopifnot(ncol(values) == length(tr))
  if (!all(tr %in% 1:6)) {
    cli::cli_abort("Transformation codes must be integers 1-6; got {.val {sort(unique(tr[!tr %in% 1:6]))}}.")
  }

  has_negative <- apply(values, 2, function(x) any(x < 0, na.rm = TRUE))
  demote <- has_negative & tr %in% c(1, 2, 3)
  if (any(demote)) {
    cli::cli_warn(c(
      "!" = "{sum(demote)} series contain{?s/} negative values under a log transformation code; demoting to the non-log equivalent.",
      "x" = paste(utils::head(colnames(values)[demote], 12), collapse = ", "),
      "i" = "TR 1->4, 2->5, 3->6 (log -> level, Dlog -> D, D2log -> D2)."
    ))
    tr[demote] <- tr[demote] + 3L
  }

  out <- matrix(NA_real_, nrow(values), ncol(values), dimnames = dimnames(values))
  d <- function(x, k) c(rep(NA_real_, k), diff(x, differences = k))

  for (j in seq_len(ncol(values))) {
    x <- values[, j]
    out[, j] <- switch(as.character(tr[j]),
      "1" = scale * log(x),
      "2" = d(scale * log(x), 1),
      "3" = d(scale * log(x), 2),
      "4" = x,
      "5" = d(x, 1),
      "6" = d(x, 2)
    )
  }
  out
}

#' Blank the covid period for real variables
#'
#' Implements the reference implementation's imputation "method 2": set
#' calendar 2020 and 2021 to `NA` for every series in class `R` (real), so
#' the subsequent EM step reconstructs them from the financial and nominal
#' series instead of from their own collapsed-and-rebounded history.
#'
#' The window is defined **semantically as calendar 2020-2021**, which
#' matches `_ReadME.pdf` ("2020 and 2021, regardless of the frequency")
#' and the Matlab reference, whose `Xnan(T19+1:T21, loc)` is inclusive of
#' 2021Q4. The Python port's `Xnan[T19+1:T21]` is exclusive and so stops
#' at 2021Q3; and on the monthly path both references anchor on *October*,
#' giving Nov-2019 to Oct-2021. Defining the window by calendar year
#' avoids inheriting either quirk.
#'
#' @param values Numeric `T x N` matrix.
#' @param class Character vector of length `N` of series classes.
#' @param start `c(year, period)` of the first row.
#' @param frequency `4` or `12`.
#'
#' @return `values` with the covid window set to `NA` for real series.
#' @keywords internal
eamdqd_covid_window <- function(values, class, start, frequency) {
  time_year <- floor(stats::time(stats::ts(rep(NA_real_, nrow(values)),
                                           start = start, frequency = frequency)))
  in_window <- time_year %in% c(2020, 2021)
  real <- class == "R"

  if (any(in_window) && any(real)) {
    values[in_window, real] <- NA_real_
  }
  values
}

#' Emit the dataset's known structural-break caveats
#'
#' These are warnings, deliberately, rather than silent corrections: each
#' is a real property of the underlying economies that the user has to
#' decide how to handle (dummy, sample split, or accept), and none of them
#' has a fix this layer could apply on the user's behalf.
#'
#' @param countries Character vector of ISO-2 codes present in the panel.
#'
#' @return Invisibly `NULL`; called for its warnings.
#' @keywords internal
eamdqd_warn_caveats <- function(countries) {
  if ("IE" %in% countries) {
    cli::cli_warn(c(
      "!" = "Irish series carry a large level break in 2015.",
      "i" = "Multinational balance-sheet redomiciliation inflated measured real GDP by roughly 25% in a single year (vs ~2-10% in adjacent years). Treat Irish output, investment and trade aggregates across 2015 as a break, not a business-cycle movement."
    ))
  }
  if ("EL" %in% countries) {
    cli::cli_warn(c(
      "!" = "Greek series are shorter and break repeatedly over 2010-2018.",
      "i" = "Greece publishes fewer series than the other member states (96 vs up to 118 for the EA aggregate), and the sovereign-debt crisis and successive adjustment programmes make 2010-2018 structurally unstable. Check coverage before relying on any Greek series."
    ))
  }
  cli::cli_warn(c(
    "!" = "All EA-MD/QD series break in 2020.",
    "i" = "The covid collapse and rebound are outliers on any pre-2020 calibration. {.code eamdqd_panel(covid_treatment = TRUE)} reconstructs real variables over 2020-2021 from the nominal and financial block; otherwise handle the break explicitly."
  ))
  invisible(NULL)
}

#' Build an analysis-ready EA-MD/QD panel
#'
#' Assembles one or more countries' series into a named list of
#' `koma::koma_ts` objects, keyed by this project's `<iso2>_<concept>`
#' variable names (see [eamdqd_variable_map()]).
#'
#' By default the panel is returned **in levels, untransformed and
#' unbalanced** -- see CLAUDE.md, "Data transformation policy". koma
#' applies the appropriate rate-of-change transform once, at estimation
#' time, from each series' `method` attribute, and fills ragged edges
#' itself. Setting `transform = TRUE` instead reproduces the upstream
#' pipeline (stationarity transform, outlier treatment, EM imputation) and
#' marks the result `series_type = "rate"`, `method = "none"` so that koma
#' does not transform it a second time.
#'
#' Outlier treatment and EM imputation are deliberately tied to
#' `transform`: both assume stationary data, and neither is meaningful on
#' untransformed levels of a trending series.
#'
#' @section Interaction between `covid_treatment` and outlier detection:
#' Blanking 2020-2021 removes the largest swings in the sample, which
#' narrows each series' interquartile range and therefore makes the
#' `>10 x IQR` outlier rule considerably more sensitive everywhere else.
#' Measured on the Irish quarterly panel, outlier replacements rise from
#' 13 cells with `covid_treatment = FALSE` to 293 with it enabled. This is
#' inherent to the reference method (which likewise runs outlier removal
#' after covid-blanking) rather than a quirk of this port, but it is worth
#' knowing before comparing the two settings: the differences between them
#' are not confined to 2020-2021.
#'
#' @param eamdqd An `eamdqd_vintage` object, as returned by
#'   [fetch_eamdqd()].
#' @param countries Character vector of ISO-2 codes (and/or `"EA"`) to
#'   include. Defaults to every economy in the vintage.
#' @param frequency `"q"` (default) for a quarterly panel of all series,
#'   monthly ones aggregated up; `"m"` for the natively monthly series
#'   only.
#' @param transform `FALSE` (default) to return levels; `TRUE` to apply
#'   the dataset's transformation codes, outlier treatment and EM
#'   imputation.
#' @param covid_treatment `FALSE` (default); `TRUE` blanks calendar
#'   2020-2021 for real variables so the EM step reconstructs them. Requires
#'   `transform = TRUE`.
#' @param tr_set Which transformation-code set to use: `"light"`
#'   (default), `"heavy"` or `"blt"`. See `_ReadME.pdf`.
#' @param q Number of factors for EM imputation, or `99` (default) to
#'   select via Bai & Ng (2002).
#' @param scale Multiplier for log-based transformation codes. Default
#'   `100`.
#'
#' @return A named list of `koma_ts` objects. Each carries `series_type`,
#'   `method`, and the extra attributes `eamdqd_code`, `country`,
#'   `series_class` and `tr_code`. (The class R/N/F/C is exposed as
#'   `series_class`, not `class`, because `class` is a reserved R
#'   attribute -- setting it would overwrite the object's `koma_ts` class.)
#' @export
eamdqd_panel <- function(eamdqd,
                         countries = NULL,
                         frequency = c("q", "m"),
                         transform = FALSE,
                         covid_treatment = FALSE,
                         tr_set = c("light", "heavy", "blt"),
                         q = 99,
                         scale = 100) {
  stopifnot(inherits(eamdqd, "eamdqd_vintage"))
  frequency <- match.arg(frequency)
  tr_set <- match.arg(tr_set)

  if (isTRUE(covid_treatment) && !isTRUE(transform)) {
    cli::cli_abort(c(
      "!" = "{.arg covid_treatment = TRUE} requires {.arg transform = TRUE}.",
      "x" = "Covid treatment blanks 2020-2021 for real variables and relies on the EM imputation step to reconstruct them; with {.arg transform = FALSE} that step does not run, so the panel would simply lose two years of data.",
      "i" = "Either set {.arg transform = TRUE}, or leave {.arg covid_treatment = FALSE} and handle the 2020 break downstream."
    ))
  }

  if (is.null(countries)) countries <- names(eamdqd$xlsx)
  unknown <- setdiff(countries, names(eamdqd$xlsx))
  if (length(unknown) > 0) {
    cli::cli_abort("Unknown econom{?y/ies} {.val {unknown}}; available: {.val {names(eamdqd$xlsx)}}.")
  }

  eamdqd_warn_caveats(countries)

  tr_col <- c(light = "tr_light", heavy = "tr_heavy", blt = "tr_blt")[[tr_set]]
  out <- list()
  no_equiv <- character()

  for (cc in countries) {
    sheet <- eamdqd_read_data_sheet(eamdqd$xlsx[[cc]], cc)
    panel <- eamdqd_to_frequency(sheet, frequency = frequency)
    values <- panel$values
    info <- panel$info

    if (transform) {
      values <- eamdqd_transform(values, tr = info[[tr_col]], scale = scale)
      if (covid_treatment) {
        values <- eamdqd_covid_window(values, class = info$class,
                                      start = panel$start, frequency = panel$frequency)
      }
      values <- apply(values, 2, function(x) eamdqd_treat_outliers(x)$x)
      values <- eamdqd_em_impute(values, q = q)
    }

    # Suppressed here and re-raised once after the loop: otherwise the
    # "no koma equivalent" warning repeats identically for every country.
    map <- suppressWarnings(eamdqd_variable_map(info, tr_set = tr_set, out_path = NULL))
    map <- map[match(info$name, map$eamdqd_code), ]
    no_equiv <- c(no_equiv, map$eamdqd_code[map$tr_code %in% c(3, 5, 6)])

    for (j in seq_len(ncol(values))) {
      series <- koma::as_ets(
        stats::ts(values[, j], start = panel$start, frequency = panel$frequency),
        series_type = if (transform) "rate" else "level",
        method = if (transform) "none" else map$method[j],
        eamdqd_code = info$name[j],
        country = cc,
        series_class = info$class[j],
        tr_code = info[[tr_col]][j]
      )
      out[[map$project_name[j]]] <- series
    }
  }

  if (length(no_equiv) > 0 && !transform) {
    cli::cli_warn(c(
      "!" = "{length(no_equiv)} series across {length(countries)} econom{?y/ies} use a transformation code with no koma equivalent; {.field method} set to {.val none}.",
      "x" = paste(utils::head(sort(unique(no_equiv)), 10), collapse = ", "),
      "i" = "TR codes 3, 5 and 6 are second log differences and plain first/second differences; koma offers only {.val percentage}, {.val diff_log}, {.val none} or a custom expression. Assign these by hand before using them in a model."
    ))
  }

  out
}
