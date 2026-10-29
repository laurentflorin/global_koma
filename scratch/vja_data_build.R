# Combine the VJA key list's series into one file, data/raw/vja_data.csv.
#
# Run from the project root:
#
#   Rscript scratch/vja_data_build.R
#
# Two sources:
#
# - Macrobond, downloaded on Windows with the Excel macro MacrobondDownload.bas
#   into data/raw/vja_macrobond_download*.xlsx. A workbook's Keys sheet is the
#   key list, and its Data sheet holds every series at native frequency. If a
#   key failed and was re-downloaded into a second workbook matching the same
#   pattern, each key is taken from the most recent workbook where its Status
#   is OK. A newer workbook can also give a country and variable a different
#   key: the newest key that downloaded wins, and the replacement is reported.
# - KOF's time-series database (koftsdb, schema "vja"), for the rows whose key
#   is a KOF key (`<cc>.kof.<...>`): China's GDP, exports and imports.
#   koftsdb is a private KOF package and the database needs a password, so the
#   result is cached in data/cache/kofdb/vja_kofdb.rds. Delete it to refetch.
#
# Output: one row per key-list row and date, with columns country, variable,
# key, source, frequency, unit, date (start of period) and value. Series stay
# at native frequency and in levels; nothing is converted or transformed.
#   - A key written as "a+b" becomes the sum of its parts, date by date.
#   - A key used by several rows (eurate0003 for Germany, Italy, France and the
#     euro area) is repeated under each of them.
#   - A missing observation is kept as an empty value, never dropped, so the
#     series keep their calendar.

wb_paths <- Sys.glob(file.path("data", "raw", "vja_macrobond_download*.xlsx"))
if (length(wb_paths) == 0) {
  cli::cli_abort("No {.file data/raw/vja_macrobond_download*.xlsx} found.")
}
kofdb_cache <- file.path("data", "cache", "kofdb", "vja_kofdb.rds")
out_path <- file.path("data", "raw", "vja_data.csv")

# --- key list -------------------------------------------------------------
# The union over all workbooks of the rows marked Download = Y, plus the KOF
# rows, in the order of the oldest workbook. A re-download workbook has every
# row but the re-downloaded ones set to N, so the newest list alone would
# drop everything else.
wb_paths <- wb_paths[order(file.mtime(wb_paths))]
key_list <- do.call(rbind, lapply(seq_along(wb_paths), function(i) {
  x <- readxl::read_excel(wb_paths[i], "Keys")[, c("Country", "Variable", "Macrobond_Key", "Download")]
  x$workbook <- i
  x
}))
key_list <- key_list[!is.na(key_list$Macrobond_Key), ]
is_kof <- grepl("^[a-z]{2}\\.kof\\.", key_list$Macrobond_Key)
key_list <- key_list[is_kof | key_list$Download %in% "Y", ]
key_list <- key_list[!duplicated(key_list[, c("Country", "Variable", "Macrobond_Key")]), ]
clash <- duplicated(key_list[, c("Country", "Variable", "workbook")])
if (any(clash)) {
  cli::cli_abort(c(
    "A workbook gives one country and variable two different keys:",
    stats::setNames(
      paste(key_list$Country[clash], key_list$Variable[clash], "in", basename(wb_paths[key_list$workbook[clash]])),
      rep("x", sum(clash))
    )
  ))
}

# --- Macrobond ------------------------------------------------------------
info <- do.call(rbind, lapply(wb_paths, function(p) {
  x <- readxl::read_excel(p, "Series info")
  x$workbook <- p
  x[, c("Key", "Status", "Frequency", "Unit", "Downloaded at", "workbook")]
}))
ok <- info[info$Status %in% "OK", ]
ok <- ok[order(ok$`Downloaded at`, decreasing = TRUE), ]
ok <- ok[!duplicated(ok$Key), ]
failed <- setdiff(info$Key, ok$Key)

# A country and variable listed with different keys in different workbooks
# takes the key from the newest workbook where every part of it downloaded,
# or the newest key if none did (it is then reported as left out below). The
# row keeps the place of the country and variable's first appearance.
is_kof <- grepl("^[a-z]{2}\\.kof\\.", key_list$Macrobond_Key)
usable <- is_kof | vapply(key_list$Macrobond_Key, function(k) {
  all(trimws(strsplit(k, "+", fixed = TRUE)[[1]]) %in% ok$Key)
}, logical(1))
group <- paste(key_list$Country, key_list$Variable, sep = "\r")
pick <- ave(seq_len(nrow(key_list)), group, FUN = function(i) {
  candidates <- if (any(usable[i])) i[usable[i]] else i
  candidates[which.max(key_list$workbook[candidates])]
})
dropped <- which(pick != seq_len(nrow(key_list)))
newer <- key_list$workbook[pick[dropped]] > key_list$workbook[dropped]
row_label <- function(i) paste0(key_list$Country[i], " ", key_list$Variable[i], ": ")
row_wb <- function(i) basename(wb_paths[key_list$workbook[i]])
if (any(newer)) {
  i <- dropped[newer]
  cli::cli_inform(c(
    "{length(i)} key{?s} replaced by a newer workbook's:",
    stats::setNames(
      paste0(row_label(i), key_list$Macrobond_Key[i], " -> ", key_list$Macrobond_Key[pick[i]], " (", row_wb(pick[i]), ")"),
      rep("i", length(i))
    )
  ))
}
if (any(!newer)) {
  i <- dropped[!newer]
  cli::cli_warn(c(
    "{length(i)} newer key{?s} failed to download, so the older key is kept:",
    stats::setNames(
      paste0(row_label(i), key_list$Macrobond_Key[i], " (", row_wb(i), ") failed; kept ", key_list$Macrobond_Key[pick[i]]),
      rep("x", length(i))
    )
  ))
}
key_list <- key_list[pick[!duplicated(group)], ]
is_kof <- grepl("^[a-z]{2}\\.kof\\.", key_list$Macrobond_Key)

macrobond_obs <- do.call(rbind, lapply(unique(ok$workbook), function(p) {
  x <- readxl::read_excel(p, "Data", col_types = c("text", "date", "numeric"))
  x[x$Key %in% ok$Key[ok$workbook == p], ]
}))
# readxl returns UTC midnight; the macro writes calendar days only.
macrobond_obs$Date <- as.Date(macrobond_obs$Date)

# --- KOF time-series database ---------------------------------------------
kof_keys <- unique(key_list$Macrobond_Key[is_kof])
if (file.exists(kofdb_cache)) {
  cli::cli_inform("cached: {.file {kofdb_cache}}")
  tslist <- readRDS(kofdb_cache)
} else {
  if (!requireNamespace("koftsdb", quietly = TRUE)) {
    cli::cli_abort(c(
      "The KOF series need {.pkg koftsdb}, which is not installed in this library.",
      "i" = "Install it, or run the fetch wherever it is installed and save the result with {.code saveRDS(tslist, \"{kofdb_cache}\")}."
    ))
  }
  library(koftsdb)
  # Connect to Postgres (disconnected again as soon as the read is done).
  # koftsdb can only ask for the password inside RStudio, so anywhere else it
  # is read from KOFDB_PASSWORD in .Renviron, under the same rules as
  # FRED_API_KEY. KOFDB_USER overrides the login name as database user.
  con <- if (nzchar(Sys.getenv("KOFDB_PASSWORD"))) {
    db_connection_create(
      "kofdb", host = "archivedb.kof.ethz.ch",
      user = Sys.getenv("KOFDB_USER", Sys.info()[["user"]]),
      passwd = "KOFDB_PASSWORD", passwd_from_env = TRUE
    )
  } else {
    db_connection_create("kofdb", host = "archivedb.kof.ethz.ch")
  }
  tslist <- tryCatch(
    db_ts_read(con = con, ts_keys = kof_keys, schema = "vja"),
    finally = DBI::dbDisconnect(con)
  )
  dir.create(dirname(kofdb_cache), recursive = TRUE, showWarnings = FALSE)
  saveRDS(tslist, kofdb_cache)
}
missing_kof <- setdiff(kof_keys, names(tslist))
if (length(missing_kof) > 0) {
  cli::cli_abort("The KOF database returned nothing for {.val {missing_kof}}.")
}

# A regular `ts` to rows dated at the start of each period, which is
# Macrobond's convention too.
ts_rows <- function(x, key) {
  if (!stats::is.ts(x)) {
    cli::cli_abort("{.val {key}}: expected a regular {.cls ts}, got {.cls {class(x)}}.")
  }
  f <- stats::frequency(x)
  frequency <- c(`1` = "annual", `4` = "quarterly", `12` = "monthly")[as.character(f)]
  if (is.na(frequency)) {
    cli::cli_abort("{.val {key}}: unsupported frequency {f}.")
  }
  year <- floor(as.numeric(stats::time(x)) + 1e-6)
  month <- (as.integer(stats::cycle(x)) - 1L) * (12L / f) + 1L
  data.frame(
    date = as.Date(sprintf("%d-%02d-01", year, month)),
    value = as.numeric(x),
    frequency = unname(frequency),
    unit = NA_character_
  )
}

# --- one key-list row -----------------------------------------------------
macrobond_rows <- function(part) {
  d <- macrobond_obs[macrobond_obs$Key == part, ]
  meta <- ok[ok$Key == part, ]
  data.frame(
    date = d$Date, value = d$Value,
    frequency = meta$Frequency, unit = meta$Unit
  )
}

row_series <- function(key, kof) {
  if (kof) {
    return(cbind(ts_rows(tslist[[key]], key), source = "kofdb:vja"))
  }
  parts <- trimws(strsplit(key, "+", fixed = TRUE)[[1]])
  if (any(parts %in% failed)) {
    return(NULL)
  }
  absent <- setdiff(parts, ok$Key)
  if (length(absent) > 0) {
    cli::cli_abort("{.val {absent}} {?is/are} on the key list but in no downloaded workbook.")
  }
  pieces <- lapply(parts, macrobond_rows)
  if (length(pieces) == 1) {
    return(cbind(pieces[[1]], source = "macrobond"))
  }
  freq <- unique(vapply(pieces, function(p) p$frequency[1], character(1)))
  unit <- unique(vapply(pieces, function(p) p$unit[1], character(1)))
  if (length(freq) != 1 || length(unit) != 1) {
    cli::cli_abort("{.val {key}}: its parts differ in frequency or unit, so they cannot be summed.")
  }
  # Sum on the union of dates: a date missing from one part gives NA, not a
  # partial sum.
  dates <- sort(unique(do.call(c, lapply(pieces, `[[`, "date"))))
  values <- lapply(pieces, function(p) p$value[match(dates, p$date)])
  data.frame(
    date = dates, value = Reduce(`+`, values),
    frequency = freq, unit = unit, source = "macrobond"
  )
}

rows <- lapply(seq_len(nrow(key_list)), function(i) {
  s <- row_series(key_list$Macrobond_Key[i], is_kof[i])
  if (is.null(s)) {
    return(NULL)
  }
  data.frame(
    country = key_list$Country[i],
    variable = key_list$Variable[i],
    key = key_list$Macrobond_Key[i],
    s[, c("source", "frequency", "unit", "date", "value")]
  )
})
out <- do.call(rbind, rows)

skipped <- key_list[vapply(rows, is.null, logical(1)), ]
if (nrow(skipped) > 0) {
  cli::cli_warn(c(
    "{nrow(skipped)} key-list row{?s} left out because Macrobond returned an error for {?its/their} key:",
    stats::setNames(
      paste(skipped$Country, skipped$Variable, skipped$Macrobond_Key),
      rep("x", nrow(skipped))
    )
  ))
}

utils::write.csv(out, out_path, row.names = FALSE, na = "")
cli::cli_inform(
  "wrote {.file {out_path}}: {nrow(out)} rows, {nrow(key_list) - nrow(skipped)} of {nrow(key_list)} key-list rows."
)
