# Assemble raw per-source series into koma_ts panels keyed by project
# variable name (see equations.R for the naming convention).
#
# Sources: EA-MD/QD (data_eamdqd.R) for the ten euro-area countries, FRED
# (data_fred.R) for the US, with Eurostat/ECB as fallback where EA-MD/QD is
# missing a needed series -- every fallback is logged with cli::cli_inform,
# not applied silently. Per CLAUDE.md's "Data transformation policy",
# everything here stays in levels.
#
# COUNTRY CODES: this project's own convention is the true ISO-2 code, so
# Greece is "gr" -- but EA-MD/QD and Eurostat both use the EU statistical
# convention "EL" for Greece, while the ECB's own datasets use "GR" (which
# happens to match ISO). See `iso2_to_eamdqd`/`iso2_to_ecb` below; get this
# wrong and Greek series silently come back empty.

ea_countries <- c("at", "be", "de", "gr", "es", "fr", "ie", "it", "nl", "pt")
modelled_countries <- c(ea_countries, "us")

#' @keywords internal
iso2_to_eamdqd <- c(
  at = "AT", be = "BE", de = "DE", gr = "EL", es = "ES",
  fr = "FR", ie = "IE", it = "IT", nl = "NL", pt = "PT"
)

#' @keywords internal
iso2_to_ecb <- c(
  at = "AT", be = "BE", de = "DE", gr = "GR", es = "ES",
  fr = "FR", ie = "IE", it = "IT", nl = "NL", pt = "PT", us = "US"
)

#' Target variable set: project concept -> EA-MD/QD series code
#' @keywords internal
eamdqd_concept_codes <- c(
  gdp = "gdp", consumption = "hfce", investment = "gfcf", government = "gfce",
  exports = "expgs", imports = "impgs", prices = "hicpov",
  core_prices = "hicpnef", unemployment = "unetot", long_rate = "ltirt"
)

#' Target variable set: project concept -> FRED series id
#' @keywords internal
fred_concept_series <- c(
  gdp = "GDPC1", consumption = "PCECC96", investment = "GPDIC1",
  government = "GCEC1", exports = "EXPGSC1", imports = "IMPGSC1",
  prices = "CPIAUCSL", core_prices = "CPILFESL", unemployment = "UNRATE",
  long_rate = "GS10"
)

#' koma `method` for each target concept
#'
#' `"diff_log"` for level/index series (GDP components, price indices);
#' `"none"` for series already expressed as a rate (unemployment,
#' long-term interest rates) -- differencing or log-differencing a rate
#' would give a meaningless second-order quantity. Matches how
#' `eamdqd_variable_map()` derives `method` from the EA-MD/QD `TR` codes
#' for the same concepts (verified: `unemployment`/`long_rate` come back
#' `"none"` there too), so FRED and EA-MD/QD series for the same concept
#' get the same treatment.
#' @keywords internal
concept_method <- c(
  gdp = "diff_log", consumption = "diff_log", investment = "diff_log",
  government = "diff_log", exports = "diff_log", imports = "diff_log",
  prices = "diff_log", core_prices = "diff_log",
  unemployment = "none", long_rate = "none"
)

#' koma `series_type` for each target concept
#'
#' `"level"` for anything whose stored numbers are a level or an index
#' that koma should difference itself; `"rate"` for series that are
#' *already* expressed as a percentage rate (unemployment, long-term
#' interest rates), which koma must take as-is.
#'
#' Pairing `series_type = "rate"` with `method = "none"` is the correct
#' tag for a rate: it says "these numbers are the rate, do not transform
#' them". Tagging a rate `series_type = "level", method = "none"` is
#' numerically identical during estimation -- `rate()` is the identity
#' under `method = "none"` either way -- but it misdescribes the series,
#' and it is what `level()` consults when inverting a forecast back to
#' levels. See `docs/koma-api.md` §1 and CLAUDE.md.
#' @keywords internal
concept_series_type <- c(
  gdp = "level", consumption = "level", investment = "level",
  government = "level", exports = "level", imports = "level",
  prices = "level", core_prices = "level",
  unemployment = "rate", long_rate = "rate"
)

# --------------------------------------------------------------------------
# Eurostat / ECB fallback fetchers, used only when EA-MD/QD is missing a
# needed series for a country. Each converts to a quarterly koma_ts in
# levels and is logged via cli::cli_inform when actually invoked.
# --------------------------------------------------------------------------

#' Directory used to cache Eurostat table downloads
#' @keywords internal
eurostat_cache_dir <- function() {
  path <- file.path("data", "cache", "eurostat")
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

#' Fetch one national-accounts component from Eurostat (`namq_10_gdp`)
#'
#' @param geo Eurostat geo code (EA-MD/QD/Eurostat convention, e.g. `"DE"`,
#'   `"EL"` for Greece).
#' @param na_item Eurostat `na_item` code (`"B1GQ"` GDP, `"P31_S14_S15"`
#'   consumption, `"P51G"` investment, `"P3_S13"` government,
#'   `"P6"` exports, `"P7"` imports).
#'
#' @return A quarterly `ts`, chain-linked real levels (`CLV15_MEUR`),
#'   seasonally and calendar adjusted.
#' @keywords internal
eurostat_gdp_component <- function(geo, na_item) {
  d <- eurostat::get_eurostat("namq_10_gdp", filters = list(
    geo = geo, freq = "Q", unit = "CLV15_MEUR", s_adj = "SCA", na_item = na_item
  ), time_format = "date", cache_dir = eurostat_cache_dir())
  eurostat_to_ts(d)
}

#' Fetch a HICP index from Eurostat (`prc_hicp_midx`)
#'
#' @param geo Eurostat geo code.
#' @param coicop `"CP00"` (headline) or `"TOT_X_NRG_FOOD"` (core: excludes
#'   energy, food, alcohol and tobacco).
#'
#' @return A monthly `ts` (index, 2015=100), aggregated to quarterly by
#'   [eamdqd_aggregate_quarterly()].
#' @keywords internal
eurostat_hicp <- function(geo, coicop) {
  d <- eurostat::get_eurostat("prc_hicp_midx", filters = list(
    geo = geo, coicop = coicop, unit = "I15"
  ), time_format = "date", cache_dir = eurostat_cache_dir())
  monthly <- eurostat_to_ts(d, frequency = 12)
  eamdqd_aggregate_quarterly(as.numeric(monthly), start = stats::start(monthly), aggregation = 1)
}

#' Fetch the unemployment rate from Eurostat (`une_rt_q`)
#' @keywords internal
eurostat_unemployment <- function(geo) {
  d <- eurostat::get_eurostat("une_rt_q", filters = list(
    geo = geo, s_adj = "SA", sex = "T", age = "Y15-74", unit = "PC_ACT"
  ), time_format = "date", cache_dir = eurostat_cache_dir())
  eurostat_to_ts(d)
}

#' Fetch the long-term (Maastricht) interest rate from Eurostat (`irt_lt_mcby_m`)
#' @keywords internal
eurostat_long_rate <- function(geo) {
  d <- eurostat::get_eurostat("irt_lt_mcby_m", filters = list(
    geo = geo, int_rt = "MCBY"
  ), time_format = "date", cache_dir = eurostat_cache_dir())
  monthly <- eurostat_to_ts(d, frequency = 12)
  eamdqd_aggregate_quarterly(as.numeric(monthly), start = stats::start(monthly), aggregation = 1)
}

#' Convert a `get_eurostat()` result to a plain `ts`
#' @keywords internal
eurostat_to_ts <- function(d, frequency = 4) {
  if (nrow(d) == 0) {
    cli::cli_abort("Eurostat query returned no rows.")
  }
  d <- d[order(d$time), ]
  start_year <- as.integer(format(d$time[1], "%Y"))
  start_period <- if (frequency == 4) {
    (as.integer(format(d$time[1], "%m")) - 1L) %/% 3L + 1L
  } else {
    as.integer(format(d$time[1], "%m"))
  }
  stats::ts(d$values, start = c(start_year, start_period), frequency = frequency)
}

#' Fetch a daily ECB series and aggregate it to quarterly
#'
#' Used for `ea_policy_rate` (main refinancing rate) and `eur_usd`
#' (reference rate). Both are stocks (period-average is the natural
#' quarterly aggregate for a rate or a price, not a sum), so
#' `aggregation = 1` throughout.
#'
#' `FM.D...MRR_FR.LEV` in particular is **not** a daily-observed series
#' despite its `FREQ = D` label: the ECB only records an observation when
#' the rate *changes*, so raw gaps of years are normal (verified: a
#' 3,032-day gap between 2000-06-27 and the next recorded change, while
#' the rate held at 4.25%). Grouping the raw observations by
#' calendar-month and handing that straight to `ts(..., frequency = 12)`
#' would silently compress those gaps -- R has no way to know a "month"
#' in the resulting series isn't the calendar month it looks like, so
#' every quarter after the first multi-year gap would be aggregated from
#' the wrong calendar dates. This function forward-fills the raw step
#' series onto a complete daily grid first, so every day (and therefore
#' every month and quarter) gets the rate that was actually in effect.
#'
#' @param key An ECB SDMX series key, e.g.
#'   `"FM.D.U2.EUR.4F.KR.MRR_FR.LEV"`.
#'
#' @return A quarterly `ts`.
#' @keywords internal
ecb_quarterly_series <- function(key) {
  d <- ecb::get_data(key)
  if (nrow(d) == 0) {
    cli::cli_abort("ECB series {.val {key}} returned no observations.")
  }
  d <- d[order(d$obstime), ]
  dates <- as.Date(d$obstime)

  full_days <- seq(dates[1], dates[length(dates)], by = "day")
  filled <- stats::approx(dates, d$obsvalue, xout = full_days, method = "constant", rule = 2)$y

  monthly <- stats::aggregate(filled, by = list(format(full_days, "%Y-%m")), FUN = mean)
  ym <- as.Date(paste0(monthly$Group.1, "-01"))
  ord <- order(ym)
  monthly <- monthly[ord, ]
  ym <- ym[ord]
  m_ts <- stats::ts(monthly$x, start = c(as.integer(format(ym[1], "%Y")), as.integer(format(ym[1], "%m"))),
                    frequency = 12)
  eamdqd_aggregate_quarterly(as.numeric(m_ts), start = stats::start(m_ts), aggregation = 1)
}

# --------------------------------------------------------------------------
# Per-country panels
# --------------------------------------------------------------------------

#' Build one EA country's panel from EA-MD/QD, with Eurostat fallback
#'
#' @param iso2 Lowercase ISO-2 code, one of `ea_countries`.
#' @param eamdqd An `eamdqd_vintage`, as returned by [fetch_eamdqd()].
#'
#' @return A named list of `koma_ts`, one per target concept plus
#'   `domestic_demand` (the identity `consumption + investment +
#'   government`, computed here rather than fetched).
#' @keywords internal
build_ea_country_panel <- function(iso2, eamdqd) {
  code <- iso2_to_eamdqd[[iso2]]
  if (is.null(code)) {
    cli::cli_abort("{.val {iso2}} is not one of the modelled EA countries: {.val {ea_countries}}.")
  }

  raw <- suppressWarnings(eamdqd_panel(eamdqd, countries = code, frequency = "q"))
  eamdqd_name <- function(concept) paste0(tolower(code), "_", eamdqd_concept_codes[[concept]])

  out <- list()
  for (concept in names(eamdqd_concept_codes)) {
    src_name <- eamdqd_name(concept)
    series <- raw[[src_name]]

    if (is.null(series) || all(is.na(as.numeric(series)))) {
      series <- ea_country_fallback(iso2, code, concept)
    }

    out[[country_var(iso2, concept)]] <- koma::as_ets(
      series,
      series_type = concept_series_type[[concept]],
      method = concept_method[[concept]],
      country = toupper(iso2),
      source = if (identical(series, raw[[src_name]])) "eamdqd" else "eurostat"
    )
  }

  out[[country_var(iso2, "domestic_demand")]] <- gdp_identity_component(out, iso2)
  out
}

#' Fetch one concept from Eurostat, logging the fallback
#' @keywords internal
ea_country_fallback <- function(iso2, eamdqd_code, concept) {
  cli::cli_inform(c(
    "i" = "EA-MD/QD is missing {.val {concept}} for {.val {toupper(iso2)}}; falling back to Eurostat."
  ))
  switch(concept,
    gdp          = eurostat_gdp_component(eamdqd_code, "B1GQ"),
    consumption  = eurostat_gdp_component(eamdqd_code, "P31_S14_S15"),
    investment   = eurostat_gdp_component(eamdqd_code, "P51G"),
    government   = eurostat_gdp_component(eamdqd_code, "P3_S13"),
    exports      = eurostat_gdp_component(eamdqd_code, "P6"),
    imports      = eurostat_gdp_component(eamdqd_code, "P7"),
    prices       = eurostat_hicp(eamdqd_code, "CP00"),
    core_prices  = eurostat_hicp(eamdqd_code, "TOT_X_NRG_FOOD"),
    unemployment = eurostat_unemployment(eamdqd_code),
    long_rate    = eurostat_long_rate(eamdqd_code),
    cli::cli_abort("No Eurostat fallback is defined for concept {.val {concept}}.")
  )
}

#' Build the US panel from FRED
#'
#' @return A named list of `koma_ts`, one per target concept plus
#'   `us_domestic_demand`.
#' @keywords internal
build_us_panel <- function(start_date = "1995-01-01") {
  out <- list()
  for (concept in names(fred_concept_series)) {
    d <- fetch_fred_series(fred_concept_series[[concept]], start_date = start_date)
    series <- df_to_quarterly_ts(d)
    out[[country_var("us", concept)]] <- koma::as_ets(
      series,
      series_type = concept_series_type[[concept]],
      method = concept_method[[concept]],
      country = "US",
      source = "fred"
    )
  }
  out[[country_var("us", "domestic_demand")]] <- gdp_identity_component(out, "us")
  out
}

#' Convert a FRED `date`/`value` data.frame to a quarterly `ts`
#'
#' FRED's quarterly series (GDP and components) already have one
#' observation per quarter; monthly series (prices, unemployment,
#' long_rate) are averaged up via [eamdqd_aggregate_quarterly()].
#' @keywords internal
df_to_quarterly_ts <- function(d) {
  start_year <- as.integer(format(d$date[1], "%Y"))
  month1 <- as.integer(format(d$date[1], "%m"))
  spacing <- if (nrow(d) > 1) as.numeric(diff(d$date)[1]) else 90
  if (spacing <= 45) { # monthly cadence
    m <- stats::ts(d$value, start = c(start_year, month1), frequency = 12)
    eamdqd_aggregate_quarterly(as.numeric(m), start = stats::start(m), aggregation = 1)
  } else {
    start_q <- (month1 - 1L) %/% 3L + 1L
    stats::ts(d$value, start = c(start_year, start_q), frequency = 4)
  }
}

#' Compute the domestic-demand identity component for one country's panel
#'
#' `domestic_demand = consumption + investment + government`, matching
#' the national-accounts identity checked in `tests/testthat/test-panel_build.R`
#' (`gdp ~= domestic_demand + exports - imports`). Not fetched from any
#' source -- see CLAUDE.md.
#' @keywords internal
gdp_identity_component <- function(panel, iso2) {
  c_ <- panel[[country_var(iso2, "consumption")]]
  i_ <- panel[[country_var(iso2, "investment")]]
  g_ <- panel[[country_var(iso2, "government")]]
  total <- as.numeric(c_) + as.numeric(i_) + as.numeric(g_)
  koma::as_ets(
    stats::ts(total, start = stats::start(c_), frequency = stats::frequency(c_)),
    series_type = "level", method = "diff_log", country = toupper(iso2), source = "derived"
  )
}

#' Build one country's panel
#'
#' Dispatches to [build_ea_country_panel()] (EA-MD/QD, Eurostat fallback)
#' or [build_us_panel()] (FRED) depending on `iso2`.
#'
#' @param iso2 Two-letter lowercase ISO country code. One of
#'   `ea_countries` or `"us"`.
#' @param eamdqd An `eamdqd_vintage` (required for EA countries; ignored
#'   for `"us"`). See [fetch_eamdqd()].
#'
#' @return A named list of `koma::koma_ts` objects, keyed by
#'   `<iso2>_<concept>` names, covering `gdp`, `consumption`,
#'   `investment`, `government`, `exports`, `imports`, `domestic_demand`,
#'   `prices`, `core_prices`, `unemployment`, `long_rate`.
#' @export
build_country_panel <- function(iso2, eamdqd = NULL) {
  iso2 <- tolower(iso2)
  if (identical(iso2, "us")) {
    return(build_us_panel())
  }
  if (!iso2 %in% ea_countries) {
    cli::cli_abort("Unknown country {.val {iso2}}; expected one of {.val {c(ea_countries, 'us')}}.")
  }
  if (is.null(eamdqd)) {
    cli::cli_abort("{.arg eamdqd} is required for EA countries (an {.cls eamdqd_vintage} from {.fn fetch_eamdqd}).")
  }
  build_ea_country_panel(iso2, eamdqd)
}

# --------------------------------------------------------------------------
# Shared (non-country) variables
# --------------------------------------------------------------------------

#' Build the US broad nominal effective exchange rate, spliced
#'
#' The Federal Reserve's broad dollar index is published as two
#' non-overlapping-in-name series: `TWEXBMTH` (goods only, 1973-01 to
#' 2019-12, **discontinued**) and `TWEXBGSMTH` (goods and services,
#' 2006-01 onwards). Neither alone spans what this project needs -- the
#' estimation sample starts 2000Q1, and exogenous variables must run
#' through the forecast horizon, which `TWEXBMTH` ends well before and
#' `TWEXBGSMTH` starts well after.
#'
#' They do, however, overlap for **168 months** (2006-01 to 2019-12), and
#' over that window they are near-interchangeable up to a level shift:
#' verified `TWEXBGSMTH / TWEXBMTH` has mean 0.9149 with sd 0.0075
#' (0.8%, range 0.896-0.925), and their month-on-month growth rates
#' correlate at **0.996**. So the two baskets move together almost
#' exactly, and the only real difference is the base.
#'
#' This function therefore rescales the older series onto the newer one's
#' base by the mean overlap ratio and takes the newer series from 2006-01
#' onwards. That is a pure **level** rescaling, not a stationarity
#' transform, so it complies with CLAUDE.md's levels-only ingestion
#' policy: koma still does the `diff_log` conversion itself at
#' estimation time.
#'
#' @return A quarterly `ts`, index level, spliced.
#' @keywords internal
fred_spliced_dollar_index <- function() {
  old <- df_to_quarterly_ts(fetch_fred_series("TWEXBMTH", start_date = "1995-01-01"))
  new <- df_to_quarterly_ts(fetch_fred_series("TWEXBGSMTH", start_date = "1995-01-01"))

  overlap_start <- max(stats::tsp(old)[1], stats::tsp(new)[1])
  overlap_end <- min(stats::tsp(old)[2], stats::tsp(new)[2])
  if (overlap_start > overlap_end) {
    cli::cli_abort(c(
      "!" = "{.val TWEXBMTH} and {.val TWEXBGSMTH} no longer overlap.",
      "i" = "The splice in {.fn fred_spliced_dollar_index} needs a common window to compute its rescaling ratio."
    ))
  }

  old_overlap <- as.numeric(stats::window(old, start = overlap_start, end = overlap_end))
  new_overlap <- as.numeric(stats::window(new, start = overlap_start, end = overlap_end))
  ratio <- mean(new_overlap / old_overlap, na.rm = TRUE)

  head_part <- as.numeric(stats::window(old, end = overlap_start - 1 / 4)) * ratio
  stats::ts(
    c(head_part, as.numeric(new)),
    start = stats::start(old),
    frequency = 4
  )
}

#' Build the shared, non-country-prefixed variables
#'
#' `ea_policy_rate` (ECB main refinancing rate), `us_policy_rate` (FRED
#' effective federal funds rate), `eur_usd` (ECB reference rate),
#' `us_exchange_rate` (spliced Fed broad dollar index, see
#' [fred_spliced_dollar_index()]), `oil_price` (FRED Brent, monthly
#' average), and `row_gdp` (see [build_row_gdp()]).
#'
#' Note the asymmetry between `eur_usd` and `us_exchange_rate`: the ten
#' euro-area countries share one currency and therefore one bilateral
#' EUR/USD rate, whereas the US block uses a *broad effective* index
#' against all its trading partners. That is deliberate -- see CLAUDE.md
#' on why no `<iso2>_exchange_rate` exists for an EA member.
#'
#' @param row_weights Named numeric vector as returned by
#'   [row_gdp_weights()], passed through to [build_row_gdp()].
#'
#' @return A named list of `koma_ts`.
#' @export
build_shared_panel <- function(row_weights) {
  ea_policy_rate <- ecb_quarterly_series("FM.D.U2.EUR.4F.KR.MRR_FR.LEV")
  eur_usd <- ecb_quarterly_series("EXR.D.USD.EUR.SP00.A")

  us_policy_rate <- df_to_quarterly_ts(fetch_fred_series("FEDFUNDS", start_date = "1995-01-01"))
  oil_price <- df_to_quarterly_ts(fetch_fred_series("MCOILBRENTEU", start_date = "1995-01-01"))
  us_exchange_rate <- fred_spliced_dollar_index()

  list(
    ea_policy_rate   = koma::as_ets(ea_policy_rate, series_type = "rate", method = "none", source = "ecb"),
    us_policy_rate   = koma::as_ets(us_policy_rate, series_type = "rate", method = "none", source = "fred"),
    eur_usd          = koma::as_ets(eur_usd, series_type = "level", method = "diff_log", source = "ecb"),
    us_exchange_rate = koma::as_ets(us_exchange_rate, series_type = "level", method = "diff_log", source = "fred"),
    oil_price        = koma::as_ets(oil_price, series_type = "level", method = "diff_log", source = "fred"),
    row_gdp          = build_row_gdp(row_weights)
  )
}

#' Build the full multi-country panel
#'
#' Combines [build_country_panel()] output across all requested countries
#' with the shared variables from [build_shared_panel()] into one flat
#' `ts_data` list suitable for `koma::estimate()`.
#'
#' @param countries Character vector of ISO-2 country codes. Defaults to
#'   every modelled country (`ea_countries` plus `"us"`).
#' @param eamdqd An `eamdqd_vintage`, as returned by [fetch_eamdqd()].
#' @param row_weights Named numeric vector as returned by
#'   [row_gdp_weights()].
#'
#' @return A named list of `koma_ts` objects, validated with
#'   [is_valid_project_name()].
#' @export
build_global_panel <- function(countries = modelled_countries, eamdqd = NULL, row_weights) {
  panel <- list()
  for (cc in countries) {
    panel <- c(panel, build_country_panel(cc, eamdqd = eamdqd))
  }
  panel <- c(panel, build_shared_panel(row_weights))

  invalid <- names(panel)[!is_valid_project_name(names(panel))]
  if (length(invalid) > 0) {
    cli::cli_abort("Generated series with invalid project names: {.val {invalid}}.")
  }

  panel
}

#' Align a panel to a common frequency and sample
#'
#' Thin wrapper that windows every series in a panel to a common start/end
#' and asserts a single shared frequency, mirroring what
#' `koma::estimate()` requires.
#'
#' @param panel A named list of `koma_ts` objects.
#' @param start,end `c(year, period)` bounds. If omitted, computed as the
#'   widest common window across every series in `panel` (the latest
#'   start, the earliest end) -- i.e. the effective common sample.
#'
#' @return The windowed panel, same names as `panel`.
#' @export
align_panel <- function(panel, start = NULL, end = NULL) {
  freqs <- unique(vapply(panel, stats::frequency, numeric(1)))
  if (length(freqs) != 1) {
    cli::cli_abort(c(
      "!" = "{.arg panel} mixes frequencies: {.val {freqs}}.",
      "i" = "All series must share one frequency before calling {.fn align_panel}."
    ))
  }

  if (is.null(start)) {
    start <- do.call(pmax, lapply(panel, function(x) stats::tsp(x)[1]))
    start <- num_to_period(start, freqs)
  }
  if (is.null(end)) {
    end <- do.call(pmin, lapply(panel, function(x) stats::tsp(x)[2]))
    end <- num_to_period(end, freqs)
  }

  lapply(panel, function(x) {
    attrs <- get_custom_attrs(x)
    windowed <- stats::window(x, start = start, end = end)
    do.call(koma::as_ets, c(list(windowed), attrs))
  })
}

#' @keywords internal
num_to_period <- function(t, frequency) {
  year <- floor(t + .Machine$double.eps)
  period <- round((t - year) * frequency) + 1L
  c(year, period)
}

#' Recover the koma-specific attributes (series_type, method, ...) off a koma_ts
#' @keywords internal
get_custom_attrs <- function(x) {
  keep <- setdiff(names(attributes(x)), c("tsp", "class", "dim", "dimnames", "names"))
  attrs <- attributes(x)[keep]
  attrs
}

#' Report which series in a panel have internal (non-edge) `NA`s
#'
#' Distinguishes an **internal** gap -- an `NA` with observed values on
#' both sides -- from a leading or trailing `NA`, which is a ragged edge.
#' The distinction matters because koma fills ragged edges itself
#' (`fill_ragged_edge()`/`conditional_fill()`), but has no facility for a
#' hole in the middle of a series and fails with an opaque
#' "time series contains internal NAs" from deep inside `level()`.
#'
#' @param panel A named list of `koma_ts`.
#'
#' @return A named list, one element per affected series, each a numeric
#'   vector of the affected times. Empty if the panel is clean.
#' @export
internal_gaps <- function(panel) {
  gaps <- lapply(panel, function(x) {
    values <- as.numeric(x)
    observed <- which(!is.na(values))
    if (length(observed) == 0) {
      return(numeric(0))
    }
    span <- seq(min(observed), max(observed))
    as.numeric(stats::time(x))[span[is.na(values[span])]]
  })
  gaps[vapply(gaps, length, integer(1)) > 0]
}

#' Linearly interpolate internal gaps in a panel
#'
#' koma cannot estimate on a series with a hole in it (see
#' [internal_gaps()]), so any internal `NA` has to be resolved before
#' estimation. This interpolates them linearly and **warns**, naming
#' every series and period touched -- it never fixes silently, because an
#' interpolated observation is invented data and the caller needs to know
#' it is there.
#'
#' Leading and trailing `NA`s are left alone: those are ragged edges,
#' which koma fills itself with proper conditioning, and overwriting them
#' here would replace a principled conditional fill with a crude
#' extrapolation.
#'
#' In the current EA-MD/QD vintage exactly one series is affected:
#' `gr_long_rate` at 2015Q3. That is not a data error -- Greek banks were
#' closed under capital controls from 29 June 2015 and the sovereign bond
#' market was effectively shut, so no Maastricht long-term rate was
#' published for that quarter. It sits between 11.46% (2015Q2) and 7.81%
#' (2015Q4).
#'
#' @param panel A named list of `koma_ts`.
#'
#' @return The panel, with internal gaps interpolated.
#' @export
fill_internal_gaps <- function(panel) {
  gaps <- internal_gaps(panel)
  if (length(gaps) == 0) {
    return(panel)
  }

  described <- vapply(names(gaps), function(name) {
    paste0(name, " (", paste(sprintf("%.2f", gaps[[name]]), collapse = ", "), ")")
  }, character(1))
  cli::cli_warn(c(
    "!" = "Linearly interpolated {sum(lengths(gaps))} internal gap{?s} in {length(gaps)} series: {.val {unname(described)}}.",
    "i" = "koma cannot estimate on a series with an internal {.val NA}; these values are interpolated, not observed."
  ))

  for (name in names(gaps)) {
    x <- panel[[name]]
    values <- as.numeric(x)
    observed <- which(!is.na(values))
    span <- seq(min(observed), max(observed))
    values[span] <- stats::approx(observed, values[observed], xout = span)$y

    attrs <- get_custom_attrs(x)
    attrs[["ets_attributes"]] <- NULL
    filled <- stats::ts(values, start = stats::start(x), frequency = stats::frequency(x))
    panel[[name]] <- do.call(koma::as_ets, c(list(filled), attrs))
  }

  panel
}

#' Give every series in a panel the same set of attributes
#'
#' `koma::estimate()` runs its `ts_data` through `as_mets()`, which
#' **requires every series to carry an identical set of attribute
#' names** -- it aborts with "Provide the same attributes for each series
#' in your list" otherwise. Our panel does not naturally satisfy that:
#' EA-MD/QD series carry `eamdqd_code`/`tr_code`/`series_class`, derived
#' and FRED series do not, and the shared (non-country) series have no
#' `country` at all.
#'
#' This takes the union of the attribute names present anywhere in
#' `panel` and fills the gaps with `NA`, so the metadata is preserved
#' where it exists and merely absent-but-declared where it does not.
#' `ets_attributes` is dropped and left to koma, which maintains it
#' itself.
#'
#' @param panel A named list of `koma_ts`.
#'
#' @return The same list, every element carrying the same attribute names.
#' @export
harmonise_panel_attrs <- function(panel) {
  custom <- lapply(panel, get_custom_attrs)
  attr_names <- setdiff(unique(unlist(lapply(custom, names))), "ets_attributes")

  out <- lapply(seq_along(panel), function(i) {
    attrs <- custom[[i]]
    attrs[["ets_attributes"]] <- NULL
    for (missing_name in setdiff(attr_names, names(attrs))) {
      attrs[[missing_name]] <- NA
    }
    do.call(koma::as_ets, c(list(panel[[i]]), attrs[attr_names]))
  })
  stats::setNames(out, names(panel))
}
