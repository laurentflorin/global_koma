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

#' Stage-3a labour and disaggregated-price concepts
#'
#' Fetched only for the countries named in `labour_countries` (see
#' [build_global_panel()]), because each one costs a download and the
#' stage-3a block is a Germany-only pilot. Split three ways by source:
#'
#' - `stage3a_eamdqd_codes` come straight from the EA-MD/QD vintage that
#'   [build_ea_country_panel()] already downloads and currently discards.
#'   Note the energy HICP code is `HICPNG`, **not** `HICPNRG`.
#' - `stage3a_eurostat_concepts` have no EA-MD/QD counterpart at all:
#'   `nonenergy_prices` because EA-MD/QD carries only the narrower
#'   ex-energy-*and*-food aggregate (`HICPNEF`, this project's
#'   `core_prices`), and the two trade deflators because EA-MD/QD has no
#'   import or export price series of any kind.
#' - `wages` is **derived**, not fetched -- see [derived_wage_rate()].
#' @keywords internal
stage3a_eamdqd_codes <- c(employment = "temp", energy_prices = "hicpng")

#' @keywords internal
stage3a_eurostat_concepts <- c(
  nonenergy_prices = "TOT_X_NRG", import_prices = "P7", export_prices = "P6"
)

#' @keywords internal
stage3a_derived_concepts <- c("wages")

#' @keywords internal
stage3a_concepts <- c(
  names(stage3a_eamdqd_codes), names(stage3a_eurostat_concepts),
  stage3a_derived_concepts
)

#' Stage-3b external, fiscal and financial concepts
#'
#' Split by source in the same three ways as `stage3a_*`:
#'
#' - `stage3b_eamdqd_codes` -- already in the downloaded vintage.
#'   `house_prices` is BIS residential property prices; note the codebook's
#'   unit for it (`MLNe`) is **wrong**, inherited from a typo in the upstream
#'   PDF -- the data is an index, 2010 = 100. Absent for Greece and Portugal,
#'   which constrains rollout but not the German pilot.
#' - `stage3b_eurostat_concepts` -- fetched, see [eurostat_govdebt()] and
#'   [eurostat_current_account()].
#' - `stage3b_derived_concepts` -- computed, see [derived_netborrowing()] and
#'   [derived_credit()]. `netborrowing` depends on `govdebt`, so the two are
#'   built in that order.
#' @keywords internal
stage3b_eamdqd_codes <- c(house_prices = "hprc")

#' @keywords internal
stage3b_eurostat_concepts <- c(govdebt = "GD", current_account = "CA")

#' @keywords internal
stage3b_derived_concepts <- c("netborrowing", "credit")

#' @keywords internal
stage3b_concepts <- c(
  names(stage3b_eamdqd_codes), names(stage3b_eurostat_concepts),
  stage3b_derived_concepts
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
  unemployment = "none", long_rate = "none",
  # stage 3a. All six are levels or indices koma differences itself: an
  # employment headcount, a wage rate in euro per worker, three price
  # indices and two deflators. None is already a rate.
  employment = "diff_log", wages = "diff_log", energy_prices = "diff_log",
  nonenergy_prices = "diff_log", import_prices = "diff_log",
  export_prices = "diff_log",
  # stage 3b. house_prices and credit are strictly-positive stocks/indices.
  # govdebt is a ratio already in percent; netborrowing and current_account
  # are SIGNED ratios -- diff_log on a series that crosses zero gives NaN,
  # which koma reports as an "internal NA" pointing at the wrong problem.
  house_prices = "diff_log", credit = "diff_log",
  govdebt = "none", netborrowing = "none", current_account = "none"
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
  unemployment = "rate", long_rate = "rate",
  # stage 3a -- all levels/indices, see concept_method above. Every one of
  # them appears in a stage-3a identity, and chain_weighted_index() aborts
  # on a component that is not series_type = "level".
  employment = "level", wages = "level", energy_prices = "level",
  nonenergy_prices = "level", import_prices = "level", export_prices = "level",
  # stage 3b -- the three ratios are rates: koma takes their numbers as-is,
  # which is what makes the debt accumulation identity an exact linear
  # relation in percentage points rather than a statement about growth.
  house_prices = "level", credit = "level",
  govdebt = "rate", netborrowing = "rate", current_account = "rate"
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

#' Fetch a national-accounts implicit deflator from Eurostat (`namq_10_gdp`)
#'
#' The import- and export-price series for the stage-3a block. EA-MD/QD has
#' no import or export price series of any kind, so unlike every other
#' country concept this one has no EA-MD/QD path and Eurostat is the primary
#' source rather than a fallback.
#'
#' `unit = "PD15_EUR"` is Eurostat's own implicit deflator (2015 = 100),
#' i.e. the ratio of the current-price to the chain-linked-volume series,
#' computed upstream. Taking it directly rather than dividing `CP_MEUR` by
#' `CLV15_MEUR` here avoids re-deriving a number Eurostat already publishes,
#' and avoids the chain-linking subtlety that the ratio of two chain-linked
#' aggregates is not itself a clean price index.
#'
#' Verified spans (2026-08 vintage): 1991Q1-2026Q1 for DE, 1995Q1 or 1996Q1
#' onward for the other nine EA countries, with **no internal `NA`s** in any
#' of them -- comfortably covering the 2000Q1-2024Q4 estimation window.
#'
#' @param geo Eurostat geo code.
#' @param na_item `"P6"` (exports) or `"P7"` (imports).
#'
#' @return A quarterly `ts`, index 2015 = 100, seasonally and calendar
#'   adjusted.
#' @keywords internal
eurostat_deflator <- function(geo, na_item) {
  d <- eurostat::get_eurostat("namq_10_gdp", filters = list(
    geo = geo, freq = "Q", unit = "PD15_EUR", s_adj = "SCA", na_item = na_item
  ), time_format = "date", cache_dir = eurostat_cache_dir())
  eurostat_to_ts(d)
}

#' Fetch general government consolidated gross debt from Eurostat
#'
#' The stage-3b `govdebt` concept: Maastricht debt (`gov_10q_ggdebt`,
#' `na_item = "GD"`), taken directly as **percentage of GDP**.
#'
#' **Why Maastricht debt and not EA-MD/QD's `GGLB`.** The vintage already on
#' disk carries `GGLB`, general government *total financial liabilities*,
#' which is a market-value measure including equity and trade credit --
#' 3.06tn for Germany against Maastricht's 2.90tn. `GD` is the consolidated
#' face-value definition every fiscal rule and every sovereign-spread study
#' uses, and it is the one whose ratio to GDP is a recognisable number.
#'
#' **Why `PC_GDP` and not a level.** A debt *level* would be
#' `series_type = "level", method = "diff_log"`, which makes the stock-flow
#' accumulation identity a statement about growth rates -- nonsense. As a
#' ratio tagged `rate`/`none` koma passes the numbers through untouched, so
#' `govdebt == 1*govdebt.L(1) + 1*netborrowing` is an exact linear relation
#' in percentage points. See [derived_netborrowing()].
#'
#' Verified (2026-08 vintage): 2000Q1-2026Q1 for all eleven economies, no
#' internal `NA`s, 57.6-81.0% for Germany.
#'
#' @param geo Eurostat geo code.
#' @return A quarterly `ts`, percent of GDP.
#' @keywords internal
eurostat_govdebt <- function(geo) {
  d <- eurostat::get_eurostat("gov_10q_ggdebt", filters = list(
    geo = geo, sector = "S13", na_item = "GD", unit = "PC_GDP"
  ), time_format = "date", cache_dir = eurostat_cache_dir())
  eurostat_to_ts(d)
}

#' Fetch the current-account balance from Eurostat, as a share of GDP
#'
#' The stage-3b `current_account` concept: `bop_c6_q`, `bop_item = "CA"`,
#' `stk_flow = "BAL"`, against the rest of the world.
#'
#' **A balance crosses zero, so it cannot be `diff_log`.** Germany's current
#' account is negative in 7 of the 100 quarters in the estimation window.
#' Tagged `level`/`diff_log`, `log()` of those quarters is `NaN` and koma
#' aborts with `"time series contains internal NAs"` -- a message pointing at
#' the wrong problem entirely. It is therefore carried as a ratio to nominal
#' GDP, `rate`/`none`.
#'
#' **The ratio is built from four-quarter rolling sums**, not quarter on
#' quarter. `bop_c6_q` has no `s_adj` dimension -- the balances are published
#' **unadjusted only** -- while every other series in this panel is
#' seasonally adjusted. A rolling annual sum over a rolling annual
#' denominator is both the conventional presentation of a current-account
#' ratio and a seasonal filter that needs no model, which is why it is
#' preferred here to adjusting the raw series.
#'
#' @param geo Eurostat geo code.
#' @return A quarterly `ts`, percent of GDP, four-quarter rolling.
#' @keywords internal
eurostat_current_account <- function(geo) {
  d <- eurostat::get_eurostat("bop_c6_q", filters = list(
    geo = geo, partner = "WRL_REST", bop_item = "CA", stk_flow = "BAL",
    currency = "MIO_EUR", sector10 = "S1", sectpart = "S1"
  ), time_format = "date", cache_dir = eurostat_cache_dir())
  ca <- eurostat_to_ts(d)
  gdp <- eurostat_nominal_gdp(geo)

  roll4 <- function(x) {
    v <- as.numeric(x)
    s <- stats::filter(v, rep(1, 4), sides = 1)
    stats::ts(as.numeric(s), start = stats::start(x), frequency = stats::frequency(x))
  }
  ca4 <- roll4(ca)
  gdp4 <- roll4(gdp)
  start <- max(stats::tsp(ca4)[1], stats::tsp(gdp4)[1])
  end <- min(stats::tsp(ca4)[2], stats::tsp(gdp4)[2])
  a <- stats::window(ca4, start = start, end = end)
  b <- stats::window(gdp4, start = start, end = end)
  stats::ts(100 * as.numeric(a) / as.numeric(b),
    start = stats::start(a), frequency = stats::frequency(a)
  )
}

#' Fetch a HICP index from Eurostat (`prc_hicp_midx`)
#'
#' @param geo Eurostat geo code.
#' @param coicop `"CP00"` (headline), `"TOT_X_NRG_FOOD"` (core: excludes
#'   energy, food, alcohol and tobacco), or `"TOT_X_NRG"` (excludes energy
#'   only -- the stage-3a `nonenergy_prices` concept).
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
#' @param stage3a Which stage-3a concepts to add on top of the base set:
#'   `FALSE` (none, the default, so the existing pipeline is unchanged),
#'   `TRUE` (all of `stage3a_concepts`), or a character vector naming a
#'   subset. The subset form matters: stage 3a phase B needs
#'   `export_prices` from every partner to make Germany's foreign-price
#'   index endogenous, but needs nothing else from them, and fetching all
#'   six concepts for eleven countries would mean ~50 downloads nobody reads.
#'
#' @return A named list of `koma_ts`, one per target concept plus
#'   `domestic_demand` (the identity `consumption + investment +
#'   government`, computed here rather than fetched).
#' @keywords internal
build_ea_country_panel <- function(iso2, eamdqd, stage3a = FALSE) {
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

  wanted <- resolve_stage3a_concepts(stage3a)

  for (concept in intersect(names(stage3a_eamdqd_codes), wanted)) {
    src_name <- paste0(tolower(code), "_", stage3a_eamdqd_codes[[concept]])
    series <- raw[[src_name]]
    if (is.null(series) || all(is.na(as.numeric(series)))) {
      cli::cli_abort(c(
        "EA-MD/QD is missing {.val {concept}} ({.val {toupper(src_name)}}) for {.val {toupper(iso2)}}.",
        "i" = "No Eurostat fallback is defined for the stage-3a EA-MD/QD concepts."
      ))
    }
    out[[country_var(iso2, concept)]] <- koma::as_ets(
      series,
      series_type = concept_series_type[[concept]],
      method = concept_method[[concept]],
      country = toupper(iso2), source = "eamdqd"
    )
  }

  for (concept in intersect(names(stage3a_eurostat_concepts), wanted)) {
    arg <- stage3a_eurostat_concepts[[concept]]
    series <- if (identical(concept, "nonenergy_prices")) {
      eurostat_hicp(code, arg)
    } else {
      eurostat_deflator(code, arg)
    }
    out[[country_var(iso2, concept)]] <- koma::as_ets(
      series,
      series_type = concept_series_type[[concept]],
      method = concept_method[[concept]],
      country = toupper(iso2), source = "eurostat"
    )
  }

  if ("wages" %in% wanted) {
    out[[country_var(iso2, "wages")]] <- derived_wage_rate(
      raw[[paste0(tolower(code), "_ws")]], raw[[paste0(tolower(code), "_temp")]], iso2
    )
  }

  # -- stage 3b --------------------------------------------------------------
  for (concept in intersect(names(stage3b_eamdqd_codes), wanted)) {
    src_name <- paste0(tolower(code), "_", stage3b_eamdqd_codes[[concept]])
    series <- raw[[src_name]]
    if (is.null(series) || all(is.na(as.numeric(series)))) {
      cli::cli_abort(c(
        "EA-MD/QD is missing {.val {concept}} ({.val {toupper(src_name)}}) for {.val {toupper(iso2)}}.",
        "i" = "{.val house_prices} is absent for Greece and Portugal in this vintage."
      ))
    }
    out[[country_var(iso2, concept)]] <- koma::as_ets(
      series,
      series_type = concept_series_type[[concept]],
      method = concept_method[[concept]],
      country = toupper(iso2), source = "eamdqd"
    )
  }

  for (concept in intersect(names(stage3b_eurostat_concepts), wanted)) {
    series <- switch(concept,
      govdebt = eurostat_govdebt(code),
      current_account = eurostat_current_account(code),
      cli::cli_abort("No fetcher for stage-3b concept {.val {concept}}.")
    )
    out[[country_var(iso2, concept)]] <- koma::as_ets(
      series,
      series_type = concept_series_type[[concept]],
      method = concept_method[[concept]],
      country = toupper(iso2), source = "eurostat"
    )
  }

  # Order matters: netborrowing is the first difference of govdebt.
  if ("netborrowing" %in% wanted) {
    out[[country_var(iso2, "netborrowing")]] <-
      derived_netborrowing(out[[country_var(iso2, "govdebt")]], iso2)
  }
  if ("credit" %in% wanted) {
    out[[country_var(iso2, "credit")]] <- derived_credit(raw, code, iso2)
  }

  out
}

#' Normalise a `stage3a` argument to a concept vector
#'
#' `FALSE` -> none, `TRUE` -> all of `stage3a_concepts`, a character vector
#' -> itself, validated. `wages` is derived from `employment`, so asking for
#' it without `employment` is a caller error worth naming rather than a
#' confusing `NULL` further down.
#' @keywords internal
resolve_stage3a_concepts <- function(stage3a) {
  available <- c(stage3a_concepts, stage3b_concepts)
  if (isFALSE(stage3a) || is.null(stage3a)) return(character())
  if (isTRUE(stage3a)) return(available)
  if (!is.character(stage3a)) {
    cli::cli_abort("{.arg stage3a} must be {.code TRUE}, {.code FALSE}, or a character vector of concepts.")
  }
  unknown <- setdiff(stage3a, available)
  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "Unknown extended concept{?s}: {.val {unknown}}.",
      "i" = "Available: {.val {available}}."
    ))
  }
  # netborrowing is the first difference of govdebt, so asking for it without
  # govdebt would fail later with a confusing NULL rather than here.
  if ("netborrowing" %in% stage3a && !"govdebt" %in% stage3a) {
    cli::cli_abort(c(
      "{.val netborrowing} is derived from {.val govdebt}.",
      "i" = "Request {.val govdebt} alongside it."
    ))
  }
  stage3a
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

#' Stage-3a US concepts: project concept -> FRED series id
#'
#' Deliberately **only** the two trade deflators, not the full stage-3a set.
#' The United States gets no labour block: it is outside the euro-area price
#' aggregate, and the block is a Germany-only pilot. What the US does need is
#' `us_export_prices`, because the US carries a non-trivial weight in
#' Germany's trade-weighted `de_foreign_prices` index -- omitting it would
#' silently renormalise Germany's largest non-EA partner out of the price
#' channel. `us_import_prices` comes along for symmetry at no modelling cost.
#'
#' `A020RD3Q086SBEA` / `A021RD3Q086SBEA` are the BEA implicit price
#' deflators for exports and imports of goods and services (2017 = 100),
#' the closest FRED equivalent of Eurostat's `PD15_EUR`. The differing base
#' year is irrelevant: koma models `diff_log`, which is base-invariant.
#' @keywords internal
stage3a_fred_series <- c(
  export_prices = "A020RD3Q086SBEA", import_prices = "A021RD3Q086SBEA"
)

#' Build the US panel from FRED
#'
#' @param start_date Earliest observation to request from FRED.
#' @param stage3a Logical. Add `stage3a_fred_series` (the two trade
#'   deflators). `FALSE` by default, matching [build_ea_country_panel()].
#'
#' @return A named list of `koma_ts`, one per target concept plus
#'   `us_domestic_demand`.
#' @keywords internal
build_us_panel <- function(start_date = "1995-01-01", stage3a = FALSE) {
  out <- list()
  series_ids <- fred_concept_series
  wanted <- intersect(resolve_stage3a_concepts(stage3a), names(stage3a_fred_series))
  if (length(wanted) > 0) series_ids <- c(series_ids, stage3a_fred_series[wanted])
  for (concept in names(series_ids)) {
    d <- fetch_fred_series(series_ids[[concept]], start_date = start_date)
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

#' Derive a wage *rate* from the wage bill and employment
#'
#' **This is the correction that makes the stage-3a identities exact, and it
#' is not optional.** EA-MD/QD's `WS` is "Wages and salaries" in current
#' prices -- a whole-economy wage **bill**, not a wage rate. The stage-3a
#' block needs a rate, for two independent reasons:
#'
#' - A wage Phillips curve (`<iso2>_wages ~ <iso2>_unemployment + ...`) is a
#'   statement about the price of labour, not about total labour income. Run
#'   on the bill it would pick up employment growth and mostly re-estimate
#'   Okun's law.
#' - `<iso2>_real_income == <iso2>_wages + <iso2>_employment - <iso2>_prices`
#'   would **double-count employment** if `wages` were already the bill,
#'   since the bill is the rate times employment. The identity is only
#'   correct for a rate.
#'
#' With `wages = WS / TEMP` the whole block is internally exact in rate
#' space: `real_income` recovers the deflated wage bill, and
#' `ulc == wages - productivity` telescopes through
#' `productivity == gdp - employment` to `WS - GDP`, the nominal wage bill
#' over real output -- the textbook definition of unit labour costs. That
#' matters because EA-MD/QD publishes **no whole-economy ULC series at all**
#' (only seven sectoral ones), so ULC has to be derived, and deriving it
#' this way makes it consistent with the rest of the block by construction
#' rather than by luck.
#'
#' **Stated assumption**: `WS` covers employees, `TEMP` counts total
#' employment including the self-employed, so this imputes employee
#' compensation to the self-employed. That is the standard construction for
#' whole-economy compensation per worker, and it is preferred here to the
#' alternative (`WS / EMP`, employees only) because `TEMP` is the employment
#' concept the rest of the block uses -- mixing `EMP` into `wages` and
#' `TEMP` into `employment` would break the exactness above. It is also the
#' cleaner series: `EMP` carries a TR3 transformation code for the
#' Netherlands, `TEMP` is TR2 for all eleven countries.
#'
#' @param wage_bill The nominal wage bill (EA-MD/QD `WS`), a `ts`.
#' @param employment Total employment (EA-MD/QD `TEMP`), a `ts`.
#' @param iso2 Two-letter lowercase ISO country code.
#'
#' @return A `koma_ts` level series, wage bill per worker. Windowed to the
#'   two inputs' overlap -- `WS` runs one quarter shorter than `TEMP` in the
#'   current vintage, and a ragged edge here is fine (koma fills those) but
#'   a length mismatch would silently recycle.
#' @keywords internal
derived_wage_rate <- function(wage_bill, employment, iso2) {
  if (is.null(wage_bill) || is.null(employment)) {
    cli::cli_abort(c(
      "Cannot derive {.val {country_var(iso2, 'wages')}}.",
      "x" = "EA-MD/QD is missing {.val WS} or {.val TEMP} for {.val {toupper(iso2)}}.",
      "i" = "There is no Eurostat fallback for a derived series; add one before modelling this country."
    ))
  }
  common_start <- max(stats::tsp(wage_bill)[1], stats::tsp(employment)[1])
  common_end <- min(stats::tsp(wage_bill)[2], stats::tsp(employment)[2])
  b <- stats::window(wage_bill, start = common_start, end = common_end)
  e <- stats::window(employment, start = common_start, end = common_end)
  koma::as_ets(
    stats::ts(as.numeric(b) / as.numeric(e),
      start = stats::start(b), frequency = stats::frequency(b)
    ),
    series_type = "level", method = "diff_log",
    country = toupper(iso2), source = "derived"
  )
}

#' Derive net borrowing as the change in the debt ratio
#'
#' The flow that drives the stage-3b debt accumulation identity
#' `govdebt == 1*govdebt.L(1) + 1*netborrowing`. Deriving it as the first
#' difference of the debt ratio makes that identity hold to machine precision
#' by construction, which is the whole point: koma's injected weights are
#' **not** time-varying (`weights.R` annualises the series, lags it a year and
#' keeps the last value -- one scalar for the entire sample and forecast), so
#' the `(1+i)/(1+g)` snowball factor that a debt-to-GDP law of motion needs
#' cannot be expressed as a weight. Folding it into the flow instead sets the
#' carry weight to exactly 1 and sidesteps the limitation.
#'
#' **State plainly what this variable is.** It is the change in the debt
#' ratio, which equals the headline deficit-to-GDP *plus* the
#' growth-denominator effect *plus* stock-flow adjustments. It is not the
#' Maastricht deficit and must not be reported as one. The honest reading of
#' a coefficient on it is "how the debt ratio moves", not "how the deficit
#' moves". Germany's actual revenue and expenditure would give the true
#' decomposition, but they only begin 2002Q1 and pulling them in costs the
#' whole system eight quarters of estimation window -- enough on its own to
#' take the residual degrees of freedom to zero.
#'
#' Signed by construction (the ratio falls as well as rises), hence
#' `series_type = "rate", method = "none"`.
#'
#' @param govdebt The debt-to-GDP ratio, as from [eurostat_govdebt()].
#' @param iso2 Two-letter lowercase ISO country code.
#' @return A `koma_ts` rate series, change in the debt ratio in percentage
#'   points. One observation shorter than `govdebt` at the front.
#' @keywords internal
derived_netborrowing <- function(govdebt, iso2) {
  if (is.null(govdebt)) {
    cli::cli_abort("Cannot derive {.val {country_var(iso2, 'netborrowing')}} without {.field govdebt}.")
  }
  d <- diff(as.numeric(govdebt))
  koma::as_ets(
    stats::ts(d,
      start = advance_periods(
        num_to_period(stats::tsp(govdebt)[1], stats::frequency(govdebt)), 1,
        stats::frequency(govdebt)
      ),
      frequency = stats::frequency(govdebt)
    ),
    series_type = "rate", method = "none",
    country = toupper(iso2), source = "derived"
  )
}

#' Derive private credit from EA-MD/QD loan components
#'
#' Credit to the private sector, as the sum of long- and short-term **loans**
#' owed by non-financial corporations and households.
#'
#' **Loans, not total liabilities.** `NFCLB` and `HHLB` are *total financial
#' liabilities*, which for corporations includes shares and other equity --
#' that is a balance-sheet aggregate, not credit, and it moves with the stock
#' market. The `.LLN`/`.SLN` sub-components are the actual loan stocks.
#'
#' **Why not ECB BSI.** The obvious alternative, MFI loans to the private
#' sector, is one clean series with a standard definition -- but its country
#' breakdowns begin 2003Q1, which would cost twelve quarters of a window that
#' starts in 2000Q1. The EA-MD/QD components span the whole window.
#'
#' **A transformation override, stated because it is one.**
#' `eamdqd_variable_map()` derives `method` mechanically from the EA-MD/QD
#' `TR` code, and `HHLB.LLN` carries `TR = 3` for Germany (and most of the
#' euro area), which the rule maps to `method = "none"`. On a stock in
#' millions of euro that is meaningless as an estimation input -- koma would
#' model the raw level inside a growth-rate system. The sum is therefore
#' tagged `diff_log` by hand. This is exactly the "needs a per-series
#' judgement call" case `data_eamdqd.R` warns about.
#'
#' @param raw The country's raw EA-MD/QD panel, from `eamdqd_panel()`.
#' @param code The EA-MD/QD country code (`"DE"`, `"EL"` for Greece).
#' @param iso2 Two-letter lowercase ISO country code.
#' @return A `koma_ts` level series, total private loans in millions of euro.
#' @keywords internal
derived_credit <- function(raw, code, iso2) {
  parts <- c("nfclb_lln", "nfclb_sln", "hhlb_lln", "hhlb_sln")
  names(parts) <- parts
  series <- lapply(parts, function(p) raw[[paste0(tolower(code), "_", p)]])
  missing <- names(series)[vapply(series, is.null, logical(1))]
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "EA-MD/QD is missing {.val {missing}} for {.val {toupper(iso2)}}.",
      "i" = "Private credit is the sum of the four loan components; a partial sum would be a different concept."
    ))
  }
  start <- max(vapply(series, function(x) stats::tsp(x)[1], numeric(1)))
  end <- min(vapply(series, function(x) stats::tsp(x)[2], numeric(1)))
  windowed <- lapply(series, function(x) as.numeric(stats::window(x, start = start, end = end)))
  total <- Reduce(`+`, windowed)
  koma::as_ets(
    stats::ts(total, start = num_to_period(start, 4), frequency = 4),
    series_type = "level", method = "diff_log",
    country = toupper(iso2), source = "derived"
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
build_country_panel <- function(iso2, eamdqd = NULL, stage3a = FALSE) {
  iso2 <- tolower(iso2)
  if (identical(iso2, "us")) {
    return(build_us_panel(stage3a = stage3a))
  }
  if (!iso2 %in% ea_countries) {
    cli::cli_abort("Unknown country {.val {iso2}}; expected one of {.val {c(ea_countries, 'us')}}.")
  }
  if (is.null(eamdqd)) {
    cli::cli_abort("{.arg eamdqd} is required for EA countries (an {.cls eamdqd_vintage} from {.fn fetch_eamdqd}).")
  }
  build_ea_country_panel(iso2, eamdqd, stage3a = stage3a)
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
#' @param stage3a A named list, `iso2 -> stage3a spec`, where each spec is
#'   `TRUE` (all `stage3a_concepts`) or a character vector naming a subset.
#'   Empty by default, so the panel is unchanged unless asked for. Stage 3a
#'   uses `list(de = TRUE, fr = "export_prices", ...)`: Germany carries the
#'   whole block, every partner contributes only the export price that
#'   Germany's foreign-price index is built from.
#'
#'   **The resulting panel is deliberately ragged across countries**, and
#'   that is safe: a panel is a named list, not a matrix, so a country simply
#'   lacking `<iso2>_wages` means no equation can name it.
#'   `harmonise_panel_attrs()` unions *attributes*, not series, and
#'   `stage2_preflight()` catches any equation that references a series the
#'   panel does not have.
#'
#' @return A named list of `koma_ts` objects, validated with
#'   [is_valid_project_name()].
#' @export
build_global_panel <- function(countries = modelled_countries, eamdqd = NULL, row_weights,
                               stage3a = list()) {
  unknown <- setdiff(names(stage3a), countries)
  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "{.arg stage3a} names {?a country/countries} not being built: {.val {unknown}}.",
      "i" = "Its names must be a subset of {.arg countries}."
    ))
  }
  panel <- list()
  for (cc in countries) {
    panel <- c(panel, build_country_panel(
      cc, eamdqd = eamdqd, stage3a = stage3a[[cc]] %||% FALSE
    ))
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
#' @param extend Pad a series that does not reach `start`/`end` with `NA`
#'   instead of leaving it short. `FALSE` by default, which preserves the
#'   original behaviour exactly.
#'
#'   **Why this exists.** With `start`/`end` computed automatically, one
#'   short series drags the *whole* panel in with it -- and because the
#'   bound is the earliest end across every series, that silently truncates
#'   the exogenous series koma needs past the forecast start, quietly
#'   shortening the forecast horizon rather than erroring. Stage 3a hits
#'   this: Eurostat publishes `<iso2>_nonenergy_prices` one quarter behind
#'   the rest of the panel, which would have pulled a 2026Q1 panel back to
#'   2025Q4. Passing an explicit `end` with `extend = TRUE` keeps the
#'   established window and leaves the short series with a trailing `NA` --
#'   a ragged edge, which koma fills itself, rather than a truncation
#'   nothing would have caught.
#'
#' @return The windowed panel, same names as `panel`.
#' @export
align_panel <- function(panel, start = NULL, end = NULL, extend = FALSE) {
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
    windowed <- stats::window(x, start = start, end = end, extend = extend)
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
