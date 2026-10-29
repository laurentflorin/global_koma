# The quarterly panel for the VJA model, in levels.
#
# Sourced by scratch/vja_model_build.R, after devtools::load_all("."). The
# equations themselves are in scratch/vja_model_equations.R; this file only
# builds the series they name, including the ones that appear only inside
# identity weights (`dots_*`, `*_nominal_consumption`).
#
# Sources:
# - data/raw/vja_data.csv (scratch/vja_data_build.R): Macrobond and KOF.
# - Eurostat namq_10_gdp, for the national accounts of the four euro-area
#   entities (cached in data/cache/eurostat/). Three reasons:
#     * Macrobond's euro area changes composition, so every accession (Croatia
#       2023, Bulgaria 2026) is a level jump in anything derived from it.
#       Eurostat's EA20 is a fixed composition.
#     * Italy's Macrobond aggregates are 2010-reference volumes, the euro
#       area's are 2020-reference, so they cannot be subtracted.
#     * France's Macrobond services trade (INSEE) is a different concept from
#       Eurostat's P62/P72: 1.33 times smaller, growth correlation 0.78.
#   Everywhere else Eurostat and Macrobond agree exactly (verified 2026-10-29):
#   Germany is identical, Italy differs by a constant factor, France by
#   nothing outside goods imports and services.
#   Ireland and the Netherlands are fetched too, to be left out (below).
#
# Everything stays in levels (CLAUDE.md). What happens to the data here, all
# of it listed in the panel's "notes" attribute:
# - Monthly and daily series become quarterly averages. A trailing quarter
#   that is not complete yet is dropped.
# - Splices, in levels: euro-area short rate (EONIA minus its fixed spread
#   before ESTR), US short rate (fed funds plus the mean SOFR spread before
#   SOFR), US government consumption (the BEA quantity index scaled to the
#   OECD level, because the OECD series jumps 7.1% at 2007Q1).
# - Seasonal adjustment (STL on logs) of the price indices published
#   unadjusted: the four HICPs and UK CPI. Adjusted series stay levels. After
#   it no series shows seasonality (quarter-dummy F-test, 2004Q3-2019Q4).
# - Rest of euro area by subtraction. National accounts: EA20 minus Germany,
#   France, Italy, Ireland and the Netherlands (2020-reference volumes add up
#   exactly only in 2020, so this is an approximation elsewhere). Prices: the
#   euro-area HICP's growth less Germany's, France's and Italy's, at the same
#   household-consumption weights the ea_prices identity gets, so ea_prices
#   reproduces the euro-area HICP exactly. The long rate: the euro-area
#   benchmark less the three, at mean nominal GDP shares.
# - Ireland and the Netherlands are left out of the bloc's national accounts
#   because multinationals' intellectual-property transactions make their
#   investment and services imports swing by 15% a quarter (sd 2004Q3-2019Q4:
#   Dutch investment 15.8). With them, the bloc's investment growth has sd 6.6
#   and autocorrelation -0.68; without them, sd 1.8 and +0.61. The two flows
#   cancel in GDP, which separate investment and imports equations cannot
#   reproduce. Their prices and bond yields are not distorted, so they stay.
# - India starts in 2004Q2: the OECD series join two price bases without
#   linking them there (GDP +48.9% in one quarter).
# - China stops in 2026Q2: later KOF values are KOF's own forecast.

vja_entities <- c("de", "fr", "it", "reu", "us", "gb", "jp", "cn", "in")
vja_ea_members <- c("de", "fr", "it", "reu")
# Entities with the full set of national-accounts equations.
vja_full <- c("de", "fr", "it", "reu", "us", "gb", "jp")

vja_raw_country <- c(
  de = "Germany", fr = "France", it = "Italy", ea = "EA", us = "USA",
  gb = "UK", jp = "Japan", cn = "China", "in" = "India"
)

vja_eurostat_geo <- c(ea = "EA20", de = "DE", fr = "FR", it = "IT", ie = "IE", nl = "NL")
vja_eurostat_items <- c(
  gdp = "B1GQ", consumption = "P31_S14_S15", government = "P3_S13",
  investment = "P51G", exports = "P6", imports = "P7",
  goods_exports = "P61", services_exports = "P62",
  goods_imports = "P71", services_imports = "P72"
)

# Macrobond variable codes for the countries outside the euro area.
vja_raw_variable <- c(
  gdp = "GDP", consumption = "PCE", government = "GCE", investment = "GFCF",
  exports = "EXP", imports = "IMP", goods_exports = "EXP_G",
  services_exports = "EXP_S", goods_imports = "IMP_G", services_imports = "IMP_S"
)

vja_read_raw <- function(path = file.path("data", "raw", "vja_data.csv")) {
  raw <- utils::read.csv(path, stringsAsFactors = FALSE)
  raw$date <- as.Date(raw$date)
  raw
}

# One series at quarterly frequency. Quarterly series are taken as they are;
# monthly and daily ones are averaged over the observations each quarter has.
# Only the last quarter is dropped if incomplete: a quarter missing a month in
# the middle of the sample (US CPI, October 2025) is still an average.
vja_quarterly <- function(raw, country, variable) {
  x <- raw[raw$country == country & raw$variable == variable & !is.na(raw$value), ]
  if (nrow(x) == 0) {
    cli::cli_abort("{.file vja_data.csv} has no {.val {country}} {.val {variable}}.")
  }
  year <- as.integer(format(x$date, "%Y"))
  month <- as.integer(format(x$date, "%m"))
  index <- year * 4 + (month - 1) %/% 3
  if (x$frequency[1] != "quarterly") {
    last <- max(index)
    if (!any(index == last & month %% 3 == 0)) {
      keep <- index < last
      x <- x[keep, ]
      index <- index[keep]
    }
  }
  values <- tapply(x$value, index, mean)
  grid <- seq(min(index), max(index))
  stats::ts(unname(values[as.character(grid)]), start = c(min(grid) %/% 4, min(grid) %% 4 + 1), frequency = 4)
}

vja_level <- function(x) koma::as_ets(x, series_type = "level", method = "diff_log")
vja_rate <- function(x) koma::as_ets(x, series_type = "rate", method = "none")

# Eurostat namq_10_gdp for the euro-area entities, as one query so it is
# cached as one file. Changing anything in `filters` changes the cache key.
vja_eurostat_na <- function() {
  d <- eurostat::get_eurostat("namq_10_gdp", filters = list(
    geo = unname(vja_eurostat_geo), freq = "Q", unit = c("CP_MEUR", "CLV20_MEUR"),
    s_adj = "SCA", na_item = unname(vja_eurostat_items)
  ), time_format = "date", cache_dir = file.path("data", "cache", "eurostat"))
  d[!is.na(d$values), ]
}

vja_eurostat_series <- function(d, geo, item, unit) {
  x <- d[d$geo == geo & d$na_item == item & d$unit == unit, ]
  if (nrow(x) == 0) {
    cli::cli_abort("Eurostat returned nothing for {geo} {item} {unit}.")
  }
  eurostat_to_ts(x)
}

# Seasonal adjustment of a positive quarterly level: STL on the log, with a
# seasonal window wide enough to treat the pattern as slowly changing, robust
# to the COVID quarters.
vja_seasonal_adjust <- function(x) {
  x <- stats::na.omit(x)
  fit <- stats::stl(log(x), s.window = 13, robust = TRUE)
  exp(log(x) - fit$time.series[, "seasonal"])
}

# a and b on their common window, NA where either is missing.
vja_common <- function(...) {
  m <- stats::na.omit(cbind(...))
  lapply(seq_len(ncol(m)), function(i) m[, i])
}

# Rate series spliced onto a predecessor, in levels: the predecessor shifted by
# the mean gap over the overlap is used before `from`.
vja_splice_rate <- function(recent, older, from, gap = NULL) {
  both <- stats::na.omit(cbind(recent, older))
  if (is.null(gap)) {
    overlap <- stats::window(both, start = from)
    gap <- mean(overlap[, 1] - overlap[, 2])
  }
  start <- stats::start(older)
  end <- stats::end(recent)
  grid <- stats::ts(NA_real_, start = start, end = end, frequency = 4)
  times <- stats::time(grid)
  from_time <- from[1] + (from[2] - 1) / 4
  out <- ifelse(times < from_time - 1e-6,
                as.numeric(stats::window(older, start = start, end = end, extend = TRUE)) + gap,
                as.numeric(stats::window(recent, start = start, end = end, extend = TRUE)))
  structure(stats::ts(out, start = start, frequency = 4), gap = gap)
}

# Nominal GDP shares of Germany, France and Italy in the euro area, mean over
# a window, with the rest as the remainder. Used only to take the three out of
# the euro-area bond yield; the identities' weights are koma's own.
vja_ea_gdp_shares <- function(d, window) {
  members <- c(de = "DE", fr = "FR", it = "IT")
  ea <- stats::window(vja_eurostat_series(d, "EA20", "B1GQ", "CP_MEUR"), start = window$start, end = window$end)
  shares <- vapply(members, function(g) {
    x <- stats::window(vja_eurostat_series(d, g, "B1GQ", "CP_MEUR"), start = window$start, end = window$end)
    mean(x / ea)
  }, numeric(1))
  c(shares, reu = 1 - sum(shares))
}

# The sum over a calendar year: koma's injected weights are ratios of these.
vja_year_sum <- function(x, year) sum(stats::window(x, start = c(year, 1), end = c(year, 4)))

# Rest of euro area for a rate-space quantity: the euro-area growth (or
# level, for a rate) less the members' weighted contributions, divided by the
# rest's weight.
vja_reu_residual <- function(ea, members, weights) {
  parts <- vja_common(ea, members$de, members$fr, members$it)
  (parts[[1]] - weights[["de"]] * parts[[2]] - weights[["fr"]] * parts[[3]] -
     weights[["it"]] * parts[[4]]) / weights[["reu"]]
}

# Integrate quarterly diff_log growth (in decimals) to an index.
vja_index_from_growth <- function(g, base = 100) {
  stats::ts(base * exp(cumsum(c(0, as.numeric(g)))), end = stats::end(g), frequency = 4)
}

# `weight_year` is the year koma takes the identities' weights from (see
# vja_add_identity_series()); the euro-area price split has to use the same.
vja_build_panel <- function(raw = vja_read_raw(), eurostat = vja_eurostat_na(), weight_year,
                            share_window = list(start = c(2004, 2), end = c(2026, 2))) {
  panel <- list()
  notes <- character()

  # --- national accounts: euro area (Eurostat, 2020-reference volumes) ---------
  for (concept in names(vja_eurostat_items)) {
    item <- vja_eurostat_items[[concept]]
    series <- lapply(vja_eurostat_geo, function(g) vja_eurostat_series(eurostat, g, item, "CLV20_MEUR"))
    parts <- do.call(vja_common, unname(series))
    reu <- Reduce(`-`, parts)
    for (cc in c("de", "fr", "it")) panel[[paste0(cc, "_", concept)]] <- vja_level(series[[cc]])
    panel[[paste0("reu_", concept)]] <- vja_level(reu)
  }
  notes <- c(notes, "Euro-area national accounts: Eurostat namq_10_gdp, CLV20_MEUR, SCA; reu = EA20 - DE - FR - IT - IE - NL.")

  # --- national accounts: the rest (Macrobond) --------------------------------
  for (cc in c("us", "gb", "jp")) {
    for (concept in names(vja_raw_variable)) {
      panel[[paste0(cc, "_", concept)]] <- vja_level(vja_quarterly(raw, vja_raw_country[[cc]], vja_raw_variable[[concept]]))
    }
  }
  # US government: the OECD series jumps 7.1% at 2007Q1 and otherwise has the
  # BEA quantity index's growth to 0.01pp. Scale the index to the OECD level
  # over the clean stretch, so expenditure shares stay right.
  bea <- vja_quarterly(raw, "USA", "GCE_PRE")
  oecd <- panel$us_government
  clean <- vja_common(stats::window(oecd, start = c(2007, 1)), stats::window(bea, start = c(2007, 1)))
  scale <- mean(clean[[1]] / clean[[2]])
  panel$us_government <- vja_level(bea * scale)
  notes <- c(notes, sprintf("us_government: BEA quantity index (bea002_96a955ra_q) x %.4g, the mean OECD/BEA ratio from 2007Q1.", scale))

  for (cc in c("cn", "in")) {
    for (concept in c("gdp", "exports", "imports")) {
      panel[[paste0(cc, "_", concept)]] <- vja_level(vja_quarterly(raw, vja_raw_country[[cc]], vja_raw_variable[[concept]]))
    }
  }
  for (concept in c("gdp", "exports", "imports")) {
    panel[[paste0("in_", concept)]] <- vja_level(stats::window(panel[[paste0("in_", concept)]], start = c(2004, 2)))
    panel[[paste0("cn_", concept)]] <- vja_level(stats::window(panel[[paste0("cn_", concept)]], end = c(2026, 2)))
  }
  notes <- c(notes, "India from 2004Q2 (unlinked base-year break in the OECD series at 2004Q2); China to 2026Q2 (KOF forecast after).")

  # --- prices ---------------------------------------------------------------
  hicp <- lapply(c(ea = "EA", de = "Germany", fr = "France", it = "Italy"), function(cn) {
    vja_seasonal_adjust(vja_quarterly(raw, cn, "CPI"))
  })
  # Nominal household consumption: the weight series of the ea_prices
  # identity, and the weights that take the three out of the euro-area HICP.
  # Their reu includes Ireland and the Netherlands, as reu_prices does.
  nominal <- lapply(c(ea = "EA20", de = "DE", fr = "FR", it = "IT"), function(g) {
    vja_eurostat_series(eurostat, g, "P31_S14_S15", "CP_MEUR")
  })
  nominal$reu <- Reduce(`-`, do.call(vja_common, unname(nominal)))
  for (cc in names(nominal)) panel[[paste0(cc, "_nominal_consumption")]] <- vja_level(nominal[[cc]])
  w_prices <- vapply(vja_ea_members, function(cc) {
    vja_year_sum(nominal[[cc]], weight_year) / vja_year_sum(nominal$ea, weight_year)
  }, numeric(1))
  g <- lapply(hicp, function(x) diff(log(x)))
  reu_growth <- vja_reu_residual(g$ea, g[c("de", "fr", "it")], w_prices)
  for (cc in c("de", "fr", "it")) panel[[paste0(cc, "_prices")]] <- vja_level(hicp[[cc]])
  panel$reu_prices <- vja_level(vja_index_from_growth(reu_growth))
  panel$gb_prices <- vja_level(vja_seasonal_adjust(vja_quarterly(raw, "UK", "CPI")))
  panel$us_prices <- vja_level(vja_quarterly(raw, "USA", "CPI"))
  notes <- c(notes, sprintf(
    "HICPs and UK CPI seasonally adjusted (STL); reu_prices = EA HICP growth less DE/FR/IT at %d household-consumption weights %s.",
    weight_year, paste(sprintf("%s %.3f", names(w_prices), w_prices), collapse = ", ")
  ))

  # --- interest rates -------------------------------------------------------
  eonia <- vja_quarterly(raw, "EA", "SRATE_PRE")
  estr <- vja_quarterly(raw, "EA", "SRATE")
  ea_short <- vja_splice_rate(estr, eonia, from = c(2019, 4))
  panel$ea_short_rate <- vja_rate(ea_short)
  effr <- vja_quarterly(raw, "USA", "SRATE_PRE")
  sofr <- vja_quarterly(raw, "USA", "SRATE")
  us_short <- vja_splice_rate(sofr, effr, from = c(2014, 4))
  panel$us_short_rate <- vja_rate(us_short)
  panel$gb_short_rate <- vja_rate(vja_quarterly(raw, "UK", "SRATE"))
  panel$jp_short_rate <- vja_rate(vja_quarterly(raw, "Japan", "SRATE"))
  notes <- c(notes, sprintf(
    "ea_short_rate: ESTR from 2019Q4, EONIA %+.3f before; us_short_rate: SOFR from 2014Q4, fed funds %+.3f before.",
    attr(ea_short, "gap"), attr(us_short, "gap")
  ))

  long <- lapply(c(de = "Germany", fr = "France", it = "Italy", ea = "EA", us = "USA", gb = "UK", jp = "Japan"),
                 function(cn) vja_quarterly(raw, cn, "LRATE"))
  w_gdp <- vja_ea_gdp_shares(eurostat, share_window)
  for (cc in c("de", "fr", "it", "us", "gb", "jp")) panel[[paste0(cc, "_long_rate")]] <- vja_rate(long[[cc]])
  panel$reu_long_rate <- vja_rate(vja_reu_residual(long$ea, long[c("de", "fr", "it")], w_gdp))
  notes <- c(notes, sprintf(
    "reu_long_rate = EA 10-year benchmark less DE/FR/IT at nominal GDP weights %s.",
    paste(sprintf("%s %.3f", names(w_gdp), w_gdp), collapse = ", ")
  ))

  # --- exchange rates (BIS broad NEER; a rise is an appreciation) ------------
  for (cc in c("ea", "us", "gb", "jp")) {
    panel[[paste0(cc, "_neer")]] <- vja_level(vja_quarterly(raw, vja_raw_country[[cc]], "NEER"))
  }

  # --- COVID dummies --------------------------------------------------------
  template <- stats::ts(0, start = c(2000, 1), end = c(2030, 4), frequency = 4)
  for (d in c("covid_2020q1", "covid_2020q2", "covid_2020q3", "covid_2021q2")) panel[[d]] <- covid_dummy(d, template)

  # --- trade weights (IMF DOTS) ---------------------------------------------
  dots <- vja_dots_series(raw)
  panel <- c(panel, dots)
  coverage <- attr(dots, "coverage")
  notes <- c(notes, sprintf(
    "Foreign-demand weights: IMF DOTS goods exports, dots_<from>_<to>. Share of each entity's %d exports going to the other eight: %s.",
    weight_year, paste(sprintf("%s %.2f", rownames(coverage), coverage[, as.character(weight_year)]), collapse = ", ")
  ))

  structure(panel, notes = notes)
}

# --- trade weights (IMF DOTS) ------------------------------------------------
# Goods exports of each entity to each other one, quarterly, in USD: the weight
# series of the foreign-demand identities. `dots_<from>_<to>` is one flow and
# `dots_<from>_partners` the sum over the other eight, so each identity's
# weights sum to one. There is no rest-of-world series, so each entity's
# foreign demand assumes its unmodelled markets grow like its modelled ones;
# the `coverage` attribute is the share of its exports that assumption is not
# needed for, by year. The rest of the euro area as a reporter is the euro
# area (which counts trade between members) less Germany, France and Italy;
# as a partner, the euro area less whichever of the three is not the reporter.
# Its flows include Ireland and the Netherlands, which IMF DOTS cannot take out.
vja_dots_series <- function(raw, start = c(2004, 2)) {
  reporters <- c("de", "fr", "it", "ea", "us", "gb", "jp", "cn", "in")
  big3 <- c("de", "fr", "it")
  flow <- function(r, p) vja_quarterly(raw, vja_raw_country[[r]], paste0("X_", toupper(p)))
  difference <- function(first, rest) Reduce(`-`, do.call(vja_common, c(list(first), rest)))
  exports <- function(r, p) {
    if (r == "reu") return(difference(flow("ea", p), lapply(setdiff(big3, p), function(j) flow(j, p))))
    if (p == "reu") return(difference(flow(r, "ea"), lapply(setdiff(big3, r), function(j) flow(r, j))))
    flow(r, p)
  }
  out <- list()
  coverage <- list()
  for (r in vja_entities) {
    partners <- setdiff(vja_entities, r)
    flows <- lapply(partners, function(p) stats::window(exports(r, p), start = start))
    if (any(vapply(flows, function(x) any(x <= 0, na.rm = TRUE), logical(1)))) {
      cli::cli_abort("A {r} export flow in IMF DOTS is zero or negative; check the derived rest-of-euro-area rows.")
    }
    names(flows) <- paste0("dots_", r, "_", partners)
    total <- Reduce(`+`, do.call(vja_common, unname(flows)))
    for (nm in names(flows)) out[[nm]] <- vja_level(flows[[nm]])
    out[[paste0("dots_", r, "_partners")]] <- vja_level(total)
    world <- stats::window(exports(r, "world"), start = start)
    annual <- function(x) tapply(as.numeric(x), floor(stats::time(x) + 1e-6), sum)
    m <- vja_common(total, world)
    coverage[[r]] <- annual(m[[1]]) / annual(m[[2]])
  }
  structure(out, coverage = do.call(rbind, coverage))
}

# The weight series named inside an identity's weight expressions.
vja_weight_series <- function(identity) {
  terms <- unlist(strsplit(unlist(identity$weights, use.names = FALSE), "[-+*/]"))
  unique(trimws(terms[nzchar(trimws(terms))]))
}

# The identity left-hand sides that have no observed series, built in rate
# space from the weights koma gives the identity, so that each holds exactly:
# domestic demand, foreign demand, ea_gdp and ea_prices. GDP and the trade
# totals are observed and are left as they are; their identities hold up to
# inventories, the statistical discrepancy and chain-linking.
#
# koma evaluates a weight like (de_gdp/ea_gdp) on annual sums shifted a year
# and keeps only the last value, so every weight is the ratio in the year
# before dates$dynamic_weights$end. This calls koma's own function for it,
# which is what estimate() calls on level(ts_data), so the weights the series
# are built with are the ones koma estimates with.
#
# An identity whose left-hand side divides its own weights (domestic demand,
# ea_gdp) needs a level before they can be evaluated: the sum of its
# components, which are in the same units. The chained index is then scaled to
# that sum's level in the weight year, which leaves every weight unchanged.
#
# Returns the panel and the system with numeric weights in place of the
# expressions, for the checks that need numbers.
vja_add_identity_series <- function(panel, sys_eq, dates) {
  identities <- sys_eq$identities
  constructed <- setdiff(names(identities), names(panel))
  year <- floor(dates$dynamic_weights$end[1]) - 1
  self_weighted <- constructed[vapply(constructed, function(nm) nm %in% vja_weight_series(identities[[nm]]), logical(1))]
  for (nm in self_weighted) {
    parts <- do.call(vja_common, unname(panel[names(identities[[nm]]$components)]))
    panel[[nm]] <- vja_level(Reduce(`+`, parts))
  }
  evaluated <- koma:::get_seq_weights(panel, identities, dates)
  for (nm in constructed) {
    w <- stats::setNames(unlist(evaluated[[nm]]$weights, use.names = FALSE), names(evaluated[[nm]]$components))
    index <- chain_weighted_index(panel, w)
    if (nm %in% self_weighted) {
      index <- index * vja_year_sum(panel[[nm]], year) / vja_year_sum(index, year)
    }
    panel[[nm]] <- vja_level(index)
  }
  again <- koma:::get_seq_weights(panel, identities, dates)
  drift <- max(abs(unlist(lapply(again, `[[`, "weights")) - unlist(lapply(evaluated, `[[`, "weights"))))
  if (drift > 1e-12) {
    cli::cli_abort("Building the identity series moved koma's weights by {drift}.")
  }
  sys_eq$identities <- evaluated
  list(panel = panel, sys_eq = sys_eq)
}
