# Stage 2d: the panel side of the regional core.
#
# Stage 2d re-partitions the modelled world. Germany, France, Italy and the
# US stay as they are; the other seven modelled euro-area economies are
# collapsed into one bloc, `reu`; and China joins as a genuinely new economy.
# This file builds the two new country panels -- the aggregated `reu_*`
# series and the assembled `cn_*` series -- on top of the panel the existing
# pipeline already produces.
#
# It is deliberately a SEPARATE step from build_global_panel(). Two reasons,
# both about not disturbing what already works:
#
#   1. `align_panel()` takes the earliest end across the whole panel, so
#      folding China's trade series (which run a quarter behind) into the
#      main panel would silently truncate every exogenous series and shorten
#      the forecast horizon -- the trap CLAUDE.md records for stage 3a.
#      add_stage2d_countries() aligns its own additions to the panel it is
#      given, with `extend = TRUE`, so the existing window is preserved.
#   2. Stages 1, 2a, 2b and 2c must keep reproducing byte-for-byte from
#      their cached fits. Nothing here touches the series they read.

#' The euro-area members the `reu` bloc aggregates
#'
#' Every modelled euro-area economy except Germany, France and Italy -- the
#' three kept separate in stage 2d. `reu` is a *pseudo-country* (see
#' `bloc_codes`): it gets a `reu_<concept>` series for every concept a real
#' country has, its own [country_block()], and its own row and column in the
#' trade-weight matrix. Its members contribute no equations of their own.
#'
#' **The name is "rest of the euro area", not "rest of the EU".** The bloc
#' covers the seven modelled EA economies, which is what the panel has data
#' for; it is not the EU, and it is not the whole euro area either (the six
#' smallest EA members were never in `modelled_countries`). Trade with
#' everything genuinely outside the six stage-2d entities still goes to
#' `row_gdp`, so nothing is double-counted -- but `reu_gdp` is *not* a
#' published aggregate and should not be compared against one.
#' @export
reu_members <- function() c("at", "be", "es", "gr", "ie", "nl", "pt")

#' Concepts the `reu` bloc aggregates
#'
#' The base country concept set plus `domestic_demand`. Stage-3a/3b concepts
#' are deliberately absent: no stage-3 block is defined for a bloc, and
#' aggregating (say) a wage rate across seven economies with different wage
#' levels is a different and much less well-posed problem than aggregating a
#' volume index.
#' @keywords internal
bloc_concepts <- function() {
  c(names(eamdqd_concept_codes), "domestic_demand")
}

#' Aggregate a set of countries into one bloc pseudo-country
#'
#' Builds `<code>_<concept>` for every concept in `concepts`, from the
#' member countries' own series, weighted by `weights`.
#'
#' **Two aggregators, chosen by the series' own attributes.**
#'
#' - A `series_type = "level"` concept (every volume and every price index)
#'   is aggregated with [chain_weighted_index()], i.e. as a weighted average
#'   of *growth rates* integrated back to an index. That is the only
#'   construction that survives koma's `diff_log` conversion:
#'   `log(sum_i w_i x_i) != sum_i w_i log(x_i)`, and the whole bloc exists to
#'   be differenced. It is the same reasoning that governs
#'   `<iso2>_foreign_demand` and `ea_gdp` -- see [chain_weighted_index()].
#' - A `series_type = "rate"` concept (`unemployment`, `long_rate`) is a
#'   weighted average of the **levels**, via [apply_weights()]. A rate is
#'   already in the space koma estimates on, so averaging the levels *is*
#'   averaging in rate space; chaining it would be meaningless (and
#'   [chain_weighted_index()] refuses a rate component outright).
#'
#' **The bloc's own accounting identities therefore hold only
#' approximately**, exactly as a real country's do. `reu_gdp` is a chained
#' average of seven national GDP growth rates, and `reu_domestic_demand` a
#' chained average of seven national domestic-demand growth rates; the
#' identity `reu_gdp == dd*reu_domestic_demand + x*reu_exports -
#' m*reu_imports` reconciles up to the same log-linearisation and statistical
#' -discrepancy slack that `<iso2>_gdp` already carries, which is why
#' [identity_consistency()] reports but never flags it.
#'
#' @param panel A named list of `koma_ts` containing every member's series.
#' @param code The bloc's code, one of `bloc_codes`.
#' @param members Character vector of ISO-2 member codes.
#' @param weights Named numeric vector over `members`, summing to 1 (nominal
#'   GDP shares within the bloc -- see [bloc_gdp_weights()]).
#' @param concepts Which concepts to build.
#'
#' @return A named list of `koma_ts`, one per concept, keyed
#'   `<code>_<concept>`.
#' @export
aggregate_bloc_panel <- function(panel, code, members, weights,
                                 concepts = bloc_concepts()) {
  members <- tolower(members)
  if (!code %in% bloc_codes) {
    cli::cli_abort(c(
      "{.val {code}} is not a declared bloc code.",
      "i" = "Add it to {.field bloc_codes} in {.file R/equations.R} first, so {.fn country_var} accepts it."
    ))
  }
  missing_w <- setdiff(members, names(weights))
  if (length(missing_w) > 0) {
    cli::cli_abort("{.arg weights} has no entry for {.val {missing_w}}.")
  }
  weights <- weights[members]
  if (abs(sum(weights) - 1) > 1e-8) {
    cli::cli_abort("{.arg weights} must sum to 1 over {.arg members}; got {.val {sum(weights)}}.")
  }

  out <- list()
  for (concept in concepts) {
    names_needed <- country_var(members, concept)
    absent <- names_needed[!names_needed %in% names(panel)]
    if (length(absent) > 0) {
      cli::cli_abort("{.arg panel} is missing {.val {absent}}, needed to aggregate {.val {code}}.")
    }
    type <- attr(panel[[names_needed[1]]], "series_type") %||% "level"

    series <- if (identical(type, "rate")) {
      apply_weights(panel, concept, stats::setNames(as.numeric(weights), members))
    } else {
      chain_weighted_index(panel, stats::setNames(as.numeric(weights), names_needed))
    }

    out[[country_var(code, concept)]] <- koma::as_ets(
      series,
      series_type = concept_series_type[[concept]] %||% type,
      method = concept_method[[concept]] %||% (attr(series, "method") %||% "diff_log"),
      country = toupper(code), source = "derived"
    )
  }
  out
}

#' Nominal-GDP weights within a bloc
#'
#' Slices [build_gdp_weight_matrix()]'s euro-area shares down to the bloc's
#' members and renormalises them to sum to 1. Renormalising is what makes the
#' bloc an *average* of its members rather than a fraction of the euro area.
#'
#' @param gdp_weights The `W_gdp` named vector from [build_gdp_weight_matrix()].
#' @param members Character vector of ISO-2 member codes.
#' @param digits Rounding, matching [stage2_linkage_weights()].
#'
#' @return A named numeric vector over `members`, summing to 1.
#' @export
bloc_gdp_weights <- function(gdp_weights, members = reu_members(), digits = 4) {
  members <- tolower(members)
  missing <- setdiff(members, names(gdp_weights))
  if (length(missing) > 0) {
    cli::cli_abort("{.arg gdp_weights} covers none of {.val {missing}}.")
  }
  w <- gdp_weights[members] / sum(gdp_weights[members])
  # Round then renormalise, so the vector still sums to exactly 1 -- both
  # aggregate_bloc_panel() and chain_weighted_index() take the weights as
  # given and neither rescales.
  w <- round(w, digits)
  w / sum(w)
}

# --------------------------------------------------------------------------
# China
# --------------------------------------------------------------------------

#' World Bank GEM indicator ids used for China
#'
#' `SA` in the middle of each id means seasonally adjusted; `KN`/`KD` are
#' constant-price local currency / US dollars, `CD` current US dollars.
#' @keywords internal
cn_gem_indicators <- c(
  gdp_real = "NYGDPMKTPSAKN",
  gdp_nominal = "NYGDPMKTPSACD",
  exports_nominal = "DXGSRMRCHSACD",
  imports_nominal = "DMGSRMRCHSACD",
  exports_price = "DXGSRMRCHSAXD",
  imports_price = "DMGSRMRCHSAXD",
  prices = "CPTOTSAXN"
)

#' China's real merchandise trade, deflated rather than taken as published
#'
#' GEM publishes constant-price trade series for China (`DXGSRMRCHSAKD`,
#' `DMGSRMRCHSAKD`) and they are **not usable as they stand**: from 2020 they
#' are missing **every first quarter** -- 2020Q1 through 2025Q1, six of the
#' eighty quarters in the stage-2d window, and all of them in the middle of the
#' sample rather than at a ragged edge. koma cannot estimate across an internal
#' `NA` at all, so those six would have to be interpolated, which is invented
#' data in the most recent and most interesting stretch of the sample. (The
#' cause is upstream: China's customs administration stopped publishing a
#' separate January figure, merging January and February to smooth the moving
#' Lunar New Year.)
#'
#' The current-price and price-index series for the same flows are **complete**
#' over the whole span, so the volume is recovered as `value / price` instead.
#' That is the same quotient GEM computes internally, and it reproduces the
#' published constant-price series where both exist to a mean ratio of
#' **0.99985** with a standard deviation of **4.4e-4** (exports; imports
#' 0.99974 / 9.4e-4) -- verified over the 77 and 117 overlapping quarters of
#' the 2026-09 vintage. So this is a reconstruction of a published series from
#' its own published components, not a proxy: it recovers six real quarters
#' rather than interpolating them.
#'
#' @param iso3 ISO-3 country code.
#' @param side `"exports"` or `"imports"`.
#' @return A quarterly `ts`, constant-price US dollars (millions).
#' @keywords internal
cn_real_trade <- function(iso3, side = c("exports", "imports")) {
  side <- match.arg(side)
  value <- wb_gem_quarterly(cn_gem_indicators[[paste0(side, "_nominal")]], iso3)
  price <- wb_gem_quarterly(cn_gem_indicators[[paste0(side, "_price")]], iso3)
  ts_ratio(value, price)
}

#' China's long-term interest rate, spliced
#'
#' **This is the weakest series in the stage-2d panel and the splice is
#' stated rather than buried.** The OECD publishes a genuine Chinese
#' long-term (10-year government bond) rate, `IRLT`, but only from
#' **2014Q1** -- 44 of the 80 quarters in the stage-2d estimation window, and
#' none of the first 36. The three-month interbank rate `IR3TIB` runs from
#' 1997Q3 and covers the whole window bar one quarter (2006Q1, an internal
#' gap that [fill_internal_gaps()] interpolates and warns about).
#'
#' So `cn_long_rate` is `IRLT` wherever `IRLT` exists, and `IR3TIB` shifted
#' by the mean `IRLT - IR3TIB` gap over their overlap before that. An
#' **additive** shift, not the multiplicative one [fred_spliced_dollar_index()]
#' uses: these are rates in percentage points, they pass through koma
#' untransformed (`series_type = "rate"`, `method = "none"`), and a ratio
#' splice on a series that can approach zero is not safe.
#'
#' **Measured, that gap is small but noisy**: over the 50 overlapping quarters
#' (2014Q1-2026Q2) `IRLT - IR3TIB` has mean **-0.262** percentage points with
#' standard deviation **0.482** and a range of -1.69 to +0.36. The standard
#' deviation is nearly twice the mean, so the spliced segment reproduces the
#' *level* of a Chinese long rate but not its independent variation: before
#' 2014 `cn_long_rate` moves exactly like the three-month interbank rate. The
#' consequence for the model is specific and worth stating -- `cn_spread`
#' (`cn_long_rate - cn_policy_rate`) is a genuine term premium over the second
#' half of the sample and a money-market term spread plus a constant over the
#' first.
#'
#' If that is not acceptable for a given exercise, the clean alternative is to
#' drop China from `spread_countries` and let `cn_domestic_demand` load
#' `cn_policy_rate` directly: `IRSTCI` is complete over the whole window and
#' needs no splice at all. `stage2d_config(spread_countries = )` makes that a
#' one-argument change.
#'
#' @param ref_area OECD reference area code.
#' @param ... Passed to [oecd_finmark_rate()].
#'
#' @return A list with `series` (a quarterly `ts`), `offset` (the mean gap
#'   applied), `offset_sd`, `overlap` (number of quarters averaged over) and
#'   `spliced_before` (the first period taken from `IRLT`).
#' @export
cn_long_rate_series <- function(ref_area = "CHN", ...) {
  long <- oecd_finmark_rate(ref_area, "IRLT", ...)
  short <- oecd_finmark_rate(ref_area, "IR3TIB", ...)

  overlap_start <- max(stats::tsp(long)[1], stats::tsp(short)[1])
  overlap_end <- min(stats::tsp(long)[2], stats::tsp(short)[2])
  if (overlap_start > overlap_end) {
    cli::cli_abort(c(
      "!" = "OECD {.val IRLT} and {.val IR3TIB} for {.val {ref_area}} no longer overlap.",
      "i" = "{.fn cn_long_rate_series} needs a common window to measure the splice offset."
    ))
  }
  a <- as.numeric(stats::window(long, start = overlap_start, end = overlap_end))
  b <- as.numeric(stats::window(short, start = overlap_start, end = overlap_end))
  gap <- a - b
  offset <- mean(gap, na.rm = TRUE)

  head_end <- overlap_start - 1 / 4
  head_part <- if (stats::tsp(short)[1] <= head_end) {
    as.numeric(stats::window(short, end = head_end)) + offset
  } else {
    numeric(0)
  }
  spliced <- stats::ts(
    c(head_part, as.numeric(long)),
    start = if (length(head_part) > 0) stats::start(short) else stats::start(long),
    frequency = 4
  )

  list(
    series = spliced,
    offset = offset,
    offset_sd = stats::sd(gap, na.rm = TRUE),
    overlap = sum(!is.na(gap)),
    spliced_before = num_to_period(overlap_start, 4)
  )
}

#' Build China's country panel
#'
#' China is the one modelled economy with no expenditure-side quarterly
#' national accounts, and that is a fact about the data rather than a gap in
#' this project's search. Verified against OECD Quarterly National Accounts
#' (China carries `B1GQ` only, and only from 2011Q1), the OECD Economic
#' Outlook (annual for everything but CPI and two interest rates), IMF
#' International Financial Statistics, World Bank GEM, FRED (its China
#' coverage was discontinued between 2019 and 2023) and the NBS itself: none
#' publishes quarterly household consumption or gross fixed capital
#' formation. `<iso2>_consumption` and `<iso2>_investment` therefore do not
#' exist for China, and [country_block()]'s `merged_demand_countries` option
#' exists so the two collapse into one estimated `cn_domestic_demand`
#' equation rather than being invented from indicator proxies.
#'
#' **Sources**, all free and keyless:
#'
#' | series | source | span (2026-09 vintage) |
#' |---|---|---|
#' | `cn_gdp` | WB GEM `NYGDPMKTPSAKN`, real GDP, SA | 1995Q1-2025Q4 |
#' | `cn_prices` | WB GEM `CPTOTSAXN`, CPI, SA, monthly | 1995M01-2026M02 |
#' | `cn_exports` | WB GEM `DXGSRMRCHSACD / DXGSRMRCHSAXD` (see [cn_real_trade()]) | 2005Q1-2025Q3 |
#' | `cn_imports` | WB GEM `DMGSRMRCHSACD / DMGSRMRCHSAXD` | 1995Q1-2025Q3 |
#' | `cn_policy_rate` | OECD `DSD_STES@DF_FINMARK` `IRSTCI` | 1990Q1-2025Q2 |
#' | `cn_long_rate` | OECD `IRLT` spliced onto `IR3TIB` | see [cn_long_rate_series()] |
#'
#' `cn_exports` starting 2005Q1 is what sets the stage-2d estimation window
#' (see [stage2d_dates()]); it is also, conveniently, after China's December
#' 2001 WTO accession.
#'
#' **Two constructions, both documented approximations.**
#'
#' *Units.* GDP is constant local currency, the trade volumes are constant US
#' dollars, and the three cannot be added. Both volume series are therefore
#' rescaled by a single constant so that their mean over `ref` equals the
#' mean nominal trade share of GDP times mean real GDP -- the shares being
#' computed from GEM's own current-dollar GDP and trade series, so the
#' numerator and denominator share a currency. Over 2005Q1-2024Q4 those
#' shares are **0.229** (exports) and **0.188** (imports) of GDP. A constant
#' multiplicative rescaling changes no growth rate, so this complies with
#' the levels-only ingestion policy: koma still does the `diff_log`
#' conversion itself, on exactly the published volume index.
#'
#' *Domestic demand as the residual.* `cn_domestic_demand` is
#' `cn_gdp - cn_exports + cn_imports` in those rescaled units, which makes
#' the GDP identity hold in level space by construction. Because the trade
#' series are **merchandise only**, the residual absorbs the services
#' balance as well as genuine domestic demand -- for China, a persistent
#' services *deficit*, so `cn_domestic_demand` is a little larger than true
#' domestic demand and grows a little differently. This is the same class of
#' approximation as `<iso2>_gdp`'s statistical discrepancy, and
#' [identity_consistency()] treats it the same way: reported, never flagged.
#'
#' @param dates A koma `dates` list; the nominal trade shares are averaged
#'   over `dates$estimation`. Defaults to [stage2d_dates()].
#' @param iso3 ISO-3 code, for the data providers.
#'
#' @return A named list of `koma_ts` keyed `cn_<concept>`, carrying a
#'   `cn_shares` attribute with the nominal shares used and the long-rate
#'   splice diagnostics.
#' @export
build_cn_panel <- function(dates = stage2d_dates(), iso3 = "CHN") {
  q <- function(key, ...) wb_gem_quarterly(cn_gem_indicators[[key]], iso3, ...)

  gdp_real <- q("gdp_real")
  gdp_nominal <- q("gdp_nominal")
  exports_nominal <- q("exports_nominal")
  imports_nominal <- q("imports_nominal")
  # Deflated here rather than taken from GEM's own constant-price series, which
  # is missing every Q1 from 2020 -- see cn_real_trade().
  exports_volume <- cn_real_trade(iso3, "exports")
  imports_volume <- cn_real_trade(iso3, "imports")
  # CPI is monthly-only in GEM for China; aggregation = 1 (period average) is
  # the right quarterly aggregate for a price index, as everywhere else here.
  prices <- q("prices", frequency = "M", aggregation = 1)

  window_mean <- function(x) {
    w <- stats::window(x, start = dates$estimation$start, end = dates$estimation$end,
                       extend = TRUE)
    mean(as.numeric(w), na.rm = TRUE)
  }
  share <- function(nominal) {
    ratio <- ts_ratio(nominal, gdp_nominal)
    window_mean(ratio)
  }
  export_share <- share(exports_nominal)
  import_share <- share(imports_nominal)

  # A pure level rescaling: every growth rate is untouched, which is what
  # makes it legal under the levels-only ingestion policy.
  rescale <- function(volume, target_share) {
    factor <- target_share * window_mean(gdp_real) / window_mean(volume)
    volume * factor
  }
  exports <- rescale(exports_volume, export_share)
  imports <- rescale(imports_volume, import_share)

  domestic_demand <- ts_combine(list(gdp_real, exports, imports), c(1, -1, 1))
  if (any(as.numeric(domestic_demand) <= 0, na.rm = TRUE)) {
    cli::cli_abort(c(
      "Constructed {.val cn_domestic_demand} is non-positive somewhere.",
      "i" = "It is tagged {.val level}/{.val diff_log}, so {.fn log} would give {.val NaN} -- which koma reports as an internal {.val NA}, pointing at the wrong problem."
    ))
  }

  long_rate <- cn_long_rate_series()
  policy_rate <- oecd_finmark_rate("CHN", "IRSTCI")

  tag <- function(x, concept, source, series_type = NULL, method = NULL) {
    koma::as_ets(
      x,
      series_type = series_type %||% concept_series_type[[concept]],
      method = method %||% concept_method[[concept]],
      country = "CN", source = source
    )
  }

  out <- list()
  out[[country_var("cn", "gdp")]] <- tag(gdp_real, "gdp", "worldbank_gem")
  out[[country_var("cn", "domestic_demand")]] <- tag(domestic_demand, "domestic_demand", "derived")
  out[[country_var("cn", "exports")]] <- tag(exports, "exports", "worldbank_gem")
  out[[country_var("cn", "imports")]] <- tag(imports, "imports", "worldbank_gem")
  out[[country_var("cn", "prices")]] <- tag(prices, "prices", "worldbank_gem")
  out[[country_var("cn", "long_rate")]] <- tag(long_rate$series, "long_rate", "oecd")
  out[[country_var("cn", "policy_rate")]] <- tag(
    policy_rate, "policy_rate", "oecd",
    series_type = "rate", method = "none"
  )

  attr(out, "cn_shares") <- list(
    exports = export_share, imports = import_share,
    domestic_demand = 1 - export_share + import_share,
    long_rate_splice = long_rate[c("offset", "offset_sd", "overlap", "spliced_before")]
  )
  out
}

#' Element-wise ratio of two `ts` over their common window
#' @keywords internal
ts_ratio <- function(numerator, denominator) {
  start <- max(stats::tsp(numerator)[1], stats::tsp(denominator)[1])
  end <- min(stats::tsp(numerator)[2], stats::tsp(denominator)[2])
  a <- stats::window(numerator, start = start, end = end)
  b <- stats::window(denominator, start = start, end = end)
  stats::ts(as.numeric(a) / as.numeric(b), start = stats::start(a),
            frequency = stats::frequency(a))
}

#' Weighted sum of `ts` levels over their common window
#'
#' The level-space counterpart to [chain_weighted_index()], used only where a
#' level-space sum is what is actually wanted -- assembling China's domestic
#' demand as an accounting residual. Unlike [apply_weights()] it takes the
#' series directly rather than looking them up by concept.
#' @keywords internal
ts_combine <- function(series_list, weights) {
  start <- max(vapply(series_list, function(x) stats::tsp(x)[1], numeric(1)))
  end <- min(vapply(series_list, function(x) stats::tsp(x)[2], numeric(1)))
  if (start > end) {
    cli::cli_abort("The series being combined have no overlapping window.")
  }
  aligned <- lapply(series_list, function(x) {
    as.numeric(stats::window(x, start = start, end = end))
  })
  total <- Reduce(`+`, Map(function(x, w) x * w, aligned, as.numeric(weights)))
  stats::ts(total, start = num_to_period(start, 4), frequency = 4)
}

#' Add the stage-2d bloc and China panels to an existing panel
#'
#' Appends `reu_*` (see [aggregate_bloc_panel()]) and `cn_*` (see
#' [build_cn_panel()]) to a panel that already carries the eleven modelled
#' economies, then aligns the additions to the panel's own window.
#'
#' **`extend = TRUE` with an explicit `end` is the whole point.**
#' [align_panel()] with no bounds takes the earliest end across everything it
#' is given, so letting China's merchandise-trade series (a quarter behind
#' the rest) into an unbounded alignment would truncate every exogenous
#' series in the panel and silently shorten the forecast horizon rather than
#' erroring -- the trap `CLAUDE.md` records for stage 3a. Padding instead
#' leaves the short series a trailing `NA`, which is a ragged edge, which
#' koma fills itself.
#'
#' @param panel A named list of `koma_ts`, as produced by the `panel` target
#'   (aligned, internal gaps filled).
#' @param gdp_weights The `W_gdp` vector from [build_gdp_weight_matrix()].
#' @param dates koma `dates` list, passed to [build_cn_panel()].
#' @param members The bloc's members.
#'
#' @return `panel` with `reu_*` and `cn_*` appended, internal gaps filled.
#' @export
add_stage2d_countries <- function(panel, gdp_weights, dates = stage2d_dates(),
                                  members = reu_members()) {
  bloc <- aggregate_bloc_panel(
    panel, "reu", members, bloc_gdp_weights(gdp_weights, members)
  )
  china <- build_cn_panel(dates = dates)

  start <- num_to_period(do.call(min, lapply(panel, function(x) stats::tsp(x)[1])), 4)
  end <- num_to_period(do.call(max, lapply(panel, function(x) stats::tsp(x)[2])), 4)
  additions <- align_panel(c(bloc, china), start = start, end = end, extend = TRUE)

  out <- c(panel, additions)
  invalid <- names(out)[!is_valid_project_name(names(out))]
  if (length(invalid) > 0) {
    cli::cli_abort("Generated series with invalid project names: {.val {invalid}}.")
  }
  # China's three-month interbank rate has one missing quarter (2006Q1), and
  # koma cannot estimate across an internal NA. fill_internal_gaps() warns and
  # names it, which is the contract: an interpolated observation is invented
  # data and the caller has to know it is there.
  fill_internal_gaps(out)
}

#' Expenditure shares for a bloc pseudo-country
#'
#' **[expenditure_shares()] cannot be used on a bloc, and using it produces a
#' plausible-looking wrong answer rather than an error.** It forms each share
#' as the mean of a *level* ratio, which works for a real country because its
#' series are all in one currency at one scale. Every bloc series is a
#' base-100 chain index instead ([chain_weighted_index()]), so the ratio is
#' not a share at all -- it is the two indices' relative growth since the base
#' period. Measured on the stage-2d panel, `mean(reu_exports / reu_gdp)` comes
#' out at **1.337**, against a true euro-area export share of roughly 0.40:
#' `reu` exports grew 2.5x since 2000 while its GDP grew 1.6x, and the ratio
#' reports that instead. koma has no identity-consistency check, so the
#' resulting `reu_gdp` identity would have been enforced silently.
#'
#' The right weights are the members' own shares, averaged with the same
#' weights the bloc's series were built from. That is exact in the sense that
#' matters: the bloc's growth rate is the weighted average of its members'
#' growth rates, each of which satisfies its own identity, so the weighted
#' average of the members' weights is the coefficient that reproduces the
#' identity most closely. It is only an *approximation* because the members'
#' shares differ from each other -- the bloc identity therefore reconciles up
#' to a dispersion term on top of the statistical discrepancy a single country
#' already carries, which is why [identity_consistency()] treats `<bloc>_gdp`
#' and `<bloc>_domestic_demand` exactly as it treats a country's.
#'
#' @param panel A named list of `koma_ts` containing every member's series.
#' @param code The bloc's code.
#' @param members Character vector of ISO-2 member codes.
#' @param weights Named numeric vector over `members`, summing to 1.
#' @param dates,digits,components As [expenditure_shares()].
#'
#' @return A list with `gdp` and `domestic_demand`, keyed by the bloc's own
#'   `<code>_<concept>` names, in [expenditure_shares()]'s shape.
#' @export
bloc_expenditure_shares <- function(panel, code, members, weights, dates = NULL,
                                    digits = 3, components = TRUE) {
  members <- tolower(members)
  weights <- weights[members]
  per_member <- lapply(members, function(m) {
    expenditure_shares(panel, m, dates, digits = 10, components = components)
  })

  average <- function(part, concepts) {
    values <- vapply(seq_along(members), function(i) {
      s <- per_member[[i]][[part]]
      as.numeric(s[country_var(members[i], concepts)]) * as.numeric(weights[[i]])
    }, numeric(length(concepts)))
    stats::setNames(round(rowSums(values), digits), country_var(code, concepts))
  }

  list(
    gdp = average("gdp", c("domestic_demand", "exports", "imports")),
    domestic_demand = if (components) {
      average("domestic_demand", c("consumption", "investment", "government"))
    } else {
      NULL
    }
  )
}
