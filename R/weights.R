# Aggregation weights for building shared (ea_ / world_) variables from
# country-level series, and for the stage 3 regional/global blocks.
#
# Two matrices, both real data, both saved as CSV for review:
#   - W_trade: 11x12 bilateral trade weights (ECB "WTS" dataflow -- the
#     same trade-weight statistics behind the ECB's own effective
#     exchange rate indices), rows = modelled countries, columns = the
#     other 10 modelled countries + "row" (rest of world), each row
#     summing to 1.
#   - W_gdp: nominal GDP shares of the 10 EA countries within the euro
#     area (Eurostat namq_10_gdp), summing to 1.
#
# GOTCHA: the ECB's WTS dataflow is a euro-area effective-exchange-rate
# product. It has trade weights *for* every EA member state and for the
# EA aggregate ("I9") as reporters, but it does NOT compute weights *for*
# the US as a reporter -- there is no USD-denominated series in this
# dataflow. The US row of W_trade is therefore built from a documented
# approximation (symmetric reciprocal of each EA country's own weight on
# the US), not from a direct US-centric bilateral series -- see
# `build_trade_weight_matrix()`. This mirrors the same "document the
# approximation" latitude the project brief explicitly grants for
# `row_gdp` when a direct source isn't available.

row_partners <- c("gb", "ch", "cn", "jp", "pl", "se")

#' @keywords internal
row_partner_to_ecb <- c(gb = "GB", ch = "CH", cn = "CN", jp = "JP", pl = "PL", se = "SE")

#' @keywords internal
row_partner_to_imf <- c(gb = "GBR", ch = "CHE", cn = "CHN", jp = "JPN", pl = "POL", se = "SWE")

# --------------------------------------------------------------------------
# ECB bilateral trade weights
# --------------------------------------------------------------------------

#' Fetch one bilateral trade weight from the ECB WTS dataflow
#'
#' `TRADE_WEIGHT = O` (overall = average of import and export weights),
#' `TRD_PRODUCT = TMS` (total manufactured products and services -- the
#' broadest available basket), `SERIES_DENOM = F` (percent). `FREQ` and
#' `AREA_DEFINITION` are left wildcarded and the ECB API resolves them
#' automatically for a given (reporter, partner) pair.
#'
#' `CURRENCY_TRANS` is *not* just a currency label here -- for a
#' euro-area-aggregate reporter (`"I9"`) it distinguishes genuinely
#' different partner-group definitions (a narrow group vs. a broader one
#' that includes emerging-market partners), and picking the wrong one
#' changes the weight by a large factor, not a rounding difference:
#' verified `WTS.A.I9.GB...O.TMS.F` returns three variants for 2021 --
#' 0.205, 0.130 and 0.104 -- not the near-identical values a single
#' bilateral country pair returns (DE-FR's four variants agree to 3
#' decimal places; that is *not* representative of the aggregate-reporter
#' case). China and Poland are additionally only defined under the two
#' broadest variants (not the narrowest), which is itself evidence they
#' are narrow/broad group definitions. This function always picks the
#' lexicographically **last** available `CURRENCY_TRANS` code, which
#' resolves to the broadest partner-group definition and -- verified --
#' is the one variant available for every country [row_gdp_weights()]
#' needs, including China and Poland.
#'
#' @param ref_area,count_area ECB country codes (e.g. `"DE"`, `"US"`;
#'   note Greece is `"GR"` here, not EA-MD/QD's `"EL"` -- see
#'   `panel_build.R`).
#' @param window Number of most recent annual observations to average
#'   over.
#'
#' @param use_cache Logical; if `TRUE` (default) and a cached response
#'   exists under `data/cache/ecb/`, skip the network call. The ECB WTS
#'   dataflow is a slow-moving, periodically-revised weight scheme (not a
#'   live daily series), so caching it is safe between deliberate refreshes.
#'
#' @return A single numeric weight (0-1), or `NA` if the ECB has no series
#'   for this pair (expected for reporters outside the euro area, e.g.
#'   `"US"`).
#' @keywords internal
ecb_trade_weight <- function(ref_area, count_area, window = 3, use_cache = TRUE) {
  key <- sprintf("WTS.A.%s.%s...O.TMS.F", ref_area, count_area)
  cache_path <- file.path("data", "cache", "ecb", paste0(key, ".rds"))

  if (use_cache && file.exists(cache_path)) {
    d <- readRDS(cache_path)
  } else {
    d <- tryCatch(ecb::get_data(key), error = function(e) NULL)
    dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(d, cache_path)
  }

  if (is.null(d) || nrow(d) == 0) {
    return(NA_real_)
  }
  d <- d[order(d$obstime), ]
  if (length(unique(d$currency_trans)) > 1) {
    d <- d[d$currency_trans == max(d$currency_trans), ]
  }
  mean(utils::tail(d$obsvalue, window), na.rm = TRUE)
}

#' Build the 11x12 bilateral trade-weight matrix
#'
#' Rows are the 10 EA countries plus the US (`modelled_countries`);
#' columns are, for each row, the *other* 10 modelled countries plus
#' `"row"` (rest of world). Weights come from [ecb_trade_weight()],
#' averaged over `window` years, with the `"row"` column filled as the
#' residual (`1 - sum(other 10)`) and the whole row rescaled to sum to
#' exactly 1. The diagonal (a country against itself) is not part of the
#' matrix at all.
#'
#' For EA reporters this residual is close to zero -- the ECB weight
#' basket for a euro-area country already covers close to its full trade,
#' so almost all of it is captured by the other 10 modelled countries plus
#' non-modelled partners genuinely outside our 11. For the US row, see the
#' module-level note: the ECB has no US-reporter series, so
#' `W_trade["us", ]` is built from the *reciprocal* of each EA country's
#' own weight on the US (`ecb_trade_weight(j, "US")`), which is a
#' documented approximation, not a direct US-centric weight -- flagged
#' with a warning, and with a `source` attribute on the returned matrix
#' recording which rows are direct vs. reciprocal.
#'
#' @param countries Character vector of ISO-2 codes to build rows for.
#'   Defaults to `modelled_countries` (the 10 EA countries plus `"us"`).
#' @param window Number of most recent annual observations to average.
#' @param out_path Where to write the CSV. Defaults to
#'   `data/raw/W_trade.csv`. Pass `NULL` to skip writing.
#'
#' @return An 11x12 numeric matrix with row/column names, each row
#'   summing to 1, `attr(, "source")` a named character vector
#'   (`"direct"`/`"reciprocal"`) per row.
#' @export
build_trade_weight_matrix <- function(countries = modelled_countries, window = 3,
                                      out_path = file.path("data", "raw", "W_trade.csv")) {
  ecb_code <- function(iso2) if (identical(iso2, "us")) "US" else iso2_to_ecb[[iso2]]
  cols <- c(setdiff(countries, ""), "row")

  mat <- matrix(0, nrow = length(countries), ncol = length(cols),
               dimnames = list(countries, cols))
  row_source <- stats::setNames(rep("direct", length(countries)), countries)

  for (i in countries) {
    partners <- setdiff(countries, i)
    if (identical(i, "us")) {
      # No US-reporter series in ECB's WTS -- use the reciprocal of each
      # EA country's own weight on the US as a documented approximation.
      row_source["us"] <- "reciprocal"
      raw <- vapply(partners, function(j) ecb_trade_weight(ecb_code(j), "US", window), numeric(1))
      cli::cli_warn(c(
        "!" = "ECB WTS has no US-reporter trade-weight series.",
        "i" = "{.field W_trade[\"us\", ]} is built from the reciprocal of each EA country's own weight on the US, then renormalised -- a documented approximation, not a direct bilateral US series."
      ))
      if ("ie" %in% partners && !is.na(raw["ie"]) && raw["ie"] > 0.15) {
        cli::cli_warn(c(
          "!" = "W_trade[\"us\", \"ie\"] = {round(raw['ie'], 2)} is unusually large.",
          "i" = "Ireland's own ECB weight on the US is inflated by multinational corporate structures (the same distortion behind the 2015 Irish GDP break -- see data_eamdqd.R), and the reciprocal approximation carries that straight into the US row. Treat this cell as unreliable, not a genuine US-Ireland trade share."
        ))
      }
    } else {
      raw <- vapply(partners, function(j) ecb_trade_weight(ecb_code(i), ecb_code(j), window), numeric(1))
    }
    names(raw) <- partners

    if (anyNA(raw)) {
      cli::cli_warn("Missing ECB trade weight for {.val {toupper(i)}} vs {.val {toupper(partners[is.na(raw)])}}; treated as 0.")
      raw[is.na(raw)] <- 0
    }

    row_total_named <- sum(raw)
    row_residual <- max(0, 1 - row_total_named)
    full_row <- c(raw, row = row_residual)
    full_row <- full_row / sum(full_row) # exact row sum of 1, per spec

    mat[i, names(full_row)] <- full_row
  }

  attr(mat, "source") <- row_source

  if (!is.null(out_path)) {
    dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(mat, out_path, row.names = TRUE)
  }

  mat
}

# --------------------------------------------------------------------------
# GDP weights
# --------------------------------------------------------------------------

#' Fetch one country's nominal GDP level from Eurostat
#' @keywords internal
eurostat_nominal_gdp <- function(geo) {
  d <- eurostat::get_eurostat("namq_10_gdp", filters = list(
    geo = geo, freq = "Q", unit = "CP_MEUR", s_adj = "SCA", na_item = "B1GQ"
  ), time_format = "date", cache_dir = eurostat_cache_dir())
  eurostat_to_ts(d)
}

#' Build the euro-area GDP-share weight vector/matrix
#'
#' Nominal GDP shares of the 10 EA countries within the euro area
#' (Eurostat `namq_10_gdp`, current prices), averaged over the most recent
#' `window` quarters and normalised to sum to 1.
#'
#' @param countries Character vector of ISO-2 codes. Defaults to
#'   `ea_countries`.
#' @param window Number of most recent quarters to average GDP levels
#'   over before computing shares.
#' @param out_path Where to write the CSV (one row per country, columns
#'   `iso2`, `weight`). Defaults to `data/raw/W_gdp.csv`. Pass `NULL` to
#'   skip writing.
#'
#' @return A named numeric vector, one weight per country, summing to 1.
#' @export
build_gdp_weight_matrix <- function(countries = ea_countries, window = 4,
                                    out_path = file.path("data", "raw", "W_gdp.csv")) {
  levels <- vapply(countries, function(iso2) {
    gdp <- eurostat_nominal_gdp(iso2_to_eamdqd[[iso2]])
    mean(utils::tail(as.numeric(gdp), window), na.rm = TRUE)
  }, numeric(1))
  names(levels) <- countries

  weights <- levels / sum(levels)

  if (!is.null(out_path)) {
    dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(data.frame(iso2 = names(weights), weight = as.numeric(weights)),
                     out_path, row.names = FALSE)
  }

  weights
}

# --------------------------------------------------------------------------
# row_gdp weights (rest-of-world demand for the countries outside our 11)
# --------------------------------------------------------------------------

#' Build weights for the `row_gdp` aggregate
#'
#' Trade-weighted importance of the UK, Switzerland, China, Japan, Poland
#' and Sweden -- the major trading partners outside the 11 modelled
#' countries -- plus an `"other"` residual, from the euro area's own ECB
#' WTS trade weights (reporter `"I9"`, EA20 fixed composition).
#'
#' The euro area's raw WTS basket also includes the US (already modelled
#' separately, so it must not leak into `row_gdp`) and, implicitly, every
#' other non-EA trading partner in `"other"`. The US's share is therefore
#' subtracted before renormalising, so the returned weights sum to 1 over
#' exactly `{gb, ch, cn, jp, pl, se, other}`:
#' `weight_j = raw_weight_j / (1 - raw_weight_us)`.
#'
#' @param window Number of most recent annual observations to average.
#'
#' @return A named numeric vector (`gb`, `ch`, `cn`, `jp`, `pl`, `se`,
#'   `other`), summing to 1.
#' @export
row_gdp_weights <- function(window = 3) {
  raw <- vapply(row_partner_to_ecb, function(cc) ecb_trade_weight("I9", cc, window), numeric(1))
  names(raw) <- names(row_partner_to_ecb)
  us_share <- ecb_trade_weight("I9", "US", window)

  ex_us <- 1 - us_share
  weights <- raw / ex_us
  other <- 1 - sum(weights)

  c(weights, other = other)
}

#' Build the `row_gdp` series
#'
#' `row_gdp` is rest-of-world real GDP for the UK, Switzerland, China,
#' Japan, Poland and Sweden, trade-weighted by [row_gdp_weights()] into a
#' single index. Each country's real GDP growth comes from the IMF
#' DataMapper `NGDP_RPCH` indicator (annual, percent) -- Eurostat does not
#' cover China/Japan, and no key-free source of *quarterly* GDP exists for
#' this partner set, so a quarterly index is built by compounding the
#' annual growth rate evenly across that year's four quarters
#' (`(1+g)^(1/4) - 1` per quarter). This is a smoothing approximation:
#' it reproduces the correct annual growth rate but invents no
#' within-year dynamics -- documented here, not silently presented as a
#' genuine quarterly release.
#'
#' The `"other"` weight is not itself observable (no growth rate exists
#' for an undefined residual bloc), so it is folded into the six named
#' countries by renormalising their weights to sum to 1 for the purposes
#' of this weighted average -- mathematically equivalent to assuming
#' `"other"` grows at the six countries' own weighted-average rate, which
#' leaves that weighted average unchanged. The *stored* weights (e.g. in
#' any reporting table) should still use the full `row_gdp_weights()`
#' output including `"other"`, since that is the real trade-weight
#' decomposition; only the series construction renormalises it away.
#'
#' @param weights Named numeric vector as returned by [row_gdp_weights()].
#' @param start_year,end_year Bounds on the annual growth data used.
#'   `end_year` defaults to the current calendar year minus 1, since IMF
#'   DataMapper mixes actuals with WEO projections and does not flag which
#'   is which in this endpoint.
#'
#' @return A quarterly `koma_ts` (index, base 100 in `start_year` Q1),
#'   `series_type = "level"`, `method = "diff_log"`.
#' @export
build_row_gdp <- function(weights, start_year = 2000, end_year = as.integer(format(Sys.Date(), "%Y")) - 1) {
  growth <- imf_datamapper("NGDP_RPCH")

  named <- weights[names(row_partner_to_imf)]
  named_weights <- named / sum(named) # renormalise across the 6 named countries only

  years <- start_year:end_year
  g <- vapply(names(row_partner_to_imf), function(p) {
    code <- row_partner_to_imf[[p]]
    vals <- growth[[code]][as.character(years)]
    as.numeric(vals) / 100
  }, numeric(length(years)))
  weighted_growth <- as.numeric(g %*% named_weights)

  q_growth <- (1 + weighted_growth)^(1 / 4) - 1
  q_growth_series <- rep(q_growth, each = 4)
  index <- 100 * cumprod(c(1, 1 + q_growth_series))[-1]
  index <- c(100, index) # base period

  koma::as_ets(
    stats::ts(index, start = c(start_year, 1), frequency = 4),
    series_type = "level", method = "diff_log", source = "imf_datamapper"
  )
}

#' Fetch one IMF DataMapper indicator (all countries, all years)
#'
#' @param indicator IMF DataMapper indicator code, e.g. `"NGDP_RPCH"`.
#'
#' @return A named list, one element per ISO-3 country code, each a named
#'   list of `year -> value`.
#' @keywords internal
imf_datamapper <- function(indicator) {
  resp <- httr2::request(paste0("https://www.imf.org/external/datamapper/api/v1/", indicator)) |>
    httr2::req_perform()
  body <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  body$values[[indicator]]
}

# --------------------------------------------------------------------------
# Generic weighting helpers (used by stage 2/3 identities)
# --------------------------------------------------------------------------

#' Compute country aggregation weights
#'
#' Computes the weights used to aggregate country-level series into a
#' shared euro-area or world series, on the requested basis.
#'
#' @param countries Character vector of ISO-2 country codes to weight.
#' @param basis One of `"gdp"` (nominal GDP shares, [build_gdp_weight_matrix()])
#'   or `"trade"` (bilateral trade shares, [ecb_trade_weight()]-based:
#'   each country's share of the *group's total external trade*, i.e. the
#'   `1 - "row"` mass of [build_trade_weight_matrix()] redistributed across
#'   `countries`).
#' @param year Ignored for `basis = "trade"` (ECB weights are already a
#'   multi-year average, see `window` in the underlying fetchers); for
#'   `basis = "gdp"`, restricts the Eurostat window average to end no
#'   later than fourth quarter of `year`, if provided.
#'
#' @return A named numeric vector, one weight per element of `countries`,
#'   summing to 1.
#' @export
country_weights <- function(countries, basis = c("gdp", "trade"), year = NULL) {
  basis <- match.arg(basis)
  countries <- tolower(countries)

  if (basis == "gdp") {
    return(build_gdp_weight_matrix(countries, out_path = NULL))
  }

  # trade basis: each country's share of the group's combined external
  # (non-group) trade importance, from the "row" residual mass avoided by
  # each row's within-group partners.
  mat <- build_trade_weight_matrix(countries, out_path = NULL)
  within_group <- 1 - mat[, "row"]
  weights <- within_group / sum(within_group)
  weights
}

#' Apply weights to build a shared series from country series
#'
#' @param panel A named list of `koma_ts` objects (see [build_global_panel()]).
#' @param concept The concept to aggregate, e.g. `"gdp"` (looks up
#'   `<iso2>_gdp` for each country in `weights`).
#' @param weights Named numeric vector as returned by [country_weights()].
#' @param scope `"ea"` or `"world"`; determines the output variable's
#'   prefix via [shared_var()].
#'
#' @return A single `koma_ts`, the weighted aggregate.
#' @export
apply_weights <- function(panel, concept, weights, scope = c("ea", "world")) {
  scope <- match.arg(scope)
  series_list <- lapply(names(weights), function(iso2) panel[[country_var(iso2, concept)]])
  missing <- names(weights)[vapply(series_list, is.null, logical(1))]
  if (length(missing) > 0) {
    cli::cli_abort("{.arg panel} is missing {.val {country_var(missing, concept)}}.")
  }

  freqs <- unique(vapply(series_list, stats::frequency, numeric(1)))
  if (length(freqs) != 1) {
    cli::cli_abort("All series being weighted must share one frequency.")
  }

  starts <- vapply(series_list, function(x) stats::tsp(x)[1], numeric(1))
  ends <- vapply(series_list, function(x) stats::tsp(x)[2], numeric(1))
  common_start <- max(starts)
  common_end <- min(ends)

  aligned <- vapply(series_list, function(x) {
    as.numeric(stats::window(x, start = common_start, end = common_end))
  }, numeric(round((common_end - common_start) * freqs) + 1L))

  weighted <- as.numeric(aligned %*% as.numeric(weights))
  first <- series_list[[1]]

  koma::as_ets(
    stats::ts(weighted, start = stats::start(stats::window(first, start = common_start)), frequency = freqs),
    series_type = attr(first, "series_type"),
    method = attr(first, "method")
  )
}

#' Build a level index whose growth rate is a weighted average of others
#'
#' The rate-space counterpart to [apply_weights()], and the one to use when
#' the series being built is the left-hand side of a koma **identity**.
#'
#' **Why not [apply_weights()].** koma estimates on growth rates: a series
#' tagged `method = "diff_log"` is converted by `rate()` to
#' `100*diff(log(x))` before it reaches the sampler, and an identity like
#' `ea_gdp == 0.6*de_gdp + 0.4*fr_gdp` is therefore a statement about
#' *growth rates*, not levels. [apply_weights()] adds the **levels**, and
#' `log(0.6*a + 0.4*b) != 0.6*log(a) + 0.4*log(b)` -- so its output does not
#' satisfy the identity once koma differences it. The estimator would then
#' read one relationship out of the data while the identity's `Gamma` column
#' encodes another, with no error: koma performs no identity-consistency
#' check (see CLAUDE.md).
#'
#' That mismatch is tolerable for `<iso2>_gdp`, which is an independently
#' observed series whose identity holds only up to the national-accounts
#' statistical discrepancy anyway. It is **not** tolerable for a variable
#' like `<iso2>_foreign_demand` or `ea_gdp`, which has no observed
#' counterpart and exists only as its identity -- there, any gap is pure
#' construction error.
#'
#' So this builds the weighted average in growth space and integrates back:
#' `g = sum_i w_i * diff(log(x_i))`, then `index = base * exp(cumsum(g))`.
#' koma's `level()` inverts `diff_log` as `exp(cumsum(x/100))*100`
#' (`docs/koma-api.md` §1), so `rate()` of the result reproduces `100*g`
#' exactly and the identity holds to machine precision.
#'
#' Weights are **not** renormalised: they are used as given, so a set that
#' does not sum to 1 produces an index that is not a weighted average. That
#' is deliberate -- an identity's weights are a specification choice (see
#' [identity_equation()]), and silently rescaling them here would desync the
#' data from the equation string built elsewhere.
#'
#' Ragged edges are preserved: leading/trailing periods where any component
#' is `NA` come back `NA` rather than poisoning the whole chain through
#' `cumsum()`. An *internal* `NA` is an error, since a chained index cannot
#' bridge one -- use [fill_internal_gaps()] first.
#'
#' @param panel A named list of `koma_ts` objects (see [build_global_panel()]).
#' @param weights Named numeric vector keyed by **full variable name**
#'   (e.g. `c(fr_gdp = 0.065, row_gdp = 0.935)`), not by ISO-2 code as in
#'   [apply_weights()] -- the components of one of these indices need not
#'   share a concept, or even be country series.
#' @param base Index value at the first computable observation.
#'
#' @return A single `koma_ts` with `series_type = "level"` and
#'   `method = "diff_log"`, spanning the components' common window.
#' @export
chain_weighted_index <- function(panel, weights, base = 100) {
  if (length(weights) == 0 || is.null(names(weights)) || any(!nzchar(names(weights)))) {
    cli::cli_abort("{.arg weights} must be a non-empty vector named by variable.")
  }
  series_list <- lapply(names(weights), function(v) panel[[v]])
  missing <- names(weights)[vapply(series_list, is.null, logical(1))]
  if (length(missing) > 0) {
    cli::cli_abort("{.arg panel} is missing {.val {missing}}.")
  }

  # diff_log is meaningless on a series that is already a rate, and on a
  # non-positive one. Both are caller errors worth naming.
  types <- vapply(series_list, function(x) attr(x, "series_type") %||% NA_character_, character(1))
  not_level <- names(weights)[!identical(NA_character_, types) & types != "level"]
  if (length(not_level) > 0) {
    cli::cli_abort(c(
      "{.fn chain_weighted_index} needs {.field series_type} {.val level} components.",
      "x" = "{.val {not_level}} {?is/are} not."
    ))
  }
  non_positive <- names(weights)[vapply(series_list, function(x) any(x <= 0, na.rm = TRUE), logical(1))]
  if (length(non_positive) > 0) {
    cli::cli_abort("{.val {non_positive}} {?has/have} non-positive values; {.fn log} is undefined.")
  }

  freqs <- unique(vapply(series_list, stats::frequency, numeric(1)))
  if (length(freqs) != 1) {
    cli::cli_abort("All series being chained must share one frequency.")
  }
  common_start <- max(vapply(series_list, function(x) stats::tsp(x)[1], numeric(1)))
  common_end <- min(vapply(series_list, function(x) stats::tsp(x)[2], numeric(1)))
  if (common_start > common_end) {
    cli::cli_abort("{.arg weights} names series with no overlapping window.")
  }

  log_levels <- lapply(series_list, function(x) {
    log(as.numeric(stats::window(x, start = common_start, end = common_end)))
  })
  n <- length(log_levels[[1]])
  growth <- Reduce(`+`, Map(function(l, w) diff(l) * w, log_levels, as.numeric(weights)))

  observed <- which(!is.na(growth))
  if (length(observed) == 0) {
    cli::cli_abort("Every period is {.val NA} for at least one component; nothing to chain.")
  }
  first <- min(observed)
  last <- max(observed)
  if (anyNA(growth[first:last])) {
    cli::cli_abort(c(
      "A component has an internal {.val NA}; a chained index cannot bridge one.",
      "i" = "Run {.fn fill_internal_gaps} on {.arg panel} first."
    ))
  }

  index <- rep(NA_real_, n)
  index[first] <- base
  index[seq(first + 1L, last + 1L)] <- base * exp(cumsum(growth[first:last]))

  koma::as_ets(
    stats::ts(index, start = num_to_period(common_start, freqs), frequency = freqs),
    series_type = "level",
    method = "diff_log"
  )
}

#' HICP energy / non-energy weights, from Eurostat's published basket
#'
#' The weights for the stage-3a `<iso2>_prices` identity, taken **from the
#' data** rather than assumed: Eurostat `prc_hicp_inw` publishes the official
#' HICP item weights in per mille of the basket, re-set every year.
#'
#' **Why this split and not core/energy.** `NRG` and `TOT_X_NRG` partition
#' the basket exactly -- verified against the 2026-08 vintage, they sum to
#' `CP00` = 1000 in **every** year from 1996 to 2025, for Germany, with no
#' residual. The narrower "core" aggregate this project already carries
#' (`core_prices`, COICOP `TOT_X_NRG_FOOD`) does *not*: core plus energy
#' leaves food, alcohol and tobacco unaccounted for -- about 19% of the
#' German basket -- so a core/energy identity cannot be made exact, only
#' renormalised, which silently attributes food inflation to core. The
#' identity therefore uses non-energy, and `core_prices` is kept only as an
#' unmodelled cross-check.
#'
#' **The fixed-weight approximation, stated because it is one.** koma
#' identity weights are constants, but HICP weights are re-based annually.
#' Over 2000-2024 the German energy weight ranges from 88.4 to 125.5 per
#' mille (8.8%-12.6%) around a mean of 108.7. Averaging over the estimation
#' window is the honest fixed-weight choice; `max_deviation` in the result
#' reports how far any single year departs from it, so the size of the
#' approximation is visible rather than buried. For reference, a "0.85/0.15"
#' guess would have been wrong by roughly 4 percentage points on energy --
#' which is why this is fetched.
#'
#' @param geo Eurostat geo code (`"DE"`; `"EL"` for Greece -- see
#'   `iso2_to_eamdqd`, the EU statistical convention, not ISO).
#' @param dates A koma `dates` list; weights are averaged over
#'   `dates$estimation`. `NULL` averages over every year available.
#'
#' @return A list with `weights` (named numeric, `nonenergy_prices` and
#'   `energy_prices`, summing to 1), `years` (the years averaged over),
#'   `max_deviation` (largest absolute year-to-mean gap in the energy
#'   weight, in weight units) and `exact` (whether the two components summed
#'   to `CP00` in every year averaged over -- `FALSE` means Eurostat's own
#'   parts did not partition the basket and the weights were renormalised).
#' @export
hicp_weights <- function(geo, dates = NULL) {
  d <- eurostat::get_eurostat("prc_hicp_inw", filters = list(
    geo = geo, coicop = c("CP00", "TOT_X_NRG", "NRG")
  ), time_format = "date", cache_dir = eurostat_cache_dir())
  d <- as.data.frame(d)
  if (nrow(d) == 0) {
    cli::cli_abort("Eurostat {.val prc_hicp_inw} returned no rows for {.val {geo}}.")
  }
  d$year <- as.integer(format(d$time, "%Y"))

  if (!is.null(dates)) {
    span <- dates$estimation$start[1]:dates$estimation$end[1]
    d <- d[d$year %in% span, ]
    if (nrow(d) == 0) {
      cli::cli_abort("Eurostat {.val prc_hicp_inw} has no weights for {.val {geo}} in {.val {range(span)}}.")
    }
  }

  pull <- function(code) {
    x <- d[d$coicop == code, c("year", "values")]
    stats::setNames(x$values[order(x$year)], sort(x$year))
  }
  nrg <- pull("NRG")
  xnrg <- pull("TOT_X_NRG")
  total <- pull("CP00")

  years <- intersect(names(nrg), names(xnrg))
  if (length(years) == 0) {
    cli::cli_abort("No year has both {.val NRG} and {.val TOT_X_NRG} weights for {.val {geo}}.")
  }
  nrg <- nrg[years]
  xnrg <- xnrg[years]

  # Do the two parts actually partition the basket? If Eurostat ever changes
  # the aggregate definitions this silently stops holding, so check rather
  # than assume -- a renormalised weight is still usable, a wrong one is not.
  exact <- TRUE
  if (length(intersect(years, names(total))) == length(years)) {
    exact <- isTRUE(all.equal(unname(nrg + xnrg), unname(total[years]), tolerance = 1e-6))
  }
  if (!exact) {
    cli::cli_warn(c(
      "!" = "{.val NRG} + {.val TOT_X_NRG} does not equal {.val CP00} for {.val {geo}} in every year.",
      "i" = "Weights renormalised to sum to 1; the {.field prices} identity is an approximation."
    ))
  }

  mean_nrg <- mean(nrg)
  mean_xnrg <- mean(xnrg)
  denom <- mean_nrg + mean_xnrg

  list(
    weights = c(
      nonenergy_prices = unname(mean_xnrg / denom),
      energy_prices = unname(mean_nrg / denom)
    ),
    years = as.integer(years),
    max_deviation = unname(max(abs(nrg - mean_nrg)) / denom),
    exact = exact
  )
}

#' Build a koma identity equation from country weights
#'
#' Convenience wrapper around [identity_equation()] that turns a
#' [country_weights()] vector into the `(weight)*component` terms of an
#' aggregation identity, e.g. `ea_gdp == 0.3*de_gdp + 0.2*fr_gdp + ...`.
#'
#' @param concept The concept being aggregated, e.g. `"gdp"`.
#' @param weights Named numeric vector as returned by [country_weights()].
#' @param scope `"ea"` or `"world"`.
#'
#' @return A single equation string.
#' @export
weighted_identity <- function(concept, weights, scope = c("ea", "world")) {
  scope <- match.arg(scope)
  dep <- shared_var(concept, scope = scope)
  terms <- stats::setNames(as.list(as.numeric(weights)), country_var(names(weights), concept))
  identity_equation(dep, terms)
}
