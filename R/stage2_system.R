# Stage 2: the joint multi-country system.
#
# Combines every country's equations with the shared/aggregate equations
# into a single koma::system_of_equations() call, so cross-country
# simultaneity is estimated jointly rather than country-by-country.
#
# Stage 2a is the two-country (DE + FR) pilot of that mechanism. It differs
# from stage 1 in three ways, all of which are the point of the exercise:
#
#   1. Exports are driven by `<iso2>_foreign_demand`, a trade-weighted
#      identity built out of the *other country's endogenous GDP* -- so
#      demand genuinely circulates between countries. Stage 1 used the
#      exogenous `row_gdp`, which excludes every modelled economy.
#   2. `ea_policy_rate` is endogenous, with its own Taylor-type rule over
#      GDP-weighted `ea_gdp`/`ea_prices` identities.
#   3. `<iso2>_long_rate` enters the investment equation, which is what
#      actually gives the policy rate a path to GDP. Making the policy rate
#      endogenous does *not* on its own: without this term `ea_policy_rate`
#      reaches only the two `long_rate` equations, which are terminal, and
#      the "a rate rise cannot move GDP" finding from the stage-1
#      diagnostics report survives unchanged into stage 2.

#' Renormalised trade and GDP weights for a stage-2 country set
#'
#' Slices the pre-built weight matrices down to the modelled subset. Two
#' sets are returned, matching the two kinds of linkage identity:
#'
#' - **`foreign_demand`**: per country, a vector over the *other* modelled
#'   countries' `<iso2>_gdp` plus `row_gdp`. Each modelled partner keeps its
#'   true `W_trade` weight and everything else is lumped into `row_gdp`, so
#'   the weights sum to 1 and foreign demand stays a proper weighted average
#'   of growth rates.
#' - **`ea`**: nominal-GDP shares renormalised over the modelled countries,
#'   used for both the `ea_gdp` and `ea_prices` identities.
#'
#' **Documented approximation.** [build_row_gdp()] excludes all eleven
#' modelled economies by construction, so whenever `countries` is a strict
#' subset of `modelled_countries` the `row_gdp` weight is proxying the
#' absent modelled partners as well as genuine rest-of-world. In the DE/FR
#' pilot that is 0.374 of German trade riding on `row_gdp`. The
#' approximation shrinks monotonically as countries are added -- each new
#' country claims its own `W_trade` weight out of the `row` residual and no
#' other weight is rescaled -- and vanishes at the full eleven.
#'
#' Do **not** reach for `country_weights(basis = "trade")` here: it calls
#' `build_trade_weight_matrix(countries)`, which refetches from the ECB for
#' that subset and whose `"row"` column is therefore a different quantity
#' from the cached eleven-country matrix's.
#'
#' @param countries Character vector of ISO-2 country codes.
#' @param trade_weights The `W_trade` matrix from
#'   [build_trade_weight_matrix()] (rows = reporter, columns = partners plus
#'   `"row"`).
#' @param gdp_weights The `W_gdp` named vector from
#'   [build_gdp_weight_matrix()].
#' @param digits Weights are rounded to this many decimals so the generated
#'   equation strings stay readable, exactly as [expenditure_shares()] does.
#' @param threshold Bilateral weights strictly below this are folded into
#'   `row_gdp` rather than carried as their own term. At the stage-2b default
#'   of `0.01` this drops 18 of 110 cells carrying just **1.6%** of total
#'   bilateral mass. Note this does **not** make estimation faster --
#'   `foreign_demand` is an identity and koma never samples identity columns.
#'   The payoff is identification: every zeroed cell is an extra exclusion
#'   restriction, and the rank condition is what binds in a 103-equation
#'   system. It also gives a sparser, better-conditioned `Gamma` for the
#'   forecast solve.
#' @param ireland_proxy Load partners' foreign demand on `ie_consumption`
#'   rather than `ie_gdp`. Irish measured GDP is distorted by multinational
#'   IP and aircraft-leasing flows (the same effect behind the 2015 break --
#'   see `data_eamdqd.R`) and has quarterly growth sd of 4.00 against 1.29-2.58
#'   for its EA peers, so it injects noise into every partner's export demand.
#'   `ie_consumption` (sd 2.54) is the best-behaved Irish demand series that
#'   is **already endogenous**, which matters: swapping it in changes no
#'   equation count, no `k` and no degrees of freedom, so a fit with it is
#'   directly comparable to one without. `ie_consumption + ie_government`
#'   would be marginally better still (sd 1.76) but needs its own identity
#'   plus `ie_government` back as an exogenous, which would cost a column and
#'   break that comparability. Note `ie_domestic_demand` is **not** an option:
#'   at sd 17.39 it is four times *worse* than GDP, because the distortion
#'   lives in `ie_investment` (sd 38.0), which is inside it.
#'
#' @return A list with elements `foreign_demand` (named list, one weight
#'   vector per country, keyed by full variable name), `ea` (named numeric
#'   vector over the euro-area members of `countries`) and `ea_members`.
#' @export
stage2_linkage_weights <- function(countries, trade_weights, gdp_weights,
                                   digits = 3, threshold = 0,
                                   ireland_proxy = FALSE) {
  countries <- tolower(countries)
  if (length(countries) < 2) {
    cli::cli_abort("A linked system needs at least two countries; got {.val {countries}}.")
  }
  missing_trade <- setdiff(countries, rownames(trade_weights))
  if (length(missing_trade) > 0) {
    cli::cli_abort("{.arg trade_weights} has no row for {.val {missing_trade}}.")
  }
  # W_gdp covers the euro-area members only -- there is no US nominal-GDP
  # share, and there should not be: ea_gdp/ea_prices aggregate the currency
  # union, not the whole system.
  ea_members <- intersect(countries, names(gdp_weights))
  if (length(ea_members) == 0) {
    cli::cli_abort("{.arg gdp_weights} covers none of {.val {countries}}.")
  }

  # Which variable carries a partner's demand signal. Normally its GDP; for
  # Ireland optionally its consumption -- see the `ireland_proxy` docs.
  demand_var <- function(cc) {
    if (isTRUE(ireland_proxy) && identical(cc, "ie")) {
      country_var("ie", "consumption")
    } else {
      country_var(cc, "gdp")
    }
  }

  foreign_demand <- stats::setNames(lapply(countries, function(cc) {
    partners <- setdiff(countries, cc)
    # setNames() is load-bearing: matrix indexing drops names when `partners`
    # has length 1 (the two-country pilot), and the by-name lookup below then
    # silently returns NA.
    partner_w <- stats::setNames(round(trade_weights[cc, partners], digits), partners)
    # Below-threshold partners are folded into row_gdp rather than dropped,
    # so the weights still sum to 1 and foreign demand stays a proper
    # weighted average of growth rates.
    kept <- partners[partner_w >= threshold & partner_w > 0]
    kept_w <- partner_w[kept]
    stats::setNames(
      c(kept_w, round(1 - sum(kept_w), digits)),
      c(vapply(kept, demand_var, character(1)), "row_gdp")
    )
  }), countries)

  ea <- gdp_weights[ea_members]
  ea <- round(ea / sum(ea), digits)

  list(foreign_demand = foreign_demand, ea = ea, ea_members = ea_members)
}

#' Build the stage-2 specification for a set of countries
#'
#' The stage-2 counterpart to [stage1_spec()], generalised from one country
#' to many. Per country `cc`:
#'
#' ```
#' cc_consumption ~ cc_gdp + cc_consumption.L(1)
#' cc_investment  ~ cc_gdp + cc_long_rate + cc_investment.L(1)
#' cc_exports     ~ cc_foreign_demand + cc_exports.L(1)
#' cc_imports     ~ cc_domestic_demand + cc_imports.L(1)
#' cc_prices      ~ eur_usd + oil_price + cc_prices.L(1)
#' cc_long_rate   ~ cc_prices + ea_policy_rate + cc_gdp + cc_long_rate.L(1)
#' ```
#'
#' plus one system-wide monetary equation
#' `ea_policy_rate ~ ea_prices + ea_gdp + ea_policy_rate.L(1)`, and per
#' country the three identities `cc_gdp`, `cc_domestic_demand` (both from
#' [expenditure_shares()], as in stage 1) and `cc_foreign_demand` (from
#' [stage2_linkage_weights()]), plus the two aggregates `ea_gdp` and
#' `ea_prices`.
#'
#' Relative to [stage1_spec()], three equations change and three do not.
#' `consumption`, `imports` and `prices` are **structurally identical** to
#' their stage-1 form, which is what makes them the clean comparison when
#' judging what joint estimation did; `investment`, `exports` and
#' `long_rate` each gained or swapped a regressor and are therefore not
#' directly comparable.
#'
#' This deliberately does **not** modify [stage1_spec()]. The cached
#' stage-1 fits have to stay reproducible or there is nothing to compare
#' against.
#'
#' @param countries Character vector of ISO-2 country codes.
#' @param shares Named list, one [expenditure_shares()] result per country.
#' @param linkage_weights A [stage2_linkage_weights()] result.
#'
#' @return A list with elements `stochastic` (named list, dependent
#'   variable -> `list(terms, lags)`) and `identities` (named list,
#'   dependent variable -> named numeric weights).
#' @export
stage2_spec <- function(countries, shares, linkage_weights,
                        opts = stage2_options()) {
  countries <- tolower(countries)
  missing_shares <- setdiff(countries, names(shares))
  if (length(missing_shares) > 0) {
    cli::cli_abort("{.arg shares} has no entry for {.val {missing_shares}}.")
  }

  labour_countries <- intersect(countries, opts$labour_countries %||% character())
  export_price_countries <- intersect(countries, opts$export_price_countries %||% character())
  if (length(labour_countries) > 0 && is.null(opts$hicp_weights)) {
    cli::cli_abort(c(
      "{.arg opts} sets {.field labour_countries} but carries no {.field hicp_weights}.",
      "i" = "Build them with {.fn hicp_weights} and pass via {.fn stage3a_options}; they must come from the data, not a guess."
    ))
  }

  # paste0() treats a zero-length vector as "", so paste0(character(0), "_x")
  # is "_x", not character(0) -- setNames() would then try to put one name on
  # an empty list. Build the named list only when there is something in it.
  named_blocks <- function(ccs, suffix, build) {
    if (length(ccs) == 0) return(list())
    stats::setNames(lapply(ccs, build), paste0(ccs, suffix))
  }

  blocks <- c(
    stats::setNames(
      lapply(countries, function(cc) {
        country_block(cc, shares[[cc]], linkage_weights$foreign_demand[[cc]], opts)
      }),
      countries
    ),
    named_blocks(labour_countries, "_labour", function(cc) {
      labour_block(cc, opts$hicp_weights[[cc]], opts$foreign_price_weights[[cc]], opts)
    }),
    named_blocks(export_price_countries, "_export_prices", export_price_block),
    list(monetary = monetary_block(), ea_aggregates = ea_aggregate_block(linkage_weights$ea))
  )

  # unname() before c(): concatenating a *named* list of lists prefixes every
  # inner name with its outer one ("de.de_gdp"), which silently breaks every
  # downstream lookup by variable name.
  merged <- lapply(c("stochastic", "identities"), function(part) {
    do.call(c, c(list(list()), unname(lapply(blocks, function(b) b[[part]] %||% list()))))
  })
  list(stochastic = merged[[1]], identities = merged[[2]])
}

#' Options controlling how a stage-2 block is built
#'
#' Defaults reproduce **stage 2a** exactly, so the cached two-country fit and
#' its tests keep reproducing. Stage 2b opts in to each departure explicitly.
#'
#' @param include_government Keep `<iso2>_government` in the domestic-demand
#'   identity (`TRUE`, stage 2a) or drop it (`FALSE`, stage 2b). Government
#'   appears *only* there, so dropping it removes it from the system
#'   entirely -- which is what buys back the 11 exogenous columns stage 2b
#'   needs to fit at all (see `reports/stage2b_full_system.qmd`). When
#'   dropped, the remaining consumption/investment shares are **renormalised
#'   to sum to 1**; leaving them raw (summing to ~0.8) would make the
#'   identity systematically under-predict domestic-demand growth.
#' @param extra_regressors Character vector added to every **real-side**
#'   equation (consumption, investment, exports, imports, prices) -- the
#'   COVID dummies in stage 2b. Deliberately not added to `long_rate` or the
#'   policy rules, which show no mechanical COVID break.
#' @param policy_rule Give the US its own Taylor-type `us_policy_rate`
#'   equation, moving that variable from exogenous to endogenous.
#' @param fx Named character vector, `iso2 -> exchange-rate variable`, for
#'   countries that should not use the default `eur_usd`. Stage 1 and 2a gave
#'   the US `us_exchange_rate`; stage 2b's three-variable exogenous spec puts
#'   it back on `eur_usd`.
#' @param labour_countries Character vector of ISO-2 codes that carry the
#'   stage-3a labour and disaggregated-price block (see [labour_block()]).
#'   For a country named here, [country_block()] changes in exactly two ways:
#'   its `<iso2>_prices` **stochastic** equation is dropped, because
#'   `labour_block()` redefines `prices` as an identity over the energy and
#'   non-energy sub-indices, and its exports and imports equations gain
#'   relative-price terms so the new price variables actually transmit to
#'   trade volumes. Empty by default, so stage 2a and 2b are unchanged.
#' @param export_price_countries Character vector of ISO-2 codes that get the
#'   minimal satellite equation `<iso2>_export_prices ~ <iso2>_prices +
#'   <iso2>_export_prices.L(1)` and nothing else. This is stage 3a phase B:
#'   it is what makes a *partner's* export price endogenous, so
#'   `de_foreign_prices` can be a genuine identity rather than an exogenous
#'   series. Countries in `labour_countries` already get a richer
#'   export-price equation and must not appear here.
#' @param hicp_weights Named list, `iso2 -> named numeric vector`, one entry
#'   per labour country, each the `weights` element of [hicp_weights()].
#'   Required whenever `labour_countries` is non-empty -- there is
#'   deliberately no default, because a guessed HICP split is exactly the
#'   kind of hidden decision the identity must not contain.
#' @param foreign_price_weights Named list, `iso2 -> named numeric vector`
#'   from [foreign_price_weights()], for labour countries whose
#'   `<iso2>_foreign_prices` should be an **identity** (phase B). Omit an
#'   entry to leave that country's foreign prices exogenous (phase A).
#'
#' @return A list of options for [country_block()].
#' @export
stage2_options <- function(include_government = TRUE,
                           extra_regressors = character(),
                           policy_rule = FALSE,
                           fx = character(),
                           labour_countries = character(),
                           export_price_countries = character(),
                           hicp_weights = NULL,
                           foreign_price_weights = NULL) {
  overlap <- intersect(labour_countries, export_price_countries)
  if (length(overlap) > 0) {
    cli::cli_abort(c(
      "{.val {overlap}} {?is/are} in both {.arg labour_countries} and {.arg export_price_countries}.",
      "i" = "A labour-block country already defines {.field export_prices}; two blocks cannot define the same variable."
    ))
  }
  missing_weights <- setdiff(labour_countries, names(hicp_weights %||% list()))
  if (length(missing_weights) > 0) {
    cli::cli_abort(c(
      "{.arg hicp_weights} has no entry for {.val {missing_weights}}.",
      "i" = "Every country in {.arg labour_countries} needs its own weights from {.fn hicp_weights}."
    ))
  }
  stray <- setdiff(names(foreign_price_weights %||% list()), labour_countries)
  if (length(stray) > 0) {
    cli::cli_abort(c(
      "{.arg foreign_price_weights} names {.val {stray}}, which {?is/are} not {?a labour country/labour countries}.",
      "i" = "Only a labour country has a {.field foreign_prices} variable to turn into an identity."
    ))
  }

  list(
    include_government = include_government,
    extra_regressors = extra_regressors,
    policy_rule = policy_rule,
    fx = fx,
    labour_countries = labour_countries,
    export_price_countries = export_price_countries,
    hicp_weights = hicp_weights,
    foreign_price_weights = foreign_price_weights
  )
}

#' One country's block of equations
#'
#' The six behavioural equations plus the three identities that make up a
#' single economy, in the shape [build_system()] consumes:
#'
#' ```
#' cc_consumption ~ cc_gdp + cc_consumption.L(1)
#' cc_investment  ~ cc_gdp + cc_long_rate + cc_investment.L(1)
#' cc_exports     ~ cc_foreign_demand + cc_exports.L(1)
#' cc_imports     ~ cc_domestic_demand + cc_imports.L(1)
#' cc_prices      ~ <fx> + oil_price + cc_prices.L(1)
#' cc_long_rate   ~ cc_prices + <policy rate> + cc_gdp + cc_long_rate.L(1)
#' ```
#'
#' plus `cc_gdp`, `cc_domestic_demand` and `cc_foreign_demand` identities.
#' The US additionally gets `us_policy_rate ~ us_prices + us_gdp +
#' us_policy_rate.L(1)` when `opts$policy_rule` is set, and loads its own
#' `long_rate` on `us_policy_rate` rather than the shared `ea_policy_rate`.
#'
#' @param iso2 Two-letter lowercase ISO country code.
#' @param shares One country's [expenditure_shares()] result.
#' @param foreign_weights Named weight vector for this country's
#'   `foreign_demand` identity (see [stage2_linkage_weights()]).
#' @param opts A [stage2_options()] list.
#'
#' @return A list with `stochastic` and `identities`.
#' @export
country_block <- function(iso2, shares, foreign_weights, opts = stage2_options()) {
  iso2 <- tolower(iso2)
  v <- function(concept) country_var(iso2, concept)
  own_lag <- function(name) stats::setNames(list("1"), name)
  extra <- opts$extra_regressors %||% character()

  is_us <- identical(iso2, "us")
  # `[[` on a character vector aborts on a missing name rather than returning
  # NULL, so %||% cannot rescue it -- look the name up explicitly.
  fx_overrides <- opts$fx %||% character()
  fx <- if (iso2 %in% names(fx_overrides)) unname(fx_overrides[[iso2]]) else "eur_usd"
  policy_rate <- if (is_us && isTRUE(opts$policy_rule)) "us_policy_rate" else "ea_policy_rate"
  has_labour <- iso2 %in% (opts$labour_countries %||% character())

  # With the labour block on, the price variables must reach trade volumes or
  # they are estimated and then transmit nothing -- the terminal-variable
  # pathology CLAUDE.md records for long_rate. Exports fall in own price and
  # rise in competitors'; imports fall in their own price.
  export_price_terms <- if (has_labour) c(v("export_prices"), v("foreign_prices")) else character()
  import_price_terms <- if (has_labour) v("import_prices") else character()
  real_income_terms <- if (has_labour) v("real_income") else character()

  stochastic <- list()
  stochastic[[v("consumption")]] <- list(
    terms = c(v("gdp"), real_income_terms, extra, v("consumption")), lags = own_lag(v("consumption"))
  )
  stochastic[[v("investment")]] <- list(
    terms = c(v("gdp"), v("long_rate"), extra, v("investment")), lags = own_lag(v("investment"))
  )
  stochastic[[v("exports")]] <- list(
    terms = c(v("foreign_demand"), export_price_terms, extra, v("exports")),
    lags = own_lag(v("exports"))
  )
  stochastic[[v("imports")]] <- list(
    terms = c(v("domestic_demand"), import_price_terms, extra, v("imports")),
    lags = own_lag(v("imports"))
  )
  # A labour-block country defines `prices` as an identity over its energy and
  # non-energy sub-indices (see labour_block()), so it must NOT also have a
  # stochastic price equation -- build_system() aborts on a duplicated LHS.
  if (!has_labour) {
    stochastic[[v("prices")]] <- list(
      terms = c(fx, "oil_price", extra, v("prices")), lags = own_lag(v("prices"))
    )
  }
  stochastic[[v("long_rate")]] <- list(
    terms = c(v("prices"), policy_rate, v("gdp"), v("long_rate")),
    lags = own_lag(v("long_rate"))
  )
  if (is_us && isTRUE(opts$policy_rule)) {
    stochastic[["us_policy_rate"]] <- list(
      terms = c("us_prices", "us_gdp", "us_policy_rate"),
      lags = own_lag("us_policy_rate")
    )
  }

  dd <- shares$domestic_demand
  if (!isTRUE(opts$include_government)) {
    dd <- dd[setdiff(names(dd), v("government"))]
    # Renormalise: the raw C and I shares sum to ~0.8, and leaving them so
    # would make the identity under-predict domestic-demand growth by the
    # whole government contribution.
    dd <- round(dd / sum(dd), 3)
  }

  identities <- list()
  identities[[v("gdp")]] <- shares$gdp
  identities[[v("domestic_demand")]] <- dd
  identities[[v("foreign_demand")]] <- foreign_weights

  list(stochastic = stochastic, identities = identities)
}

#' Trade weights for a country's foreign-price index
#'
#' Reuses the same bilateral trade weights as the demand channel, but over
#' partners' **export prices** instead of their GDP -- the price side of the
#' same trade relationship.
#'
#' **The `row_gdp` residual is dropped and the rest renormalised, and that is
#' a substitution worth stating.** `stage2_linkage_weights()` puts whatever
#' weight is not on a modelled partner onto `row_gdp`, so foreign *demand*
#' accounts for the whole world. There is no rest-of-world export-price
#' series in this project and no obvious candidate for one, so foreign
#' *prices* cannot do the same. Renormalising over modelled partners assumes
#' the rest of the world's export prices move like the modelled partners'
#' average. For Germany the modelled partners carry roughly half the trade
#' weight, so this is a real assumption, not a rounding detail: it will
#' understate the effect of a purely non-modelled-world price shock, and it
#' is reported alongside the results rather than buried here.
#'
#' @param linkage_weights A [stage2_linkage_weights()] result.
#' @param iso2 Two-letter lowercase ISO country code.
#' @param digits Rounding, matching [stage2_linkage_weights()].
#'
#' @return A named numeric vector, `<partner>_export_prices -> weight`,
#'   summing to 1. Carries a `row_weight_dropped` attribute recording how
#'   much weight the renormalisation reallocated.
#' @export
foreign_price_weights <- function(linkage_weights, iso2, digits = 3) {
  iso2 <- tolower(iso2)
  w <- linkage_weights$foreign_demand[[iso2]]
  if (is.null(w)) {
    cli::cli_abort("{.arg linkage_weights} has no {.field foreign_demand} entry for {.val {iso2}}.")
  }
  partners <- setdiff(names(w), "row_gdp")
  if (length(partners) == 0) {
    cli::cli_abort(c(
      "{.val {iso2}} has no modelled trade partners, only the {.val row_gdp} residual.",
      "i" = "A foreign-price index needs at least one partner with an export-price series."
    ))
  }
  dropped <- unname(w[["row_gdp"]] %||% 0)
  kept <- w[partners]
  renormalised <- round(kept / sum(kept), digits)
  # Rounding can leave the sum a hair off 1; put the crumb on the largest
  # partner so the identity is still a proper weighted average.
  slack <- 1 - sum(renormalised)
  if (abs(slack) > 0) renormalised[which.max(renormalised)] <- renormalised[which.max(renormalised)] + slack

  out <- stats::setNames(
    as.numeric(renormalised),
    country_var(sub("_(gdp|consumption)$", "", partners), "export_prices")
  )
  attr(out, "row_weight_dropped") <- dropped
  out
}

#' A partner's minimal export-price equation (stage 3a, phase B)
#'
#' One equation and nothing else:
#'
#' ```
#' cc_export_prices ~ cc_prices + cc_export_prices.L(1)
#' ```
#'
#' **Why it exists.** `de_foreign_prices` is a trade-weighted index over
#' *partners'* export prices. Without this, no partner has an
#' `export_prices` equation, so `de_foreign_prices` cannot be an endogenous
#' identity and has to be a fixed exogenous series -- which means Germany's
#' price competitiveness responds to nothing the model does, and the second
#' cross-country channel never closes. This is the cheapest equation that
#' makes a partner's export price endogenous: one lag column each.
#'
#' **Why it is not the full block.** Giving all eleven countries the German
#' block costs ~66 predetermined columns against a T of 98 -- arithmetically
#' impossible (see `reports/stage3a_labour_prices.qmd`). Driving a partner's
#' export price off its own consumer prices, rather than off a ULC it has no
#' labour block to produce, buys the channel for 10 columns instead of 66.
#' The cost is that a partner's export price carries no independent cost
#' information -- it is a markup on domestic prices. State that when reading
#' any spillover through this channel.
#'
#' @param iso2 Two-letter lowercase ISO country code.
#'
#' @return A list with `stochastic` and an empty `identities`.
#' @export
export_price_block <- function(iso2) {
  iso2 <- tolower(iso2)
  v <- function(concept) country_var(iso2, concept)
  list(
    stochastic = stats::setNames(
      list(list(
        terms = c(v("prices"), v("export_prices")),
        lags = stats::setNames(list("1"), v("export_prices"))
      )),
      v("export_prices")
    ),
    identities = list()
  )
}

#' One country's labour-market and disaggregated-price block (stage 3a)
#'
#' Seven behavioural equations and five identities, added on top of the
#' [country_block()] the country already has. Together they give the system a
#' wage-price loop and a price-competitiveness channel:
#'
#' ```
#' cc_employment       ~ cc_gdp + cc_employment.L(1)
#' cc_unemployment     ~ cc_gdp + cc_unemployment.L(1)
#' cc_wages            ~ cc_unemployment + cc_prices + cc_wages.L(1)
#' cc_nonenergy_prices ~ cc_ulc + cc_import_prices + cc_unemployment + cc_nonenergy_prices.L(1)
#' cc_energy_prices    ~ oil_price + <fx> + cc_energy_prices.L(1)
#' cc_import_prices    ~ <fx> + oil_price + cc_foreign_prices + cc_import_prices.L(1)
#' cc_export_prices    ~ cc_ulc + cc_foreign_prices + cc_export_prices.L(1)
#'
#' cc_productivity == cc_gdp - cc_employment
#' cc_ulc          == cc_wages - cc_productivity
#' cc_prices       == w_xnrg*cc_nonenergy_prices + w_nrg*cc_energy_prices
#' cc_real_income  == cc_wages + cc_employment - cc_prices
#' cc_foreign_prices == <trade-weighted partners' export_prices>   [phase B only]
#' ```
#'
#' **`cc_wages` is a wage rate, not a wage bill.** The three ±1 identities
#' are only correct for a rate -- see [derived_wage_rate()], which explains
#' why and how the panel series is built. In rate space `ulc` telescopes to
#' nominal wage bill over real output, the textbook definition, and
#' `real_income` recovers the deflated wage bill.
#'
#' **`cc_prices` moves from a stochastic equation to an identity.** The
#' matching change is in [country_block()], which drops its price equation
#' for a labour country. The identity's LHS series must be **constructed** in
#' rate space by [build_stage2_panel()], not taken from observed headline
#' HICP: koma has no identity-consistency check, and observed HICP satisfies
#' the fixed-weight identity only approximately (annual re-weighting), so
#' feeding it the observed series would silently enforce a false identity.
#'
#' **Exogenous vs endogenous `cc_foreign_prices`.** In phase A no partner has
#' an export-price equation, so `foreign_price_weights = NULL` leaves
#' `cc_foreign_prices` an exogenous series that [build_stage2_panel()]
#' constructs from partner data. In phase B, partners get
#' [export_price_block()] and passing weights here promotes it to an
#' identity, closing the loop.
#'
#' @param iso2 Two-letter lowercase ISO country code.
#' @param hicp_weights Named numeric vector with `nonenergy_prices` and
#'   `energy_prices`, summing to 1 -- the `weights` element of
#'   [hicp_weights()].
#' @param foreign_price_weights Named numeric vector, `<partner>_export_prices
#'   -> weight`, or `NULL` (phase A) to leave `cc_foreign_prices` exogenous.
#' @param opts A [stage2_options()] list; only `fx` is consulted.
#'
#' @return A list with `stochastic` and `identities`.
#' @export
labour_block <- function(iso2, hicp_weights, foreign_price_weights = NULL,
                         opts = stage2_options()) {
  iso2 <- tolower(iso2)
  v <- function(concept) country_var(iso2, concept)
  own_lag <- function(name) stats::setNames(list("1"), name)

  expected <- c("nonenergy_prices", "energy_prices")
  if (!setequal(names(hicp_weights), expected)) {
    cli::cli_abort(c(
      "{.arg hicp_weights} must be named {.val {expected}}.",
      "x" = "Got {.val {names(hicp_weights)}}."
    ))
  }
  if (!isTRUE(all.equal(sum(hicp_weights), 1, tolerance = 1e-6))) {
    cli::cli_abort(c(
      "{.arg hicp_weights} must sum to 1; got {.val {sum(hicp_weights)}}.",
      "i" = "The energy/non-energy split partitions the basket -- a sum below 1 means a component is missing."
    ))
  }

  fx_overrides <- opts$fx %||% character()
  fx <- if (iso2 %in% names(fx_overrides)) unname(fx_overrides[[iso2]]) else "eur_usd"

  stochastic <- list()
  stochastic[[v("employment")]] <- list(
    terms = c(v("gdp"), v("employment")), lags = own_lag(v("employment"))
  )
  stochastic[[v("unemployment")]] <- list(
    terms = c(v("gdp"), v("unemployment")), lags = own_lag(v("unemployment"))
  )
  stochastic[[v("wages")]] <- list(
    terms = c(v("unemployment"), v("prices"), v("wages")), lags = own_lag(v("wages"))
  )
  stochastic[[v("nonenergy_prices")]] <- list(
    terms = c(v("ulc"), v("import_prices"), v("unemployment"), v("nonenergy_prices")),
    lags = own_lag(v("nonenergy_prices"))
  )
  # No contemporaneous endogenous regressor, so koma gives this equation no
  # Metropolis step: count_accepted is NA and check_acceptance_rates() must
  # not flag it. That is expected, not a problem.
  stochastic[[v("energy_prices")]] <- list(
    terms = c("oil_price", fx, v("energy_prices")), lags = own_lag(v("energy_prices"))
  )
  stochastic[[v("import_prices")]] <- list(
    terms = c(fx, "oil_price", v("foreign_prices"), v("import_prices")),
    lags = own_lag(v("import_prices"))
  )
  stochastic[[v("export_prices")]] <- list(
    terms = c(v("ulc"), v("foreign_prices"), v("export_prices")),
    lags = own_lag(v("export_prices"))
  )

  identities <- list()
  # Order matters for readability but not for koma: build_system_equations()
  # emits every stochastic equation before every identity regardless.
  identities[[v("productivity")]] <- stats::setNames(c(1, -1), c(v("gdp"), v("employment")))
  identities[[v("ulc")]] <- stats::setNames(c(1, -1), c(v("wages"), v("productivity")))
  identities[[v("prices")]] <- stats::setNames(
    as.numeric(hicp_weights[expected]), v(expected)
  )
  identities[[v("real_income")]] <- stats::setNames(
    c(1, 1, -1), c(v("wages"), v("employment"), v("prices"))
  )
  if (!is.null(foreign_price_weights)) {
    identities[[v("foreign_prices")]] <- foreign_price_weights
  }

  list(stochastic = stochastic, identities = identities)
}

#' The shared euro-area monetary policy rule
#'
#' A single Taylor-type equation over the GDP-weighted euro-area aggregates.
#' There is exactly one of these in the system no matter how many countries
#' it contains, which is what makes `ea_policy_rate` a genuinely common
#' policy rate rather than eleven separate ones.
#'
#' @return A list with `stochastic` and `identities` (the latter empty).
#' @export
monetary_block <- function() {
  list(
    stochastic = list(
      ea_policy_rate = list(
        terms = c("ea_prices", "ea_gdp", "ea_policy_rate"),
        lags = list(ea_policy_rate = "1")
      )
    ),
    identities = list()
  )
}

#' The euro-area aggregation identities
#'
#' `ea_gdp` and `ea_prices` as GDP-weighted sums over the member countries.
#' This is the template a stage-3 `world_`-scope block follows: a block that
#' contributes identities only, parameterised by a weight vector.
#'
#' @param weights Named numeric vector over ISO-2 codes, summing to 1 (see
#'   [stage2_linkage_weights()]'s `ea` element).
#'
#' @return A list with `stochastic` (empty) and `identities`.
#' @export
ea_aggregate_block <- function(weights) {
  list(
    stochastic = list(),
    identities = list(
      ea_gdp = stats::setNames(weights, country_var(names(weights), "gdp")),
      ea_prices = stats::setNames(weights, country_var(names(weights), "prices"))
    )
  )
}

#' Assemble the joint multi-country equation string
#'
#' Renders a [stage2_spec()] into koma equation strings via
#' [stochastic_equation()] and [identity_equation()].
#'
#' **Every stochastic equation is emitted before every identity, and that
#' ordering is load-bearing.** koma assumes it positionally in two places:
#' `model_identification()` loops `for (j in seq(1, n_endogenous -
#' n_identities))` over *columns*, and `estimate_sem()` indexes
#' `y_matrix[, jx]` by the same positional index. An identity declared
#' anywhere but last therefore makes koma check and estimate the wrong
#' columns, and mislabel the results, **with no error**. Verified against
#' koma 0.3.1 source; it is undocumented, but every example koma ships
#' obeys it.
#'
#' @param spec A [stage2_spec()] result.
#' @param tau Optional named numeric vector, dependent variable -> sampler
#'   `tau` override, appended as `[tau = value]`. Same contract as
#'   [stage1_country_equations()]'s argument of the same name.
#'
#' @return A character vector of equation strings, stochastic first.
#' @export
build_system_equations <- function(spec, tau = NULL) {
  stochastic <- spec$stochastic %||% list()
  identities <- spec$identities %||% list()
  if (length(stochastic) == 0) {
    cli::cli_abort("{.arg spec} must contain at least one stochastic equation; koma requires one.")
  }

  unknown_tau <- setdiff(names(tau), names(stochastic))
  if (length(unknown_tau) > 0) {
    cli::cli_abort("{.arg tau} names {.val {unknown_tau}}, which {?is/are} not a stochastic equation in {.arg spec}.")
  }

  stochastic_strings <- vapply(names(stochastic), function(dep) {
    eq <- stochastic[[dep]]
    out <- stochastic_equation(dep, terms = eq$terms, lags = eq$lags)
    if (!is.null(tau) && dep %in% names(tau)) {
      out <- paste0(out, " [tau = ", format(tau[[dep]], trim = TRUE), "]")
    }
    out
  }, character(1))

  identity_strings <- vapply(names(identities), function(dep) {
    identity_equation(dep, as.list(identities[[dep]]))
  }, character(1))

  unname(c(stochastic_strings, identity_strings))
}

#' Determine the joint system's exogenous variables
#'
#' Exogenous variables are **derived**, not declared: any variable on a
#' right-hand side that is not itself the dependent variable of a stochastic
#' equation or an identity is exogenous. That keeps the set in sync with the
#' spec automatically, so moving a variable from exogenous to endogenous --
#' as stage 2 does with `ea_policy_rate` -- needs no second edit.
#'
#' Getting this exactly right matters more than it looks: koma's
#' `validate_completeness()` is an **exact-set** check. An undeclared
#' variable aborts with "Undeclared exogenous variables detected", and a
#' declared-but-unused one aborts with "Redundant exogenous variables
#' detected". A superset is just as fatal as a subset.
#'
#' For the DE/FR pilot this returns five names, not the three truly global
#' ones: `<iso2>_government` has no equation of its own (fiscal policy is a
#' given, as in stage 1) but does appear in the domestic-demand identities,
#' so it is exogenous and must be declared.
#'
#' @param spec A [stage2_spec()] result.
#'
#' @return A character vector of exogenous variable names.
#' @export
stage2_exogenous_variables <- function(spec) {
  stochastic <- spec$stochastic %||% list()
  identities <- spec$identities %||% list()

  endogenous <- c(names(stochastic), names(identities))
  rhs_variables <- unique(c(
    unlist(lapply(stochastic, function(eq) eq$terms), use.names = FALSE),
    unlist(lapply(identities, names), use.names = FALSE)
  ))
  setdiff(rhs_variables, endogenous)
}

#' Build the joint multi-country `koma_seq`
#'
#' @inheritParams build_system_equations
#'
#' @return A `koma::koma_seq` object for the full system.
#' @export
build_stage2_system <- function(spec, tau = NULL) {
  # One code path with [build_system()]: pass the already-merged spec as a
  # single block, so ordering, duplicate detection and exogenous derivation
  # are the same logic stage 3 will extend.
  build_system(
    countries = character(),
    blocks = list(stage2 = spec),
    weights = list(),
    tau = tau
  )
}

#' Add the stage-2 derived series to a panel
#'
#' `koma::estimate()` requires a series for **every** endogenous variable,
#' identity-defined ones included -- it aborts with "The following series
#' are missing in `ts_data`" otherwise, and only at estimation time, after
#' the expensive setup. Stage 2 introduces four endogenous variables with no
#' observed counterpart in the panel (`<iso2>_foreign_demand` per country,
#' plus `ea_gdp` and `ea_prices`), so they have to be constructed.
#'
#' All four are built with [chain_weighted_index()], **not**
#' [apply_weights()], so that each one satisfies its own identity in the
#' growth-rate space koma actually estimates in. See that function for why
#' the level-space alternative is wrong here -- for the pilot's
#' `de_foreign_demand` the two constructions differ by up to 13.7
#' percentage points of quarterly growth.
#'
#' @param panel Named list of `koma_ts`, as built by [build_global_panel()].
#' @param linkage_weights A [stage2_linkage_weights()] result.
#' @param dummies Character vector of `covid_<year>q<quarter>` names to add
#'   as 0/1 indicator series (see [covid_dummy()]). Empty by default, which
#'   is stage 2a's behaviour.
#'
#' @return `panel` with the derived series appended.
#' @export
build_stage2_panel <- function(panel, linkage_weights, dummies = character(),
                               labour_countries = character(),
                               hicp_weights = NULL) {
  out <- panel

  for (nm in dummies) {
    out[[nm]] <- covid_dummy(nm, panel[[1]])
  }

  for (cc in names(linkage_weights$foreign_demand)) {
    out[[country_var(cc, "foreign_demand")]] <-
      chain_weighted_index(panel, linkage_weights$foreign_demand[[cc]])
  }

  # Stage 3a. Order is load-bearing: `ulc` is defined against `productivity`,
  # and `prices` feeds `real_income`, so each has to exist in `out` before the
  # next reads it. Every one of these is built in RATE space by
  # chain_weighted_index() -- see its own docs and CLAUDE.md for why a level-
  # space construction would silently violate the identity koma then enforces.
  for (cc in tolower(labour_countries)) {
    v <- function(concept) country_var(cc, concept)
    w <- hicp_weights[[cc]]
    if (is.null(w)) {
      cli::cli_abort("{.arg hicp_weights} has no entry for labour country {.val {cc}}.")
    }
    out[[v("productivity")]] <- chain_weighted_index(
      out, stats::setNames(c(1, -1), c(v("gdp"), v("employment")))
    )
    out[[v("ulc")]] <- chain_weighted_index(
      out, stats::setNames(c(1, -1), c(v("wages"), v("productivity")))
    )
    # NOT the observed headline HICP already in the panel: that satisfies the
    # fixed-weight identity only approximately, and koma has no identity-
    # consistency check, so handing it the observed series would silently
    # enforce a false identity. The observed series is kept for comparison by
    # stage3a_identity_report() rather than used here.
    out[[v("prices")]] <- chain_weighted_index(
      out, stats::setNames(
        as.numeric(w[c("nonenergy_prices", "energy_prices")]),
        v(c("nonenergy_prices", "energy_prices"))
      )
    )
    out[[v("real_income")]] <- chain_weighted_index(
      out, stats::setNames(c(1, 1, -1), c(v("wages"), v("employment"), v("prices")))
    )
    # Built unconditionally. In phase A it is the exogenous series the model
    # reads; in phase B the identity reproduces it and this is the series koma
    # checks that identity against. Same numbers either way.
    out[[v("foreign_prices")]] <- chain_weighted_index(
      out, foreign_price_weights(linkage_weights, cc)
    )
  }

  ea <- linkage_weights$ea
  out[["ea_gdp"]] <- chain_weighted_index(
    panel, stats::setNames(ea, country_var(names(ea), "gdp"))
  )
  # Reads `out`, not `panel`: a labour country's `prices` is redefined above as
  # the constructed sub-index aggregate, and the EA aggregate must be built
  # from the same series the model uses, not from the observed HICP it replaced.
  out[["ea_prices"]] <- chain_weighted_index(
    out, stats::setNames(ea, country_var(names(ea), "prices"))
  )

  out
}

#' Build a 0/1 quarter dummy shaped like the rest of the panel
#'
#' Named `covid_<year>q<quarter>`, e.g. `"covid_2020q2"`, and returned over
#' the same span and frequency as `template` so it lines up with everything
#' else and extends as zeros through the forecast window.
#'
#' Tagged `series_type = "rate", method = "none"` so koma's `rate()` passes it
#' through untouched -- a dummy must reach the sampler as the 0/1 indicator it
#' is, not be differenced into one.
#'
#' Stage 2b uses `2020Q1`, `2020Q2`, `2020Q3` and `2021Q2`, chosen empirically
#' as the only quarters whose cross-country mean absolute GDP growth exceeds
#' three times the non-COVID baseline of 0.81pp (they run 3.16, 12.19, 10.68
#' and 2.48). They enter the five real-side equations only; `long_rate` and
#' the policy rules show no mechanical COVID break.
#'
#' @param name Dummy name, `covid_<year>q<quarter>`.
#' @param template A `koma_ts` whose span and frequency to copy.
#'
#' @return A `koma_ts` of zeros with a single 1.
#' @keywords internal
covid_dummy <- function(name, template) {
  parsed <- regmatches(name, regexec("^covid_([0-9]{4})q([1-4])$", name))[[1]]
  if (length(parsed) != 3) {
    cli::cli_abort("{.val {name}} is not of the form {.val covid_2020q2}.")
  }
  year <- as.integer(parsed[2])
  quarter <- as.integer(parsed[3])

  frequency <- stats::frequency(template)
  times <- stats::time(template)
  target <- year + (quarter - 1) / frequency
  hit <- which(abs(as.numeric(times) - target) < 1e-6)
  if (length(hit) == 0) {
    cli::cli_abort("{.val {name}} falls outside the panel's span.")
  }

  values <- rep(0, length(times))
  values[hit] <- 1
  koma::as_ets(
    stats::ts(values, start = stats::start(template), frequency = frequency),
    series_type = "rate", method = "none"
  )
}

#' Pre-flight checks on a stage-2 system before committing to estimation
#'
#' Answers, up front and with a readable message, the questions koma either
#' answers too late or does not answer at all:
#'
#' - **every RHS variable resolves** to an endogenous or declared-exogenous
#'   name (koma checks this in `validate_completeness()`, but at
#'   `system_of_equations()` time, so reaching this function means it
#'   already passed -- it is re-reported here for the record);
#' - **no variable is defined twice**;
#' - **every variable has a series in `panel`** -- koma checks this only
#'   inside `estimate()`, after building the design matrices;
#' - **the system is identified**. `koma::model_identification()` fills the
#'   free coefficients with `rnorm` draws, so a single pass proves very
#'   little; this repeats it across `seeds` and reports how many passed.
#'
#' @param sys_eq A `koma::koma_seq`, e.g. from [build_stage2_system()].
#' @param panel Named list of `koma_ts`.
#' @param seeds Integer vector of RNG seeds to repeat the identification
#'   check under.
#'
#' @return A `data.frame` with columns `check`, `ok`, `detail`, invisibly
#'   printed by the caller. Never aborts -- the point is to report every
#'   problem at once rather than stop at the first.
#' @export
stage2_preflight <- function(sys_eq, panel, seeds = 1:10, dates = NULL) {
  if (!koma::is_system_of_equations(sys_eq)) {
    cli::cli_abort("{.arg sys_eq} must be a {.cls koma_seq} from {.fn koma::system_of_equations}.")
  }

  endogenous <- sys_eq$endogenous_variables
  declared <- c(endogenous, sys_eq$exogenous_variables, sys_eq$weight_variables, "constant")

  # Every base (lag-stripped) identity component must resolve. Note it is
  # `components` that is keyed by variable name -- `weights` is keyed by
  # koma's internal theta symbols ("theta16_17"), which would never match.
  rhs_names <- unique(unlist(lapply(sys_eq$identities, function(id) names(id$components)), use.names = FALSE))
  unresolved <- setdiff(sub("\\.L\\(.*", "", rhs_names %||% character(0)), declared)

  duplicated_endogenous <- endogenous[duplicated(endogenous)]

  needed <- c(endogenous, sys_eq$exogenous_variables, sys_eq$weight_variables)
  missing_series <- setdiff(needed, names(panel))

  gaps <- if (length(missing_series) == 0) internal_gaps(panel[needed]) else list()

  identified <- vapply(seeds, function(s) {
    set.seed(s)
    tryCatch(
      {
        koma::model_identification(
          sys_eq$character_gamma_matrix, sys_eq$character_beta_matrix, sys_eq$identities
        )
        TRUE
      },
      error = function(e) FALSE
    )
  }, logical(1))

  # koma assumes identities occupy the last columns; see build_system_equations().
  n_stochastic <- length(sys_eq$stochastic_equations)
  identities_last <- setequal(
    utils::tail(endogenous, length(sys_eq$identities)),
    names(sys_eq$identities)
  )

  # koma projects EVERY equation on the FULL k-column x_matrix on EVERY draw
  # (construct_pi_hat_0 / construct_theta_hat_j both do
  # Matrix::solve(t(x) %*% x)), and draws Omega from riwish(T - k, .). So
  # k >= T is an unconditional double failure -- "system is computationally
  # singular" plus "v must be >= dimension of S in rwish()" -- and it happens
  # only after koma has built the design matrices. Catch it here instead.
  k <- length(sys_eq$total_exogenous_variables)
  n_obs <- if (is.null(dates)) NA_integer_ else estimation_length(panel, dates)
  df_residual <- if (is.na(n_obs)) NA_integer_ else n_obs - k
  df_ok <- if (is.na(df_residual)) NA else df_residual > 0

  data.frame(
    check = c(
      "identity RHS variables all resolve",
      "no variable defined twice",
      "every variable has a panel series",
      "no internal NAs in required series",
      "identities declared last",
      "system identified",
      "k < T (x'x invertible, Wishart df > 0)"
    ),
    ok = c(
      length(unresolved) == 0,
      length(duplicated_endogenous) == 0,
      length(missing_series) == 0,
      length(gaps) == 0,
      identities_last,
      all(identified),
      df_ok
    ),
    detail = c(
      if (length(unresolved) == 0) "-" else paste(unresolved, collapse = ", "),
      if (length(duplicated_endogenous) == 0) "-" else paste(duplicated_endogenous, collapse = ", "),
      if (length(missing_series) == 0) {
        paste0(length(needed), " variables present")
      } else {
        paste("missing:", paste(missing_series, collapse = ", "))
      },
      if (length(gaps) == 0) "-" else paste(names(gaps), collapse = ", "),
      paste0(n_stochastic, " stochastic then ", length(sys_eq$identities), " identities"),
      paste0(sum(identified), "/", length(seeds), " seeds pass order + rank"),
      if (is.na(df_residual)) {
        paste0("k = ", k, " (pass `dates` to check against T)")
      } else {
        paste0("k = ", k, ", T = ", n_obs, ", df = ", df_residual)
      }
    ),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

#' Number of observations koma will actually estimate on
#'
#' The estimation window less the periods koma drops to build the first lag.
#' Used by [stage2_preflight()] to check `k < T` before committing to a run.
#'
#' @param panel Named list of `koma_ts`.
#' @param dates koma `dates` list.
#'
#' @return Integer count of usable observations.
#' @keywords internal
estimation_length <- function(panel, dates) {
  frequency <- stats::frequency(panel[[1]])
  to_index <- function(yq) yq[1] * frequency + (yq[2] - 1)
  start <- to_index(dates$estimation$start)
  end <- to_index(dates$estimation$end)
  # Two periods go, not one: `rate()` costs the first observation of every
  # diff_log series, and the L(1) lag costs one more. Verified against the
  # stage-2a fit -- 2000Q1-2019Q4 is 80 quarters and koma reports
  # "Estimation start moved to 2000 Q3" with T = 78.
  as.integer(end - start + 1 - 2)
}

#' Fit the joint multi-country system
#'
#' The stage-2 counterpart to [estimate_stage1_system()], and it follows the
#' same recipe: subset the panel to what the system needs, refuse internal
#' `NA`s with a message that names them, harmonise attributes for
#' `as_mets()`, and truncate the **endogenous** series to the estimation end
#' so koma conditionally fills forward to the forecast start (which is how
#' 2020-2022 is neutralised -- see CLAUDE.md).
#'
#' **No warm start.** [fit_stage1_all()]'s fits cannot be reused here even
#' though several equations are unchanged. `koma::estimate(estimates = )`
#' decides what to re-estimate by comparing only the symbolic **`B`**
#' matrices (`identify_reestimation_indices()`); the `Gamma` block is never
#' compared. Every cross-country term stage 2 introduces is a
#' *contemporaneous endogenous* regressor, i.e. `Gamma`-only, so a warm
#' start would silently keep stale draws of the wrong dimension for exactly
#' the equations the pilot exists to change. Verified against koma 0.3.1
#' source.
#'
#' @param sys_eq A `koma_seq` as built by [build_stage2_system()].
#' @param panel Named list of `koma_ts` including the stage-2 derived
#'   series (see [build_stage2_panel()]).
#' @param dates koma `dates` list, e.g. from [stage1_dates()].
#' @param options Passed through to `koma::estimate(options = )`.
#' @param workers Number of `future` workers to spread the per-equation
#'   sampling across. `NULL` (the default) leaves koma sequential.
#'
#' @return A `koma::koma_estimate` object, carrying `runtime_s` and `workers`
#'   attributes.
#' @export
fit_stage2 <- function(sys_eq, panel, dates, options = list(), workers = NULL) {
  needed <- c(sys_eq$endogenous_variables, sys_eq$exogenous_variables, sys_eq$weight_variables)
  missing <- setdiff(needed, names(panel))
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "{.arg panel} is missing {.val {missing}}, required by the stage-2 system.",
      "i" = "Identity-defined variables need a series too; see {.fn build_stage2_panel}."
    ))
  }

  gaps <- internal_gaps(panel[needed])
  if (length(gaps) > 0) {
    cli::cli_abort(c(
      "!" = "{.val {names(gaps)}} {?has/have} internal {.val NA}s, which koma cannot estimate on.",
      "i" = "Run {.fn fill_internal_gaps} on the panel first -- it interpolates them and warns."
    ))
  }

  ts_data <- harmonise_panel_attrs(panel[needed])
  ts_data[sys_eq$endogenous_variables] <- lapply(
    sys_eq$endogenous_variables,
    function(name) window_keeping_attrs(ts_data[[name]], end = dates$estimation$end)
  )

  # koma fans out one future per stochastic equation but never sets a plan
  # itself, so the default is fully sequential -- stage 2a's 65s was 13
  # equations one after another. With 68 equations this is the single biggest
  # runtime lever, so set the plan here and restore it on exit.
  if (!is.null(workers) && workers > 1) {
    warn_if_blas_threaded(workers)
    old_plan <- future::plan(stage1_parallel_strategy(), workers = workers)
    on.exit(future::plan(old_plan), add = TRUE)
    cli::cli_inform("Estimating {length(sys_eq$stochastic_equations)} equation{?s} on {workers} worker{?s}.")
  }

  started <- Sys.time()
  fit <- koma::estimate(ts_data, sys_eq, dates, options = options)
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))

  attr(fit, "runtime_s") <- elapsed
  attr(fit, "workers") <- workers %||% 1L
  fit
}

#' Build and fit the stage-2 pilot end to end
#'
#' Convenience wrapper tying [stage2_linkage_weights()], [stage2_spec()],
#' [build_stage2_system()], [build_stage2_panel()] and [fit_stage2()]
#' together, the way [fit_stage1()] does for one stage-1 country.
#'
#' @param countries Character vector of ISO-2 country codes.
#' @param panel Named list of `koma_ts`, as built by [build_global_panel()].
#' @param dates koma `dates` list, e.g. from [stage1_dates()].
#' @param trade_weights,gdp_weights Passed to [stage2_linkage_weights()].
#' @param options Passed through to `koma::estimate(options = )`.
#' @param tau Optional per-equation `tau` overrides, as in
#'   [build_system_equations()].
#' @param opts A [stage2_options()] list. Defaults reproduce stage 2a.
#' @param threshold,ireland_proxy Passed to [stage2_linkage_weights()].
#' @param dummies Passed to [build_stage2_panel()].
#' @param workers Passed to [fit_stage2()].
#'
#' @return A list with `fit`, `sys_eq`, `spec`, `linkage_weights`, `panel`
#'   (the augmented one) and `preflight`.
#' @export
fit_stage2_pilot <- function(countries, panel, dates, trade_weights, gdp_weights,
                             options = list(), tau = NULL, opts = stage2_options(),
                             threshold = 0, ireland_proxy = FALSE,
                             dummies = character(), workers = NULL) {
  countries <- tolower(countries)
  linkage_weights <- stage2_linkage_weights(
    countries, trade_weights, gdp_weights,
    threshold = threshold, ireland_proxy = ireland_proxy
  )
  shares <- stats::setNames(
    lapply(countries, function(cc) expenditure_shares(panel, cc, dates)),
    countries
  )
  spec <- stage2_spec(countries, shares, linkage_weights, opts = opts)
  sys_eq <- build_stage2_system(spec, tau = tau)
  stage2_panel <- build_stage2_panel(panel, linkage_weights, dummies = dummies)

  preflight <- stage2_preflight(sys_eq, stage2_panel, dates = dates)
  # which() drops NAs, so a check that could not be run (`ok` is NA) is
  # reported as unchecked rather than counted as a failure.
  failed <- preflight[which(!preflight$ok), ]
  if (nrow(failed) > 0) {
    cli::cli_abort(c(
      "Stage-2 pre-flight failed.",
      stats::setNames(paste0(failed$check, ": ", failed$detail), rep("x", nrow(failed)))
    ))
  }

  fit <- fit_stage2(sys_eq, stage2_panel, dates, options = options, workers = workers)

  list(
    fit = fit, sys_eq = sys_eq, spec = spec,
    linkage_weights = linkage_weights, panel = stage2_panel,
    preflight = preflight
  )
}

#' Standard stage-2b configuration
#'
#' The settings the eleven-country system is built with, in one place so the
#' benchmark steps, the two Ireland variants and the pipeline target cannot
#' drift apart. See `reports/stage2b_full_system.qmd` for why each is what it
#' is.
#'
#' @param ireland_proxy Load partners' foreign demand on `ie_consumption`
#'   instead of `ie_gdp` (see [stage2_linkage_weights()]).
#'
#' @return A list of arguments for [fit_stage2_pilot()].
#' @export
stage2b_config <- function(ireland_proxy = FALSE) {
  list(
    opts = stage2_options(
      # Government leaves the model: it appears only in the domestic-demand
      # identity, and removing it there buys back the 11 exogenous columns
      # without which the eleven-country system cannot be estimated at all.
      include_government = FALSE,
      extra_regressors = stage2b_dummies(),
      policy_rule = TRUE,
      # Per the three-variable exogenous spec the US goes on eur_usd, not the
      # stage-1/2a us_exchange_rate.
      fx = c(us = "eur_usd")
    ),
    threshold = 0.01,
    ireland_proxy = ireland_proxy,
    dummies = stage2b_dummies()
  )
}

#' @rdname stage2b_config
#' @export
stage2b_dummies <- function() {
  c("covid_2020q1", "covid_2020q2", "covid_2020q3", "covid_2021q2")
}

#' Configuration for stage 3a: the German labour and price block
#'
#' Stage 2b's configuration plus the stage-3a block, in two phases. Every
#' departure from stage 2b is carried here rather than baked into a builder,
#' so the cached stage-2b fit keeps reproducing.
#'
#' - **Phase A** (`phase = "a"`): Germany gets the full [labour_block()];
#'   `de_foreign_prices` stays exogenous, constructed by
#'   [build_stage2_panel()] from partners' export-price data. Costs six net
#'   predetermined columns (seven new behavioural equations, less the price
#'   equation that becomes an identity) plus one exogenous, so `k` goes from
#'   76 to 83 against `T = 98`.
#' - **Phase B** (`phase = "b"`): the other ten countries each get
#'   [export_price_block()], and `de_foreign_prices` becomes an identity.
#'   Ten more columns, `k = 92`, `df = 6`.
#'
#' **df = 6 is far outside anything this project has estimated.** Stage 2b's
#' only known-good point is `df = 22`, and koma draws `Omega` from
#' `riwish(T - k, .)`, so a thin df feeds directly into the explosive-draw
#' behaviour documented in `R/spillovers.R`. Phase A is the checkpoint: it is
#' estimated and diagnosed first, so if phase B degrades, the cause is
#' isolated to the ten extra equations rather than to the block as a whole.
#'
#' @param linkage_weights A [stage2_linkage_weights()] result, needed for the
#'   phase-B foreign-price identity weights.
#' @param hicp_weights Named list, `iso2 -> weights`, from [hicp_weights()].
#' @param phase `"a"` or `"b"`.
#' @param labour_countries Which countries carry the labour block.
#' @param ireland_proxy Passed through to [stage2b_config()].
#'
#' @return A list shaped like [stage2b_config()], with an added `phase`.
#' @export
stage3a_config <- function(linkage_weights, hicp_weights, phase = c("a", "b"),
                           labour_countries = "de", ireland_proxy = FALSE) {
  phase <- match.arg(phase)
  base <- stage2b_config(ireland_proxy = ireland_proxy)
  labour_countries <- tolower(labour_countries)

  export_price_countries <- character()
  fp_weights <- NULL
  if (identical(phase, "b")) {
    export_price_countries <- setdiff(names(linkage_weights$foreign_demand), labour_countries)
    fp_weights <- stats::setNames(
      lapply(labour_countries, function(cc) foreign_price_weights(linkage_weights, cc)),
      labour_countries
    )
  }

  base$opts <- stage2_options(
    include_government = base$opts$include_government,
    extra_regressors = base$opts$extra_regressors,
    policy_rule = base$opts$policy_rule,
    fx = base$opts$fx,
    labour_countries = labour_countries,
    export_price_countries = export_price_countries,
    hicp_weights = hicp_weights,
    foreign_price_weights = fp_weights
  )
  base$phase <- phase
  base$labour_countries <- labour_countries
  base
}

#' Estimation window for stage 2b
#'
#' Stage 1 and 2a use 2019Q4/2023Q1 so that 2020-2022 is conditionally filled
#' rather than estimated, which is how COVID is neutralised (see CLAUDE.md).
#' Stage 2b **cannot** afford that: at 78 observations the eleven-country
#' system's `k = 76` leaves too little, and with government retained it is
#' outright unestimable. Extending to 2024Q4 buys T = 98, and COVID is
#' controlled with dummies instead of excluded.
#'
#' @param panel Named list of `koma_ts` (unused; kept for symmetry with
#'   [stage1_dates()]).
#'
#' @return A koma `dates` list.
#' @export
stage2b_dates <- function(panel = NULL) {
  list(
    estimation = list(start = c(2000, 1), end = c(2024, 4)),
    forecast = list(start = c(2025, 1), end = c(2026, 1))
  )
}

#' Benchmark the stage-2 system as it scales
#'
#' Fits a sequence of progressively larger country sets under one fixed
#' configuration and records what each costs. Running the steps under
#' *identical* settings is the point: the scaling curve is only meaningful if
#' nothing but the country count changes, which is why this does not reuse the
#' stage-2a fit as a data point (that used a different window and a different
#' `k`).
#'
#' The `k` and `df` columns are the ones to watch. koma projects every
#' equation on the full `k`-column `x_matrix` on every draw, so cost grows
#' with `k` as well as with the equation count, and `df = T - k` hitting zero
#' is a hard failure rather than a slow run -- see [stage2_preflight()].
#'
#' @param steps Named list of country vectors, one per step.
#' @param panel Named list of `koma_ts`.
#' @param dates koma `dates` list.
#' @param trade_weights,gdp_weights Passed to [stage2_linkage_weights()].
#' @param config A [stage2b_config()] list.
#' @param workers Passed to [fit_stage2()].
#' @param options Passed to `koma::estimate(options = )`.
#' @param cache_dir Directory to save each step's fit into, or `NULL`.
#'
#' @return A `data.frame`, one row per step, with `attr(, "fits")` holding the
#'   fitted objects.
#' @export
benchmark_stage2 <- function(steps, panel, dates, trade_weights, gdp_weights,
                             config = stage2b_config(), workers = NULL,
                             options = list(), cache_dir = NULL) {
  if (!is.null(cache_dir)) dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  fits <- list()
  rows <- lapply(names(steps), function(label) {
    countries <- steps[[label]]
    cli::cli_inform("Benchmark step {.val {label}}: {length(countries)} countr{?y/ies}.")

    gc(reset = TRUE, full = TRUE)
    started <- Sys.time()
    pilot <- fit_stage2_pilot(
      countries, panel, dates, trade_weights, gdp_weights,
      options = options, opts = config$opts, threshold = config$threshold,
      ireland_proxy = config$ireland_proxy, dummies = config$dummies,
      workers = workers
    )
    elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
    peak_mb <- sum(gc()[, "max used"] * c(8, 8) / 1024^2)

    acceptance <- check_acceptance_rates(pilot$fit)
    with_mh <- acceptance$acceptance_rate[acceptance$has_mh_step]
    sys_eq <- pilot$sys_eq
    k <- length(sys_eq$total_exogenous_variables)

    fits[[label]] <<- pilot
    if (!is.null(cache_dir)) {
      saveRDS(pilot, file.path(cache_dir, paste0(gsub("[^A-Za-z0-9]+", "_", label), ".rds")))
    }

    data.frame(
      step = label,
      countries = length(countries),
      stochastic = length(sys_eq$stochastic_equations),
      identities = length(sys_eq$identities),
      equations = length(sys_eq$endogenous_variables),
      k = k,
      T_obs = estimation_length(panel, dates),
      df = estimation_length(panel, dates) - k,
      seconds = round(elapsed, 1),
      workers = pilot$fit |> attr("workers"),
      peak_mem_mb = round(peak_mb, 1),
      fit_mb = round(as.numeric(utils::object.size(pilot$fit)) / 1024^2, 1),
      min_acceptance = round(min(with_mh), 4),
      max_acceptance = round(max(with_mh), 4),
      n_flagged = sum(acceptance$flagged),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  attr(out, "fits") <- fits
  out
}

#' Fit the scaling curve and project a larger step
#'
#' Regresses `log(seconds)` on `log(stochastic equations)` over the benchmark
#' steps run so far and extrapolates. Use it as a **gate**: run the small
#' steps, project the big one, and decide whether to launch it or raise
#' `workers` first.
#'
#' An exponent near 1 means cost is dominated by the number of equations;
#' above 1 means the per-equation cost is itself growing, which is expected
#' here because `k` grows with the system and koma recomputes
#' `solve(t(x) %*% x)` on every draw.
#'
#' @param benchmark A [benchmark_stage2()] result (two or more rows).
#' @param stochastic Equation count to project to.
#'
#' @return A list with `exponent`, `intercept`, `r_squared` and
#'   `projected_seconds`.
#' @export
project_stage2_runtime <- function(benchmark, stochastic) {
  if (nrow(benchmark) < 2) {
    cli::cli_abort("Need at least two benchmark steps to fit a scaling curve.")
  }
  model <- stats::lm(log(seconds) ~ log(stochastic), data = benchmark)
  exponent <- unname(stats::coef(model)[2])
  projected <- exp(stats::predict(model, data.frame(stochastic = stochastic)))

  list(
    exponent = exponent,
    intercept = unname(stats::coef(model)[1]),
    r_squared = summary(model)$r.squared,
    projected_seconds = unname(projected)
  )
}

#' Warn when a threaded BLAS will fight the per-equation workers
#'
#' This project's R links against a **pthread** OpenBLAS
#' (`libopenblasp-*.so`). Each process it runs in claims every core for its own
#' BLAS thread pool, and `future::multicore` forks inherit that. Running `w`
#' workers therefore spawns roughly `w x ncores` threads onto `ncores` cores.
#'
#' Measured on this machine: 8 workers against a 25-equation system drove the
#' load average to **116 on 16 cores**, with each worker burning ~180% CPU and
#' the step failing to finish in twenty minutes -- against ~1 minute for the
#' same work done properly.
#'
#' The fix is to pin BLAS to one thread per process **before R starts**, so the
#' parallelism is purely across equations:
#'
#' ```sh
#' OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 Rscript ...
#' ```
#'
#' It has to be the environment rather than `Sys.setenv()` inside the session,
#' because OpenBLAS sizes its pool when the library first initialises. Pinning
#' costs nothing here: koma's matrices are at most `k x k` (76 x 76 for the
#' eleven-country system), far too small for threaded BLAS to pay for its own
#' synchronisation.
#'
#' @param workers Number of workers about to be started.
#'
#' @return Invisibly `TRUE` if a warning was issued.
#' @keywords internal
warn_if_blas_threaded <- function(workers) {
  pinned <- any(vapply(
    c("OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS"),
    function(v) identical(Sys.getenv(v), "1"),
    logical(1)
  ))
  if (pinned) {
    return(invisible(FALSE))
  }
  threaded <- grepl("openblasp|libmkl|libblis", sessionInfo()$BLAS %||% "", ignore.case = TRUE)
  if (!threaded) {
    return(invisible(FALSE))
  }

  cli::cli_warn(c(
    "!" = "A threaded BLAS is active and {.arg workers} is {workers}: each forked
           worker will claim every core for its own thread pool.",
    "i" = "Re-run with {.code OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1} set in the
           environment before R starts.",
    "i" = "Left unpinned this oversubscribes badly -- measured load average 116 on
           16 cores, and a step that should take a minute did not finish in twenty."
  ))
  invisible(TRUE)
}
