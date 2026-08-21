# Stage 1: per-country satellite models.
#
# Each of the 11 modelled economies is estimated as its own small koma
# system -- a direct generalisation of koma's `small_open_economy`
# (Switzerland) vignette -- taking shared/world variables as exogenous.
# Cross-country simultaneity is deliberately NOT handled here; that is
# stage 2's job (stage2_system.R), which can warm-start from these fits
# via koma::estimate(..., estimates = ).
#
# The per-country template is identical for all 11 except the US, which
# differs in exactly three ways (see `stage1_spec()`).

#' Directory used to cache stage-1 fits
#'
#' Mirrors [eurostat_cache_dir()]. Git-ignored via `data/cache/*`.
#' @keywords internal
stage1_cache_dir <- function() {
  path <- file.path("data", "cache", "stage1")
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

#' Average expenditure shares for a country's stage-1 identities
#'
#' Computes the identity coefficients for [stage1_spec()] from the data
#' rather than copying the vignette's illustrative numbers. Each share is
#' the **mean of the period-by-period ratio** over the estimation sample
#' (not the ratio of the means), which is the usual reading of "average
#' expenditure share".
#'
#' Two sets are returned:
#'
#' - **GDP identity**, relative to GDP: `domestic_demand`, `exports`, and
#'   `imports` (returned **negative**, since imports subtract).
#' - **Domestic-demand identity**, relative to domestic demand:
#'   `consumption`, `investment`, `government`.
#'
#' The model is estimated in growth rates (`method = "diff_log"`) while
#' these shares are computed on levels. That is correct, not an
#' inconsistency: log-linearising an accounting identity turns it into a
#' share-weighted sum of the components' growth rates, so the level
#' shares *are* the right coefficients for the growth-rate identity. It
#' is also what the Switzerland vignette does.
#'
#' **Rounding.** Shares are rounded to `digits` (default 3) decimals so
#' the generated equation strings stay readable. This moves each weight
#' by at most 0.0005. It is safe to round because koma performs **no**
#' identity-consistency check against the data (verified against koma
#' 0.3.1) -- identity weights are a specification choice that fixes
#' entries of the `Gamma` matrix, not a constraint the data must satisfy.
#' The vignette's own weights sum to 1.3, for comparison.
#'
#' The consumption/investment/government shares sum to exactly 1 by
#' construction, because `domestic_demand` *is* their sum (see
#' [gdp_identity_component()]). The GDP-identity shares reconcile only up
#' to the national-accounts statistical discrepancy, measured at 2-6% for
#' the euro-area countries and wider for Ireland -- see the identity test
#' in `tests/testthat/test-panel_build.R`.
#'
#' @param panel A named list of `koma_ts`, as built by
#'   [build_global_panel()].
#' @param iso2 Two-letter lowercase ISO country code.
#' @param dates Optional koma `dates` list; if given, shares are computed
#'   over `dates$estimation` only, so the identity coefficients never see
#'   data outside the estimation sample.
#' @param digits Number of decimals to round each share to.
#'
#' @return A list with elements `gdp` and `domestic_demand`, each a named
#'   numeric vector keyed by full `<iso2>_<concept>` variable name.
#' @export
expenditure_shares <- function(panel, iso2, dates = NULL, digits = 3) {
  iso2 <- tolower(iso2)

  series <- function(concept) {
    name <- country_var(iso2, concept)
    x <- panel[[name]]
    if (is.null(x)) {
      cli::cli_abort("{.arg panel} is missing {.val {name}}, needed to compute expenditure shares.")
    }
    if (!is.null(dates)) {
      x <- stats::window(x, start = dates$estimation$start, end = dates$estimation$end)
    }
    as.numeric(x)
  }

  gdp <- series("gdp")
  dd <- series("domestic_demand")

  share <- function(numerator, denominator) {
    round(mean(numerator / denominator, na.rm = TRUE), digits)
  }

  gdp_shares <- c(
    share(dd, gdp),
    share(series("exports"), gdp),
    -share(series("imports"), gdp)
  )
  names(gdp_shares) <- country_var(iso2, c("domestic_demand", "exports", "imports"))

  dd_shares <- c(
    share(series("consumption"), dd),
    share(series("investment"), dd),
    share(series("government"), dd)
  )
  names(dd_shares) <- country_var(iso2, c("consumption", "investment", "government"))

  list(gdp = gdp_shares, domestic_demand = dd_shares)
}

#' Build one country's stage-1 model specification
#'
#' Generates the country's equation spec from a single template. The
#' stochastic block is, for country `cc`:
#'
#' ```
#' cc_consumption ~ cc_gdp + cc_consumption.L(1)
#' cc_investment  ~ cc_gdp + cc_investment.L(1)
#' cc_exports     ~ row_gdp + cc_exports.L(1)
#' cc_imports     ~ cc_domestic_demand + cc_imports.L(1)
#' cc_prices      ~ eur_usd + oil_price + cc_prices.L(1)
#' cc_long_rate   ~ cc_prices + ea_policy_rate + cc_long_rate.L(1)
#' ```
#'
#' plus the two identities whose coefficients come from
#' [expenditure_shares()].
#'
#' **The US differs in exactly three ways**: `eur_usd` becomes
#' `us_exchange_rate` (the broad dollar index -- the US has its own
#' currency, so a bilateral EUR/USD rate is the wrong price for it),
#' `ea_policy_rate` becomes `us_policy_rate`, and `us_policy_rate` gains
#' its own Taylor-type equation
#' `us_policy_rate ~ us_prices + us_gdp + us_policy_rate.L(1)`, which
#' moves it from exogenous to endogenous.
#'
#' Conversely, no euro-area country gets its own policy rate or exchange
#' rate: all ten share `ea_policy_rate` and `eur_usd`, and country
#' heterogeneity enters through `<iso2>_long_rate` (the sovereign yield),
#' so spreads are in the model. See CLAUDE.md.
#'
#' **Foreign demand caveat.** Exports are driven by `row_gdp`, which
#' covers only the UK, Switzerland, China, Japan, Poland, Sweden and an
#' "other" residual -- it excludes all 11 modelled countries by
#' construction (see [build_row_gdp()]). For a euro-area country that
#' omits intra-EA export demand, which is the majority of its export
#' market, so stage 1 understates foreign demand by design. Stage 2
#' estimates the cross-country links jointly and is where that is
#' resolved.
#'
#' `<iso2>_government` has no equation of its own and is therefore
#' exogenous: fiscal policy is a stage-1 given.
#'
#' @param iso2 Two-letter lowercase ISO country code, one of
#'   `modelled_countries`.
#' @param shares A list as returned by [expenditure_shares()], supplying
#'   the two identities' coefficients.
#'
#' @return A list with elements `stochastic` (named list, dependent
#'   variable -> `list(terms, lags)`) and `identities` (named list,
#'   dependent variable -> named numeric weights).
#' @export
stage1_spec <- function(iso2, shares) {
  iso2 <- tolower(iso2)
  if (!iso2 %in% modelled_countries) {
    cli::cli_abort("Unknown country {.val {iso2}}; expected one of {.val {modelled_countries}}.")
  }

  v <- function(concept) country_var(iso2, concept)
  is_us <- identical(iso2, "us")
  fx <- if (is_us) "us_exchange_rate" else "eur_usd"
  policy_rate <- if (is_us) "us_policy_rate" else "ea_policy_rate"

  # Every equation carries its own first lag; `own_lag()` builds the
  # one-element lags list that stochastic_equation() expects.
  own_lag <- function(name) stats::setNames(list("1"), name)

  stochastic <- list()
  stochastic[[v("consumption")]] <- list(
    terms = c(v("gdp"), v("consumption")), lags = own_lag(v("consumption"))
  )
  stochastic[[v("investment")]] <- list(
    terms = c(v("gdp"), v("investment")), lags = own_lag(v("investment"))
  )
  stochastic[[v("exports")]] <- list(
    terms = c("row_gdp", v("exports")), lags = own_lag(v("exports"))
  )
  stochastic[[v("imports")]] <- list(
    terms = c(v("domestic_demand"), v("imports")), lags = own_lag(v("imports"))
  )
  stochastic[[v("prices")]] <- list(
    terms = c(fx, "oil_price", v("prices")), lags = own_lag(v("prices"))
  )
  stochastic[[v("long_rate")]] <- list(
    terms = c(v("prices"), policy_rate, v("long_rate")), lags = own_lag(v("long_rate"))
  )

  if (is_us) {
    stochastic[["us_policy_rate"]] <- list(
      terms = c("us_prices", "us_gdp", "us_policy_rate"),
      lags = own_lag("us_policy_rate")
    )
  }

  list(
    stochastic = stochastic,
    identities = list(
      gdp = shares$gdp,
      domestic_demand = shares$domestic_demand
    ) |> stats::setNames(v(c("gdp", "domestic_demand")))
  )
}

#' Build one country's stage-1 equation set
#'
#' Turns a [stage1_spec()] into a `koma_seq` via [stochastic_equation()]
#' and [identity_equation()].
#'
#' Exogenous variables are **derived**, not declared: any variable
#' appearing on a right-hand side that is not itself the dependent
#' variable of a stochastic equation or an identity is exogenous. That
#' keeps the exogenous set in sync with the template automatically -- so
#' moving `us_policy_rate` from exogenous to endogenous, as the US
#' variant does, requires no second edit.
#'
#' @param iso2 Two-letter lowercase ISO country code.
#' @param spec A list as returned by [stage1_spec()], with elements
#'   `stochastic` and `identities`.
#'
#' @return A `koma::koma_seq` object.
#' @export
stage1_country_equations <- function(iso2, spec) {
  iso2 <- tolower(iso2)

  stochastic <- spec$stochastic %||% list()
  identities <- spec$identities %||% list()
  if (length(stochastic) == 0) {
    cli::cli_abort("{.arg spec} must contain at least one stochastic equation; koma requires one.")
  }

  stochastic_strings <- vapply(names(stochastic), function(dep) {
    eq <- stochastic[[dep]]
    stochastic_equation(dep, terms = eq$terms, lags = eq$lags)
  }, character(1))

  identity_strings <- vapply(names(identities), function(dep) {
    identity_equation(dep, as.list(identities[[dep]]))
  }, character(1))

  endogenous <- c(names(stochastic), names(identities))
  rhs_variables <- unique(c(
    unlist(lapply(stochastic, function(eq) eq$terms), use.names = FALSE),
    unlist(lapply(identities, names), use.names = FALSE)
  ))
  exogenous <- setdiff(rhs_variables, endogenous)

  koma::system_of_equations(
    equations = unname(c(stochastic_strings, identity_strings)),
    exogenous_variables = exogenous
  )
}

#' Build the stage-1 estimation and forecast dates
#'
#' Estimation runs from the panel's own common start to `estimation_end`
#' (2019Q4 by default). The forecast start is deliberately set well
#' *after* the estimation end so that koma **conditionally fills** the
#' intervening quarters rather than estimating on them: with the default
#' 2019Q4 / 2023Q1 pair, 2020Q1-2022Q4 is filled, which is this project's
#' chosen way of neutralising the COVID period. It is the vignette's own
#' device, adopted deliberately -- see `docs/koma-api.md` §9, which
#' benchmarks exactly this 12-quarter conditional fill.
#'
#' The start is read from the panel rather than hard-coded, so a new
#' EA-MD/QD vintage that shifts coverage is picked up automatically.
#'
#' @param panel A named list of `koma_ts`, ideally already passed through
#'   [align_panel()].
#' @param estimation_end,forecast_start,forecast_end `c(year, quarter)`.
#'
#' @return A koma `dates` list with `estimation` and `forecast`.
#' @export
stage1_dates <- function(panel, estimation_end = c(2019, 4),
                         forecast_start = c(2023, 1), forecast_end = c(2023, 4)) {
  if (length(panel) == 0) {
    cli::cli_abort("{.arg panel} is empty.")
  }
  frequency <- unique(vapply(panel, stats::frequency, numeric(1)))
  if (length(frequency) != 1) {
    cli::cli_abort("{.arg panel} mixes frequencies: {.val {frequency}}.")
  }

  start <- num_to_period(max(vapply(panel, function(x) stats::tsp(x)[1], numeric(1))), frequency)

  list(
    estimation = list(start = start, end = estimation_end),
    forecast = list(start = forecast_start, end = forecast_end)
  )
}

#' Fit one country's stage-1 model
#'
#' Computes the country's expenditure shares, builds its system, subsets
#' the panel to exactly the variables that system needs, truncates the
#' **endogenous** series to the estimation end so koma conditionally
#' fills the gap up to the forecast start, and estimates.
#'
#' @param iso2 Two-letter lowercase ISO country code.
#' @param panel Named list of `koma_ts` covering this country plus the
#'   shared exogenous regressors, as built by [build_global_panel()].
#' @param dates koma `dates` list, e.g. from [stage1_dates()].
#' @param options Passed through to `koma::estimate(options = )`. The
#'   default is koma's own default (`ndraws = 2000`, `burnin_ratio = 0.5`,
#'   `nstore = 1`, `tau = 1.1`) -- stage 1 is deliberately untuned, so
#'   that the raw acceptance rates from [stage1_summary()] show where
#'   tuning is actually needed.
#'
#' @return A `koma::koma_estimate` object, carrying `runtime_s` and
#'   `iso2` attributes.
#' @export
fit_stage1 <- function(iso2, panel, dates, options = list()) {
  iso2 <- tolower(iso2)

  shares <- expenditure_shares(panel, iso2, dates)
  sys_eq <- stage1_country_equations(iso2, stage1_spec(iso2, shares))

  # Same subset koma itself takes in new_prepare_estimation().
  # `weight_variables` is empty here (our identity weights are numeric,
  # not injected expressions), but including it keeps this correct if a
  # future spec switches to injected weights.
  needed <- c(sys_eq$endogenous_variables, sys_eq$exogenous_variables, sys_eq$weight_variables)
  missing <- setdiff(needed, names(panel))
  if (length(missing) > 0) {
    cli::cli_abort("{.arg panel} is missing {.val {missing}}, required by the {.val {toupper(iso2)}} stage-1 system.")
  }

  # koma has no facility for an internal NA and fails with an opaque
  # "time series contains internal NAs" from inside level(). Catch it
  # here, where we can say which series and point at the fix.
  gaps <- internal_gaps(panel[needed])
  if (length(gaps) > 0) {
    cli::cli_abort(c(
      "!" = "{.val {names(gaps)}} {?has/have} internal {.val NA}s, which koma cannot estimate on.",
      "i" = "Run {.fn fill_internal_gaps} on the panel first -- it interpolates them and warns."
    ))
  }

  # koma's as_mets() requires a uniform attribute set across the list.
  ts_data <- harmonise_panel_attrs(panel[needed])

  # Endogenous series must stop at the estimation end; koma then
  # conditionally fills forward to one period before the forecast start.
  # Exogenous series keep their full sample -- they are what the fill
  # conditions on.
  ts_data[sys_eq$endogenous_variables] <- lapply(
    sys_eq$endogenous_variables,
    function(name) window_keeping_attrs(ts_data[[name]], end = dates$estimation$end)
  )

  started <- Sys.time()
  fit <- koma::estimate(ts_data, sys_eq, dates, options = options)
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))

  attr(fit, "runtime_s") <- elapsed
  attr(fit, "iso2") <- iso2
  fit
}

#' Window a koma_ts, restoring its koma attributes
#'
#' `stats::window()` is documented to preserve extra attributes, but
#' [align_panel()] re-applies them defensively and this does the same, so
#' a `koma_ts` never silently degrades to a plain `ts` -- which would make
#' `koma::estimate()` block on an interactive prompt (`docs/koma-api.md`
#' gotcha 1).
#' @keywords internal
window_keeping_attrs <- function(x, ...) {
  attrs <- get_custom_attrs(x)
  do.call(koma::as_ets, c(list(stats::window(x, ...)), attrs))
}

#' Fit stage 1 for every country
#'
#' Runs the 11 country estimations concurrently and caches each fit to
#' `data/cache/stage1/<iso2>.rds`.
#'
#' **Parallelism.** koma has no `cores` argument -- it uses
#' `future::plan()` set by the caller, and fans out one future *per
#' stochastic equation*, so on its own it parallelises only ~6 ways
#' within a single country. Here the outer loop over countries is the
#' wider axis (11 independent fits), so this function sets the plan and
#' spreads countries across workers with `future.apply::future_lapply()`;
#' `future` makes the nested inner level sequential automatically. The
#' caller's plan is restored on exit. `future.seed = TRUE` keeps results
#' reproducible and identical to a sequential run.
#'
#' @param countries Character vector of ISO-2 country codes.
#' @param panel Named list of `koma_ts`, the full multi-country panel.
#' @param dates koma `dates` list, e.g. from [stage1_dates()].
#' @param options Passed through to [fit_stage1()].
#' @param parallel Logical; if `FALSE`, runs sequentially under whatever
#'   plan is already active (useful when debugging, since worker errors
#'   are easier to read).
#' @param cache_dir Directory to write `<iso2>.rds` into. `NULL` skips
#'   caching.
#'
#' @return A named list of `koma_estimate` objects, one per country.
#' @export
fit_stage1_all <- function(countries = modelled_countries, panel, dates,
                           options = list(), parallel = TRUE,
                           cache_dir = stage1_cache_dir()) {
  if (parallel) {
    # Apple's Accelerate BLAS is not fork-safe and segfaults inside
    # koma's eigen() call, and Windows cannot fork at all -- both need
    # multisession. See docs/koma-api.md §3.
    can_fork <- .Platform$OS.type != "windows" && Sys.info()[["sysname"]] != "Darwin"
    strategy <- if (can_fork) "future::multicore" else "future::multisession"
    workers <- min(length(countries), parallelly::availableCores(omit = 1))

    old_plan <- future::plan(strategy, workers = workers)
    on.exit(future::plan(old_plan), add = TRUE)
    cli::cli_inform("Fitting {length(countries)} stage-1 model{?s} on {workers} worker{?s} ({strategy}).")
  }

  fits <- future.apply::future_lapply(
    countries,
    function(iso2) fit_stage1(iso2, panel, dates, options = options),
    future.seed = TRUE
  )
  names(fits) <- countries

  if (!is.null(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    for (iso2 in countries) {
      saveRDS(fits[[iso2]], file.path(cache_dir, paste0(iso2, ".rds")))
    }
    cli::cli_inform("Wrote {length(countries)} fit{?s} to {.path {cache_dir}}.")
  }

  fits
}

#' Summarise a set of stage-1 fits
#'
#' Builds the country x equation acceptance-rate table and prints it.
#' Equations with no contemporaneous endogenous regressor have no
#' Metropolis step at all -- their `count_accepted` is `NA` and they are
#' shown as `-`, never flagged. In this template that is `exports` and
#' `prices` for every country.
#'
#' @param fits A named list of `koma_estimate` objects, as returned by
#'   [fit_stage1_all()].
#' @param band Numeric length-2 acceptance-rate band. Default
#'   `c(0.2, 0.6)`, matching `koma:::get_default_acceptance_prob()`.
#' @param print Logical; print the table as a side effect.
#'
#' @return Invisibly, a `data.frame` with columns `country`,
#'   `runtime_s`, `equation`, `acceptance_rate`, `has_mh_step`,
#'   `flagged`.
#' @export
stage1_summary <- function(fits, band = c(0.2, 0.6), print = TRUE) {
  rows <- lapply(names(fits), function(iso2) {
    fit <- fits[[iso2]]
    out <- check_acceptance_rates(fit, band = band)
    out$country <- iso2
    out$runtime_s <- attr(fit, "runtime_s") %||% NA_real_
    out[, c("country", "runtime_s", "equation", "acceptance_rate", "has_mh_step", "flagged")]
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL

  if (print) {
    cli::cli_h1("Stage 1 -- {length(fits)} country model{?s}")
    for (iso2 in names(fits)) {
      country_rows <- out[out$country == iso2, ]
      runtime <- country_rows$runtime_s[1]
      n_flagged <- sum(country_rows$flagged, na.rm = TRUE)
      cli::cli_h3("{toupper(iso2)} -- {round(runtime, 1)}s{if (n_flagged > 0) paste0(', ', n_flagged, ' flagged') else ''}")
      for (i in seq_len(nrow(country_rows))) {
        r <- country_rows[i, ]
        label <- sub(paste0("^", iso2, "_"), "", r$equation)
        if (!r$has_mh_step) {
          cli::cli_li("{.field {label}}: {.emph -} (no Metropolis step)")
        } else if (r$flagged) {
          cli::cli_li("{.field {label}}: {cli::col_red(sprintf('%.1f%%', r$acceptance_rate * 100))} {cli::symbol$warning}")
        } else {
          cli::cli_li("{.field {label}}: {sprintf('%.1f%%', r$acceptance_rate * 100)}")
        }
      }
    }

    flagged <- out[out$flagged %in% TRUE, ]
    runtimes <- unique(out[, c("country", "runtime_s")])$runtime_s
    cli::cli_h2("Summary")
    # Countries are fitted concurrently, so the sum is CPU time; the
    # slowest single country is the wall-clock floor.
    cli::cli_alert_info(
      "Runtime {round(sum(runtimes), 1)}s summed across {length(fits)} model{?s}; slowest country {round(max(runtimes), 1)}s."
    )
    if (nrow(flagged) == 0) {
      cli::cli_alert_success("All acceptance rates inside {band[1] * 100}%-{band[2] * 100}%.")
    } else {
      cli::cli_alert_warning(
        "{nrow(flagged)} equation{?s} outside {band[1] * 100}%-{band[2] * 100}%: {.val {paste0(flagged$country, '/', sub('^[a-z]{2}_', '', flagged$equation))}}."
      )
      cli::cli_text("Untuned by design -- raise that equation's {.code tau} to lower its acceptance rate.")
    }
  }

  invisible(out)
}
