# Shared helpers for the per-stage equation-overview reports
# (stage1_equations.qmd, stage2_equations.qmd, stage3a_equations.qmd,
# stage3b_equations.qmd). Presentational only -- not part of the package,
# sourced directly by each report's load chunk, the same way individual
# reports already define their own small formatting helpers (e.g.
# stage3b_external_fiscal_financial.qmd's `coefs_of`/`ci_width`).
#
# Three views of the same equation set, built from the SAME `spec` object
# (list(stochastic = , identities = )) every fitted system already carries:
#   1. koma_string_*()    -- exactly stochastic_equation()/identity_equation(),
#                             i.e. the literal string koma parsed.
#   2. readable_*()        -- the same equation, human variable names, no
#                             koma syntax.
#   3. coefficient_table() -- already exists (R/diagnostics.R); this file
#                             only adds a readable-name join for it.

country_names <- c(
  at = "Austria", be = "Belgium", de = "Germany", gr = "Greece", es = "Spain",
  fr = "France", ie = "Ireland", it = "Italy", nl = "Netherlands", pt = "Portugal",
  us = "United States",
  # Stage 2d. `cn` is a real ISO-2 code; `reu` is a bloc pseudo-country (see
  # `bloc_codes`), three letters deliberately so it cannot collide with one.
  cn = "China", reu = "Rest of Euro Area"
)

concept_names <- c(
  gdp = "GDP", consumption = "Consumption", investment = "Investment",
  government = "Government Consumption", exports = "Exports", imports = "Imports",
  domestic_demand = "Domestic Demand", prices = "Prices (HICP)", core_prices = "Core Prices",
  nonenergy_prices = "Non-Energy Prices", energy_prices = "Energy Prices",
  import_prices = "Import Prices", export_prices = "Export Prices",
  long_rate = "Long-Term Rate", policy_rate = "Policy Rate", spread = "Sovereign Spread",
  unemployment = "Unemployment Rate", employment = "Employment", wages = "Wage Rate",
  productivity = "Productivity", ulc = "Unit Labour Cost", real_income = "Real Income",
  foreign_demand = "Foreign Demand", foreign_prices = "Foreign Prices",
  competitiveness = "Competitiveness", terms_of_trade = "Terms of Trade",
  current_account = "Current Account (% GDP)", govdebt = "Government Debt (% GDP)",
  netborrowing = "Net Borrowing (% GDP, change)", credit = "Private Credit",
  house_prices = "House Prices"
)

special_names <- c(
  constant = "Constant", oil_price = "Oil Price", row_gdp = "Rest-of-World GDP",
  eur_usd = "EUR/USD Exchange Rate", us_exchange_rate = "USD Exchange Rate (index)",
  ea_policy_rate = "Euro Area Policy Rate", us_policy_rate = "US Policy Rate",
  ea_gdp = "Euro Area GDP", ea_prices = "Euro Area Prices",
  cn_policy_rate = "China Policy Rate"
)

#' Human-readable label for one koma variable name, no lag/weight decoration.
readable_var <- function(x) {
  if (!is.na(special_names[x])) {
    return(unname(special_names[x]))
  }
  if (grepl("^covid_[0-9]{4}q[1-4]$", x)) {
    parsed <- regmatches(x, regexec("^covid_([0-9]{4})q([1-4])$", x))[[1]]
    return(sprintf("COVID Dummy (%sQ%s)", parsed[2], parsed[3]))
  }
  # Two letters for a country, or a bloc code -- `reu` is three, so the
  # pattern cannot be `[a-z]{2}` alone or every reu_* variable would fall
  # through to the generic title-case branch and read "Reu Gdp".
  m <- regmatches(x, regexec("^([a-z]{2,3})_(.+)$", x))[[1]]
  if (length(m) == 3 && !is.na(country_names[m[2]])) {
    concept <- concept_names[m[3]]
    return(paste0(country_names[[m[2]]], ": ", if (is.na(concept)) tools::toTitleCase(gsub("_", " ", m[3])) else concept))
  }
  m2 <- regmatches(x, regexec("^ea_(.+)$", x))[[1]]
  if (length(m2) == 2) {
    concept <- concept_names[m2[2]]
    return(paste0("Euro Area: ", if (is.na(concept)) tools::toTitleCase(gsub("_", " ", m2[2])) else concept))
  }
  tools::toTitleCase(gsub("_", " ", x))
}

#' Strip a `.L(n)` suffix off a koma term, returning list(base, lag) -- lag is
#' NA if the term is not lagged.
split_lag <- function(term) {
  m <- regmatches(term, regexec("^(.+)\\.L\\(([0-9]+)\\)$", term))[[1]]
  if (length(m) == 3) list(base = m[2], lag = as.integer(m[3])) else list(base = term, lag = NA_integer_)
}

#' A single koma term (possibly lagged), human-readable.
readable_term <- function(term) {
  sl <- split_lag(term)
  label <- readable_var(sl$base)
  if (!is.na(sl$lag)) paste0(label, " (t-", sl$lag, ")") else label
}

#' The koma equation string for one stochastic equation -- exactly what was
#' estimated, via the project's own stochastic_equation().
koma_string_stochastic <- function(dep, eq) stochastic_equation(dep, eq$terms, eq$lags)

#' The koma equation string for one identity -- exactly what was estimated,
#' via the project's own identity_equation().
koma_string_identity <- function(dep, weights) identity_equation(dep, weights)

#' Human-readable form of one stochastic equation: same "~" notation,
#' readable variable names, "(t-1)" instead of ".L(1)".
readable_stochastic <- function(dep, eq) {
  terms <- vapply(eq$terms, function(term) {
    lag <- eq$lags[[term]]
    label <- readable_var(term)
    if (!is.null(lag)) paste0(label, " (t-", lag, ")") else label
  }, character(1), USE.NAMES = FALSE)
  paste0(readable_var(dep), "  ~  ", paste(terms, collapse = "  +  "))
}

#' Human-readable form of one identity: same "=" notation, readable variable
#' names, "x" instead of "*", en-dash for a subtracted term (matching
#' identity_equation()'s own "- w*x, never + -w*x" sign convention).
readable_identity <- function(dep, weights) {
  components <- names(weights)
  rendered <- lapply(seq_along(components), function(i) {
    w <- weights[[i]]
    label <- readable_term(components[i]) # identity components can be lagged (stock-flow accumulation)
    wtxt <- if (is.character(w)) paste0("(", w, ")") else format(round(abs(w), 3), trim = TRUE)
    sep <- if (is.character(w) || w >= 0) "+" else "-"
    list(sep = sep, text = paste0(wtxt, " x ", label))
  })
  rhs <- rendered[[1]]$text
  if (identical(rendered[[1]]$sep, "-")) rhs <- paste0("-", rhs)
  for (i in seq_along(components)[-1]) rhs <- paste(rhs, rendered[[i]]$sep, rendered[[i]]$text)
  paste0(readable_var(dep), "  =  ", rhs)
}

#' A three-column overview table (koma string / readable / equation key) for
#' a whole spec -- the shared build behind every report's "koma strings" and
#' "readable overview" sections.
equation_overview <- function(spec) {
  stoch <- do.call(rbind, lapply(names(spec$stochastic), function(dep) {
    eq <- spec$stochastic[[dep]]
    data.frame(
      kind = "stochastic", equation = dep,
      koma_string = koma_string_stochastic(dep, eq),
      readable = readable_stochastic(dep, eq),
      n_regressors = length(eq$terms), stringsAsFactors = FALSE
    )
  }))
  ident <- do.call(rbind, lapply(names(spec$identities), function(dep) {
    w <- spec$identities[[dep]]
    data.frame(
      kind = "identity", equation = dep,
      koma_string = koma_string_identity(dep, w),
      readable = readable_identity(dep, w),
      n_regressors = length(w), stringsAsFactors = FALSE
    )
  }))
  rbind(stoch, ident)
}

#' coefficient_table() with a readable-name column added, and equations
#' ordered to match `equation_order` (typically the spec's own declaration
#' order, i.e. `names(c(spec$stochastic, spec$identities))`) rather than
#' coefficient_table()'s own (alphabetical) order.
readable_coefficient_table <- function(fit, equation_order = NULL) {
  ct <- coefficient_table(fit)
  ct$equation_readable <- vapply(ct$equation, readable_var, character(1))
  ct$term_readable <- vapply(ct$term, readable_term, character(1))
  if (!is.null(equation_order)) {
    ct$equation <- factor(ct$equation, levels = intersect(equation_order, unique(ct$equation)))
    ct <- ct[order(ct$equation), ]
    ct$equation <- as.character(ct$equation)
  }
  rownames(ct) <- NULL
  ct[, c("equation", "equation_readable", "term", "term_readable", "estimate", "ci_low", "ci_high")]
}

# --- country-generic ("template") views --------------------------------
# The per-country equations in this project are generated from one template
# per concept, so a report that lists 68 stochastic equations one by one buries
# the fact that there are really only a handful of distinct forms. These build
# the template view -- and, importantly, VERIFY it rather than assert it: any
# country whose equation does not collapse to the shared form shows up as its
# own group rather than being silently absorbed.

#' Replace one country's own ISO-2 prefix with a generic placeholder.
#'
#' Only prefixes at a token boundary are replaced, so `de_gdp` becomes
#' `cc_gdp` while a partner reference inside another country's identity is
#' left alone.
templatise <- function(x, iso2, placeholder = "cc") {
  gsub(paste0("(?<![A-Za-z0-9_])", iso2, "_"), paste0(placeholder, "_"), x, perl = TRUE)
}

#' Group every country's stochastic equations by their country-generic form.
#'
#' @return A `data.frame` with one row per (concept, distinct form): `concept`,
#'   `form`, `n_countries`, `countries`. A concept with more than one row is a
#'   concept where the countries genuinely differ.
template_groups <- function(spec, countries, placeholder = "cc") {
  rows <- do.call(rbind, lapply(countries, function(cc) {
    own <- grep(paste0("^", cc, "_"), names(spec$stochastic), value = TRUE)
    if (length(own) == 0) return(NULL)
    do.call(rbind, lapply(own, function(dep) data.frame(
      concept = sub(paste0("^", cc, "_"), "", dep), iso2 = cc,
      form = templatise(koma_string_stochastic(dep, spec$stochastic[[dep]]), cc, placeholder),
      stringsAsFactors = FALSE)))
  }))
  grouped <- do.call(rbind, lapply(split(rows, list(rows$concept, rows$form), drop = TRUE), function(g)
    data.frame(concept = g$concept[1], form = g$form[1], n_countries = nrow(g),
               countries = paste(sort(g$iso2), collapse = ", "), stringsAsFactors = FALSE)))
  grouped <- grouped[order(grouped$concept, -grouped$n_countries), ]
  rownames(grouped) <- NULL
  grouped
}

#' The component structure of one identity, country-generic and weight-free.
#'
#' Weights are dropped (they are country-specific shares) but their SIGNS are
#' kept, because a sign is structural: `- w*imports` is part of the accounting
#' identity, not a fitted quantity.
identity_structure <- function(dep, weights, iso2, placeholder = "cc") {
  comps <- names(weights)
  txt <- vapply(seq_along(comps), function(i) {
    sign <- if (is.character(weights[[i]]) || weights[[i]] >= 0) "+" else "-"
    paste0(sign, " w*", templatise(comps[i], iso2, placeholder))
  }, character(1))
  out <- paste(txt, collapse = " ")
  paste0(templatise(dep, iso2, placeholder), " == ", sub("^\\+ ", "", out))
}

#' Group every country's identities by their country-generic component
#' structure, and report the range each weight takes across countries.
identity_template_groups <- function(spec, countries, placeholder = "cc") {
  rows <- do.call(rbind, lapply(countries, function(cc) {
    own <- grep(paste0("^", cc, "_"), names(spec$identities), value = TRUE)
    if (length(own) == 0) return(NULL)
    do.call(rbind, lapply(own, function(dep) data.frame(
      concept = sub(paste0("^", cc, "_"), "", dep), iso2 = cc,
      structure = identity_structure(dep, spec$identities[[dep]], cc, placeholder),
      n_components = length(spec$identities[[dep]]),
      stringsAsFactors = FALSE)))
  }))
  grouped <- do.call(rbind, lapply(split(rows, list(rows$concept, rows$structure), drop = TRUE), function(g)
    data.frame(concept = g$concept[1], structure = g$structure[1], n_countries = nrow(g),
               components = if (length(unique(g$n_components)) == 1) as.character(g$n_components[1])
                            else paste0(min(g$n_components), "-", max(g$n_components)),
               countries = paste(sort(g$iso2), collapse = ", "), stringsAsFactors = FALSE)))
  grouped <- grouped[order(grouped$concept, -grouped$n_countries), ]
  rownames(grouped) <- NULL
  grouped
}

#' Range of one identity's weight on a named component across countries.
identity_weight_range <- function(spec, countries, concept, component) {
  vals <- vapply(countries, function(cc) {
    w <- spec$identities[[country_var(cc, concept)]]
    if (is.null(w)) return(NA_real_)
    nm <- country_var(cc, component)
    if (!nm %in% names(w)) return(NA_real_)
    as.numeric(w[[nm]])
  }, numeric(1))
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) return(data.frame(min = NA_real_, median = NA_real_, max = NA_real_))
  data.frame(min = min(vals), median = stats::median(vals), max = max(vals))
}
