# Naming convention and equation-string builders for koma::system_of_equations().
#
# Variable names follow "<iso2>_<concept>", all lowercase (e.g. "de_gdp",
# "us_prices"). Variables shared across countries are prefixed "ea_" (euro
# area aggregate) or "world_" (global aggregate), or left unprefixed when the
# series has no natural aggregation (e.g. "oil_price"). See CLAUDE.md.

#' Bloc pseudo-country codes
#'
#' A **bloc** is a group of countries that occupies a single country slot in
#' the model: it has a `<code>_<concept>` series for every concept a real
#' country has, its own [country_block()], its own row and column in the trade
#' weight matrix, and its own `foreign_demand` identity. It is therefore *not*
#' an `ea_`/`world_` aggregate in the [shared_var()] sense -- those are
#' identities *over* modelled countries, computed alongside them; a bloc
#' replaces its members entirely, and the members have no equations at all.
#'
#' Stage 2d introduces exactly one: `reu`, the rest of the euro area (the
#' seven modelled EA economies other than Germany, France and Italy). Three
#' letters rather than two, deliberately -- no real ISO-2 code can collide
#' with it, so `startsWith(name, "reu_")` cannot accidentally match a country.
#'
#' Codes here are accepted by [country_var()] wherever an ISO-2 code is.
#' @keywords internal
bloc_codes <- c("reu")

#' Build a country-scoped variable name
#'
#' Combines an ISO-2 country code and a concept into the project's
#' `<iso2>_<concept>` naming convention, and validates the result against
#' koma's variable-name grammar (`^[a-zA-Z][a-zA-Z0-9_]*$`).
#'
#' A `bloc_codes` entry (currently only `"reu"`) is accepted in place of an
#' ISO-2 code: a bloc occupies a country slot and needs every `<code>_<concept>`
#' name a real country has. See `bloc_codes`.
#'
#' @param iso2 Two-letter lowercase ISO country code, e.g. `"de"`, or a
#'   `bloc_codes` entry. May be a vector, e.g. to build every `<iso2>_gdp` name
#'   for a set of countries at once; `concept` is recycled against it.
#' @param concept Lowercase concept name, e.g. `"gdp"`.
#'
#' @return A character vector of `"<iso2>_<concept>"` names, same length as
#'   `iso2`.
#' @export
country_var <- function(iso2, concept) {
  bad <- iso2[!grepl("^[a-z]{2}$", iso2) & !iso2 %in% bloc_codes]
  if (length(bad) > 0) {
    cli::cli_abort(c(
      "{.arg iso2} must be two-letter lowercase codes, got {.val {bad}}.",
      "i" = "Bloc pseudo-countries are also accepted: {.val {bloc_codes}}."
    ))
  }
  name <- paste0(iso2, "_", concept)
  if (!all(is_valid_project_name(name))) {
    cli::cli_abort("{.val {name[!is_valid_project_name(name)]}} is not a valid project variable name.")
  }
  name
}

#' Build a shared (cross-country) variable name
#'
#' Builds a variable name for a series aggregated across countries
#' (`"ea_"` for euro-area aggregates, `"world_"` for global aggregates), or
#' returns `concept` unchanged when `scope = "none"` (e.g. `"oil_price"`).
#'
#' @param concept Lowercase concept name, e.g. `"gdp"`.
#' @param scope One of `"ea"`, `"world"`, `"none"`.
#'
#' @return A single string.
#' @export
shared_var <- function(concept, scope = c("ea", "world", "none")) {
  scope <- match.arg(scope)
  name <- switch(scope,
    ea = paste0("ea_", concept),
    world = paste0("world_", concept),
    none = concept
  )
  if (!is_valid_project_name(name)) {
    cli::cli_abort("{.val {name}} is not a valid project variable name.")
  }
  name
}

#' Validate a variable name against the project naming convention
#'
#' A name is valid if it matches koma's variable grammar AND either:
#' - starts with a two-letter ISO country prefix followed by `_`, or
#' - starts with `ea_` or `world_`, or
#' - contains no underscore-delimited prefix at all (a global, unprefixed
#'   concept such as `"oil_price"`).
#'
#' @param x Character vector of candidate variable names.
#'
#' @return Logical vector, same length as `x`.
#' @export
is_valid_project_name <- function(x) {
  # Lowercase-only form of koma's own variable grammar
  # (`^[a-zA-Z][a-zA-Z0-9_]*$`, see docs/koma-api.md §2.1). Restricting to
  # lowercase is this project's convention, not koma's; the `<iso2>_`,
  # `ea_`/`world_`, and unprefixed forms are all covered by this one
  # pattern, so there is nothing further to branch on.
  grepl("^[a-z][a-z0-9_]*$", x)
}

#' Build a stochastic (behavioural) equation string
#'
#' Assembles a koma `~` equation from a dependent variable and a set of RHS
#' terms, each optionally carrying a lag spec and/or a coefficient prior.
#' See `vignette("equations", package = "koma")` for the grammar.
#'
#' A term named in `lags` is rendered **only** in its lagged form:
#' `stochastic_equation("de_c", c("de_gdp", "de_c"), lags = list(de_c = "1"))`
#' gives `"de_c ~ de_gdp + de_c.L(1)"`, not `"... + de_c + de_c.L(1)"`.
#' That is the usual case for an autoregressive own-term, where the
#' contemporaneous value is the dependent variable and cannot also be a
#' regressor. To include both a contemporaneous term and its lag, name the
#' variable twice in `terms` and give the lag spec for one of them.
#'
#' The intercept is implicit in koma's grammar, so `intercept = TRUE` adds
#' nothing; `intercept = FALSE` appends `- 1`.
#'
#' @param dep Dependent variable name.
#' @param terms Character vector of RHS term names (without lag/prior
#'   decoration).
#' @param lags Optional named list, `terms name -> lag spec string`
#'   (e.g. `list(gdp = "1", consumption = "1:2")`), appended as `.L(...)`.
#' @param priors Optional named list, `term name -> c(mean, variance)`,
#'   rendered as a leading `{mean, variance}` prior on that term.
#' @param error_prior Optional `c(df, scale)` for the trailing error-term
#'   prior. Must be last in the equation, per koma's grammar.
#' @param intercept Logical; if `FALSE`, appends `- 1` to drop the intercept.
#'
#' @return A single equation string, e.g. `"de_c ~ de_gdp + de_c.L(1)"`.
#' @export
stochastic_equation <- function(dep, terms, lags = NULL, priors = NULL,
                                error_prior = NULL, intercept = TRUE) {
  if (length(terms) == 0) {
    cli::cli_abort("{.arg terms} must name at least one regressor.")
  }

  unknown_lags <- setdiff(names(lags), terms)
  if (length(unknown_lags) > 0) {
    cli::cli_abort("{.arg lags} names {.val {unknown_lags}}, which {?is/are} not in {.arg terms}.")
  }
  unknown_priors <- setdiff(names(priors), terms)
  if (length(unknown_priors) > 0) {
    cli::cli_abort("{.arg priors} names {.val {unknown_priors}}, which {?is/are} not in {.arg terms}.")
  }
  if (dep %in% names(priors)) {
    cli::cli_abort("The dependent variable {.val {dep}} cannot carry a prior.")
  }

  rendered <- vapply(terms, function(term) {
    lag_spec <- lags[[term]]
    out <- if (is.null(lag_spec)) term else paste0(term, ".L(", lag_spec, ")")

    prior <- priors[[term]]
    if (!is.null(prior)) {
      out <- paste0("{", prior[1], ", ", prior[2], "} ", out)
    }
    out
  }, character(1))

  rhs <- paste(rendered, collapse = " + ")

  if (!is.null(error_prior)) {
    rhs <- paste0(rhs, " + {", error_prior[1], ", ", error_prior[2], "}")
  }
  if (!intercept) {
    rhs <- paste(rhs, "- 1")
  }

  paste0(dep, " ~ ", rhs)
}

#' Build an identity (accounting) equation string
#'
#' Assembles a koma `==` equation from a dependent variable and a named list
#' of weighted components. Weights may be a fixed number or an injected
#' expression string (e.g. `"n_de_c/n_de_gdp"`), rendered as
#' `(weight)*component`.
#'
#' A negative weight is rendered with a `-` separator (`"... - 0.4*de_m"`),
#' never as `"+ -0.4*de_m"`. This is not cosmetic: koma parses the
#' `+ -0.4*x` form **without error** but stores the identity's weights
#' wrong -- verified against koma 0.3.1, `gdp == 0.6*c + -0.4*i` yields
#' three weights (`0.6`, `character(0)`, `-0.4`) for two components,
#' whereas `gdp == 0.6*c - 0.4*i` correctly yields `c(0.6, -0.4)`. koma
#' performs no identity-consistency check that would catch the corrupted
#' form later, so it has to be avoided here. See `docs/koma-api.md`
#' (gotcha 8) and CLAUDE.md.
#'
#' @param dep Dependent variable name.
#' @param weighted_terms Named list, `component name -> weight`, where each
#'   weight is either `numeric(1)` or a character expression string.
#'   Injected (character) weights are always joined with `+`, since their
#'   sign is not knowable until koma evaluates them against the data.
#'
#' @return A single equation string, e.g.
#'   `"de_gdp == 0.6*de_c + 0.4*de_i"`.
#' @export
identity_equation <- function(dep, weighted_terms) {
  if (length(weighted_terms) == 0) {
    cli::cli_abort("{.arg weighted_terms} must have at least one component.")
  }

  components <- names(weighted_terms)
  rendered <- character(length(components))
  separators <- character(length(components))

  for (i in seq_along(components)) {
    weight <- weighted_terms[[i]]
    if (is.character(weight)) {
      separators[i] <- "+"
      rendered[i] <- paste0("(", weight, ")*", components[i])
    } else {
      separators[i] <- if (weight < 0) "-" else "+"
      # `digits = 15`, not format()'s default 7. koma parses the STRING, while
      # the identity's left-hand side series is built from the numeric weight
      # (chain_weighted_index()), so a weight carrying more than 7 significant
      # digits makes the two disagree -- silently, since koma has no
      # identity-consistency check. Caught on `reu_prices`, whose HICP split is
      # a seven-member average and therefore not a round number: the identity
      # was violated by 1.5e-07, small but real, and amplified by how volatile
      # energy prices are. A weight that IS round (every expenditure share,
      # rounded to 3dp) renders identically either way, so no existing equation
      # string changes.
      rendered[i] <- paste0(format(abs(weight), trim = TRUE, digits = 15), "*", components[i])
    }
  }

  rhs <- rendered[1]
  if (separators[1] == "-") {
    rhs <- paste0("-", rhs)
  }
  for (i in seq_along(components)[-1]) {
    rhs <- paste(rhs, separators[i], rendered[i])
  }

  paste0(dep, " == ", rhs)
}

#' Assemble a koma system from a set of blocks
#'
#' The generic system assembler. A **block** is a self-contained group of
#' equations -- one country, an aggregation identity set, a policy rule --
#' expressed as `list(stochastic = , identities = )` in exactly the shape
#' [stage1_spec()] and [stage2_spec()] return. `build_system()` resolves the
#' blocks, concatenates them, and hands the result to
#' `koma::system_of_equations()`.
#'
#' The point of the block indirection is extension: stage 3 adds a
#' `world_`-scope aggregation block to the list rather than editing the
#' country loop. A block is either
#'
#' - a plain `list(stochastic = , identities = )`, or
#' - a **function** `(countries, weights) -> list(stochastic = , identities = )`,
#'   resolved here so a block can be parameterised by the country set and the
#'   weight matrices without the caller pre-computing it.
#'
#' Two invariants are enforced, both of which koma will not check for you:
#'
#' - **Every stochastic equation is emitted before every identity.** koma
#'   assumes this *positionally*: `model_identification()` loops
#'   `for (j in seq(1, n_endogenous - n_identities))` over columns, and
#'   `estimate_sem()` indexes `y_matrix[, jx]` with the same index. An
#'   identity anywhere else makes koma check and estimate the **wrong
#'   columns** and mislabel the results, silently. [build_system_equations()]
#'   does the ordering; this function guarantees blocks cannot defeat it by
#'   interleaving.
#' - **No variable is defined twice.** Two blocks both claiming `ea_gdp`
#'   would otherwise produce a duplicate left-hand side, which koma rejects
#'   with a message that does not say which block was responsible.
#'
#' @param countries Character vector of ISO-2 country codes, passed to any
#'   block supplied as a function.
#' @param blocks A list of blocks (see above). Named for error messages.
#' @param weights Named list of weight objects, passed to any block supplied
#'   as a function (e.g. `list(foreign_demand = , ea = )`).
#' @param tau Optional named numeric vector of per-equation sampler `tau`
#'   overrides, as in [build_system_equations()].
#'
#' @return A `koma::koma_seq` object.
#' @export
build_system <- function(countries, blocks, weights, tau = NULL) {
  if (length(blocks) == 0) {
    cli::cli_abort("{.arg blocks} is empty; a system needs at least one block.")
  }

  resolved <- lapply(seq_along(blocks), function(i) {
    block <- blocks[[i]]
    out <- if (is.function(block)) block(countries, weights) else block
    if (!is.list(out) || !any(c("stochastic", "identities") %in% names(out))) {
      label <- names(blocks)[i] %||% as.character(i)
      cli::cli_abort(
        "Block {.val {label}} must be a list with {.field stochastic} and/or {.field identities}."
      )
    }
    out
  })
  names(resolved) <- names(blocks)

  # unname() before c(): concatenating a *named* list of lists prefixes every
  # inner name with its outer one ("de.de_gdp"), which silently breaks every
  # downstream lookup by variable name.
  stochastic <- do.call(c, c(list(list()), unname(lapply(resolved, function(b) b$stochastic %||% list()))))
  identities <- do.call(c, c(list(list()), unname(lapply(resolved, function(b) b$identities %||% list()))))

  defined <- c(names(stochastic), names(identities))
  duplicated_lhs <- unique(defined[duplicated(defined)])
  if (length(duplicated_lhs) > 0) {
    owners <- vapply(duplicated_lhs, function(v) {
      paste(names(resolved)[vapply(resolved, function(b) {
        v %in% c(names(b$stochastic), names(b$identities))
      }, logical(1))], collapse = " + ")
    }, character(1))
    cli::cli_abort(c(
      "Two blocks define the same variable.",
      stats::setNames(paste0(duplicated_lhs, " (from ", owners, ")"), rep("x", length(duplicated_lhs)))
    ))
  }

  spec <- list(stochastic = stochastic, identities = identities)
  koma::system_of_equations(
    equations = build_system_equations(spec, tau = tau),
    exogenous_variables = stage2_exogenous_variables(spec)
  )
}
