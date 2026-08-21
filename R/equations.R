# Naming convention and equation-string builders for koma::system_of_equations().
#
# Variable names follow "<iso2>_<concept>", all lowercase (e.g. "de_gdp",
# "us_prices"). Variables shared across countries are prefixed "ea_" (euro
# area aggregate) or "world_" (global aggregate), or left unprefixed when the
# series has no natural aggregation (e.g. "oil_price"). See CLAUDE.md.

#' Build a country-scoped variable name
#'
#' Combines an ISO-2 country code and a concept into the project's
#' `<iso2>_<concept>` naming convention, and validates the result against
#' koma's variable-name grammar (`^[a-zA-Z][a-zA-Z0-9_]*$`).
#'
#' @param iso2 Two-letter lowercase ISO country code, e.g. `"de"`.
#' @param concept Lowercase concept name, e.g. `"gdp"`.
#'
#' @return A single string `"<iso2>_<concept>"`.
#' @export
country_var <- function(iso2, concept) {
  stop("not implemented", call. = FALSE)
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
  stop("not implemented", call. = FALSE)
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
#' @param dep Dependent variable name.
#' @param terms Character vector of RHS term names (without lag/prior
#'   decoration).
#' @param lags Optional named list, `terms name -> lag spec string`
#'   (e.g. `list(gdp = "1", consumption = "1:2")`), appended as `.L(...)`.
#' @param priors Optional named list, `term name -> c(mean, variance)`,
#'   rendered as a leading `{mean, variance}` prior on that term.
#' @param error_prior Optional `c(df, scale)` for the trailing error-term
#'   prior.
#' @param intercept Logical; if `FALSE`, appends `- 1` to drop the intercept.
#'
#' @return A single equation string, e.g. `"de_c ~ de_gdp + de_c.L(1)"`.
#' @export
stochastic_equation <- function(dep, terms, lags = NULL, priors = NULL,
                                error_prior = NULL, intercept = TRUE) {
  stop("not implemented", call. = FALSE)
}

#' Build an identity (accounting) equation string
#'
#' Assembles a koma `==` equation from a dependent variable and a named list
#' of weighted components. Weights may be a fixed number or an injected
#' expression string (e.g. `"n_de_c/n_de_gdp"`), rendered as
#' `(weight)*component`.
#'
#' @param dep Dependent variable name.
#' @param weighted_terms Named list, `component name -> weight`, where each
#'   weight is either `numeric(1)` or a character expression string.
#'
#' @return A single equation string, e.g.
#'   `"de_gdp == 0.6*de_c + 0.4*de_i"`.
#' @export
identity_equation <- function(dep, weighted_terms) {
  stop("not implemented", call. = FALSE)
}
