#!/usr/bin/env Rscript
# Empirical probes of the koma equation grammar.
#
# Run with:  scratch/Rrun scratch/koma_probes.R
#
# Each probe prints  OK / ERROR  plus the relevant parsed output, so that
# docs/koma-api.md can cite real behaviour rather than a reading of the source.

suppressPackageStartupMessages(library(koma))

cat(R.version.string, "| koma", as.character(packageVersion("koma")), "\n\n")

# ---------------------------------------------------------------- helpers ----
probe <- function(label, equations, exogenous = character(), show = NULL) {
  cat("### ", label, "\n", sep = "")
  cat("    eq: ", gsub("\n", " ", paste(equations, collapse = " | ")), "\n", sep = "")
  res <- tryCatch(
    withCallingHandlers(
      system_of_equations(equations, exogenous),
      warning = function(w) {
        cat("    WARN: ", conditionMessage(w), "\n", sep = "")
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )
  if (inherits(res, "error")) {
    cat("    ERROR: ", gsub("\n", " ", conditionMessage(res)), "\n\n", sep = "")
    return(invisible(NULL))
  }
  cat("    OK\n")
  cat("    parsed:      ", paste(res$equations, collapse = " ; "), "\n", sep = "")
  if (is.null(show) || "endog" %in% show) {
    cat("    endogenous:  ", paste(res$endogenous_variables, collapse = ", "), "\n", sep = "")
  }
  if (is.null(show) || "predet" %in% show) {
    cat("    predetermined: ", paste(res$predetermined_variables, collapse = ", "), "\n", sep = "")
  }
  if (!is.null(show) && "priors" %in% show) {
    cat("    priors:      ", paste(utils::capture.output(str(res$priors)), collapse = "\n                 "), "\n", sep = "")
  }
  if (!is.null(show) && "settings" %in% show) {
    cat("    settings:    ", paste(utils::capture.output(str(res$equation_settings)), collapse = "\n                 "), "\n", sep = "")
  }
  if (!is.null(show) && "identities" %in% show) {
    cat("    identities:  ", paste(utils::capture.output(str(res$identities)), collapse = "\n                 "), "\n", sep = "")
  }
  cat("\n")
  invisible(res)
}

hdr <- function(x) cat("\n", strrep("=", 78), "\n", x, "\n", strrep("=", 78), "\n\n", sep = "")

# ============================================================== 1. NAMES ====
hdr("1. VARIABLE NAMES")

probe("country-prefixed names (de_gdp, us_prices)",
      "de_gdp ~ us_prices + de_gdp.L(1)", "us_prices")

probe("many country prefixes across equations",
      "de_gdp ~ de_gdp.L(1) + us_gdp,
       fr_gdp ~ fr_gdp.L(1) + de_gdp,
       ea_gdp == 0.3*de_gdp + 0.2*fr_gdp",
      "us_gdp")

long_name <- paste0("v", strrep("a", 199))
probe(paste0("200-character name (nchar=", nchar(long_name), ")"),
      paste0(long_name, " ~ x + ", long_name, ".L(1)"), "x")

probe("dot inside name (de.gdp)", "de.gdp ~ x", "x")
probe("leading underscore (_gdp)", "_gdp ~ x", "x")
probe("leading digit (2gdp)", "2gdp ~ x", "x")
probe("uppercase / mixed case (DE_GDP)", "DE_GDP ~ X_1 + DE_GDP.L(1)", "X_1")

hdr("2. RESERVED / COLLIDING NAMES")

probe("variable named 'constant'", "y ~ constant + x", "x")
probe("exogenous variable literally named 'constant'", "y ~ constant", "constant")
probe("variable named 'epsilon' (internal error-prior key)", "y ~ epsilon + y.L(1)", "epsilon")
probe("variable named 'lag'", "y ~ lag + y.L(1)", "lag")
probe("variable named 'theta1_2' (internal weight key)", "y ~ theta1_2", "theta1_2")
probe("variable named 'gamma1_2' / 'beta1_2'", "y ~ gamma1_2 + beta1_2", c("gamma1_2", "beta1_2"))

# ================================================================ 3. LAGS ====
hdr("3. LAG NOTATION")

probe("single lag .L(1)",        "y ~ x + y.L(1)", "x", show = c("predet"))
probe("range .L(1:4)",           "y ~ x + y.L(1:4)", "x", show = c("predet"))
probe("mixed range+single .L(1:3,5)", "y ~ x + y.L(1:3,5)", "x", show = c("predet"))
probe("lag(y, 2)",               "y ~ x + lag(y,2)", "x", show = c("predet"))
probe("lag(y, 2:3)",             "y ~ x + lag(y,2:3)", "x", show = c("predet"))
probe("high order .L(24)",       "y ~ x + y.L(24)", "x", show = c("predet"))
probe("zero lag .L(0)",          "y ~ x + y.L(0)", "x", show = c("predet"))
probe("negative lag / lead .L(-1)", "y ~ x + y.L(-1)", "x", show = c("predet"))
probe("lag on an exogenous variable", "y ~ x.L(1) + y.L(1)", "x", show = c("predet"))
probe("bracket form y[1:4] (validator accepts, parser may not expand)",
      "y ~ x + y[1:4]", "x", show = c("predet"))

# ====================================================== 4. NON-LINEARITY ====
hdr("4. INTERACTIONS AND NON-LINEAR TERMS")

probe("interaction x1*x2",  "y ~ x1*x2", c("x1", "x2"))
probe("square via ^",       "y ~ x^2", "x")
probe("I(x^2)",             "y ~ I(x^2)", "x")
probe("log(x)",             "y ~ log(x)", "x")
probe("numeric coefficient on a regressor in a stochastic eq (0.5*x)",
      "y ~ 0.5*x", "x")
probe("division x/z",       "y ~ x/z", c("x", "z"))

# ================================================== 5. IDENTITY STRUCTURE ====
hdr("5. IDENTITIES")

probe("identity referencing another identity's LHS",
      "c ~ gdp + c.L(1),
       i ~ i.L(1),
       dd == 0.6*c + 0.4*i,
       gdp == 0.7*dd + 0.3*c",
      character(), show = c("endog", "identities"))

probe("identity with a lagged term on the RHS",
      "x ~ w + x.L(1),
       x_level == 1*x + 1*x_level.L(1)",
      "w", show = c("endog", "predet"))

probe("identity with injected (ratio) weights",
      "c ~ gdp + c.L(1),
       i ~ i.L(1),
       gdp == (n_c/n_gdp)*c + (n_i/n_gdp)*i",
      character(), show = c("identities"))

probe("system with no stochastic equation (identity only)",
      "gdp == 0.6*c + 0.4*i", c("c", "i"))

probe("duplicate dependent variable", "y ~ x, y ~ x.L(1)", "x")
probe("duplicate regressor in one equation", "y ~ x + x", "x")
probe("undeclared exogenous variable", "y ~ x + z", "x")
probe("redundant declared exogenous variable", "y ~ x", c("x", "unused"))
probe("exogenous variable that is also endogenous", "y ~ x, x ~ y", character())

# =========================================================== 6. INTERCEPT ====
hdr("6. INTERCEPT CONTROL")

probe("implicit intercept",   "y ~ x", "x")
probe("explicit '1'",         "y ~ 1 + x", "x")
probe("explicit 'constant'",  "y ~ constant + x", "x")
probe("drop via '- 1'",       "y ~ x - 1", "x")
probe("drop via '+ 0'",       "y ~ x + 0", "x")

# ============================================================== 7. PRIORS ====
hdr("7. PRIORS")

probe("coefficient prior on an exogenous regressor",
      "y ~ {0.4,0.1} x + y.L(1)", "x", show = c("priors"))
probe("prior on the intercept, written as 1",
      "y ~ {0,1000} 1 + x", "x", show = c("priors"))
probe("prior on the intercept, written as constant",
      "y ~ {0,1000} constant + x", "x", show = c("priors"))
probe("prior on a lagged term",
      "y ~ x + {0.9,10} y.L(1)", "x", show = c("priors"))
probe("error-term prior last",
      "y ~ x + y.L(1) + {3,0.001}", "x", show = c("priors"))
probe("error-term prior NOT last (expect warning + removal)",
      "y ~ x + {3,0.001} + y.L(1)", "x", show = c("priors"))
probe("NEGATIVE prior mean {-0.5,0.1}",
      "y ~ {-0.5,0.1} x + y.L(1)", "x", show = c("priors"))
probe("prior on a contemporaneous endogenous regressor",
      "c ~ {0.2,0.5} gdp + c.L(1), i ~ i.L(1), gdp == 0.6*c + 0.4*i",
      character(), show = c("priors"))
probe("prior in an IDENTITY equation (expect ignored)",
      "c ~ gdp + c.L(1), i ~ i.L(1), gdp == {0.5,1} 0.6*c + 0.4*i",
      character(), show = c("priors"))
probe("prior on the dependent variable (expect error)",
      "{0.4,0.1} y ~ x", "x", show = c("priors"))
probe("malformed prior {0.4}", "y ~ {0.4} x", "x", show = c("priors"))

# =========================================== 8. EQUATION-LEVEL SETTINGS ====
hdr("8. EQUATION-LEVEL SETTINGS  [key = value]")

probe("tau override on one equation",
      "c ~ gdp + c.L(1) [tau = 1.2], i ~ i.L(1), gdp == 0.6*c + 0.4*i",
      character(), show = c("settings"))
probe("ndraws + tau override",
      "c ~ gdp + c.L(1) [tau = 0.8, ndraws = 500], i ~ i.L(1), gdp == 0.6*c + 0.4*i",
      character(), show = c("settings"))
probe("settings on an identity",
      "c ~ gdp + c.L(1), i ~ i.L(1), gdp == 0.6*c + 0.4*i [tau = 2]",
      character(), show = c("settings"))

# ============================================== 9. SEPARATORS / WHITESPACE ====
hdr("9. SEPARATORS AND WHITESPACE")

probe("character vector instead of one comma-joined string",
      c("y ~ x + y.L(1)", "z ~ z.L(1) + y"), "x")
probe("newline separated, heavy whitespace",
      "y   ~   x   +   y.L( 1 )\n z ~ z.L(1)", "x")
probe("trailing comma / empty equation", "y ~ x,,", "x")
probe("empty system", character())
probe("equation with neither ~ nor ==", "y + x", character())

# ================================================ 10. FOLLOW-UP EDGE CASES ====
hdr("10. FOLLOW-UP EDGE CASES")

probe("error prior not last, WITH another prior after it (expect warning)",
      "y ~ {3,0.001} + {0.4,0.1} x + y.L(1)", "x", show = c("priors"))
probe("two error priors",
      "y ~ x + {3,0.001} + {4,0.002}", "x", show = c("priors"))
probe("prior with spaces inside braces",
      "y ~ { 0.4 , 0.1 } x", "x", show = c("priors"))
probe("prior variance of zero", "y ~ {0.4,0} x", "x", show = c("priors"))

probe("identity with a NEGATIVE numeric weight",
      "c ~ gdp + c.L(1), m ~ m.L(1), gdp == 0.6*c - 0.4*m",
      character(), show = c("identities"))
probe("identity weight given as a bare number without '*'",
      "c ~ gdp + c.L(1), i ~ i.L(1), gdp == c + i",
      character(), show = c("identities"))
probe("identity mixing numeric and injected weights",
      "c ~ gdp + c.L(1), i ~ i.L(1), gdp == 0.6*c + (n_i/n_gdp)*i",
      character(), show = c("identities"))
probe("identity with an lhs weight, (nom_agg)*agg == ...",
      "c ~ gdp + c.L(1), i ~ i.L(1), (n_gdp)*gdp == (n_c)*c + (n_i)*i",
      character(), show = c("endog", "identities"))

cat("\nAll probes finished.\n")
