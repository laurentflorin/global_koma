# koma API reference

A verified map of the [`koma`](https://cran.r-project.org/package=koma) R package —
*Bayesian Simultaneous Equation Models for Forecasting* (Sarferaz, Florin, Scherer; KOF ETH Zürich).

## Provenance

| | |
|---|---|
| Package version | **0.3.1** (CRAN, published 2026-07-29) |
| Verified against | R 4.4.3, source tarball `koma_0.3.1.tar.gz` + a live install |
| Method | Every export below was read in `R/*.R` **and** exercised in R. Probe scripts: `scratch/koma_probes.R`, `scratch/vignette_repro.R`, `scratch/koma_scaling.R` |
| Model reference | Rathke A. and Sarferaz S. (forthcoming), *Bayesian Estimation of Simultaneous Equations Model* |

**Do not use the pkgdown site as a reference.** <https://timothymerlin.github.io/koma/> is
built at **v0.2.2**, two releases behind CRAN. Its reference index documents a `type()`
function that does not exist in 0.3.1 — it was replaced by `filter_by_attribute()`.

**GitHub `main` is ahead of CRAN.** It additionally exports `set_koma_attr_policy()`,
`get_koma_attr_policy()` and `reset_koma_attr_policy()`, which are **not** in the CRAN release.
Everything documented here is CRAN 0.3.1 unless stated otherwise.

`koma` is pure R (`NeedsCompilation: no`). Hard dependencies: `cli`, `doFuture`, `foreach`,
`glue`, `Matrix`, `methods`, `progressr`, `purrr`, `rlang`, `stats`, `tempdisagg`, `utils`.
Optional but load-bearing: `ggplot2` (all diagnostics plots), `plotly` (forecast plots),
`texreg` (default `summary()` output), `future` + `parallelly` (parallelism).

---

## What the model is

A system of `n` endogenous variables split into **stochastic (behavioural) equations** and
**identities**, written in structural form

$$\Gamma\, y_t = B\, x_t + \varepsilon_t$$

where `x_t` stacks the intercept, predetermined (lagged) variables and exogenous variables.
Estimation is a **Metropolis-within-Gibbs sampler run one equation at a time**; identities
contribute fixed (or data-derived) weights to `Γ`, not estimated coefficients.

**koma models growth rates.** Levels are converted to rates internally via the `series_type` /
`method` attributes on each series; forecasts come back as rates and are converted back with
`level()`.

---

## Quick start

```r
library(koma)

equations <- "consumption ~ gdp + consumption.L(1),
investment ~ investment.L(1),
gdp == 0.7*consumption + 0.3*investment"

sys_eq <- system_of_equations(equations, exogenous_variables = character())

dates <- list(
  estimation = list(start = c(1996, 1), end = c(2019, 4)),
  forecast   = list(start = c(2020, 1), end = c(2020, 4))
)

ts_data <- lapply(my_level_series, as_ets, series_type = "level", method = "diff_log")

fit <- estimate(ts_data = ts_data, sys_eq = sys_eq, dates = dates)
fc  <- forecast(fit, dates = dates,
                restrictions = list(gdp = list(horizon = 1:2, value = c(0.5, 0.4))))

rate(fc$mean$gdp)
level(fc$mean$gdp)
```

---

## Reading this document

Everything under a `##` section heading is **public API** — the 25 exported functions plus the
S3 methods listed below. Some other function names appear in the text purely as citations —
`get_default_gibbs_spec()`, `get_quantiles()`, `parse_lags()`, `fill_ragged_edge()`,
`set_restrictions()`, `estimate_sem()`, `forecast_values()` and similar. Those are **internal**
to koma; they are named only to point at where a behaviour is implemented, are not part of the
API, and may change without notice. Reach them, if you must, with `koma:::`.

## Complete export list (25 functions)

```
acf_plot          as_ets       as_list                as_mets      concat
estimate          ets          extract.koma_estimate  filter_by_attribute
forecast          generate_sample_data                hdi          hdr
init_koma_theme   is_ets       is_system_of_equations level        model_evaluation
model_identification           rate                   rebase       running_mean
running_mean_plot system_of_equations                 trace_plot
```

Plus S3 methods dispatched via `print()`, `format()`, `summary()`, `plot()` on the
`koma_seq`, `koma_estimate`, `koma_forecast`, `koma_hdi`, `koma_hdr` classes, and
`ta.koma_ts()` (temporal aggregation, from `tempdisagg`).

Classes: `koma_ts` (a `ts`/`mts` subclass), `koma_seq`, `koma_estimate`, `koma_forecast`,
`koma_hdi` / `koma_estimate_hdi` / `koma_forecast_hdi`, `koma_hdr` / `koma_estimate_hdr` /
`koma_forecast_hdr`, `koma_summary`, `koma_texreg`.

---

## 1. Data handling — the `koma_ts` / "ets" object

Every series handed to `estimate()` must be a `koma_ts` carrying two attributes that tell koma
how to move between levels and growth rates.

```r
ets(data = NA, start = NULL, end = NULL, frequency = NULL,
    deltat = NULL, ts.eps = getOption("ts.eps"), ...)

as_ets(x = stats::ts(), ...)          # attach attributes to an existing ts
is_ets(x)
```

`...` accepts **arbitrary** attributes, which survive `rate()`, `level()`, `window()`,
`lag()` and arithmetic. Two are required by the estimation pipeline:

| Attribute | Valid values | Meaning |
|---|---|---|
| `series_type` | `"level"`, `"rate"` | what the stored numbers are |
| `method` | `"percentage"`, `"diff_log"`, `"none"`, or an R `expression()` | how to convert between the two |

`koma` also manages an internal `anker` attribute (base value + date) so `rate()` → `level()`
round-trips exactly. Users never set it.

```r
rate(x, ...)    # level -> growth; no-op if series_type == "rate"
level(x, ...)   # growth -> level; no-op if series_type == "level"
```

| `method` | `rate()` computes | `level()` inverts with |
|---|---|---|
| `"diff_log"` | `diff(log(x)) * 100` | `exp(cumsum(x/100)) * 100`, rescaled by `anker` |
| `"percentage"` | `(x / lag(x,-1) - 1) * 100` | `cumprod(1 + x/100) * 100`, rescaled by `anker` |
| `"none"` | identity | identity |
| `expression(...)` | the expression, evaluated with `x` in scope | the expression |

Zeros are replaced by `1e-9` before the log/ratio to avoid `-Inf`. Both functions have
`.ts`, `.mts` and `.list` methods, so you can call `rate(ts_data)` on a whole named list.

**Choose `"percentage"` for series that can be negative or cross zero** (net exports, balances);
`diff_log` is wrong there. Interest rates and other series already in percent should be
`series_type = "rate", method = "none"`.

Other helpers:

```r
as_list(x, ...)                                   # mts  -> named list of univariate koma_ts
as_mets(x, ...)                                   # list -> multivariate koma_ts
concat(x, y, ...)                                 # append y after x
rebase(x, start, end, ...)                        # mean over [start, end] set to 100
filter_by_attribute(x, attribute, value, var = NULL, ...)
tempdisagg::ta(x, conversion, to, ...)            # quarterly -> annual etc.
```

`filter_by_attribute()` is the 0.3.1 replacement for the `type()` shown on the pkgdown site.

`ta` is **imported, not re-exported**: koma registers `ta.koma_ts()` so a `koma_ts` keeps its
attributes through aggregation, but you must call it as `tempdisagg::ta()` (or attach that
package).

Verified round-trip:

```
x                <- as_ets(ts(c(100,102,101,105,110), start=c(2020,1), frequency=4),
                           series_type="level", method="diff_log")
rate(x)          #  1.9803 -0.9852  3.8840  4.6520
level(rate(x))   #  100 102 101 105 110      <- exact
rebase(x, c(2020,1), c(2020,2))  #  99.010 100.990 100.000 103.960 108.911
```

---

## 2. Model specification

```r
system_of_equations(equations = vector(), exogenous_variables = vector(), ...)
is_system_of_equations(x)
format(<koma_seq>, ...)
print(<koma_seq>, ...)
```

`equations` is either **one string with equations separated by commas or newlines**, or a
character vector of equations. All whitespace is stripped before parsing.

The returned `koma_seq` is a list of 12:

| Element | Contents |
|---|---|
| `equations` | normalised equation strings (intercept made explicit, lag ranges expanded) |
| `endogenous_variables` | every LHS, in declaration order |
| `stochastic_equations` | the LHS of `~` equations only |
| `identities` | per-identity `equation`, `components`, `weights`, `matrix` |
| `character_gamma_matrix` | `n × n` symbolic `Γ` (`"gamma1_6"`, `"-theta6_4"`, `"1"`, `"0"`) |
| `character_beta_matrix` | `k × n` symbolic `B` |
| `exogenous_variables` | as declared |
| `predetermined_variables` | every distinct `var.L(p)` |
| `total_exogenous_variables` | `c("constant", predetermined, exogenous)` |
| `weight_variables` | series named inside `( )` weight expressions |
| `priors` | per-equation prior list, in equation order |
| `equation_settings` | per-equation `[key = value]` overrides, named by LHS |

### 2.1 Equation grammar

```
<equation>  ::= <lhs> "~"  <rhs>          stochastic (behavioural) equation
              | <lhs> "==" <rhs>          identity / equilibrium condition
<separator> ::= "," | newline             (not inside {} [] or ())
```

| Construct | Where | Example |
|---|---|---|
| `~` | stochastic equation | `consumption ~ gdp + consumption.L(1)` |
| `==` | identity | `gdp == 0.6*consumption + 0.4*investment` |
| `+` `-` | term separators, both sides | `gdp == 0.6*c - 0.4*m` |
| `*` | weight × component, **identities only** | `0.6*consumption` |
| `var.L(p)` | lag | `gdp.L(1)` |
| `var.L(a:b)` | lag range, expanded | `gdp.L(1:4)` → `gdp.L(1)+…+gdp.L(4)` |
| `var.L(a:b,c)` | mixed range + singles | `gdp.L(1:3,5)` |
| `lag(var, p)` | alias for `.L(p)`, ranges allowed | `lag(gdp, 2:3)` |
| `{m, v} term` | coefficient prior (mean, variance) | `{0.4, 0.1} gdp` |
| `{df, s}` | error-term prior, trailing, no variable | `… + {3, 0.001}` |
| `[key = value, …]` | equation-level sampler settings | `… [tau = 1.2]` |
| `(expr)*component` | injected / dynamic identity weight | `(n_cons/n_gdp)*consumption` |
| `constant` or `1` | explicit intercept (implicit by default) | `y ~ 1 + x` |
| `0` or `-1` | drop the intercept | `y ~ x - 1` |

Variable names must match `^[a-zA-Z][a-zA-Z0-9_]*$`.

**That is the entire operator set.** `:` is meaningful only inside a lag spec and `/` only
inside a `( )` weight expression; outside those, `^`, `/`, `:`, `log()`, `I()` and every other
R operator or function is rejected by the validator. See §8 for the exact error each produces.

### 2.2 Identities

Every component on the RHS of an identity needs an explicit weight, either a **number** or a
**parenthesised expression over series in `ts_data`**:

```r
gdp == 0.6*consumption + 0.4*investment                  # fixed numeric weights
gdp == (n_cons/n_gdp)*consumption + (n_inv/n_gdp)*investment   # data-derived weights
```

Injected weights are computed by `get_seq_weights()` → `calculate_eq_weights()`
(`R/weights.R`): each named series is **temporally aggregated to annual** with
`tempdisagg::ta(conversion = "sum")`, **lagged one year**, restricted to
`dates$dynamic_weights`, the expression is evaluated element-wise, and the **last value** is
taken as the weight. Arithmetic inside the parentheses supports `+ - * /`.

Using any injected weight makes **`dates$dynamic_weights = list(start=, end=)` mandatory**.

Identities may:
- reference another identity's LHS (`gdp == 0.7*domestic_demand + …` where `domestic_demand`
  is itself an identity) — **verified**;
- contain lagged terms (`x_level == 1*x + 1*x_level.L(1)`, the error-correction idiom) —
  **verified**;
- carry negative weights (`gdp == 0.6*c - 0.4*m`) — **verified**;
- mix numeric and injected weights in one equation — **verified**.

At least one `~` equation must exist, or `system_of_equations()` aborts.

### 2.3 Priors

Two kinds, both written inline in the equation string. There is no separate `priors =` argument.

```r
consumption ~ {0, 1000} 1 +          # prior on the intercept (mean, variance)
              {0.4, 0.1} gdp +       # on a contemporaneous endogenous regressor
              {0.9, 10} consumption.L(1) +   # on a lagged term
              {0.2, 0.5} service +   # on an exogenous regressor
              {3, 0.001}             # error-term prior (df, scale) — must be LAST
```

Rules (all verified):
- Priors are **per equation and per term**; an equation with any prior is estimated with
  `draw_parameters_j_informative()` instead of `draw_parameters_j()`.
- Coefficient priors are `{mean, variance}`. **Negative means work** (`{-0.5, 0.1}`).
  Whitespace inside the braces is fine.
- The error-term prior is `{df, scale}` and carries no variable name.
- The dependent variable cannot have a prior (hard error).
- Priors in identities are a hard error.
- If the error-term prior is followed by other priors, it is **dropped with a warning**.
  If it is the only prior, it is kept regardless of textual position — see §10.

### 2.4 Equation-level settings

Append `[key = value, ...]` to a **stochastic** equation to override the global Gibbs settings
for that equation only. The content is parsed as `list(...)` and passed to `set_gibbs_spec()`,
so the accepted keys are its arguments: `tau`, `ndraws`, `burnin_ratio`, `nstore`. Unknown keys
are discarded silently. (`tau` and `ndraws` verified directly; the other two follow from the
same call path.)

```r
"consumption ~ constant + gdp + consumption.L(1) [tau = 1.2, ndraws = 5000]"
```

Settings on an identity are a hard error.

---

## 3. Estimation

```r
estimate(ts_data, sys_eq, dates,
         ...,
         options   = list(gibbs = list(), fill = list(method = "mean")),
         estimates = NULL)
```

| Argument | Notes |
|---|---|
| `ts_data` | **named list** of `koma_ts`. Names must match the equation variables exactly. Plain `ts` triggers an interactive prompt — see §10. |
| `sys_eq` | a `koma_seq` |
| `dates` | `list(estimation = list(start=, end=))`, optionally plus `forecast` and `dynamic_weights`, each `list(start=, end=)`. Only `estimation` is validated by `estimate()`; `dynamic_weights` becomes mandatory as soon as any identity uses an injected weight. Dates are `c(year, period)` or a decimal year. |
| `options$gibbs` | `ndraws`, `burnin_ratio`, `nstore`, `tau` |
| `options$fill$method` | `"mean"` (default) or `"median"` — how ragged edges are filled |
| `estimates` | an existing `koma_estimate`; only equations whose regressor set changed are re-estimated |

### Gibbs sampler defaults (internal `get_default_gibbs_spec()`)

| Setting | Default | Meaning |
|---|---|---|
| `ndraws` | `2000` | total sampler iterations |
| `burnin_ratio` | `0.5` | ⇒ `burnin = 1000` |
| `nstore` | `1` | thinning ⇒ `nsave = (ndraws − burnin)/nstore = 1000` |
| `tau` | `1.1` | Metropolis proposal scale |

`estimate()` prints these at the top of every run, then any per-equation deviations.

### What `tau` does

In each iteration the `γ` block (the coefficients on **contemporaneous endogenous** regressors)
is drawn by a random-walk Metropolis step with a Student-*t*(2) proposal:

```
γ_candidate = γ_current + tau * chol(H⁻¹) %*% rt(n_j, df = 2)
```

`H⁻¹` is the inverse Hessian of the concentrated target at its mode, computed once per equation
in `initialize_sampler()`. **`tau` is the only knob on step size**: larger `tau` → larger
proposals → lower acceptance. It affects nothing else — not the posterior, not the priors.

Target band is **20 %–60 %** (`get_default_acceptance_prob()`); anything outside triggers a
warning after estimation. (The `equations` vignette says 30 %–60 %; the code says 20 %–60 %.
The code wins.)

Measured on the Switzerland model (`scratch/koma_scaling.R`, section C2, `ndraws = 1000`):

| `tau` | consumption | imports | interest_rate |
|---|---|---|---|
| 0.5 | 77.9 % | 77.4 % | 79.2 % |
| **1.1** (default) | 57.7 % | 60.8 % | 59.8 % |
| 2 | 46.3 % | 46.2 % | 41.6 % |
| 4 | 25.6 % | 26.4 % | 25.6 % |
| 8 | 13.9 % | 15.4 % | 12.9 % |
| 16 | 5.2 % | 7.7 % | 6.4 % |

**How to tune:** the default sits near the top of the band, so the usual move is to *raise*
`tau`. Roughly, doubling `tau` halves the acceptance rate. Set it per equation
(`[tau = 2]`) rather than globally — a per-equation override provably leaves the other
equations' acceptance rates untouched (`scratch/koma_scaling.R`, section C). Aim for 30–40 %.

**Equations with no contemporaneous endogenous regressor have no Metropolis step at all.**
Their `γ` block is empty, `count_accepted` is set to `NA`, and they are excluded from the
warning. In the Switzerland model that is `investment`, `exports` and `prices`.

### Return value: `koma_estimate`

`list(estimates, sys_eq, ts_data, y_matrix, x_matrix, gibbs_specifications, dates)`.
`estimates` is a list keyed by stochastic-equation name; each element holds `nsave` draws of
`beta_jw`, `theta_jw`, `gamma_jw`, `omega_jw`, `omega_tilde_jw`, plus `count_accepted`.

### Reporting

```r
print(<koma_estimate>, ..., variables = NULL, central_tendency = "mean",
      ci_low = 5, ci_up = 95, digits = 2)

summary(<koma_estimate>, ...)   # variables, central_tendency, ci_low, ci_up,
                                # use_texreg, digits; extra args go to texreg::screenreg()

extract.koma_estimate(model, variables = NULL, central_tendency = "mean",
                      ci_low = 5, ci_up = 95, digits = 2, ...)
```

`summary()` returns a **texreg** object when `texreg` is installed (the default), so coefficients
are reached as `summary(fit, variables = "exports")[["exports"]]@coef`. Pass
`use_texreg = FALSE` for a plain ASCII table and a `koma_summary` list.

### Re-estimating part of a system

Pass a previous fit as `estimates =`. `identify_reestimation_indices()` compares the symbolic
`B` matrices and re-runs only the equations whose regressor set changed. Verified: adding
`investment.L(2)` to one equation of the 6-equation model took **0.37 s** against ~4 s for the
full re-run, and the untouched equations' draws were bit-identical.

### Parallelism

```r
workers <- parallelly::availableCores(omit = 1)
future::plan("future::multisession", workers = workers)   # Windows, macOS
future::plan("future::multicore",    workers = workers)   # Linux
estimate(...)
```

- `estimate()` fans out **one future per stochastic equation** (`R/estimate_sem.R`), so useful
  parallelism is capped at the number of behavioural equations — and in practice at the number
  with a Metropolis step.
- `forecast()` fans out **one future per posterior draw** (`nsave`), so it parallelises far
  better than estimation.
- `estimate_sem()` **hard-aborts on macOS under `multicore`**: Apple's Accelerate BLAS is not
  fork-safe and segfaults inside `eigen()`. Use `multisession` there.
- Futures are seeded (`seed = TRUE`), so results are reproducible and identical to the
  sequential run — verified: acceptance rates matched to 0.1 pp across both modes.

---

## 4. Forecasting and conditioning

```r
forecast(estimates, dates,
         ...,
         restrictions = NULL,
         options = list(approximate = FALSE,
                        probs = NULL,
                        fill = list(method = "mean"),
                        conditional_innov_method = "projection"))
```

| Option | Default | Meaning |
|---|---|---|
| `approximate` | `FALSE` | `FALSE` = simulate `nsave` predictive draws. `TRUE` = a single fast pass from the mean/median of the coefficient draws; returns **no** `quantiles` and **no** `forecasts`, so `hdi()`/`hdr()` and fan charts stop working |
| `probs` | `NULL` → `c(0.05, 0.95)` | quantile levels; incompatible with `approximate = TRUE` |
| `fill$method` | `"mean"` | ragged-edge conditional fill |
| `conditional_innov_method` | `"projection"` | `"projection"` shifts each unconditional innovation draw onto the constraint set; `"eigen"` draws afresh from the singular conditional covariance. Both satisfy the constraints exactly; `projection` preserves more of the unconditional draw |

`dates$current` is always derived as one period before `dates$forecast$start` — do not set it.
Endogenous series must end at or before that period; anything shorter is **conditionally
filled** first, with an interactive confirmation prompt (§10).

### Return value: `koma_forecast`

`list(mean, median, forecasts, quantiles, ts_data, y_matrix, x_matrix)`.
`mean`, `median` and each element of `quantiles` are **named lists of `koma_ts`** covering both
endogenous and exogenous variables. Default quantile names are `q_5` and `q_95`
(the internal `get_quantiles()` is `c(0.05, 0.5, 0.95)`, and `0.5` is dropped because it is the
median).

```r
print(<koma_forecast>, ..., variables = NULL, central_tendency = NULL, digits = 4)
summary(<koma_forecast>, ..., variables = NULL, horizon = NULL, digits = 3)
plot(<koma_forecast>, y = NULL, ...)   # variables, fig, theme, fan, fan_quantiles,
                                       # central_tendency  -> a plotly htmlwidget
```

`plot()` requires `plotly`; `fan = TRUE` builds a fan chart from the quantile draws.
`init_koma_theme()` supplies the default theme and takes nested `index`, `title`, `font`,
`trace_name`, `xaxis`, `yaxis`, `legend`, `color` lists.

### Restrictions (conditional forecasting)

```r
restrictions = list(
  <endogenous_variable> = list(horizon = <integer vector>, value = <numeric vector>),
  ...
)
```

- **Multiple variables and multiple horizons at once: yes.** One row of the constraint matrix
  `R` is built per `(variable, horizon)` pair. Horizons need not be contiguous and different
  variables may use different horizon sets.
- **Restrictions may target identity variables** (e.g. `gdp`), not only behavioural ones.
- Names not among `sys_eq$endogenous_variables` are **dropped with a warning** rather than
  erroring — so a restriction on an exogenous series has no effect, and the only signal is that
  warning. Watch for it.
- `horizon` is 1-based and relative to `dates$forecast$start`.
- To condition on *observed* data (e.g. pin a variable to its actual path), the internal
  `koma:::set_restrictions(ts_data, variables, start, end)` builds the list for you; it is what
  `fill_ragged_edge()` uses.

Verified behaviour (`scratch/vignette_repro.R`):

```r
forecast(fit, dates = dates, restrictions = list(
  prices        = list(horizon = 1:4,     value = c(0.5, 0.4, 0.3, 0.2)),
  interest_rate = list(horizon = c(1, 4), value = c(1.0, 1.5))))

#> prices        0.5   0.4   0.3   0.2       <- all four hit exactly
#> interest_rate 1.000 1.396 1.657 1.500     <- h=1 and h=4 hit, h=2,3 free
```

**Hard values only. Soft conditions are NOT SUPPORTED.** The mechanism is an exact linear
constraint `R v = r` on the stacked reduced-form innovations, solved as
`v_cond = v + Ω Rᵀ (R Ω Rᵀ)⁻¹ (r − R v)`. There is no tolerance, variance, or interval
parameter anywhere in the interface or the implementation, and *every draw* satisfies the
constraint exactly — so a conditioned path has zero forecast uncertainty at the restricted
points. Passing an extra field (e.g. `sd = 0.25`) is silently ignored.

Failure modes:
- **Rank-deficient `R Ω Rᵀ`** (redundant or contradictory restrictions) aborts with
  *"A = R %\*% Omega %\*% t(R) is singular"*. Restricting every endogenous variable of a system
  at the same horizon over-determines it and fails this way — verified.
- **A `horizon` outside `1..H`** aborts. The internal check raises per draw, so what you see is
  the aggregate *"All forecast draws failed. → Likely causes: redundant/incompatible
  restrictions."* — misleading, but the cause is the out-of-range horizon.
- **Ill-conditioning**: if `κ(R Ω Rᵀ) > 1e12` you get a warning, not an error, and results you
  should not trust.

### Out-of-sample evaluation

```r
model_evaluation(sys_eq, variables, horizon, ts_data, dates,
                 ...,
                 evaluate_on_levels = TRUE,
                 options = list(gibbs = list(), summary = "mean", approximate = FALSE),
                 restrictions = NULL)
```

Rolling-origin RMSE: starts at `dates$forecast$start`, adds one period to the in-sample data
each iteration until `start + horizon == dates$forecast$end`. Returns a data frame of RMSE per
variable. Note this **re-estimates the model at every origin**, so cost is
`(number of origins) × estimate()` — budget accordingly.

---

## 5. Diagnostics and posterior summaries

```r
hdi(x, ...)
hdi(<numeric>,        probs = c(0.5, 0.99), ...)
hdi(<koma_estimate>,  variables = NULL, probs = c(0.5, 0.99), include_sigma = FALSE, ...)
hdi(<koma_forecast>,  variables = NULL, probs = c(0.5, 0.99), ...)

hdr(x, ...)
hdr(<numeric>,        probs = c(0.5, 0.99), n_grid = 4096,
                      integration = c("monte_carlo", "grid"),
                      mc_use_observed = FALSE, mc_draws = NULL, mc_quantile_type = 7,
                      bw = "nrd0", adjust = 1, kernel = "gaussian", ...)
hdr(<koma_estimate>,  ... same, plus include_sigma = FALSE)
hdr(<koma_forecast>,  ... same)
```

`hdi()` gives highest-density **intervals** straight from the draws and reports the **median**
as the point estimate. `hdr()` gives highest-density **regions** from a kernel density estimate
— possibly disjoint — and reports the **mode**. `include_sigma = TRUE` adds the equation
variance (`omega`) to the table. Both need predictive draws, so `hdi()`/`hdr()` on a forecast
produced with `approximate = TRUE` errors with a clear message.

```r
trace_plot(x, ...)          # ggplot
acf_plot(x, ...)            # ggplot
running_mean(x, ...)        # data.frame of cumulative means
running_mean_plot(x, ...)   # ggplot
```

All four dispatch on `koma_estimate` and take their arguments through `...`:

| Argument | `trace_plot` | `acf_plot` | `running_mean` | `running_mean_plot` |
|---|:-:|:-:|:-:|:-:|
| `variables` | ✓ | ✓ | ✓ | ✓ |
| `params` (`"beta"`, `"gamma"`, `"omega"`) | ✓ | ✓ | ✓ | ✓ |
| `thin`, `max_draws` | ✓ | ✓ | ✓ | ✓ |
| `grace_draws` | | | ✓ | |
| `max_lag`, `conf_level` | | ✓ | | |
| `interactive`, `facet_ncol`, `scales` | ✓ | ✓ | | ✓ |

`scales` ∈ `{"fixed","free","free_x","free_y"}`; default `"free_y"` except `acf_plot` which
defaults to `"fixed"`. All three plots require `ggplot2`.

```r
model_identification(character_gamma_matrix, character_beta_matrix,
                     identity_weights, call = rlang::caller_env())
```

Checks the classical **order** and **rank** conditions for each stochastic equation. Called
automatically inside `estimate()`; call it directly to validate a `koma_seq` before committing
to a long run. Returns `TRUE` early if the system has no simultaneity at all.

---

## 6. Datasets and simulation

| Dataset | Shape |
|---|---|
| `small_open_economy` | 12 quarterly `ts` for Switzerland, spans 1979 Q4 – 2024 Q4 (varies by series; `gdp` starts 1990 Q2, `world_gdp` 1990 Q4). Levels. |
| `klein` | 15 quarterly US `ts` from FRED, Klein Model I variables |
| `simulated_sem` | `list(ts_data, sys_eq, dates)` — a ready-made 6-equation example used throughout the help pages |

```r
generate_sample_data(sample_size, sample_start, burnin, gamma_matrix, beta_matrix,
                     sigma_matrix, endogenous_variables, exogenous_variables,
                     predetermined_variables)
```

Simulates from a structural system with known parameters — the right tool for recovery checks.

**`klein`'s documentation is out of date.** `?klein` lists `gdp_deflator`, but the object
actually contains `d_gdp`; it also contains `n_capital_stock`, which is undocumented. Actual
names: `gdp, consumption, investment, government, net_exports, n_gdp, n_consumption,
n_investment, n_government, n_profits, n_wages, n_government_wages, n_taxes, n_capital_stock,
d_gdp`.

---

## 7. Vignettes

| Vignette | Topic |
|---|---|
| `vignette("koma")` | getting started |
| `vignette("equations")` | equation syntax reference |
| `vignette("small_open_economy")` | Switzerland small macro model (reproduced in `scratch/vignette_repro.R`) |
| `vignette("klein")` | Klein Model I, injected/dynamic weights |
| `vignette("koma-error-correction")` | ECM via lagged level terms |
| `vignette("koma-extended-timeseries")` | the `koma_ts` object |
| `vignette("koma-hpd")` | HDI/HDR summaries |
| `vignette("koma-diagnostics")` | trace / ACF / running-mean plots |
| `vignette("parallel")` | `future::plan()` usage |

---

## 8. Answers to the specification questions

### Q1. Equation-string grammar

**CONFIRMED.** The full operator set is in §2.1. Beyond `~`, `==` and `.L(n)` there are:
`+`, `-`, `*` (identity weights only), `lag(var, n)`, lag ranges `a:b` and lists `a:b,c`,
`{mean, variance}` coefficient priors, trailing `{df, scale}` error priors, `[key = value]`
equation settings, `(expression)*component` injected weights, and the intercept controls
`constant` / `1` / `0` / `-1`. Equations are separated by commas or newlines.

**Lags of arbitrary order: CONFIRMED.** `y.L(24)` parses and produces `y.L(24)`. No cap exists
in `parse_lag_spec()`. Leads are **NOT SUPPORTED** — `y.L(-1)` errors.

**Identity referencing another identity's LHS: CONFIRMED.** Probe: with
`dd == 0.6*c + 0.4*i, gdp == 0.7*dd + 0.3*c`, the parser builds
`gdp$components$dd = "theta4_3"`, i.e. `dd` enters `Γ` normally. Both the Switzerland and
Klein vignettes rely on this.

**Non-linear terms and interactions: NOT SUPPORTED.** Verified errors:

| Written | Result |
|---|---|
| `y ~ x1*x2` | `Weight must be a number or expression in parentheses` |
| `y ~ x^2` | `Invalid single component variable` |
| `y ~ I(x^2)` | `Invalid single component variable` |
| `y ~ log(x)` | `Invalid single component variable` |
| `y ~ x/z` | `Invalid single component variable` |
| `y ~ 0.5*x` | `Weight must be a number or expression in parentheses` |

**Any non-linearity must be pre-computed in the data** and entered as its own series — the
error-correction vignette does exactly this, adding `log(x)*100` as a separate `series_type =
"level", method = "none"` series.

### Q2. Country-prefixed names, reserved words, length limits

**CONFIRMED — `de_gdp` and `us_prices` parse correctly.**

```
de_gdp ~ us_prices + de_gdp.L(1)
  -> de_gdp~constant+us_prices+de_gdp.L(1)
     endogenous: de_gdp   predetermined: de_gdp.L(1)
```

A three-country system with cross-country regressors and an aggregate identity parses cleanly.

| Name | Result |
|---|---|
| `de_gdp`, `us_prices`, `ea_gdp` | OK |
| `DE_GDP`, `X_1` (mixed case) | OK |
| 200-character name | OK — **no length limit** |
| `de.gdp` (dot) | **ERROR** — dots are reserved for `.L()` |
| `_gdp` (leading underscore) | **ERROR** |
| `2gdp` (leading digit) | **ERROR** |
| `epsilon`, `lag`, `theta1_2`, `gamma1_2`, `beta1_2` | parse OK at the spec level |
| `constant` as a **regressor** | absorbed as the intercept |
| `constant` as an **exogenous series** | **silent trap** — `y ~ constant` with `exogenous_variables = "constant"` parses to `y~constant+`, a malformed trailing `+`. **Never name a series `constant`.** |

Rule: `^[a-zA-Z][a-zA-Z0-9_]*$`. Country prefixes are safe; avoid `constant` and avoid dots.

### Q3. How many equations, and how does estimation scale?

**No hard cap — CONFIRMED.** Nothing in the code bounds `n`. A 24-equation synthetic system
estimated without complaint. The binding constraint is **identification**, not count:
`model_identification()` enforces the order and rank conditions per equation at `estimate()`
time.

**Scaling — measured** (`scratch/koma_scaling.R`, R 4.4.3, 16 logical cores).

> *Benchmark conditions.* The host was not idle — an unrelated process was holding roughly
> 5 cores throughout. Sequential timings are essentially unaffected (they need one core out of
> sixteen), but the **parallel** columns and the measured speed-ups are pessimistic, and the
> 4.6× plateau is partly contention rather than pure task granularity. Treat the sequential
> numbers as solid and the parallel ones as a lower bound. Re-run `scratch/koma_scaling.R` on an
> idle machine for numbers you want to quote.

*In `ndraws` — exactly linear.* Switzerland model, 6 stochastic equations, sequential:

| `ndraws` | elapsed | ms/draw |
|---|---|---|
| 250 | 3.4 s | 13.5 |
| 500 | 4.8 s | 9.7 |
| 1 000 | 9.5 s | 9.5 |
| 2 000 | 18.6 s | 9.3 |
| 4 000 | 37.1 s | 9.3 |

*In equation count — mildly super-linear.* Synthetic recursive systems, `ndraws = 1000`,
T = 180, one contemporaneous endogenous regressor per equation:

| equations | sequential | per equation | multicore (15 workers) | speed-up |
|---|---|---|---|---|
| 3 | 5.6 s | 1.9 s | 3.1 s | 1.8× |
| 6 | 15.2 s | 2.5 s | 4.8 s | 3.2× |
| 12 | 35.7 s | 3.0 s | 7.7 s | 4.7× |
| 24 | 94.9 s | 4.0 s | 20.5 s | 4.6× |

Per-equation cost rises only slowly with system size (1.9 s → 4.0 s while `n` grows 8×, so
roughly `n^0.35` — the `Γ` determinant and the reduced-form solve grow, but the dominant term is
the per-draw work on a fixed `T`). Total sequential cost therefore scales as about
**`n^1.4 · ndraws`**, not `n²`. Parallel speed-up plateaus near 4.6× here; two effects are
confounded — each equation is one indivisible, unequal-length task (a real ceiling), and the
host was ~5 cores busy (an artefact). Expect somewhat better than 4.6× on an idle machine, but
not linear: the longest single equation bounds the whole run.

**Practical budget**, extrapolating from the table: a 40-equation system at the default
`ndraws = 2000` lands around **6 minutes sequentially, ~1.5 minutes on 15 cores**. Treat that as
a floor — these synthetic equations have three regressors each and no priors. Real macro
equations with more regressors, informative priors (which switch to the slower
`draw_parameters_j_informative()` path) or a longer sample will cost more. Forecasting
parallelises far better, fanning out over `nsave = 1000` draws rather than over equations.

### Q4. Priors — how specified, per-equation, and `tau`

**CONFIRMED, per-equation and per-term.** See §2.3 and §3. Priors are written inline in the
equation string; there is no `priors =` argument. `tau` is the Metropolis proposal scale — see
the measured tuning table in §3.

### Q5. Does `estimate()` support parallelism?

**CONFIRMED.** `future::plan()`, one future per stochastic equation. Measured on the
Switzerland model at default settings: `estimate()` 19.4 s sequential → **7.5 s** on 15 workers;
`forecast()` 3.3 s → **1.4 s**. Results are identical in both modes (acceptance rates matched
exactly), because futures are seeded. macOS must use `multisession`. See the benchmark-conditions
note under Q3 — the host was not idle, so these speed-ups are a lower bound.

### Q6. `forecast(restrictions = )` — soft or hard? Multiple variables/horizons?

**Hard values only — soft conditions NOT SUPPORTED.** **Multiple variables × multiple horizons
simultaneously: CONFIRMED.** See §4.

### Q7. IRF, FEVD, historical decomposition, SV, t-errors, mixed frequency

All **NOT SUPPORTED**. An exhaustive case-insensitive search of `R/`, `man/`, `vignettes/`,
`tests/`, `NEWS.md` and `README.md` for `impulse|irf|fevd|variance decomposition|forecast error
variance|historical decomposition|stochastic volatility|student|t-distribut|heavy.tail|
mixed.frequency|nowcast` returns exactly two hits, both code comments describing the Student-*t*
**proposal density** in the Metropolis step (`R/mh_within_gibbs_algorithm.R:196`,
`R/mh_within_gibbs_algorithm_informative.R:228`). The structural errors are Gaussian with an
inverse-Wishart prior on the covariance; there is no volatility process and no fat-tailed
likelihood.

The companion matrix (`construct_companion_matrix()`) and the reduced form
(`construct_reduced_form()`) are both built internally and are exactly what an IRF/FEVD routine
would need — but no such routine is exported, and `Ψ` powers are only formed inside
`forecast_values()` for conditioning. Rolling out impulse responses would mean reimplementing
against unexported internals.

**Mixed frequency: NOT SUPPORTED.** `get_single_frequency()` requires one frequency across all
series. `tempdisagg` is imported only for `ta()` (temporal **aggregation**, e.g. quarterly →
annual) used when computing dynamic identity weights and exposed as `ta.koma_ts()`.
Disaggregation, MIDAS and ragged-frequency nowcasting are absent — though koma *does* handle
**ragged edges** within a single frequency, via `fill_ragged_edge()` and `conditional_fill()`.

### Q8. `as_ets()` / `koma_ts`, `rate()`, `level()`, and all valid values

**CONFIRMED.** See §1. `series_type` ∈ `{"level", "rate"}`; `method` ∈ `{"percentage",
"diff_log", "none"}` or an R `expression()`. Both are validated by `match.arg()`, so anything
else is a hard error.

### Q9. Switzerland vignette reproduction

**CONFIRMED — runs end to end.** `scratch/vignette_repro.R`; see §9.

---

## 9. Measured runtime — Switzerland small macro model

R 4.4.3, koma 0.3.1, 16 cores, defaults (`ndraws = 2000`, `burnin = 1000`, `nsave = 1000`,
`tau = 1.1`), 6 stochastic equations + 2 identities, estimation window 1996 Q1 – 2019 Q4
(96 quarters), forecast 2023 Q1 – Q4 with a 12-quarter conditional fill.

| Step | Sequential | `future::multicore` (15 workers) |
|---|---|---|
| `estimate()` | **19.4 s** | **7.5 s** |
| `forecast()` unconditional | 3.3 s | 1.4 s |
| `forecast()` 1 restriction | 3.7 s | 1.4 s |
| `forecast()` 6 restrictions | 3.8 s | 1.4 s |
| `forecast()` `eigen` method | 3.6 s | 2.1 s |
| whole script | ~35 s | ~15 s |

### Acceptance rates (identical in both modes)

| Equation | contemp. endog. regressors | acceptance |
|---|---|---|
| consumption | 1 | 59.6 % |
| investment | 0 | — (no MH step) |
| exports | 0 | — |
| imports | 1 | **61.0 %** ← flagged |
| prices | 0 | — |
| interest_rate | 1 | 58.7 % |

The default run **emits a warning out of the box**:

```
── ⚠ MCMC Acceptance Probability Warnings ──
• imports: 61.0%
ℹ Some acceptance probabilities are outside the recommended range (20%-60%).
```

`imports` overshoots the band by 1 pp. Adding `[tau = 2]` to that equation brings it to 42.5 %
without touching the others.

Estimated system at the defaults:

```
    consumption ~  0.36 - 0.02 * gdp + 0.10 * consumption.L(1)
     investment ~  0.46 + 0.21 * investment.L(1)
        exports ~ -0.29 + 3.19 * world_gdp - 0.30 * exports.L(1)
        imports ~ -0.07 + 2.68 * domestic_demand - 0.11 * imports.L(1)
         prices ~  0.03 + 0.03 * exchange_rate + 0.01 * oil_price + 0.56 * prices.L(1)
  interest_rate ~ -0.51 + 0.50 * prices + 0.57 * interest_rate_germany - 0.21 * prices.L(1)
```

(CRAN ships no rendered HTML for this vignette, so there is no published coefficient table to
diff against; the check here is that the script runs end to end and that every step of the
vignette — build, estimate, summarise, forecast, condition — produces sensible output.)

---

## 10. Gotchas

1. **`estimate()` blocks on `readline()` if any series is a plain `ts`.**
   `convert_ts_data_to_ets()` (`R/estimate.R:196`) walks an interactive Q&A about `series_type`
   and `method`. In a non-interactive script this consumes stdin and hangs or silently accepts
   defaults. **Always pass `as_ets()` objects.**

2. **`forecast()` prompts on conditional fill** when endogenous series end before
   `dates$forecast$start - 1` (`R/forecast.R:218`). Guarded by `interactive()`, so `Rscript` is
   safe, but an R session or a notebook will stop and ask.

3. **`var[1:4]` is silently swallowed.** `is_valid_var()` accepts the bracket form but
   `parse_lags()` never expands it. Probe: `y ~ x + y[1:4]` parses to `y~constant+x+y` — the
   lags vanish and `y` becomes a contemporaneous regressor on itself. **There is no error and
   no warning.** Only `.L()` and `lag()` create lags.

4. **`y.L(0)` is accepted** and creates a bogus predetermined variable `y.L(0)`. Nothing
   validates the lag order as ≥ 1.

5. **Never name a series `constant`** — see Q2.

6. **A malformed prior `{0.4}` silently yields variance `NA`** rather than erroring.

7. **The "error prior must be last" warning only fires if another prior follows it.**
   `y ~ x + {3,0.001} + y.L(1)` keeps the error prior with no warning, because it is still last
   *in the prior list*. Do not rely on the check.

8. **Identity components need explicit weights.** `gdp == c + i` parses without error, but the
   stored weights come out as the literal string `"character(0)"` instead of numbers — not a
   usable weight. Always write `gdp == 1*c + 1*i`.

9. **The `(nom_agg)*agg == …` LHS-weight form is unreachable.** An internal error message
   suggests it, but `validate_priors()` rejects any dependent variable containing parentheses.

10. **`approximate = TRUE` silently disables the posterior machinery** — no `quantiles`, no
    `forecasts`, so `hdi()`, `hdr()` and `fan = TRUE` all stop working.

11. **Restrictions on non-endogenous names are dropped with a warning**, not an error.

12. **`summary()` returns an S4 texreg object by default**, so coefficients come out via `@coef`
    rather than `$coef`. Use `use_texreg = FALSE` for a list.

13. **Estimation and forecasting mutate global state** (`the$gibbs_sampler`). Settings from one
    `estimate()` call persist into the next unless overridden.

14. **`?klein` is out of date** — see §6.

15. **macOS + `future::multicore` = hard abort.** Use `multisession`.
