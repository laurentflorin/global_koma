# CLAUDE.md

Guidance for working in this repository — a multi-country Bayesian macro
model built on the [`koma`](docs/koma-api.md) package.

## Repo map

```
R/                     package code (roxygen-documented, exported via NAMESPACE)
  data_fred.R             FRED fetch + local cache (FRED_API_KEY -- see below)
  data_eamdqd.R           Euro Area Monthly/Quarterly Database fetch + code mapping
  panel_build.R           combine raw sources into named koma_ts panels
  weights.R               country aggregation weights (GDP/trade) + weighted identities
  stage1_models.R         per-country satellite models
  stage2_system.R         joint multi-country koma system (stage 1 equations + ea_/world_ identities)
  stage3_blocks.R         post-estimation regional/global aggregation blocks
  equations.R             naming convention + koma equation-string builders
  diagnostics.R           MCMC diagnostics wrappers, applied across countries
  scoring.R               out-of-sample RMSE scoring + leaderboards
tests/testthat/         one test-<name>.R per R/<name>.R file
data/raw/               git-ignored: raw downloaded series
data/cache/             git-ignored: cached API responses
docs/
  koma-api.md              verified reference for the koma package's API
  methodology.qmd          this project's modelling methodology (stub)
reports/                rendered output (git-ignored)
_targets.R              the targets pipeline: data -> panel -> stage 1 -> stage 2 -> stage 3
scratch/                ad hoc exploration, not part of the pipeline or package
```

This is both an R package (`DESCRIPTION`, `NAMESPACE`, roxygen docs in
`R/`, `Config/testthat/edition: 3`) and a `targets` pipeline. `_targets.R`
calls `devtools::load_all(".")` to use the package's own functions without
requiring `R CMD INSTALL`.

The three-stage structure:

- **Stage 1** (`stage1_models.R`): each country is estimated as its own
  small `koma` system, taking shared/world variables as exogenous.
- **Stage 2** (`stage2_system.R`): every country's stage-1 equations plus
  the `ea_`/`world_` aggregation identities are combined into *one*
  `koma::system_of_equations()` call, so cross-country simultaneity is
  estimated jointly rather than country-by-country. Can be warm-started
  from stage-1 fits via `koma::estimate(..., estimates = )`.
- **Stage 3** (`stage3_blocks.R`): post-estimation reporting aggregates —
  distinct from the stage-2 identities, which are enforced *during*
  estimation.

## Variable naming convention

All variable names are **lowercase** and follow one of three forms:

| Form | Meaning | Example |
|---|---|---|
| `<iso2>_<concept>` | a country-level series | `de_gdp`, `us_prices` |
| `ea_<concept>` or `world_<concept>` | a series aggregated across countries | `ea_gdp`, `world_gdp` |
| `<concept>` (unprefixed) | a global series with no natural aggregation | `oil_price` |

`iso2` is always the two-letter lowercase ISO country code. `concept` is a
short lowercase name with no further structure (e.g. `gdp`, `prices`,
`interest_rate`) — it is **not** itself prefixed or suffixed; the
`<iso2>_`/`ea_`/`world_` prefix is the only structure in a variable name,
matching what `koma`'s equation grammar accepts
(`^[a-zA-Z][a-zA-Z0-9_]*$` — see `docs/koma-api.md` §2.1). Never name a
series `constant`: `koma` reserves that word for the equation intercept.

Use [`country_var()`][R/equations.R], [`shared_var()`][R/equations.R], and
[`is_valid_project_name()`][R/equations.R] rather than pasting strings by
hand — they encode and validate this convention.

## Data transformation policy: everything stays in levels

**Every data-ingestion function in this project (`data_fred.R`,
`data_eamdqd.R`, `panel_build.R`) must return series in levels.** Do not
apply growth-rate, log-difference, or any other stationarity transform
while fetching or cleaning data, even when the upstream source's own
reference pipeline would normally apply one at this stage.

The reason: `koma_ts`/`as_ets()` (see `docs/koma-api.md` §1 and §8) does
its own level-to-rate conversion via its `series_type`/`method` attributes
(e.g. `as_ets(x, series_type = "level", method = "diff_log")`), applied
once, at the point a series is handed to `koma::estimate()`/`forecast()`.
If a series arrives pre-transformed, that conversion silently
double-transforms it (e.g. differencing an already-differenced series) —
there is no check in `koma` that would catch this, since it has no way to
know the data was already transformed upstream. **Never call `rate()` or
otherwise difference/log-transform a series in this repo's own ingestion
code; leave that entirely to `as_ets(..., method = )` at the point of
estimation.**

This specifically means, for the EA-MD/QD port in `data_eamdqd.R`:

- `eamdqd_panel()` defaults to **`transform = FALSE`** and returns levels.
  Leave it that way for anything feeding the koma model.
- The upstream `EA_transform()` / `TR1`/`TR2`/`TR3` (heavy/light/BLT)
  transformation codes **are** ported, and are reachable via
  `transform = TRUE`. That exists so the upstream pipeline can be
  reproduced exactly (e.g. to check our numbers against the published
  dataset), **not** because our own model pipeline should use it.
- The guard against double-transformation is encoded in the `koma_ts`
  attributes that `eamdqd_panel()` sets, and it is the whole reason the
  argument exists:

  | | `series_type` | `method` |
  |---|---|---|
  | `transform = FALSE` (default) | `"level"` | from the series' `TR` code — koma transforms later |
  | `transform = TRUE` | `"rate"` | `"none"` — already transformed, koma must leave it alone |

  So a `transform = TRUE` panel is still safe to hand to `koma`, because
  `method = "none"` tells `rate()` not to touch it. What is **not** safe is
  transforming a series yourself and then labelling it `method =
  "diff_log"`.
- Quarterly aggregation of monthly series applies in both modes — it is a
  frequency change, not a stationarity transform.
- **Outlier treatment and EM imputation run only when `transform = TRUE`.**
  Both assume stationary data: a PCA factor model has no stationarity to
  work with on levels, and an "outlier is >10 IQRs from the median" rule is
  meaningless for a trending series, where early and late observations are
  legitimately far from the sample median. With `transform = FALSE` the
  levels panel is returned unbalanced, with its `NA`s intact — which is
  fine, because koma fills ragged edges itself (`fill_ragged_edge()` /
  `conditional_fill()`, see `docs/koma-api.md` §4 and §10).
- `eamdqd_variable_map()` maps each EA-MD/QD series to a project variable
  with `series_type = "level"` and a `method` derived from its `TR` code
  (`2 -> "diff_log"`, `4 -> "none"`, ...). Codes with no koma equivalent
  (`3`, `5`, `6` — second differences and plain first differences) fall
  back to `"none"` **with a warning**: those need a per-series judgement
  call, and silently picking one would be exactly the kind of hidden
  decision this section exists to prevent.

## Harmonised panel and trade weights (`panel_build.R`, `weights.R`)

- **Country codes are not the same string across sources.** This project's
  own convention is the true ISO-2 code (Greece = `"gr"`), but EA-MD/QD and
  Eurostat both use the EU statistical convention `"EL"`, while the ECB's
  own dataflows use `"GR"` (which happens to match ISO). `panel_build.R`'s
  `iso2_to_eamdqd`/`iso2_to_ecb` crosswalks exist specifically for this; get
  it wrong and Greek series come back silently empty rather than erroring.
- **`FM.D.U2.EUR.4F.KR.MRR_FR.LEV` (the ECB main refinancing rate) is a
  step function, not a daily-observed series**, despite its `FREQ = D`
  label — the ECB only records an observation when the rate *changes*, so
  multi-year gaps in the raw series are normal. Grouping raw observations
  by calendar month and handing that straight to `ts(..., frequency = 12)`
  silently compresses those gaps and misaligns every later quarter's
  aggregation with real calendar time. `ecb_quarterly_series()` forward-
  fills the step series onto a complete daily grid before aggregating —
  apply the same pattern to any other ECB step-function series (e.g. other
  `FM` key rates) rather than aggregating raw SDMX observations directly.
- **The ECB WTS dataflow's `CURRENCY_TRANS` dimension is not just a
  currency label for an aggregate reporter.** For a genuine bilateral
  country pair (e.g. DE-FR) its variants agree to 3 decimal places and
  picking any one is fine. For the euro-area aggregate reporter (`"I9"`)
  the variants are genuinely different partner-group definitions — verified
  `WTS.A.I9.GB...O.TMS.F` returns 0.205/0.130/0.104 for 2021 depending on
  variant, and China/Poland are only defined under the broader variants at
  all. Always pick the lexicographically **last** `CURRENCY_TRANS` (the
  broadest group) when querying an aggregate reporter — see
  `ecb_trade_weight()`'s doc comment for the full reasoning.
- **The ECB has no US-reporter series in WTS** (no USD-denominated
  dataflow), so `W_trade["us", ]` is built from a documented approximation
  — the reciprocal of each EA country's own weight on the US, renormalised
  — flagged with `cli::cli_warn()` at build time, not silently substituted.
  Ireland's cell in that row is additionally flagged as unreliable: its own
  ECB weight-on-US is inflated by the same multinational/tax-redomiciliation
  distortion behind the 2015 Irish GDP break (`data_eamdqd.R`).
- Eurostat (`eurostat` package) and ECB (`ecb` package) responses are
  cached to `data/cache/eurostat/` and `data/cache/ecb/` respectively
  (git-ignored, same as `data/cache/fred/` and `data/cache/eamdqd/`) —
  both APIs are slow enough (single-digit minutes for a full trade-weight
  matrix, uncached) that iterating without the cache is impractical.
- `eurostat` itself exports a data object literally named `ea_countries`.
  Do not write `[ea_countries]`/`[modelled_countries]` roxygen markdown
  links for this project's own (undocumented, internal) constants of the
  same name — they silently resolve to `eurostat::ea_countries` instead of
  erroring. Use plain `` `ea_countries` `` code spans.

## Writing koma equations and panels (`equations.R`, `stage1_models.R`)

- **Never emit `+ -0.4*x` in an identity — always `- 0.4*x`.** koma parses
  the first form *without any error* but stores the identity's weights
  wrong: verified against koma 0.3.1, `gdp == 0.6*c + -0.4*i` yields three
  weights (`0.6`, `character(0)`, `-0.4`) for two components, whereas
  `gdp == 0.6*c - 0.4*i` correctly yields `c(0.6, -0.4)`. koma has **no
  identity-consistency check** that would catch the corrupted form later,
  so it silently mis-specifies the model. `identity_equation()` handles
  this; don't paste identity strings by hand.
- **Rate series are `series_type = "rate", method = "none"`**; levels and
  indices are `series_type = "level"` with a `method` koma applies itself.
  `concept_series_type` in `panel_build.R` is the lookup. Tagging a rate
  as `"level"` is numerically inert during estimation but misdescribes the
  series and misleads `level()` when inverting a forecast.
- **koma requires a uniform attribute set across `ts_data`.** `as_mets()`
  aborts with "Provide the same attributes for each series in your list"
  if, say, EA-MD/QD series carry `eamdqd_code` and FRED ones don't. Run
  `harmonise_panel_attrs()` before `koma::estimate()`.
- **koma cannot estimate on an internal `NA`.** It fills *ragged edges*
  (leading/trailing) itself, but a hole in the middle fails with an opaque
  "time series contains internal NAs" from inside `level()`. Use
  `internal_gaps()` to find them and `fill_internal_gaps()` to
  interpolate — it warns, naming every series and period, because an
  interpolated observation is invented data. Exactly one series in the
  current vintage needs it: `gr_long_rate` at 2015Q3, when Greek capital
  controls shut the bond market and no Maastricht rate was published.
- **Conditional fill is automatic, not a function call.**
  `fill_ragged_edge()`/`conditional_fill()` are koma-internal. The idiom
  is: set `dates$forecast$start` well after `dates$estimation$end`, window
  the **endogenous** series to the estimation end, and leave the exogenous
  ones at full length — koma derives `dates$current` and fills the gap.
  This project uses 2019Q4 / 2023Q1 so 2020–2022 is filled rather than
  estimated, which is how COVID is neutralised. Do **not** `align_panel()`
  to the estimation window before stage 1; that would truncate the
  exogenous series the fill conditions on.
- **Acceptance rates: the band is 20–60%**, from
  `koma:::get_default_acceptance_prob()`. koma's own `equations` vignette
  prose says 30–60% — the code is authoritative. There is no accessor:
  read `mean(fit$estimates[[eq]]$count_accepted, na.rm = TRUE)`.
  Equations with no contemporaneous endogenous regressor have no
  Metropolis step, `count_accepted` is `NA`, and they must never be
  flagged (`check_acceptance_rates()` handles this).
- **koma parallelises per equation, not per country.** It has no `cores`
  argument — the caller sets `future::plan()`. For the 11 stage-1 models
  the wider axis is countries, so `fit_stage1_all()` sets the plan itself
  and spreads countries across workers with `future.apply`; `future`
  makes the nested inner level sequential. macOS must use `multisession`
  (Accelerate BLAS is not fork-safe and segfaults in `eigen()`).
- `targets` schedules any target whose dependencies are met, so
  `tar_make()` does **not** fail in stage order — a stage-3 stub with no
  upstream dependency errors before stage 1 runs. Use
  `tar_make(names = "stage1_diagnostics")` to exercise the implemented
  part of the pipeline.

## Linking countries into one system (`stage2_system.R`)

- **Declare every stochastic equation before every identity.** koma assumes
  this *positionally* in two places: `model_identification()` loops
  `for (j in seq(1, n_endogenous - n_identities))` over columns, and
  `estimate_sem()` indexes `y_matrix[, jx]` by the same index. An identity
  declared anywhere else makes koma check and estimate the **wrong columns**
  and mislabel the results, with **no error**. This is undocumented — every
  example koma ships happens to obey it. `build_system_equations()` enforces
  the ordering; do not hand-assemble the equation vector.
- **Every endogenous variable needs a series in `ts_data`, identities
  included.** `koma::estimate()` aborts with "The following series are
  missing in `ts_data`" — but only at estimation time, after the expensive
  setup. Stage 2's `<iso2>_foreign_demand`, `ea_gdp` and `ea_prices` have no
  observed counterpart, so `build_stage2_panel()` constructs them.
  `stage2_preflight()` catches the whole class of problem up front.
- **Build an identity's LHS series in rate space, not level space.** koma
  estimates on growth rates, so an identity is a statement about
  `diff(log())`, and `log(0.6a + 0.4b) != 0.6*log(a) + 0.4*log(b)`. Use
  `chain_weighted_index()` (weighted average of growth rates, integrated
  back to an index), **not** `apply_weights()` (weighted sum of levels).
  For `de_foreign_demand` the two differ by up to **13.7 percentage points**
  of quarterly growth; the chained version reproduces its identity to
  8.5e-14. koma has no identity-consistency check, so a level-space
  construction fails silently. This does not apply to `<iso2>_gdp`, which is
  independently observed and whose identity legitimately holds only up to
  the statistical discrepancy.
- **Never warm-start a linkage change via `estimate(estimates = )`.**
  `identify_reestimation_indices()` compares only the symbolic **`B`**
  matrices; the `Gamma` block is never compared. Every cross-country term is
  a *contemporaneous endogenous* regressor, i.e. `Gamma`-only, so a warm
  start silently keeps stale draws of the wrong dimension for exactly the
  equations that changed. `fit_stage2()` therefore always estimates cold.
- **`model_identification()` consumes RNG** — it fills free coefficients
  with `rnorm` draws, so a single pass proves little. `stage2_preflight()`
  repeats it across seeds. For the same reason two runs of the same system
  give slightly different acceptance rates.
- **Exogenous is an exact set.** `validate_completeness()` aborts on an
  undeclared variable *and* on a declared-but-unused one, so a superset is
  as fatal as a subset. `stage2_exogenous_variables()` derives it from the
  spec. Note `<iso2>_government` is exogenous but easy to forget: it has no
  equation yet appears in the domestic-demand identity.
- **`k < T` is a hard constraint, and `k` grows fast.** koma projects every
  equation on the **full** `k`-column `x_matrix` on every draw
  (`construct_pi_hat_0`, `construct_theta_hat_j` both do
  `Matrix::solve(t(x) %*% x)`) and draws `Omega` from `riwish(T - k, .)`.
  With `k = 1 + (one lag per stochastic equation) + (exogenous)`, an
  eleven-country system reaches `k = 83`; on the stage-1/2a window `T = 78`,
  and estimation fails twice over — "system is computationally singular" plus
  "v must be >= dimension of S in rwish()". Stage 2b buys the room with a
  longer window (2024Q4, `T = 98`), dropping `<iso2>_government`, and COVID
  dummies, landing at `k = 76`, `df = 22`. `stage2_preflight(dates = )`
  checks this *before* koma builds anything; always pass `dates`.
- **Pin BLAS threads when using `workers`.** This R links a **pthread**
  OpenBLAS, so every process claims all cores for its own thread pool and
  `future::multicore` forks inherit that. Eight workers produced a **load
  average of 116 on 16 cores** and a >30x slowdown (a 25-equation step did
  not finish in twenty minutes, against 41.5s fixed). Run with
  `OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1` **in the environment** — not
  `Sys.setenv()`, because OpenBLAS sizes its pool at library init. koma's
  matrices are at most `k x k`, far too small to benefit from threading.
  `warn_if_blas_threaded()` warns when this is missed.
- **Making a variable endogenous does not give it a transmission channel.**
  Promoting `ea_policy_rate` to its own Taylor rule left it reaching only
  the two `long_rate` equations, which are terminal — the stage-1 "a rate
  rise cannot move GDP" finding survived unchanged. It is `<iso2>_long_rate`
  appearing in the *investment* equation that actually closes the loop.
  Check reachability on `sys_eq$character_gamma_matrix` rather than assuming.

## Stage 3a: labour market and disaggregated prices (`stage2_system.R`)

`labour_block()` gives one country seven behavioural equations and five
identities on top of its `country_block()`, adding a wage–price loop and a
price-competitiveness channel. It is **Germany-only and cannot be rolled
out**: the full block costs six net predetermined columns per country, so
all eleven would need `k = 142` against `T = 98`. Phase A (`de_foreign_prices`
exogenous) is `k = 83, df = 15`; phase B (partners get
`export_price_block()`, foreign prices become an identity) is `k = 92,
df = 6`. See `reports/stage3a_labour_prices.qmd`.

- **EA-MD/QD's `WS` is a wage *bill*, not a wage rate**, and every stage-3a
  identity is wrong if you use it directly: `real_income == wages +
  employment - prices` double-counts employment, and a wage Phillips curve
  on the bill mostly re-estimates Okun's law. `derived_wage_rate()` divides
  by `TEMP`. With that, `ulc == wages - productivity` telescopes through
  `productivity == gdp - employment` to `WS - GDP` — nominal wage bill over
  real output, the textbook ULC. This matters because EA-MD/QD has **no
  whole-economy ULC series at all**, only seven sectoral ones.
- **HICP core + energy does not partition the basket; non-energy + energy
  does.** Core (`HICPNEF`/`TOT_X_NRG_FOOD`) excludes energy *and* food, so
  core plus energy is ~81% of the German basket and the rest would have to
  be renormalised away, silently attributing food inflation to core. Verified
  against `prc_hicp_inw`: `NRG + TOT_X_NRG == CP00 == 1000` exactly, every
  year 1996–2025. `hicp_weights()` returns 0.891/0.109 for Germany — not the
  0.85/0.15 one would guess. Weights are re-based annually, so the fixed
  identity weight is an approximation; `max_deviation` reports its size.
- **`align_panel()` takes the *earliest* end across the whole panel**, so one
  short series silently truncates every other — including the exogenous ones
  that must reach past the forecast start, which shortens the forecast
  horizon rather than erroring. Eurostat publishes `nonenergy_prices` a
  quarter behind EA-MD/QD, which would have pulled a 2026Q1 panel back to
  2025Q4. Pass an explicit `end` with `extend = TRUE` to pad instead.
- **`de_prices` becomes an identity but is also independently observed.**
  Build the LHS with `chain_weighted_index()` from the sub-indices, never
  from observed headline HICP: observed HICP satisfies a *fixed*-weight
  identity only approximately, and koma has no identity-consistency check,
  so it would enforce a false identity silently. Same reasoning as
  `<iso2>_foreign_demand`. Note `chain_weighted_index()` sums
  `diff(log(x)) * w` **without renormalising**, so ±1 weights give exact
  differences — which is what makes the productivity/ULC/real-income
  identities reproduce to ~1e-13.
- **Acceptance rates are not a per-equation property in a simultaneous
  system.** Adding the stage-3a block pushed 26 *pre-existing* equations
  just over 60%, because the sampler draws a system-wide residual
  covariance. Reporting those as stage-3a failures would misattribute them;
  re-tune with `tune_tau_system()` (the system-level analogue of
  `tune_tau()`, same doubling rule) before comparing against a tuned
  baseline.
- **`koma::rate()` already returns a correctly-dated `ts`** — it drops the
  first observation and any trailing `NA`. Re-dating its result by hand
  (`ts(as.numeric(rate(x)), end = end(x))`) shifts series that have trailing
  NAs by a quarter and makes an exact identity look broken. Compare
  identities by `cbind()`-ing the objects `rate()` returns.
- **`paste0(character(0), "_x")` is `"_x"`, not `character(0)`.** Building a
  named block list with `setNames(lapply(ccs, ...), paste0(ccs, suffix))`
  therefore aborts when `ccs` is empty. Guard the empty case.
- **A country's `foreign_prices` index drops the `row_gdp` residual and
  renormalises**, because there is no rest-of-world export-price series. For
  Germany that reallocates **0.572** — over half the trade weight — so the
  index assumes the unmodelled half of the world prices like the modelled
  half. `foreign_price_weights()` records the dropped weight in a
  `row_weight_dropped` attribute; report it rather than treating the index
  as complete.
- **An equation with no contemporaneous endogenous regressor gets no
  Metropolis step**, so `count_accepted` is `NA` and it must never be
  flagged. In phase A that is `<iso2>_energy_prices` *and*
  `<iso2>_import_prices` (its only non-exogenous regressor,
  `foreign_prices`, is still exogenous). Phase B gives `import_prices` a
  Metropolis step for the first time.
- **`<iso2>_imports` deliberately carries no price term, and adding one back
  is a known dead end.** Four specifications were estimated: no term
  (stage 2b), contemporaneous `import_prices`, contemporaneous plus a
  domestic-price counterpart, and lagged. The contemporaneous term is
  wrong-signed *and* collapses the domestic-demand elasticity from 0.38 to
  0.08 — the import deflator is the only proxy in that equation for a global
  impulse that also drives volumes, since `foreign_demand` sits on the export
  side. Lagging it restores the elasticity exactly (0.386 vs 0.382) but
  leaves a price coefficient spanning zero. Import-price pass-through belongs
  in `<iso2>_nonenergy_prices`, where it is correctly signed; it does not
  belong in the volume equation.
- **A wrong sign is often not the first symptom — a neighbouring coefficient
  moving is.** Three of those four specifications gave a sign check a clean
  verdict while the demand elasticity next to it was being gutted.
  `sign_checks()` looks at one coefficient at a time and cannot see that;
  comparing specifications can. When a new regressor is added to an existing
  equation, check what happened to the coefficients that were already there.
- **A bad specification in one equation can masquerade as a systemic
  problem.** Estimated against the contaminated imports equation above,
  phase B looked catastrophic — three sign checks lost, including one in a
  stage-2b equation it never touched. Re-estimated after dropping the
  offending term, phase B loses only one marginal sign and that flip does not
  occur. The real `df = 6` penalty is a ~2x widening of every credible
  interval and a ~1.5% share of wage-price loop draws turning explosive:
  expensive, but not the collapse the first run implied. Fix known
  specification errors *before* drawing conclusions about the estimator.

## Stage 3b: external, fiscal and financial blocks (`stage2_system.R`)

`external_block()`, `fiscal_block()` and `financial_block()` extend a
labour-block country. Added cumulatively to Germany: `k` 83 -> 84 -> 88 -> 90
against `T = 98`, so `df` 15 -> 14 -> 10 -> **8**. See
`reports/stage3b_external_fiscal_financial.qmd`.

- **A lagged term in an identity works, and costs a column of `k`.** koma
  supports the stock-flow accumulation idiom (`de_govdebt == 1*de_govdebt.L(1)
  + 1*de_netborrowing`) -- its own Klein vignette ships one. But
  `stage2_exogenous_variables()` used to declare `de_govdebt.L(1)` exogenous,
  which koma rejects as "Redundant exogenous variables detected" because it
  strips the suffix on its side. It now strips `.L(...)` too.
- **Never write an identity term without an explicit weight.** `b == b.L(1) +
  d` parses but stores `character(0)` weights -- the same silent corruption as
  `+ -0.4*x`. `identity_equation()` always emits `1*x`; hand-written strings do
  not.
- **No lagged endogenous name may be a string prefix of another.** koma's
  `construct_phi()` prefix-matches with an unanchored `grepl("^name", ...)`, so
  with `de_debt.L(1)` and `de_debt_ratio.L(1)` both present the shorter name
  matches both and the second pass silently overwrites the first's companion-
  matrix entry -- wrong forecasts, no error. Hence `de_govdebt` /
  `de_netborrowing`. `stage2_preflight()` now checks this.
- **koma's injected weights are NOT time-varying.** `(w)*x.L(1)` parses and `w`
  costs no `k`, but `weights.R` annualises the series, lags it a year and keeps
  the **last value** -- one scalar for the whole sample and forecast. A debt
  snowball factor `(1+i)/(1+g)` cannot be expressed this way. Derive the flow
  so the carry weight is exactly 1 instead.
- **A signed series tagged `level`/`diff_log` fails misleadingly.** `log()` of a
  negative gives `NaN`, which koma reports as `"time series contains internal
  NAs"` -- pointing at gaps, not at the sign. An exact zero is silently
  replaced by `1e-9`, giving a ~2000% growth rate with no warning. Deficits,
  balances and net-lending figures must be ratios tagged `rate`/`none`.
- **The fiscal block cannot include revenue and expenditure.** German
  `gov_10q_ggnfa` begins 2002Q1 and `T` is system-wide, so pulling them in
  shortens *every* equation's window from 98 to 90 while `k` rises -- `df = 0`.
  Build on `gov_10q_ggdebt` (`GD`, `PC_GDP`, clean from 2000Q1) instead, and
  note that the derived `netborrowing` is the change in the debt ratio, not the
  headline deficit.
- **An equation near the identification limit lets its own lag eat everything.**
  `de_long_rate` held an own lag of 0.92-0.94 across three systems, then crossed
  to 1.004 -- explosive -- the moment `de_govdebt` became its fifth regressor at
  `df = 8`, and its policy-rate loading flipped negative at the same instant.
  The debt-to-spread coefficient came out at -0.006. When a coefficient goes
  null, check whether the equation's own lag went to a unit root and absorbed
  it; adding a regressor to an already-full equation can cost more than it buys.
- **A quiet loop is not necessarily a stable one.** `loop_gain()` on the
  fiscal-financial cycle showed no explosive draws -- because the
  `long_rate <- govdebt` link was ~0. Check the per-link means before reading a
  low gain as reassurance.

## Spillover / conditional-forecast analysis (`spillovers.R`)

koma has **no impulse-response function**. A spillover or shock response is
built the only way the API supports it: two `koma::forecast()` calls (one
unconditional, one with `restrictions = `), differenced.

- **Pair the two calls with common random numbers, via `set.seed()`
  immediately before each `forecast()` call.** koma's stochastic forecasts
  are not reproducible call-to-call by default (`conditional_forecast_check()`
  already documents this: two identical calls gave `de_gdp` means an order
  of magnitude apart). But a posterior draw's coefficients are read
  deterministically from `fit$estimates[[eq]]$beta_jw[[i]]` — no RNG — and
  the only randomness, `forecast_draw()`'s `z_matrix <- matrix(rnorm(...))`,
  is drawn *before* the restriction branch and consumed identically whether
  or not a restriction is present. So matching the seed on both calls
  reproduces the identical innovation draw at index `i` in both, and
  differencing isolates the shock. Verified: for a variable structurally
  unrelated to the shocked one, the matched-seed diff was *exactly* zero at
  every draw; unmatched, the same pair had sd nine orders of magnitude
  larger. `scenario_diff()` does this automatically; don't call
  `koma::forecast()` twice by hand.
- **koma drops failed draws by subsetting the list**, so `fc$forecasts[[i]]`
  after any drop no longer corresponds to posterior draw `i`. If the
  baseline and scenario calls drop a *different* set of draws, pairing by
  list position silently mispairs. `scenario_diff()` aborts if the two calls
  return different draw counts — never disable that check to "make it work".
- **`restrictions` only targets `sys_eq$endogenous_variables`.** An
  exogenous-variable shock (e.g. oil price) has no innovation to condition
  and cannot use `restrictions` at all — build a whole shocked copy of the
  fit instead (`shock_exogenous_level()`, or `oil_price_shock()`) and pass
  it as `scenario_diff()`'s `scenario_fit`. The common-random-numbers
  argument still holds, since `z_matrix`'s size depends only on `horizon`
  and the endogenous-variable count, not on `ts_data`'s content.
- **A `restrictions` value is in the same space `forecast()` operates in**:
  rate-space (percent `diff_log` growth) for a level/diff_log variable,
  literal level for a rate/none variable (a policy rate's "+100bp" is just
  `+1.0`, since level and rate space coincide when `method = "none"`). A
  "GDP +1%" shock is therefore a **one-quarter growth-rate** impulse
  (`gdp_demand_shock()`), not a permanent level step — state which
  convention is in use; the two look identical in the restriction syntax
  but mean very different things.
- **koma silently *shortens* the horizon when exogenous data runs out**,
  rather than erroring — `forecast_draw()` resets `horizon <-
  nrow(na.omit(forecast_x_matrix))` and only warns. All of this project's
  exogenous series happen to end exactly at the fit's native forecast end,
  so asking for a longer horizon without first extending them
  (`extend_forecast_horizon()`) silently returns fewer quarters than
  requested, with no error to catch it.
- **An individual spillover-matrix cell is not a reliable number at 1000
  draws, and never was.** Re-running the same battery on the *same fit*
  changing only the seed moves an off-diagonal cell by **0.35 pp on average
  and up to 1.85**, and flips the sign of 7–10 of 30. This holds for stage
  2b (`df = 22`), not just the thinner stage-3 system — the seed noise is
  0.346 for stage 2b against 0.392 for the extended system, barely
  different. Common random numbers make the *baseline-vs-scenario* pair
  exact, but
  they do nothing about sampling error in the level of the estimand itself,
  which is a median over draws of which a third have exploded by horizon 8.
  Read the matrix as a pattern (sign, rough ordering), never cell by cell.
- **Before reporting that a change moved the spillover matrix, run the
  seed placebo**: re-run a few source countries on both fits at a second
  seed and compare the cross-system difference against the seed-to-seed
  difference. On the stage-3c comparison the placebo killed the two largest
  apparent findings (`de -> ie` at −2.28 became −0.54; Ireland's own-diagonal
  gap of +1.204 became −0.168) and confirmed the one real one (Germany's
  own diagonal, +0.717 and +0.612 at the two seeds — the only country
  carrying the blocks). Use non-block countries as controls: their gaps
  should flip sign across seeds, and if they do not, the result is not
  coming from the blocks.

## FRED API key

The FRED API key lives in `.Renviron` as `FRED_API_KEY`. Copy
`.Renviron.example` to `.Renviron` and fill in the real value locally.

- `.Renviron` is git-ignored. **Never commit it, and never commit a key
  literal anywhere else in the repo** (code, tests, docs, commit messages).
- `.Renviron` is **never printed**. `fred_api_key()` (`R/data_fred.R`) is
  the only place the key is read; any error it or its callers raise must
  not include the key value in the message (see
  `tests/testthat/test-data_fred.R` for the test that enforces this).
- If a key is ever accidentally committed, treat it as compromised: rotate
  it at FRED before doing anything else, then scrub the commit.

## Backtesting and scoring (`R/scoring.R`)

- **`k < T` (`df > 0`) is the right first gate for a joint system but not a
  sufficient one — treat `df < 4` as infeasible in practice.** Backtesting
  stage 2/3 across many re-estimation origins (`reports/evaluation.qmd`)
  found an exact cutoff across 92 attempted estimations: every origin with
  `df >= 4` produced a usable `koma::forecast()` result; every origin with
  `df` of 1, 2 or 3 passed `origin_feasible()`'s gate but then failed
  outright at the forecast stage (`riwish(T - k, .)` drawing from a
  near-degenerate Wishart throws `"v must be >= dimension of S in
  rwish()"` on most draws at `df` this small). `stage3c_rollout.qmd`'s own
  language ("no usable degrees of freedom" at `df = 1`) already anticipated
  this; the backtest turns it into a precise, load-bearing threshold rather
  than an impression.
- **A closed-form AR(1) naive benchmark needs the same explosive-value
  awareness as koma's own Gibbs draws, for a different reason.**
  `naive_ar1_forecast()`'s textbook recursion (`mean_h = mu + phi^h*(y_T -
  mu)`) has no stability guard, and a country/concept whose estimated `phi`
  lands at or above 1 at some origin compounds geometrically over an 8-quarter
  horizon — verified: the naive benchmark's median squared error stays flat
  around 0.13-0.15 across every horizon in the evaluation backtest, but its
  *maximum* reaches 42 million at horizon 8. This is not a bug to fix (the
  design deliberately keeps the benchmark unclamped, as the honest textbook
  comparison), but it means **RMSE is not a safe metric to read on its own
  here** — a handful of explosive outliers on either side swamp the mean and
  make an MSE-based comparison directionless even when MAE shows a clear,
  monotonic pattern. Prefer MAE (or CRPS, which is itself robust) over RMSE
  when comparing against this benchmark.
- **`koma::model_evaluation()` needs the full, untruncated panel as its
  `ts_data` argument — never a fit's own `fit$ts_data`.** Every
  `fit_stage1()`/`fit_stage2()` fit in this project deliberately truncates
  its endogenous series to `dates$estimation$end` (that is how the
  conditional-fill/COVID-neutralisation trick works), so `fit$ts_data` has
  no real values in the forecast window to score against at all —
  `model_evaluation()` silently returns `NA` RMSE for every variable if
  handed it. `score_country_forecast()`/`score_all_countries()` therefore
  take an explicit `panel` argument (the full panel, e.g. `stage2b_panel`
  for a stage-2 fit) rather than reading it off `fit`.
- **`koma::forecast()`'s `$forecasts[[i]]` draws carry no attributes** —
  only `$mean`/`$median` are tagged `series_type`/`method`/`anker`, which
  `koma::level()` needs to invert rate space back to levels.
  `forecast_draws_level()` reattaches `$mean[[var]]`'s attributes onto each
  draw's raw numeric column before calling `level()`; verified against
  `docs/koma-api.md`'s own round-trip example. `level()` returns one extra
  leading value (the `anker` itself — the last known *actual*, not a
  forecast), which must be dropped.
- **A perfectly-reciprocal small trade-weight matrix breaks
  `koma::construct_posterior()`.** `stage2_linkage_weights()` always folds
  whatever share of trade weight is left over into a `row_gdp` residual
  identity term; if the declared countries' weights on each other happen to
  sum to exactly 1 (a hand-built two-country synthetic fixture with, e.g.,
  weights `[[0,1],[1,0]]`), that residual is exactly zero, and koma aborts
  with `"The posterior beta matrix has zeros at different indices compared
  to the character beta matrix"` — the symbolic matrix marks the declared
  `row_gdp` term as structurally nonzero, but its actual weight is exactly
  0. Real bilateral trade weights never sum to 1 across a handful of
  countries, so this only bites synthetic test fixtures; keep any hand-built
  `trade_weights` matrix's off-diagonal entries below 1 (e.g. scale by 0.7)
  so `row_gdp` stays genuinely nonzero. `diagnostics_synthetic_stage2_fit()`
  (`tests/testthat/helper-fixtures.R`) does this.

## Proving a change works

```r
targets::tar_make()
devtools::test()
```

Run both after any change to `R/`, `_targets.R`, or the data pipeline.
`tar_make()` confirms the pipeline still executes top-to-bottom (or fails
at the expected, not-yet-implemented step); `devtools::test()` confirms
the unit tests for whatever you touched now pass. `data_fred.R`,
`data_eamdqd.R`, `panel_build.R`, `weights.R`, `stage1_models.R`,
`diagnostics.R`, `stage2_system.R`, `spillovers.R` and `scoring.R` are
implemented; `stage3_blocks.R` is not, so `tar_make()` will still error
partway through by design — that is expected, not a regression; make sure
it errors at the *next* unimplemented stub, not an earlier one you touched.
`devtools::test()` likewise still shows 3 `not implemented` errors, all
from `test-stage3_blocks.R`.

`stage2_system.R` implements **stage 2a** (the two-country DE + FR pilot of
the linkage mechanism), **stage 2b** (all eleven economies: 68 stochastic
equations and 35 identities in one `system_of_equations()`) and **stage 3a**
(the German labour and disaggregated-price block, `labour_block()`). See
`reports/stage2a_pilot.qmd`, `reports/stage2b_full_system.qmd` and
`reports/stage3a_labour_prices.qmd`.

Stage 2b needs its **own** estimation window — `stage2b_dates()`, ending
2024Q4 — because the stage-1/2a window does not leave enough observations
(see the `k < T` note below). It therefore does *not* neutralise COVID by
conditional fill the way stages 1 and 2a do; it uses dummies instead.
`stage2_options()` defaults reproduce stage 2a exactly, so every stage-2b
departure is opt-in and the cached 2a fit keeps reproducing.

Run anything that sets `workers` with `OMP_NUM_THREADS=1
OPENBLAS_NUM_THREADS=1` in the environment.

`R/scoring.R` implements an expanding-window, re-estimating backtest across
four specifications (naive AR(1)/RW, stage 1, stage 2, stage 3) —
`pseudo_oos_backtest()`'s per-spec drivers are `backtest_stage1()` /
`backtest_stage2()` / `backtest_stage3()` / `backtest_naive()`, all built on
the shared `score_forecast()` scorer. See `reports/evaluation.qmd` for the
results and the `df >= 4` usability finding above; reproducing the full run
takes several hours (92 joint-system estimations at production settings) and
should not be re-run casually — the per-origin cache under
`data/cache/evaluation/<spec>/` makes it resumable if interrupted.

Do not fetch real data as part of "proving a change works" unless the
change is specifically about the fetch layer — most iteration should run
against small synthetic/fixture data in tests.

You will see renv print `- The project is out-of-sync -- use renv::status()
for details.` on most commands, and `renv::status()` will list `devtools`,
`targets`, `tarchetypes`, `testthat`, `withr`, `httr2`, `jsonlite` (and their
dependencies) as `used: n`. This is expected and harmless: those packages are
declared under `Suggests` (dev/pipeline tooling, not something a user of the
package needs), and renv's "used" check only looks at `Imports`/`Depends`/
`LinkingTo` by design. Do not "fix" this by moving them into `Imports` or by
widening `renv::settings$package.dependency.fields()` to include `Suggests`
project-wide — the latter makes `renv::snapshot()` recurse into every
dependency's own `Suggests` too and explodes the lockfile. If you add a new
`Suggests`-only package and want it captured, snapshot it explicitly:
`renv::snapshot(packages = c(renv::dependencies(".")$Package, "newpkg"))`.
