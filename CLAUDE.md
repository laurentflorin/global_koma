# CLAUDE.md

Guidance for working in this repository — a multi-country Bayesian macro
model built on the [`koma`](docs/koma-api.md) package.

## Repo map

```
R/                     package code (roxygen-documented, exported via NAMESPACE)
  data_fred.R             FRED fetch + local cache (FRED_API_KEY -- see below)
  data_eamdqd.R           Euro Area Monthly/Quarterly Database fetch + code mapping
  data_worldbank.R        World Bank Global Economic Monitor fetch (China's panel)
  data_oecd.R             OECD SDMX fetch (China's interest rates)
  data_dbnomics.R         DBnomics fetch, for IMF DOTS bilateral trade
  panel_build.R           combine raw sources into named koma_ts panels
  panel_stage2d.R         the `reu` bloc aggregate and China's assembled panel
  weights.R               country aggregation weights (GDP/trade) + weighted identities
  stage1_models.R         per-country satellite models
  stage2_system.R         joint multi-country koma system (stage 1 equations + ea_/world_ identities)
  stage3_blocks.R         post-estimation regional/global aggregation blocks
  equations.R             naming convention + koma equation-string builders
  diagnostics.R           MCMC diagnostics wrappers, applied across countries
  scoring.R               out-of-sample RMSE scoring + leaderboards
  forecasts.R             8-quarter stage forecasts + fan charts for the reports
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

Stage 2 has four lettered variants, which differ in the equation template
(2a → 2b → 2c) or in the country partition (2d), never in both at once —
that is what keeps any pair of them comparable. **Stage 2d** re-partitions
rather than re-specifies: it takes stage 2c's equations verbatim and applies
them to six entities instead of eleven (`de`, `fr`, `it`, `us`, `cn` and the
`reu` bloc). It is the only stage that *increases* degrees of freedom.

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
  Use `contemporaneous_reachability()` (`diagnostics.R`) rather than assuming;
  it is the test-local BFS over `sys_eq$character_gamma_matrix` promoted to
  tested package code.
- **Self-reachability is not the loop you care about.** Stage 2b's
  `ea_policy_rate` *is* reachable from itself — through the Taylor rule's
  `ea_gdp` term — while reaching **no price variable at all** (91 of 103
  endogenous variables, zero of them prices). So "the loop is closed" was true
  of the rule's output term and false of its inflation term at the same time,
  and that is the structural cause of the monetary sanity-check failure in
  `reports/stage2_spillovers.qmd` §7.2. Count what is reached, not whether the
  start point comes back.

## Stage 2c: the refined linked core (`stage2_system.R`)

Stage 2b's structure with **four** refinements applied to every country, all of
which cost **zero** additional `k` — `k = 76`, `T = 98`, `df = 22`, identical
to stage 2b and therefore directly comparable to it, unlike every stage-3 block.
68 stochastic equations (unchanged: the long-rate level equation is *swapped*
for a spread equation, not added to); identities 35 -> 46. `stage2c_config()`
carries the settings. See `reports/stage2c_refined_core.qmd`.

- **A fifth refinement was estimated and rejected: `<iso2>_imports` gaining a
  contemporaneous `<iso2>_exports` term** (the import content of exports,
  `stage2c_config(import_content = TRUE)`). It collapses the domestic-demand
  elasticity in the imports equation in **9 of 11 countries** — Germany 0.382
  -> **0.049**, Austria 0.450 -> **−0.104** — while *halving* those equations'
  in-sample RMSE (Belgium fits 2.8x better). Re-estimating without it recovers
  **all eleven** elasticities toward their stage-2b values (Germany 0.320,
  France 1.215 against 1.245, the US 1.782 against 1.798). This is the same
  failure `<iso2>_import_prices` produced four times in stage 3a (0.38 -> 0.08):
  a contemporaneous regressor correlated with the dependent variable buys fit by
  taking variance from the structural term. **The improved fit is the symptom,
  not the reassurance.**
- **That one bad term contaminated equations it never touched.** With it in, 3
  Phillips-curve and 5 consumption-rate sign checks failed; without it, 1 and 3.
  Only Austria's import elasticity actually crossed zero, so `sign_checks()`
  reported *one* failure while nine elasticities were being gutted — the
  now-familiar lesson that a neighbouring coefficient moving is the first
  symptom, and that a bad specification in one equation masquerades as a
  systemic problem.
- **`sign_checks(stage2c = )` takes a refinement subset, not just `TRUE`.** A
  rule whose term is absent scores `NA`/`FALSE`, so `TRUE` on a system without
  the import-content term turns eleven deliberate absences into eleven reported
  failures (19 of 66 rather than the true 8 of 55). Pass
  `stage2c_config()$refinements`.
- **The spread's own lag is not less persistent than the level's, contrary to
  the design note.** Refinement 3's motivation is confirmed exactly — all eleven
  stage-2b policy-rate loadings have 90% intervals straddling zero, with own
  lags 0.910–0.964 — but the stage-2c spread own lags run **0.932–0.981**, and
  for Spain and Portugal they are *higher* than the levels they replaced. The
  other three arguments for the spread (imposed pass-through, fewer competing
  regressors, `k`-neutral) are unaffected.
- **A closed loop can be arithmetically inert.** `monetary_loop()` +
  `loop_gain()` give a contemporaneous round-trip gain around **1e-5** in every
  country, with no draw anywhere near 1. That is a six-link chain of small
  coefficients multiplying out, and it measures within-quarter feedback
  (stability), *not* whether a monetary shock transmits over the horizon. Read
  the per-link means, not the product: the Taylor rule's inflation loading is a
  healthy 0.227, but 3 of 11 consumption-rate links are wrong-signed.
- **Stage 2c's spillover matrix is ~3x more seed-reproducible than stage 2b's.**
  The seed placebo on the same fit moves an off-diagonal cell by **0.119 on
  average (max 0.356) and flips 1 sign of 44**, against the 0.35 / 1.85 / 7-10
  of 30 recorded above for stage 2b. This is the strongest stage-2c result
  precisely because it is a measurement *of* the noise and so cannot itself be
  dismissed as noise. The `own effect dominates` sanity check also goes 7/11
  (fail) -> 11/11 (pass) -- stage 2b's Austria two-hop artefact is gone -- and
  the trade-weight correlation goes 0.141 -> 0.292, still under the 0.3 bar.
- **Closing the monetary loop structurally did not make the monetary shock
  work, and the placebo is what showed it.** Reachability is exact and
  verifiable; the response is not. Prices peak **positive in 11 of 11** under a
  +100bp tightening (wrong sign) in both systems, and the apparent gain in GDP
  cumulative sign (2b 6/11 -> 2c 8/11 negative) **collapses to 6/11 at a second
  seed on the same fit**. Only Greece and Ireland move by more than seed noise.
  The oil shock, by contrast, is correctly signed in all eleven (cumulative
  0.8-2.3pp), which localises the fault: the price block responds sensibly to a
  cost shock and perversely to a policy rate, because the policy rate's route
  into GDP is dominated by the horizon-1 simultaneity echo rather than a lagged
  demand channel.
- **Closing the monetary loop cost the price forecast, and the two are the same
  fact.** In stage 2b `<iso2>_prices` had *no* contemporaneous endogenous
  regressor, which made it effectively a univariate AR with exogenous cost-push
  terms and therefore **structurally immune** to the explosive-draw population:
  its explosive share in the backtest is **0.0000 at horizon 1 and 0.0026 at
  horizon 8**. Stage 2c's Phillips curve couples it to `<iso2>_gdp`, whose share
  reaches 0.76, and prices inherit it: **0.016 rising to 0.634**. GDP's own share
  is unchanged. Price CRPS goes from 0.62 to 3.99 at horizon 8 and stage 2b wins
  the Diebold-Mariano CRPS comparison at `p < 0.002` from horizon 3 on, while
  GDP forecasts are *better* under 2c and MAE is roughly a draw. Making prices
  reachable from the policy rate necessarily makes them reachable from
  everything else that reaches GDP. **Use stage 2c for mechanism and stage 2b
  for a price fan chart.**
- **`backtest_stage2c()` is the only unconfounded head-to-head in the
  evaluation.** Every refinement is `k`-neutral, so stage 2c has *identical* `k`,
  `df`, feasibility verdict and usable-origin set to stage 2b at every origin
  (verified, both annual and quarterly, including the shared `df = 3` failure at
  2019Q1). Every other pair in `reports/evaluation.qmd` differs in sample size,
  capacity, or both.
- **Five annual origins signed the 2b-vs-2c comparison the wrong way.** On the
  annual sample stage 2c looked *better* at long horizons (h=8 MAE 1.33 vs
  1.61); on the 20-origin quarterly sample it is clearly worse (ratio 1.36).
  Treat the five-origin annual stage-2 window as unable to sign a comparison at
  all, not merely as imprecise.
- **`diebold_mariano()`'s HAC correction is order-dependent, and a pooled panel
  has no natural order.** It sums autocovariances of the loss differential at
  lags `1..h-1` *in the order the vector is given*, so pooling over (origin x
  country x concept) makes every `h > 1` statistic depend on how the rows were
  sorted -- `h = 1` is order-free, which is how this was diagnosed. Sort by
  `(variable, origin)` before testing, so each series' own origin sequence is
  contiguous and in time order.
- **Stage 2c's in-sample gain was entirely the imports equations.** With
  refinement 5 in, 77% of shared equations fit better and the median ratio is
  0.995 — but every non-imports equation's ratio is ~1.000. Splitting the
  summary by equation family is what exposed that; the headline percentage did
  not.

- **The spread identity has two sides, and only one of them ever saw `opts`.**
  `<iso2>_long_rate == <iso2>_spread + <policy rate>` is written by
  `long_rate_identity()` *and* implied by the `<iso2>_spread` series
  `build_stage2_panel()` constructs. The identity resolved `us_policy_rate`
  for the US under `policy_rule = TRUE`; the panel builder's scalar
  `policy_rate` default subtracted `ea_policy_rate` for everybody. The US
  identity was therefore violated by the entire EA–US policy-rate gap —
  **verified at up to 3.25 percentage points** — with no error, because koma
  has no identity-consistency check. Both sides now derive the mapping from
  `policy_rate_map(opts)`. This only bites once the US is in
  `spread_countries`, which stage 2c is the first configuration to do.
- **`stage2_preflight()` now does the arithmetic koma never does.**
  `identity_consistency()` recomputes every identity from the panel and
  reports its worst absolute error. It picks the space from the series'
  attributes: `rate`/`none` identities (the long-rate/spread/policy-rate
  family) are literal linear combinations of levels, everything else is
  compared after `koma::rate()`, and a mixed-attribute or lagged identity is
  skipped rather than reported wrong. `<iso2>_gdp` and
  `<iso2>_domestic_demand` are **reported but never flagged** — the first
  holds only up to the statistical discrepancy, the second is deliberately
  inexact under `include_government = FALSE`. On the stage-2c panel: 23 exact
  identities, worst error 1.05e-13; the inexact ones run up to 17.7pp
  (Ireland's domestic demand).
- **The Phillips curve gives every price equation a Metropolis step for the
  first time.** In stage 2b all eleven `<iso2>_prices` equations had an empty
  contemporaneous endogenous regressor set, so `count_accepted` was `NA` and
  `check_acceptance_rates()` never had anything to say about them — **57 of 68
  equations had a Metropolis step, and the eleven that did not were exactly
  the price equations**. Adding `<iso2>_gdp` takes stage 2c to 68 of 68, so a
  stage-2c acceptance table is not row-comparable to a stage-2b one.

## Stage 2d: the regional core with China (`stage2_system.R`, `panel_stage2d.R`)

Stage 2c's **equations**, unchanged, on a re-partitioned world: `de`, `fr`, `it`
and `us` kept separate, the other seven modelled euro-area economies collapsed
into the `reu` bloc, and China added. 38 stochastic equations and 25 identities;
`k = 46`, `T = 78` (2005Q1–2024Q4), `df = 32`. `stage2d_config()` /
`stage2d_dates()` / `stage2d_countries()` carry the settings. See
`reports/stage2d_regional_core.qmd`, with the equation-by-equation view in
`reports/stage2d_equations.qmd` and the shock battery in
`reports/stage2d_spillovers.qmd`.

- **This is the first stage that *buys* degrees of freedom.** Every earlier
  structural change traded `df` for detail — stage 3a went 22 → 15 → 6, stage 3b
  to 8. Collapsing seven countries into one bloc removes enough equations that
  `k` falls 76 → 46, so even after giving up eighteen quarters at the start of
  the sample `df` rises 22 → **32**. Consequences are visible: no own lag is
  flagged (the most persistent is `us_policy_rate` at 0.983), against stage 3b
  where `de_long_rate` went explosive at `df = 8`.
- **A bloc is a pseudo-country, not an `ea_`-style aggregate.** `reu` occupies a
  country slot: it has a `reu_<concept>` series for every concept, its own
  `country_block()`, its own row *and* column in the trade-weight matrix, and its
  own `foreign_demand` identity; its members contribute no equations at all.
  `country_var()` accepts it because `bloc_codes` (in `equations.R`) lists it —
  three letters deliberately, so no real ISO-2 code can collide and
  `startsWith(name, "reu_")` cannot accidentally match a country.
- **`expenditure_shares()` on a bloc returns a plausible number that is wrong by
  a factor of three, and koma would have enforced it silently.** It forms each
  share as the mean of a *level* ratio, which works for a real country because
  its series share a currency and a scale. Every bloc series is a base-100 chain
  index, so the ratio is the two indices' relative growth since the base period:
  `mean(reu_exports / reu_gdp)` comes out at **1.337** against a true share of
  **0.598**, because `reu` exports grew 2.5x since 2000 while its GDP grew 1.6x.
  `bloc_expenditure_shares()` averages the *members'* own shares instead, and
  `stage2_shares()` **aborts** rather than falling back — there is no safe
  default, so `stage2_options(bloc_weights = )` is required for every bloc.
- **The bloc is the one place Ireland's investment distortion is not worked
  around, and it breaks `reu_investment`.** Averaging seven economies should
  *reduce* volatility; instead `reu_investment` has a quarterly growth sd of
  **6.46** against 2.16–4.08 for `de`/`fr`/`it`/`us`. Ireland's own investment sd
  is **43.0** on this window (Netherlands 14.0, Austria 2.2) and Ireland carries
  10.9% of the bloc weight, so it contributes more variance than every other
  member combined. The equation is consequently **not identified**: its
  accelerator is -0.170 with a 90% interval of [-2.77, 2.14], its own lag has gone
  to -0.567 (mean-reversion around noise, not an accelerator), and its in-sample
  RMSE is 4.34 against 0.78–1.91 elsewhere. `ireland_proxy` cannot help — it
  swaps a *foreign-demand basis*, and this distortion is inside an estimated
  equation. **Nothing flagged it**: there is no investment-accelerator rule in
  `base_sign_rules()`, and it surfaced only from computing the implied
  domestic-demand elasticity, which came out lowest for `reu` (0.428). Same
  lesson in a new place — a neighbouring coefficient moving is the first symptom.
  Read `reu_investment` as unidentified and `reu_domestic_demand` as
  correspondingly soft; a less Ireland-heavy bloc weighting, or pulling Ireland
  out of `reu` entirely, is the untried fix.
- **A bloc's identities carry a dispersion term on top of the statistical
  discrepancy.** `reu_gdp`'s worst identity error is 5.0pp of quarterly growth
  against Germany's 2.0 and Italy's 1.9, because the seven members' own
  expenditure shares differ from the average the identity uses. Bounded, same
  order as the country-level slack, and `identity_consistency()` reports it under
  the existing `_(gdp|domestic_demand)$` inexact rule.
- **China publishes no quarterly consumption or investment, anywhere.** Verified
  absent from OECD Quarterly National Accounts (China carries `B1GQ` only, and
  only from 2011Q1), the OECD Economic Outlook (annual except CPI and two rates),
  IMF IFS, World Bank GEM, FRED (its China coverage was discontinued 2019–2023)
  and the NBS itself. Hence `stage2_options(merged_demand_countries = "cn")`:
  the two equations collapse into one estimated `<iso2>_domestic_demand ~ gdp +
  long_rate + lag`, and the `domestic_demand` **identity is dropped** because the
  variable is now estimated rather than defined. The rate term is unconditional
  in the merged form — it was investment's term as well as consumption's and it
  is the country's only monetary channel.
- **The bound on a merged demand equation is `> 0`, not `(0, 1)`.** The MPC rule
  was copied across first and China's 1.101 failed it. But an MPC below one is a
  claim about *consumption*; total absorption also contains investment, which is
  far more cyclical than output. The five entities that do have the split imply
  elasticities of 0.43 (reu), 0.63 (de), 0.96 (fr), 0.98 (it) and **1.26 (us)** —
  so `(0, 1)` would have failed the United States too. Check what the rule
  actually claims before reporting a coefficient against it.
- **GEM's constant-price China trade series is missing every Q1 from 2020**, six
  of the eighty quarters and all mid-sample, because China's customs
  administration stopped publishing a separate January figure. The current-price
  and price-index series are complete, so `cn_real_trade()` recovers the volume as
  `value / price` — the same quotient GEM computes internally, reproducing the
  published series to a mean ratio of 0.99985 (sd 4.4e-4). That recovers six real
  quarters instead of interpolating them; prefer reconstructing a series from its
  own published components over filling it.
- **`cn_long_rate` is spliced over 45% of the window and that is its main
  weakness.** OECD's `IRLT` for China starts only 2014Q1, so
  `cn_long_rate_series()` shifts `IR3TIB` by the mean `IRLT - IR3TIB` gap before
  then — **-0.262pp, sd 0.482 over 50 quarters**, a standard deviation nearly
  twice the mean. The level is right; the independent variation is not, so
  `cn_spread` is a real term premium after 2014 and a money-market spread plus a
  constant before it. `stage2d_config(spread_countries = )` is the one-argument
  way out: `IRSTCI` is complete and needs no splice.
- **The reciprocal trade-weight row is not good enough at six entities.** ECB WTS
  computes weights *for* euro-area reporters only, so stages 1–3 build the US row
  from the reciprocal of each EA country's weight on the US, renormalised — which
  forces the modelled partners to account for the reporter's whole trade and
  gives the US a **4.6%** rest-of-world weight in `data/raw/W_trade.csv`. Applied
  to China it would have given **1.0%** and put 56% of Chinese trade inside the
  euro area. `build_trade_weight_matrix(dots_reporters = )` takes both rows from
  observed IMF DOTS instead (US 72.9% rest-of-world, China 78.2%). Note DOTS is
  **goods only** and a plain trade share, where WTS covers services too and is
  double-weighted — the `source` attribute records which row is which.
- **IMF DOTS comes through DBnomics, and that is a deliberate exception.** The
  IMF's legacy SDMX endpoint no longer responds and `api.imf.org` does not serve
  the `DOT` dataflow (verified 2026-09: "No such dataflow found"). DBnomics
  mirrors it keyless and preserves the upstream series identifiers verbatim, so
  only `fetch_dbnomics_series()` has to change if the IMF restores an endpoint.
- **`row_gdp` must be rebuilt, not reused.** `row_gdp_weights()` folds China into
  the rest-of-world aggregate by default. Once China has equations, passing the
  stage-2b `row_weights` would not error — it would load Chinese demand into every
  partner's `foreign_demand` identity twice, once directly and once through
  `row_gdp`. Stage 2d uses `row_gdp_weights(exclude = c("us", "cn"))`, and
  `build_row_gdp()` now only fetches growth for partners the weight vector still
  carries.
- **`policy_rule` is a country vector now, and `TRUE` still means the US alone.**
  Stage 2d needs a third rule (`cn_policy_rate`) because China is outside both
  currency unions; without it the spread identity would price Chinese debt off
  the ECB's refi rate. `stage2_policy_rule_countries()` resolves
  `FALSE`/`TRUE`/character in one place so `country_block()`,
  `policy_rate_map()` and the panel's `<iso2>_spread` cannot disagree.
- **All three policy rates reach all 63 endogenous variables and all seven price
  variables.** Stage 2c's structural fix survives the re-partition intact (stage
  2b reached 91 of 103 and *no* price). As stage 2c established, that is a claim
  about reachability, not about the sign or size of the response — see the
  spillover battery below, which settles it.
- **Stage 2d's spillover matrix is the first in this project whose cells can be
  read individually.** The seed placebo on the same fit moves an off-diagonal
  cell by **0.108 on average (max 0.273) and flips 0 signs of 20**, against
  stage 2c's 0.119 / 0.356 / 1-of-44 and stage 2b's 0.35 / 1.85 / 7-10-of-30.
  That is what the ten extra degrees of freedom actually buy, and it is the
  strongest stage-2d result because it measures the noise rather than asserting
  something on top of it. `own effect dominates` is 6/6. The trade-weight
  correlation is **0.138**, still failing the 0.3 bar and worse than stage 2c's
  0.292 — reported as a failure, not explained away. See
  `reports/stage2d_spillovers.qmd`, built by
  `scratch/stage2d_spillovers_build.R`.
- **A `+100bp` shock to `cn_policy_rate` is not a monetary experiment — it is a
  horizon-1 accounting adjustment in China's own Taylor rule.** Chinese GDP rises
  **6.67pp at horizon 1** and decays to nothing by horizon 3 (cumulative +8.17).
  The restriction forces the rate a point above what the rule wants, and the rule
  loads only 0.152 on `cn_prices` and 0.015 on `cn_gdp`, so the solver has to move
  Chinese output and prices enormously to make the quarter consistent. Nothing
  pushes back, because `cn_domestic_demand`'s loading on `cn_long_rate` is
  -0.019 with an interval straddling zero — which traces straight to the
  45%-spliced long rate. `ea_policy_rate` does **not** have this problem: its
  rule responds to `ea_gdp`/`ea_prices`, identities over four entities, so the
  adjustment spreads across the area. **Use `spread_countries` without China if
  a Chinese monetary channel is needed.**
- **The euro-area monetary shock works for GDP and still fails for prices.**
  Peak GDP response negative in 6/6, robustly so for the four euro-area entities
  (Germany -1.32pp cumulative); the US and China cross zero between seeds and
  should be read as nothing. Prices peak negative in only **1 of 6** — the same
  wrong sign stage 2c reported in 11 of 11, unchanged by the re-partition because
  the re-partition does not touch the mechanism. The oil shock is correctly
  signed **6/6** (0.8-2.6pp), which localises the fault to the policy rate's
  route in rather than to the price block.
- **The sustained monetary restriction fails 25 of 1000 draws**, so
  `scenario_diff()` aborts rather than mispair. That is the documented path, not
  an obstacle: `failed_restriction_draws()` recovers the indices and
  `drop_baseline_draws =` re-pairs the baseline against exactly those.
- **Build the stage-2d panel on the *aligned* `panel`, with `extend = TRUE`.**
  China's merchandise-trade series run a quarter behind the rest, so folding them
  into `build_global_panel()` and letting `align_panel()` pick bounds
  automatically would take the earliest end across everything and silently
  truncate every exogenous series — the trap already recorded for stage 3a.
  `add_stage2d_countries()` pads instead.

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

## Reporting a forecast (`forecasts.R`)

Every stage report carries an **eight-quarter forecast** section — fan chart,
median table, discarded-draw table, exogenous-extension table — built by
`stage_forecast()` and `plot_forecast_fan()`, with the artefacts produced by
`scratch/forecasts_build.R` into `data/cache/forecasts/<stage>.rds` and the
shared report code in `reports/_forecast_helpers.R`. Stage 3c is deliberately
absent: it is a rollout *feasibility* study whose only fitted system is the
stage-2b benchmark it compares against.

- **The explosive-draw share is the headline number, and it tracks `df`
  exactly.** Averaged over each stage's GDP and price variables, the share of
  posterior draws discarded at horizon 8 is **0.00** for stage 2a, **0.04** for
  stage 1, **0.35** for stage 2d (`df = 32`), **0.49** for stage 2b and 2c
  (`df = 22`), **0.58** for stage 3a (`df = 15`) and **0.67** for stage 3b
  (`df = 8`). That is the degrees-of-freedom cost this repo has been describing
  qualitatively, finally measured on the thing users actually want. It is also
  the strongest argument for stage 2d: re-partitioning does not just raise `df`
  on paper, it visibly buys back forecast usability.
- **koma silently shortens a horizon it cannot deliver**, so `stage_forecast()`
  calls `extend_forecast_horizon()` and then **checks `nrow(fc$forecasts[[1]])`
  and aborts** if fewer quarters came back. Without the extension every stage
  returns 4-5 quarters for an 8-quarter request and only warns.
- **Filter the draws before taking any quantile.** At horizon 8 half the
  posterior has compounded to nonsense in the mid-sized systems, so an
  unfiltered mean or interval is meaningless. `stage_forecast()` uses
  `score_forecast()`'s two conventions unchanged: **per-horizon** in rate space,
  **cumulative** in level space (a draw whose rate explodes at horizon 3 has
  corrupted its compounded level from horizon 3 on).
- **A level path is anchored on the last value in the fit's own `ts_data`,
  which is not always observed.** Stages 1 and 2a estimate to 2019Q4 and
  forecast from 2023Q1, so koma conditionally fills 2020-2022 first and anchors
  on the *fill*. Measured: `ie_investment`'s stage-1 anker is **156% above** the
  observed 2022Q4 value, `gr_investment` 68% below, `de_prices` in stage 2a
  9.1% below, median across all stage-1 variables 4.2%. Those two stages'
  **level** charts are therefore not comparable to actuals and their **rate**
  charts are; `forecast_ankers()` returns the numbers and `fc_anker_note()`
  prints them. Stages 2b onward estimate to 2024Q4 and forecast from 2025Q1
  with nothing to fill, so their anker gaps are exactly 0.00%.
- **`koma::level()` is `anker * exp(cumsum(x/100))` for `rate`/`diff_log`**, so
  `forecast_draws_level_matrix()` inverts a whole draw matrix at once instead
  of calling `koma::level()` once per draw. That is ~130x faster — the stage-3b
  artefact would not build inside ten minutes otherwise — and
  `test-forecasts.R` asserts it reproduces `forecast_draws_level()` to 1e-12,
  so a change in koma's inversion fails a test rather than drifting silently.
  A `method = "none"` series (every policy rate, spread and ratio) has **no
  anker at all** and `koma::level()` errors on it: level space *is* rate space
  there, and returning `NA` instead would blank every policy rate.
- **Fan charts are clipped, on purpose.** The 10-90 interval on *quarterly*
  growth spans 40-50 percentage points at horizon 8 in every stage, which makes
  the median invisible if the axis contains it. `plot_forecast_fan()` draws an
  inner 25-75 band, scales each panel to that, and **clamps** the outer band to
  the panel edge — `ggplot2::geom_blank()` cannot do this, because it only ever
  *expands* a scale. `forecast_uncertainty_table()` prints the untruncated
  bounds next to the chart so the clipping stays honest.
- **Set the seed.** koma's stochastic forecasts are not reproducible
  call-to-call (the same fact `scenario_diff()` documents), so without
  `stage_forecast(seed = )` a report's numbers change on every render.

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
- **`spillover_sanity_checks()`'s trade-weight check must not assume the
  `foreign_demand` basis.** It used to look the source country up as
  `<source>_gdp`, but a `foreign_demand` identity does not always load a
  partner's GDP: stage 2c loads partner **imports**, and `ireland_proxy` loads
  `ie_consumption`. Under either, every lookup missed, the weight column came
  out all zeros, and the reported Spearman correlation was computed against a
  constant — a meaningless verdict rather than an error. It now matches on the
  ISO-2 prefix, which covers every basis and still excludes the `row_gdp`
  residual (not a bilateral pair).
- **A sustained restriction's failing draws can be recovered, not just
  worked around.** koma's per-draw `safely()` wrapper drops a failed draw by
  *subsetting the list*, recording nothing about which index it was, which is
  why `scenario_diff()` aborts rather than mispair. `failed_restriction_draws()`
  re-derives the indices by tracing `koma:::forecast_draw()` and checking
  `returnValue()` per draw; feed them to `scenario_diff(drop_baseline_draws = )`.
  Two traps in writing that: `trace()` **deparses and re-parses** the expression
  it is given, so an environment inlined with `bquote()` does not survive — the
  exit code then silently records nothing, which reads as "no draw failed"
  rather than as an error (hence the abort when the log comes back empty). And
  the trace only patches the binding in *this* process, so a multi-process
  `future` plan runs the untraced function and produces the same empty log.
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
`data_worldbank.R`, `data_oecd.R`, `data_dbnomics.R` and `panel_stage2d.R`
are implemented too; their tests are network-free (the fetchers are exercised
against their parsers and fixtures, not against the live APIs), so a failure
there is a real failure and not a flaky endpoint.
`forecasts.R` is implemented and its tests are network-free too, but they are
gated behind `skip_on_cran()`: run them with `devtools::test()` (which sets
`NOT_CRAN`), not a bare `testthat::test_file()`, or all seven silently skip.
`devtools::test()` likewise still shows 3 `not implemented` errors, all
from `test-stage3_blocks.R`.

`stage2_system.R` implements **stage 2a** (the two-country DE + FR pilot of
the linkage mechanism), **stage 2b** (all eleven economies: 68 stochastic
equations and 35 identities in one `system_of_equations()`), **stage 2c**
(the same eleven economies with five zero-cost refinements, 68 stochastic
equations and 46 identities), **stage 2d** (stage 2c's equations on six
entities — `de`, `fr`, `it`, `us`, `cn` and the `reu` bloc — 38 stochastic
equations and 25 identities, with the bloc and China panels in
`panel_stage2d.R`) and **stage 3a** (the German labour and
disaggregated-price block, `labour_block()`). See `reports/stage2a_pilot.qmd`,
`reports/stage2b_full_system.qmd`, `reports/stage2c_refined_core.qmd`,
`reports/stage2d_regional_core.qmd` and `reports/stage3a_labour_prices.qmd`.

Stage 2d's report artefacts are built by `scratch/stage2d_build.R` (about
seven minutes cold, dominated by the four estimations `tune_tau_system()`
runs; every step is cached under `data/cache/stage2d/` and skipped if
present).

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
