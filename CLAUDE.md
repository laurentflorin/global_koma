# CLAUDE.md

Guidance for working in this repository: the **VJA model**, a nine-entity
Bayesian simultaneous-equations macro model built on the
[`koma`](docs/koma-api.md) package, with its equations written out by hand as
one koma string.

The project's predecessor -- stages 1-3 of the eleven-country `global_koma`
model, their reports, backtests, data fetchers (FRED, EA-MD/QD, ECB, World
Bank, OECD, DBnomics) and the targets pipeline -- is on the branch
**`archive/global-koma-stages`**, with its own CLAUDE.md recording every
finding. Look there before re-deriving something about koma's behaviour; most
of what is below was first found there.

## Repo map

```
R/                     package code (roxygen-documented, exported via NAMESPACE)
  panel.R                 koma_ts helpers, chained identity series, COVID dummies
  system.R                pre-flight checks, identity consistency, estimation
  diagnostics.R           acceptance rates, coefficients, own lags, reachability
  forecasts.R             forecasts with explosive-draw filtering, horizon extension
  utils.R                 `%||%`
tests/testthat/         one test-<name>.R per R/<name>.R, fixtures in helper-fixtures.R
scratch/
  vja_data_build.R        Macrobond downloads + KOF database -> data/raw/vja_data.csv
  vja_model_equations.R   the model: one explicit koma equation string
  vja_model_panel.R       the quarterly panel the equations name
  vja_model_build.R       checks, estimation, forecast -> data/outputs/
  vja_model_plots.R       koma's forecast and MCMC plots -> data/outputs/plots/
  Rrun                    runs R with the koma env's compilers on the path
data/raw/               git-ignored: raw downloaded series
data/cache/             git-ignored: cached API responses (Eurostat, KOF database)
data/outputs/           git-ignored: the fit, panel, checks, forecasts and plots
docs/koma-api.md        verified reference for the koma package's API
```

This is an R package (`DESCRIPTION`, `NAMESPACE`, roxygen docs in `R/`,
`Config/testthat/edition: 3`). The scripts in `scratch/` call
`devtools::load_all(".")` to use it without `R CMD INSTALL`.

## Variable naming

All variable names are **lowercase**: `<entity>_<concept>` (`de_gdp`,
`reu_prices`), `ea_<concept>` for the euro area's shared or aggregated series
(`ea_short_rate`, `ea_gdp`), and `dots_<from>_<to>` /
`<entity>_nominal_consumption` for series that appear only inside identity
weights. Entities are lowercase ISO-2 codes, plus `reu`, the rest of the euro
area: three letters, so it cannot collide with a country. koma's grammar
accepts `^[a-zA-Z][a-zA-Z0-9_]*$`. Never name a series `constant`: koma
reserves it for the intercept.

## Data stay in levels

**Every series handed to koma is a level** (or a rate in percent, for interest
rates), tagged with `koma::as_ets()`: `series_type = "level", method =
"diff_log"` for levels and indices, `series_type = "rate", method = "none"`
for rates and dummies. koma converts levels to growth rates itself, once, at
estimation. A series transformed beforehand would be transformed twice, and
koma has no way to notice. Frequency conversion (monthly to quarterly
averages), splicing, rebasing and seasonal adjustment are fine: the result is
still a level. `rate()` and `diff(log())` are not.

## Writing koma equations and panels

- **Never write `+ -0.4*x` in an identity, always `- 0.4*x`.** koma parses
  the first form without error but stores the weights wrong (`0.6`,
  `character(0)`, `-0.4` for two components). It has no identity-consistency
  check that would catch it later.
- **Every identity term needs an explicit weight.** `gdp == c + i` parses but
  stores `character(0)` weights; write `1*c + 1*i`.
- **Injected weights `(a/b)*x` are one number, not a time series.** koma sums
  `a` and `b` to annual, shifts them a year and keeps the value at
  `dates$dynamic_weights$end` (mandatory once any identity uses one). The
  expression can only be one series over another: `(a/(a+b))` does not parse.
  See the VJA section below.
- **Declare every stochastic equation before every identity.** koma assumes
  it positionally (`model_identification()` and `estimate_sem()` index the
  first `n_endogenous - n_identities` columns), and an identity declared
  earlier makes it check and estimate the wrong columns with no error.
  `preflight_system()` checks it.
- **No lagged endogenous name may be a string prefix of another.**
  `construct_phi()` prefix-matches unanchored, so `de_debt.L(1)` next to
  `de_debt_ratio.L(1)` silently overwrites a companion-matrix entry: wrong
  forecasts, no error. `preflight_system()` checks it.
- **Every endogenous variable needs a series in `ts_data`, identities
  included**, and koma only says so inside `estimate()`. Build an identity's
  left-hand side in **rate space** with `chain_weighted_index()`, not by
  adding levels: `log(0.6a + 0.4b) != 0.6 log(a) + 0.4 log(b)`. Published
  series (GDP) stay as they are and their identities hold only approximately.
- **Exogenous is an exact set.** koma aborts on an undeclared variable and on
  a declared-but-unused one.
- **`k < T` is a hard constraint.** koma projects every equation on the full
  `k`-column `x_matrix` and draws `Omega` from `riwish(T - k, .)`, with `k = 1
  + (distinct lagged variables) + (exogenous variables)`. `k >= T` fails with
  "computationally singular" plus a negative Wishart df, after the setup. In
  practice treat `df < 4` as infeasible, and expect the share of explosive
  forecast draws to rise steeply as `df` falls. Contemporaneous regressors
  cost nothing; lags and exogenous series cost a column each.
- **koma cannot estimate on an internal `NA`.** It fills ragged edges itself,
  but a hole in the middle fails with an opaque "time series contains internal
  NAs". `internal_gaps()` finds them, `fill_internal_gaps()` interpolates and
  warns.
- **koma requires one attribute set across `ts_data`**; `as_mets()` aborts
  otherwise. `fit_system()` runs `harmonise_panel_attrs()`.
- **A signed series tagged `level`/`diff_log` fails misleadingly**: `log()` of
  a negative is `NaN`, reported as "internal NAs", and an exact zero becomes
  `1e-9` and a huge growth rate. Balances and ratios must be `rate`/`none`.
- **Acceptance rates: the band is 20-60%**, from
  `koma:::get_default_acceptance_prob()` (the vignette's 30-60% is wrong).
  Raise `tau` with `[tau = x]` on the equation; doubling it roughly halves the
  rate. An equation with no contemporaneous endogenous regressor has no
  Metropolis step and `NA` acceptance, and must not be flagged.
- **`model_identification()` consumes RNG**, so one pass proves little;
  `preflight_system()` repeats it across seeds.
- **Never warm-start a changed system with `estimate(estimates = )`.** koma
  compares only the `B` matrices, so a changed contemporaneous (`Gamma`)
  regressor keeps stale draws of the wrong dimension. `fit_system()` always
  estimates cold.
- **koma parallelises per equation and sets no `future::plan()` itself.**
  `fit_system(workers = )` sets one. **Pin BLAS first**: this R links a
  pthread OpenBLAS, and unpinned forks drove the load average to 116 on 16
  cores. Run with `OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1` in the
  environment, not via `Sys.setenv()`.
- **Making a variable endogenous does not give it a transmission channel.**
  Use `contemporaneous_reachability()`, and count what a policy rate reaches
  (prices, GDP) rather than whether it reaches itself.
- **A wrong sign is often not the first symptom; a neighbouring coefficient
  moving is.** A contemporaneous regressor correlated with the dependent
  variable can improve fit by taking variance from the structural term. When
  adding a regressor, check what happened to the coefficients already there.

## Forecasting (`forecasts.R`)

- **koma silently shortens a horizon it cannot deliver** when an exogenous
  series runs out, and only warns. `forecast_system()` extends the exogenous
  series first (`extend_forecast_horizon()`: trailing four-quarter growth for
  levels, flat for rates, zeros for dummies) and aborts if koma still returns
  fewer quarters.
- **Filter explosive draws before taking any quantile**, per horizon in rate
  space and cumulatively in level space, and report the share dropped
  (`forecast_explosive_table()`). koma constrains no draw to be stationary.
- **An explosive forecast can be a within-quarter loop, not a lag.** If a
  loop of contemporaneous coefficients (GDP -> consumption -> GDP) has a gain
  near 1 in some draws, those draws explode from the first quarter with every
  own lag well below 1.
- **koma anchors a level forecast on the last value in the fit's `ts_data`**,
  which is a conditional fill, not data, when the forecast starts after the
  estimation end. `forecast_ankers()` reports the gap.
- **`koma::forecast()`'s draws carry no attributes**; only `$mean`/`$median`
  do. `forecast_draws_level_matrix()` inverts a whole draw matrix as `anker *
  exp(cumsum(x/100))`, tested against `koma::level()`.
- **Set the seed.** koma's forecasts are not reproducible call to call.
- **koma has no impulse-response function.** A shock response is two
  `forecast()` calls, one with `restrictions = `, differenced, with
  `set.seed()` immediately before each so the innovation draws match. Failed
  draws are dropped by subsetting, so the two calls can mispair; compare draw
  counts. `restrictions` take only endogenous variables, in rate space for
  `diff_log` series. The archive branch's `R/spillovers.R` has the full
  machinery.

## Secrets

`.Renviron` is git-ignored. **Never commit it, never print it, and never put a
key or password literal anywhere in the repo**, including messages and error
text. `KOFDB_PASSWORD` is read only through `Sys.getenv()` in
`scratch/vja_data_build.R`. Copy `.Renviron.example` for the variable names.

## Macrobond (desktop licence only)

This project has a Macrobond **desktop** licence and no Web API licence, so
Macrobond data cannot be fetched from R on Linux.

- **Neither of Macrobond's programmatic routes works here.** `MacrobondAPI`, its
  R package, drives the desktop application over COM, so it is a Windows-only
  binary that needs the app installed on the same machine. The REST Web API is
  **licensed separately**: a desktop login is not a Web API credential.
- **The data comes from an Excel macro run on Windows.** `data/raw/` holds the
  macro, `MacrobondDownload.bas`, and one workbook,
  `vja_macrobond_download.xlsx`, whose Keys sheet lists only the series the
  model reads: 141 rows, of which 138 are from Macrobond and 3 are China's KOF
  series. Its Instructions sheet has the refresh steps.
  `scratch/vja_data_build.R` turns it into `data/raw/vja_data.csv` (next
  section). Both files are git-ignored with the rest of `data/raw/`, so keep a
  copy elsewhere.
- **Macrobond's COM interface returns search results as object arrays, which
  VBA cannot hold** (error 458). The macro's `FindMacrobondKeys` reads them
  through `DispCallFunc` instead. PowerShell gets the managed object and fails
  on serialisation, so don't retry it.

## KOF time-series database (`koftsdb`)

`scratch/vja_data_build.R` reads China's VJA series (`cn.kof.vja.*`, schema
`vja`) from `archivedb.kof.ethz.ch` with `koftsdb`, and combines them with the
Macrobond download into `data/raw/vja_data.csv`.

- **`koftsdb` and KOF's build of `timeseriesdb` come from
  `https://kran.kof.ethz.ch`, not GitLab or CRAN**, as the
  `KOF/koma/koma-vja` lockfile records. That repository answers 403 unless you
  are on the KOF VPN, while the database itself is reachable from the general
  ETH network. Install with `options(repos = c(KRAN =
  "https://kran.kof.ethz.ch", CRAN = "https://cloud.r-project.org"))` and then
  `renv::install(c("timeseriesdb", "koftsdb"))`. They are not in `renv.lock`.
- **`RPostgres` builds from source against conda-forge's `libpq`**, added to
  the `koma` env on 2026-10-28. Install through `scratch/Rrun`, which puts the
  env's compilers and `pkg-config` on the path.
- **`db_connection_create()` is `timeseriesdb`'s, and it can only prompt for
  the password inside RStudio.** Anywhere else it stops with "Unable to obtain
  password". The script passes `KOFDB_PASSWORD` (and optionally `KOFDB_USER`)
  from `.Renviron` through `passwd_from_env`, under the same rules as
  `FRED_API_KEY`. The result is cached in `data/cache/kofdb/vja_kofdb.rds`.

## The VJA model (`scratch/vja_model_*.R`)

Nine entities (`de`, `fr`, `it`, `reu`, `us`, `gb`, `jp`, `cn`, `in`) in one
system: 69 stochastic equations and 39 identities, `k = 45`, `T = 87`
(2004Q4-2026Q2), `df = 42`. **The equations are not generated**:
`scratch/vja_model_equations.R` is one hand-written koma string, `[tau = x]`
settings included. `scratch/vja_model_panel.R` builds the panel, and
`scratch/vja_model_build.R` checks, estimates (about two minutes on four
workers) and forecasts into `data/outputs/`. Set `VJA_PREFLIGHT_ONLY=1` to
stop before estimating.

- **Identity weights are koma injected weights, `(a/b)*x`, and koma keeps one
  year of them.** It sums `a` and `b` to annual, shifts them back a year and
  keeps the value at `dates$dynamic_weights$end`. So every weight is the 2025
  ratio, used for the whole sample and the forecast, not an average.
  - **A weight can only be one series over another.** `(a/(a+b))` and
    `(a/a+b)` both fail to parse, so every denominator must be a series in the
    numerator's units.
  - Foreign demand needs `dots_<from>_<to>` (IMF DOTS bilateral exports) and
    `dots_<from>_partners`.
  - `ea_prices` needs `<cc>_nominal_consumption`, because a price index cannot
    weight itself.
  - Those series sit in the panel as `weight_variables`, not model variables.
- **A constructed identity whose own left-hand side divides its weights needs a
  level first.** `<cc>_domestic_demand` and `ea_gdp` are built as the sum of
  their components, to evaluate the weights. They are then rebuilt as a chained
  index from those weights, scaled to the same 2025 level. The weights therefore
  come out unchanged and the identity holds exactly. `vja_add_identity_series()`
  evaluates the weights with `koma:::get_seq_weights()`, the function
  `estimate()` itself calls. The build checks that koma's fitted weights match
  to 1e-12.
- **The trade totals are now observed, so their identities are inexact.** With
  2025 goods shares fixed across twenty years, `gb_exports` misses its
  published growth by up to 7.7pp in one quarter, and GDP's identities by
  1.6-4.3pp. Neither enters an estimated equation as a regressor; only the
  forecast enforces them.
- **`reu_prices` is derived with the same 2025 consumption weights.** That
  makes `ea_prices` reproduce the euro-area HICP's growth to 9e-14, so the ECB
  rule reacts to the published index.
- **Macrobond's euro area changes composition.** Its level against Eurostat's
  fixed EA20 runs from 1.04 in 1995 to 1.000 in 2023-24 and 0.994 in 2026, the
  year Bulgaria joined. Subtracting members from it puts every accession into
  the rest of the euro area as a growth spike. The euro-area entities' national
  accounts therefore come from Eurostat `namq_10_gdp` (`CLV20_MEUR`, SCA).
  Those agree with Macrobond exactly for Germany, and for Italy up to a constant
  factor: Macrobond's Italian aggregates are 2010-reference volumes, 1.03-1.12
  times smaller. France is the exception.
- **France's Macrobond (INSEE) services trade is not Eurostat's P62/P72.** The
  level ratio is 1.33 and the growth correlation 0.78, while France's goods
  exports match exactly.
- **`reu` leaves Ireland and the Netherlands out of its national accounts.**
  Multinationals' IP transactions give Dutch investment a growth sd of 15.8 on
  2004Q3-2019Q4, and Irish investment is worse still. With both in, the bloc's
  investment growth has sd 6.6 and autocorrelation -0.68, and its services
  imports 7.2 and -0.73. Excluding both gets them to 1.8 (autocorrelation
  +0.61) and 1.75; Ireland alone only to 3.8 and 4.8. The IP flow appears as
  investment and as imported services, so it cancels in GDP, which two separate
  equations cannot reproduce. Their prices and yields are not distorted and stay
  in `reu`, and `ea_gdp` covers the euro area less those two.
- **The euro-area HICPs and UK CPI are published unadjusted.** Italy's quarterly
  inflation averages -0.91 / +1.98 / -0.72 / +1.20 by quarter. The panel
  adjusts them with STL on logs; afterwards no series fails a quarter-dummy
  F-test. US CPI is already adjusted. China's KOF trade does not test as
  seasonal, so it is left alone.
- **Three series break where nothing happened.** The OECD's US government
  consumption jumps 7.1% at 2007Q1 and otherwise matches the BEA quantity index
  to 0.01pp, so the panel uses the index, scaled to the OECD level. India's
  OECD QNA GDP, exports and imports all jump 42-50% at 2004Q2, two price bases
  joined without linking, so India starts there. China's KOF series run to
  2031Q4 and are cut at 2026Q2, after which they are KOF's own forecast.
- **COVID is four 0/1 dummies in the real-side equations.** The quarters are
  2020Q1-Q3 and 2021Q2: those whose mean absolute GDP growth across the nine is
  more than three times its 2004Q3-2019Q4 average of 0.93pp (the archived
  stage 2b's rule, which selected the same quarters).
- **Consumption, investment and prices carry no GDP term**, by specification.
  GDP enters only the four Taylor rules and China's and India's equations.
  - **Without 2021Q2, the earlier version with GDP in those equations
    exploded.** UK consumption loaded 1.37 on UK GDP. The within-quarter gain of
    GDP -> consumption and investment -> domestic demand -> GDP, net of
    imports, then reached 1 in 24% of draws, and 30% of all forecast draws
    exploded by horizon 8. **An explosive forecast can be a within-quarter
    loop, not a lag**: no own lag was near 1.
  - With the GDP terms gone that loop does not exist. The share of explosive
    draws at horizon 8 is 0.00.
- **The estimates repeat known weaknesses.** All 69 acceptance rates are in
  band (39-60%, 31 equations at `tau = 2.2`). 69 of 79 expected signs are right.
  - Euro-area long rates do not load on the ECB rate (`it` -0.061 straddling
    zero, `reu` -0.125 with an interval just below zero).
  - The exchange rate barely moves goods exports.
  - Japan's short rate is within 0.15 of zero in 71 of 89 quarters. So its own
    lag is 1.038, and the yen loads -24 on its changes, fitted to the 2024-26
    hikes, during which the yen kept falling.
  - China's and India's constants give sample-average growth (China about
    1.7%/q), with no trend decline.

## Proving a change works

```r
devtools::document()
devtools::test()
```

Run both after any change to `R/`. The tests are network-free: they use the
synthetic two-country system in `tests/testthat/helper-fixtures.R`, itself
written as an explicit koma string. The estimating ones are behind
`skip_on_cran()`, so run them with `devtools::test()`, which sets `NOT_CRAN`.
After a change to the model or its panel, run

```sh
VJA_PREFLIGHT_ONLY=1 OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 Rscript scratch/vja_model_build.R
```

before a full estimation. Do not fetch real data to prove a change works
unless the change is about the data.

renv prints "The project is out-of-sync" on most commands, and `renv.lock`
still records the archived stages' packages. Both are harmless: the
development packages are `Suggests`, which renv's "used" check ignores by
design. Do not move them into `Imports` or widen
`renv::settings$package.dependency.fields()`; that makes `renv::snapshot()`
recurse into every dependency's `Suggests`. To capture a new package,
snapshot it explicitly.
