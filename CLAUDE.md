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

## Proving a change works

```r
targets::tar_make()
devtools::test()
```

Run both after any change to `R/`, `_targets.R`, or the data pipeline.
`tar_make()` confirms the pipeline still executes top-to-bottom (or fails
at the expected, not-yet-implemented step); `devtools::test()` confirms
the unit tests for whatever you touched now pass. `data_fred.R`,
`data_eamdqd.R`, `panel_build.R`, and `weights.R` are implemented;
`stage1_models.R`, `stage2_system.R`, `stage3_blocks.R`, `diagnostics.R`,
and `scoring.R` are not, so `tar_make()` will still error partway through
by design — that is expected, not a regression; make sure it errors at the
*next* unimplemented stub, not an earlier one you touched.

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
