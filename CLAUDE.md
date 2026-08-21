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
the unit tests for whatever you touched now pass. Until `data_fred.R`,
`data_eamdqd.R`, and `panel_build.R` are implemented, `tar_make()` will
error partway through by design — that is expected, not a regression; make
sure it errors at the *next* unimplemented stub, not an earlier one you
touched.

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
