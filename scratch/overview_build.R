# Build the cross-stage summary the presentation deck reads.
#
#   OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 Rscript scratch/overview_build.R
#
# The deck needs one row per stage: how many equations, `k`, `T`, `df`, and the
# estimation window. Those live on the fitted systems, which are 4-90 MB
# apiece, so they are extracted here once into a few KB rather than loaded by
# the .qmd at render time. One system at a time, dropped and gc()'d between.

devtools::load_all(".", quiet = TRUE)

out_dir <- file.path("data", "cache", "overview")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_path <- file.path(out_dir, "shapes.rds")

# `pick` returns list(sys_eq, panel, dates) from whatever shape the cache has.
systems <- list(
  list(stage = "stage1", representative = "de", label = "Stage 1", scope = "11 countries, estimated separately",
       file = file.path("data", "cache", "stage1", "de.rds"),
       pick = function(o) list(sys_eq = o$sys_eq, panel = o$ts_data, dates = o$dates),
       per_country = TRUE),
  list(stage = "stage2a", representative = "de", label = "Stage 2a", scope = "DE + FR, one joint system",
       file = file.path("data", "cache", "stage2a", "pilot_tuned.rds"),
       pick = function(o) list(sys_eq = o$sys_eq, panel = o$panel, dates = o$fit$dates)),
  list(stage = "stage2b", representative = "de", label = "Stage 2b", scope = "11 countries, one joint system",
       file = file.path("data", "cache", "stage2b", "full_tuned.rds"),
       pick = function(o) list(sys_eq = o$sys_eq, panel = o$panel, dates = o$fit$dates)),
  list(stage = "stage2c", representative = "de", label = "Stage 2c", scope = "11 countries, four refinements",
       file = file.path("data", "cache", "stage2c", "noic_full_tuned.rds"),
       pick = function(o) list(sys_eq = o$sys_eq, panel = o$panel, dates = o$fit$dates)),
  list(stage = "stage2d", representative = "de", label = "Stage 2d", scope = "6 entities: DE FR IT US CN + REU bloc",
       file = file.path("data", "cache", "stage2d", "tuned.rds"),
       pick = function(o) list(sys_eq = o$sys_eq,
                               panel = readRDS(file.path("data", "cache", "stage2d", "panel.rds")),
                               dates = o$fit$dates)),
  list(stage = "stage3a", representative = "de", label = "Stage 3a", scope = "Stage 2b + German labour/price block",
       file = file.path("data", "cache", "stage3a", "fit_a_final.rds"),
       pick = function(o) list(sys_eq = o$sys_eq, panel = o$panel, dates = o$fit$dates)),
  list(stage = "stage3b", representative = "de", label = "Stage 3b", scope = "Stage 3a + external/fiscal/financial",
       file = file.path("data", "cache", "stage3b", "fit_financial.rds"),
       pick = function(o) list(sys_eq = o$sys_eq, panel = o$panel, dates = o$fit$dates))
)

templates <- list()

rows <- lapply(systems, function(s) {
  if (!file.exists(s$file)) {
    cli::cli_warn("{s$stage}: {.file {s$file}} not found -- skipped.")
    return(NULL)
  }
  cli::cli_inform("{s$stage}")
  gc(full = TRUE)
  obj <- readRDS(s$file)
  p <- s$pick(obj)
  rm(obj)
  gc(full = TRUE)

  # The country-generic template: take one representative entity's equations
  # and replace its own prefix with `cc`. Derived from the fitted system rather
  # than transcribed, so a deck built from it cannot drift from what was
  # actually estimated.
  rep_cc <- s$representative
  generic <- grep(paste0("^", rep_cc, "_"), p$sys_eq$equations, value = TRUE)
  generic <- gsub(paste0("\\b", rep_cc, "_"), "cc_", generic)
  templates[[s$stage]] <<- generic

  k <- length(p$sys_eq$total_exogenous_variables)
  t_obs <- estimation_length(p$panel, p$dates)
  out <- data.frame(
    stage = s$stage, label = s$label, scope = s$scope,
    stochastic = length(p$sys_eq$stochastic_equations),
    identities = length(p$sys_eq$identities),
    equations = length(p$sys_eq$endogenous_variables),
    exogenous = length(p$sys_eq$exogenous_variables),
    k = k, t_obs = t_obs, df = t_obs - k,
    est_start = sprintf("%dQ%d", p$dates$estimation$start[1], p$dates$estimation$start[2]),
    est_end = sprintf("%dQ%d", p$dates$estimation$end[1], p$dates$estimation$end[2]),
    per_country = isTRUE(s$per_country),
    stringsAsFactors = FALSE
  )
  rm(p)
  gc(full = TRUE)
  out
})

shapes <- do.call(rbind, Filter(Negate(is.null), rows))
# Stage 1 is estimated one country at a time, so its shape is PER COUNTRY --
# the eleven satellites never share an x_matrix. Recording that here stops the
# deck comparing its k against a joint system's as though they were the same
# quantity.
shapes$scope[shapes$per_country] <- paste0(shapes$scope[shapes$per_country], " (k, T, df are per country)")

saveRDS(shapes, out_path)
saveRDS(templates, file.path(out_dir, "templates.rds"))
print(shapes[, c("stage", "stochastic", "identities", "k", "t_obs", "df", "est_start", "est_end")])
cli::cli_inform("Written to {.file {out_path}}.")
