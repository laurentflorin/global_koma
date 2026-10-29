# koma's own plots of the VJA model's output, as HTML pages.
#
# Run from the project root after scratch/vja_model_build.R:
#
#   OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 Rscript scratch/vja_model_plots.R
#
# Writes data/outputs/plots/, opened through index.html:
# - forecast_<entity>.html: koma's forecast chart (plot() on a koma_forecast)
#   for each of the entity's variables. Bars are quarterly growth, the line is
#   the level (right axis), the figures above the chart are annual growth, and
#   the fan is the 50/80/90% band of the predictive draws.
# - diagnostics_<entity>.html: koma's MCMC diagnostics for each of the
#   entity's equations: trace_plot(), running_mean_plot() and acf_plot().
#
# The forecast is koma::forecast() on the cached fit, with the exogenous series
# extended exactly as forecast_system() does and the same seed, so the draws
# are the ones scratch/vja_model_build.R summarised. It is cached in
# data/outputs/koma_forecast.rds, and redone whenever fit.rds is newer.
#
# Needs plotly, ggplot2 and htmltools.

suppressMessages(devtools::load_all(".", quiet = TRUE))
source(file.path("scratch", "vja_model_panel.R"))

model_dir <- file.path("data", "outputs")
out_dir <- file.path(model_dir, "plots")
dir.create(file.path(out_dir, "diagnostics"), recursive = TRUE, showWarnings = FALSE)

fit <- readRDS(file.path(model_dir, "fit.rds"))
panel <- readRDS(file.path(model_dir, "panel.rds"))

fc_path <- file.path(model_dir, "koma_forecast.rds")
if (file.exists(fc_path) && file.mtime(fc_path) > file.mtime(file.path(model_dir, "fit.rds"))) {
  fc <- readRDS(fc_path)
  cli::cli_inform("cached: {.file {fc_path}}")
} else {
  ext <- extend_forecast_horizon(fit, panel, 8)
  set.seed(20260101)
  fc <- suppressWarnings(koma::forecast(
    ext$fit, dates = ext$dates,
    options = list(approximate = FALSE, probs = c(0.05, 0.1, 0.25, 0.75, 0.9, 0.95))
  ))
  saveRDS(fc, fc_path)
}

# Variables per page, in reading order: what the entity's identities and
# equations are about, then what drives them.
concepts <- c(
  "gdp", "domestic_demand", "consumption", "investment", "government",
  "exports", "goods_exports", "services_exports",
  "imports", "goods_imports", "services_imports", "foreign_demand",
  "prices", "short_rate", "long_rate", "neer"
)
entity_names <- c(
  ea = "Euro area (shared rate, exchange rate and aggregates)", de = "Germany", fr = "France",
  it = "Italy", reu = "Rest of the euro area (without Ireland and the Netherlands)",
  us = "United States", gb = "United Kingdom", jp = "Japan", cn = "China", "in" = "India"
)
available <- names(fc$mean)
pages <- lapply(stats::setNames(names(entity_names), names(entity_names)), function(e) {
  v <- paste0(e, "_", concepts)
  v[v %in% available]
})

section <- function(title, ...) {
  htmltools::tags$section(htmltools::tags$h2(id = title, title), ...)
}
page <- function(title, intro, body) {
  htmltools::tagList(
    htmltools::tags$head(htmltools::tags$title(title), htmltools::tags$style(
      "body{font-family:Arial,sans-serif;max-width:1200px;margin:2em auto;padding:0 1em;color:#222}",
      "h2{border-top:1px solid #ddd;padding-top:1em;font-size:1.1em}",
      "nav a{margin-right:1em} img{max-width:100%;display:block;margin:0.5em 0}",
      ".note{font-size:0.9em;color:#555;margin-top:0}"
    )),
    htmltools::tags$p(htmltools::tags$a(href = "index.html", "Index")),
    htmltools::tags$h1(title), htmltools::tags$p(intro), body
  )
}

# --- forecast pages -------------------------------------------------------------
origin <- quarter_label(fc$mean[[1]] |> stats::time() |> utils::head(1))
for (e in names(pages)) {
  widgets <- lapply(pages[[e]], function(v) {
    p <- suppressWarnings(plot(fc, variables = v, fan = TRUE))
    p$sizingPolicy$defaultHeight <- 450
    # koma draws every series as if it were a level with a growth rate. For a
    # rate/none series (interest rates) that misleads in three ways, so say so.
    note <- if (identical(attr(fc$mean[[v]], "method"), "none")) {
      htmltools::tags$p(class = "note", paste(
        "An interest rate, which koma draws like a growth series: the bars and the line are both the rate",
        "(left and right axis), the figure under each year is the four quarterly rates added up and means",
        "nothing, and the bars cover the lower half of the fan."
      ))
    }
    section(v, p, note)
  })
  htmltools::save_html(
    page(
      paste("VJA forecast:", entity_names[[e]]),
      paste0("koma's forecast chart, eight quarters from ", origin, ". Bars: quarterly growth ",
             "(for interest rates, the rate itself); line: level, right axis; figures above: annual growth; ",
             "fan: 50/80/90% bands of the 1000 predictive draws. Government consumption is exogenous and ",
             "extended at its trailing four-quarter growth."),
      widgets
    ),
    file = file.path(out_dir, paste0("forecast_", e, ".html")), libdir = "lib"
  )
  cli::cli_inform("forecast_{e}.html: {length(widgets)} chart{?s}")
}

# --- MCMC diagnostics -----------------------------------------------------------
equations <- fit$sys_eq$stochastic_equations
diag_pages <- lapply(stats::setNames(names(entity_names), names(entity_names)), function(e) {
  equations[startsWith(equations, paste0(e, "_"))]
})
diag_pages <- diag_pages[lengths(diag_pages) > 0]
plot_types <- c(trace = "trace_plot", running_mean = "running_mean_plot", acf = "acf_plot")
for (e in names(diag_pages)) {
  sections <- lapply(diag_pages[[e]], function(eq) {
    images <- lapply(names(plot_types), function(type) {
      file <- file.path("diagnostics", paste0(type, "_", eq, ".png"))
      png <- file.path(out_dir, file)
      # Drawn once per fit: an image newer than fit.rds is kept.
      if (!file.exists(png) || file.mtime(png) < file.mtime(file.path(model_dir, "fit.rds"))) {
        p <- suppressWarnings(getExportedValue("koma", plot_types[[type]])(fit, variables = eq))
        n_panels <- length(unique(ggplot2::ggplot_build(p)$layout$layout$PANEL))
        suppressWarnings(ggplot2::ggsave(png, p, width = 11,
                                         height = 1.2 + 2 * ceiling(n_panels / 3), dpi = 90))
      }
      htmltools::tags$img(src = file, alt = paste(type, eq))
    })
    section(eq, images)
  })
  htmltools::save_html(
    page(
      paste("VJA estimation diagnostics:", entity_names[[e]]),
      "koma's MCMC diagnostics for every coefficient (beta: predetermined and exogenous terms; gamma: contemporaneous endogenous terms; sigma: the error variance), over the 1000 kept draws: the trace, the running mean, and the autocorrelation of the draws.",
      sections
    ),
    file = file.path(out_dir, paste0("diagnostics_", e, ".html")), libdir = "lib"
  )
  cli::cli_inform("diagnostics_{e}.html: {length(sections)} equation{?s}")
}

# --- index ---------------------------------------------------------------------------
explosive <- forecast_explosive_table(readRDS(file.path(model_dir, "forecast.rds"))$paths)
links <- lapply(names(entity_names), function(e) {
  htmltools::tags$tr(
    htmltools::tags$td(entity_names[[e]]),
    htmltools::tags$td(htmltools::tags$a(href = paste0("forecast_", e, ".html"), "forecast")),
    htmltools::tags$td(if (e %in% names(diag_pages)) {
      htmltools::tags$a(href = paste0("diagnostics_", e, ".html"), "diagnostics")
    } else {
      "no equations of its own"
    })
  )
})
htmltools::save_html(
  htmltools::tagList(
    htmltools::tags$head(htmltools::tags$title("VJA model: koma plots"), htmltools::tags$style(
      "body{font-family:Arial,sans-serif;max-width:900px;margin:2em auto;padding:0 1em;color:#222}",
      "td{padding:0.3em 1.2em 0.3em 0}"
    )),
    htmltools::tags$h1("VJA model: koma plots"),
    htmltools::tags$p(sprintf(
      "Estimated on %s to %s (%d stochastic equations, %d identities); forecast eight quarters from %s. Share of forecast draws dropped as explosive at horizon 8: %.2f.",
      paste(fit$dates$estimation$start, collapse = "Q"), paste(fit$dates$estimation$end, collapse = "Q"),
      length(fit$sys_eq$stochastic_equations), length(fit$sys_eq$identities), origin,
      mean(explosive$h_last)
    )),
    htmltools::tags$table(links)
  ),
  file = file.path(out_dir, "index.html"), libdir = "lib"
)
cli::cli_inform("Open {.file {file.path(out_dir, 'index.html')}}.")
