# singlehitapp

A [golem](https://thinkr-open.github.io/golem/) Shiny interface to
[singlehit](https://github.com/seanthimons/singlehit). It lives in the `app/`
directory of the same repository but is a separate R package: it depends on
`singlehit` and uses only its exported functions. Nothing in the root package
depends on the app.

## What it does today

- **Data:** upload a delimited file or use the bundled Ward rotavirus example, with optional
  column mapping when names are not recognized automatically.
- **Fit:** choose at least two models (exponential, beta-Poisson, exact
  beta-Poisson), see the dose-trend test, parameter estimates, and goodness of
  fit. The analysis runs without bootstrapping.
- **Compare:** the primary recommendation (absolute fit plus the chi-squared
  deviance comparison), the assessment table, and the experimental consensus
  vote, labelled as such.

- **Bootstrap:** percentile confidence intervals, run in the background on a
  shared pool of workers (a progress bar per model, a Cancel button, and a
  default of 1,000 replicates; the exact model has its own smaller budget, as
  it is much slower). When a run finishes, confidence bands appear on the
  Plots and Explore tabs.
- **Plots:** the model overlay, one panel per model, or a single model, with PNG
  download. Confidence bands appear in the panel and single-model views once a
  bootstrap has been run; the overlay never draws bands.

- **Explore:** build your own plots interactively with
  [esquisse](https://dreamrs.github.io/esquisse/) by dragging variables onto
  aesthetics. Choose the observed dose groups, the fitted curves, or the
  residuals. Turn on "Use {plotly} to render plot" under Options for hover and zoom.
  The interface font is overridden in `inst/app/www/custom.css`.

Pooling and export are planned.

## License

The app is licensed under GPL-3 (see `LICENSE.md`) because it imports esquisse,
which is GPL-3. The `singlehit` package itself stays MIT.

## Requires the development version of singlehit

The Bootstrap and Plots tabs use `singlehit` functions (`add_bootstrap()`,
`bootstrap_progress()`) that are on `main` but not in the released 0.2.0, so
install `singlehit` from GitHub until the next release.

## Running during development

From the repository root:

```r
setwd("app")
source("dev/run_dev.R")
```

`run_dev.R` loads the in-repo `singlehit` source (exports only) and then the app,
so changes to either package are picked up on the next run.

## Running the tests

From `app/`:

```r
source("dev/run_tests.R")
```

This loads the in-repo `singlehit` first, so the tests do not need it installed.

## Running against an installed singlehit

```r
pak::pak("seanthimons/singlehit")
pak::pak("seanthimons/singlehit/app")
singlehitapp::run_app()
```

## Deploying

Deploy from this directory (`app/`) with `rsconnect::deployApp()`; `app.R` is
the entry point. Install `singlehit` from GitHub first so the deployment
records where to fetch it from.
