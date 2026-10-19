# singlehitapp

A [golem](https://thinkr-open.github.io/golem/) Shiny interface to
[singlehit](https://github.com/seanthimons/singlehit). It lives in the `app/`
directory of the same repository but is a separate R package: it depends on
`singlehit` and uses only its exported functions. Nothing in the root package
depends on the app.

## What it does today

- **Data:** upload a delimited file or use the bundled example, with optional
  column mapping when names are not recognized automatically.
- **Fit:** choose at least two models (exponential, beta-Poisson, exact
  beta-Poisson), see the dose-trend test, parameter estimates, and goodness of
  fit. The analysis runs without bootstrapping.
- **Compare:** the primary recommendation (absolute fit plus the chi-squared
  deviance comparison), the assessment table, and the experimental consensus
  vote, labelled as such.

Plots, bootstrap uncertainty, pooling, and export are planned.

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
