# singlehitapp

A [golem](https://thinkr-open.github.io/golem/) Shiny interface to
[singlehit](https://github.com/seanthimons/singlehit). It lives in the `app/`
directory of the same repository but is a separate R package: it depends on
`singlehit` and uses only its exported functions. Nothing in the root package
depends on the app.

## Running during development

From the repository root:

```r
setwd("app")
source("dev/run_dev.R")
```

`run_dev.R` loads the in-repo `singlehit` source (exports only) and then the app,
so changes to either package are picked up on the next run.

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
