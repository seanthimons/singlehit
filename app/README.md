# singlehitapp

A [golem](https://thinkr-open.github.io/golem/) Shiny interface to
[singlehit](https://github.com/seanthimons/singlehit). It lives in the `app/`
directory of the same repository but is a separate R package: it depends on
`singlehit` and uses only its exported functions. Nothing in the root package
depends on the app.

## What it does today

- **Data:** the one place to load data. A delimited file, or the bundled Ward
  rotavirus example, becomes the dataset Fit analyses, with optional column
  mapping when names are not recognized automatically. An `.xlsx` QMRA wiki
  workbook, or the bundled sample workbook, is loaded for the Library tab
  instead and leaves the current dataset alone.
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
  residuals. Once a bootstrap has been run, "Draw confidence bands" fills in a
  ribbon plot of the fitted curves on a log dose axis (it drives esquisse's
  inputs, so it was written against esquisse 2.1.0; if a later esquisse renames
  them it switches to the fitted curves but leaves the plot unfilled). Turn on
  "Use {plotly} to render plot" under Options for hover and zoom.
  The interface font is overridden in `inst/app/www/custom.css`.

- **Library:** explores the QMRA wiki workbook (`.xlsx`, "raw data" sheet),
  or the bundled 14-experiment sample, loaded on the Data tab, and is the
  starting point for pooling. Filter experiments by pathogen group,
  pathogen, outcome group, dose unit, host, exposure route, and preferred model,
  and draw every experiment's preferred model on one log-dose plot, faceted and
  coloured by any of those fields. Curves are refit from the counts; experiments
  the workbook reports only as parameters are drawn from those (dashed). Dose
  units differ between studies, so the plot says so unless it is faceted by
  dose unit or filtered to one. "Send to Data and Fit" makes one experiment the current dataset,
  with its study id, host, unit, and endpoint; experiments
  singlehit cannot fit (fewer than three doses, parameters only) say why
  instead. Free-text host, route, and outcome spellings are grouped by simple
  rules in `R/fct_library.R`. The full workbook is not in the repository; the
  sample is rebuilt from it with `dev/make_qmra_sample.R`. The data come from
  the [QMRA wiki](https://qmrawiki.org) and the studies it cites; each sample
  row keeps its reference, DOI, and wiki link. Choose several experiments in
  the "Pooling set" and press "Test poolability" to open them on the Pooling
  tab.

- **Pooling:** tests whether the chosen experiments can share one curve, with
  singlehit's likelihood-ratio `poolability_test()` and a pooled fit from
  `fit_pooled_dose_response()`. Before running you must write why the
  experiments belong together, and every run is kept in a session log (with
  the rationale, the warnings you ran past, and the p-values) that downloads
  as CSV and counts how many unadjusted tests have been run. The log lasts the whole
  session, across workbooks, and the rationale clears whenever the pooling set
  or workbook changes, so each set gets its own justification. Results show the test with the caveat that "not rejected"
  is not evidence of equivalence, each experiment's own fit against the pooled
  fit, the spread of the parameter estimates, and the wiki's own pooled entry
  when the chosen experiments match one (21 + 23 for Burkholderia mallei
  reproduces the wiki's "21,23"). The data-driven `group_datasets()` and
  `poolability_combinations()` are deliberately not offered: picking groups
  from the fits is the forking path described in #20.

  **The checks are a tunable pinch point.** They live in one table,
  `pool_rules` in `R/fct_pooling.R`, and are a first, loose cut to be revised.
  Only data problems block a test: fewer than two experiments, an experiment
  that cannot be fitted, double counting (an entry the wiki already pooled,
  the same experiment twice, identical counts), and dose units that differ or
  are not reported (singlehit itself also refuses mixed units). Different
  outcomes, hosts, routes, or pathogens only warn, because the wiki itself
  pools across hosts and routes.

Hierarchical (family-level) models, bootstrap intervals for pooled fits, and
export are planned.

## License

The app is licensed under GPL-3 (see `LICENSE.md`) because it imports esquisse,
which is GPL-3. The `singlehit` package itself stays MIT.

## Requires singlehit 0.3.0 or later

The Bootstrap and Plots tabs use `singlehit` functions (`add_bootstrap()`,
`bootstrap_progress()`) that were added in 0.3.0, so install `singlehit` 0.3.0
or later (for example from GitHub with `pak::pak("seanthimons/singlehit")`).

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
