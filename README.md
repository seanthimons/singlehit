# singlehit

`singlehit` converts the CAMRA dose-response modeling script into a reusable R
package. It fits exponential and approximate beta-Poisson models to grouped
binomial microbial response data and returns tidy results instead of writing
files into the working directory.

In plain terms: you gave subjects increasing doses of a pathogen and counted how
many became infected at each dose. This package estimates the dose-response
curve from those counts and reports numbers like the dose that infects half of
those exposed, along with honest uncertainty bounds.

The package provides:

- validated import of dose, positive-response, and negative-response counts;
- a log-dose trend test;
- binomial maximum-likelihood fits for both mechanistic models, using a
  multi-start optimizer for reliable convergence (see below);
- broom/tidymodels-compatible `tidy()`, `glance()`, and `augment()` methods;
- chi-squared goodness-of-fit and model-deviance comparisons, with AIC/BIC;
- observed-proportion or fitted-model binomial bootstraps;
- percentile parameter intervals and pointwise confidence curves; and
- `ggplot2` model and bootstrap plots.

## Installation

<!--
Not on CRAN yet. Once released:

install.packages("singlehit")
-->

`singlehit` isn't on CRAN yet, so you install it straight from GitHub. To do
that you need a helper package — either **pak** (recommended) or **devtools**.
You only have to install the helper once; after that it stays on your machine.

**Option A — pak (recommended, faster):**

```r
# 1. Install the helper (only needed once, ever)
install.packages("pak")

# 2. Install singlehit from GitHub
pak::pak("seanthimons/singlehit")
```

**Option B — devtools:**

```r
# 1. Install the helper (only needed once, ever)
install.packages("devtools")

# 2. Install singlehit from GitHub
devtools::install_github("seanthimons/singlehit")
```

Run these lines at the R console (the `>` prompt). If you're asked to install or
update other packages, say yes. Once it finishes, load the package like any
other:

```r
library(singlehit)
```

## Input data format

Every entry point (`as_dose_response()`, `read_dose_response()`,
`analyze_dose_response()`) expects **three columns, one row per dose group**:

| Column | Auto-detected aliases | Rule |
|--------|-----------------------|------|
| `dose` | `dose` | numeric, finite, **> 0** |
| `positive` | `positive`, `pos`, `positive_response` | non-negative **whole number** |
| `negative` | `negative`, `neg`, `negative_response` | non-negative **whole number** |

Matching is case- and punctuation-insensitive; override it with
`as_dose_response(data, dose =, positive =, negative =)`. Rows sharing a dose are
summed, and the standardized output adds `total` (`positive + negative`) and
`response` (`positive / total`). For fitting to proceed the data must have
**at least 3 distinct doses** and **more than 1 positive response in total**.

The bundled `ward_rotavirus` dataset shows the required shape:

```r
library(singlehit)

ward_rotavirus
#> # A tibble: 8 x 3
#>       dose positive negative
#>      <dbl>    <dbl>    <dbl>
#> 1    0.009        0        7
#> 2    0.09         0        7
#> 3    0.9          1        6
#> # i 5 more rows

as_dose_response(ward_rotavirus) # standardized: dose, positive, negative, total, response
```

## Example

```r
library(singlehit)

# system.file() locates a data file bundled inside the installed package.
# For your own data, replace this with a path to your file, e.g. "my_data.txt".
ward_path <- system.file("extdata", "Ward_rotavirus.txt", package = "singlehit")
ward <- read_dose_response(ward_path)

analysis <- analyze_dose_response(
  ward,
  bootstrap_times = 10000,
  resample = "observed",
  seed = 2026
)

analysis$assessment # headline verdict: which model is recommended
bootstrap_confint(analysis$bootstraps$beta_poisson) # parameter and ED10/ED50 intervals

ggplot2::autoplot(analysis)
plot_model_overlay(analysis) # all fitted models in one panel, without confidence bands
```

See `vignette("getting-started")` for a narrated start-to-finish walkthrough that
interprets every output.

The [Ward rotavirus model comparison](man/figures/ward-rotavirus-model-overlay.png)
shows the exponential and beta-Poisson curves with observed infection responses.

The [cross-pathogen overview](man/figures/preferred-models-by-outcome-v3.png)
shows 152 preferred models from the v3 QMRA workbook, colored by pathogen group
and grouped into infection, illness, and death outcome panels. Four records have
no supported preferred model. Regenerate it with
`Rscript data-raw/plot-preferred-models.R workbook.xlsx man/figures/preferred-models-by-outcome-v3.png --facet-outcome`.

## Advanced usage

### Exact beta-Poisson model (opt-in)

By default the workflow fits the exponential and approximate beta-Poisson
models. The **exact** beta-Poisson model (confluent hypergeometric form,
parameterized in `alpha`/`beta`) is available as a deliberate opt-in via the
`models` argument. It participates fully in fitting, the N-model comparison,
assessment, consensus, and plotting. Because it is roughly an order of magnitude
slower to fit, its bootstrap replicate count is controlled separately
(`exact_bootstrap_times`, default 10000; set to zero to fit and compare it
without bootstrapping):

```r
analysis <- analyze_dose_response(
  ward,
  models = c("exponential", "beta_poisson", "exact_beta_poisson"),
  bootstrap_times = 10000,       # exponential + approximate beta-Poisson
  exact_bootstrap_times = 10000, # exact beta-Poisson (slower); 0 to skip
  seed = 2026
)
```

### Fitting robustness (multi-start)

When no starting values are supplied, `fit_dose_response()` fits the model from
several candidate starting values and keeps the highest-likelihood result. A
single starting value cannot avoid every start-dependent local optimum across
the range of real dose-response data — for example an exponential fit to
beta-Poisson-shaped data, or a low-infectivity pathogen whose responses appear
only at the highest doses. Validated against the QMRA-wiki reference dataset,
this multi-start strategy reproduces the CAMRA reference fits far more reliably
than a single fixed start. Supplying `start` explicitly (as the bootstrap
warm-start does) uses that value alone, so bootstrap performance is unaffected.

### Pooling multiple datasets

When several trials exist for one pathogen, `poolability_test()` runs the Haas
likelihood-ratio test to decide whether they can be combined: it fits each
dataset separately and the stacked combination, then compares the deviance
difference to a chi-squared distribution (per model). Datasets that pool
significantly worse than when fit separately are kept distinct.
`group_datasets()` extends this to find which trials are mutually poolable.

```r
trials <- list(trial_a = data_a, trial_b = data_b, trial_c = data_c)

poolability_test(trials) # are they poolable? (one row per model)
group_datasets(trials)   # which trials group together (per model)
```

Datasets are combined by **stacking** — each trial's dose groups are kept as
separate binomial observations, so repeated doses across trials are preserved
(matching the QMRA-wiki pooled-experiment convention), rather than summed.

### Parallel bootstraps (mirai)

Bootstrap runs above 1,000 replicates automatically use mirai when it is
installed and show completion progress while results are collected. If no
daemons are already configured, the package starts a temporary
pool using 75% of the machine's detected physical cores and removes that pool
after collecting the result. Supply `workers` to override that default:

```r
analysis <- analyze_dose_response(
  ward,
  bootstrap_times = 10000,
  seed = 2026,
  workers = 12
)
```

Existing mirai daemons are reused and remain under caller control. Use
`backend = "sequential"` to disable automatic parallelization, or configure a
pool explicitly:

```r
mirai::daemons(4)
analysis <- analyze_dose_response(ward, bootstrap_times = 10000)
mirai::daemons(0)
```

Bootstrap samples are generated before parallel dispatch, so the same `seed`
produces identical results with `backend = "sequential"` and
`backend = "mirai"`. Only the model refits are sent to workers.

For a non-blocking bootstrap, start a job and collect it when the result is
needed:

```r
fit <- fit_dose_response(ward, "beta_poisson")
job <- bootstrap_dose_response_async(fit, times = 10000, seed = 2026)

# The R console is available while daemons fit the bootstrap samples.
result <- collect_bootstrap(job)
```

## Development

Development notes and owner-requested workflow follow-ups live in [TODO.md](TODO.md).

Boosterpak remains optional repository-development tooling. It is not a package
dependency and is not needed to install or use `singlehit`.

For development, the original Ward source file remains at
`data/raw/Ward_rotavirus.txt`. The legacy script is retained under `dev/` as a
methodological reference; package functions do not source it or depend on its
global variables.
