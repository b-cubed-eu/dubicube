# dubicube <a href="https://b-cubed-eu.github.io/dubicube/"><img src="man/figures/logo.png" align="right" height="139" alt="dubicube website" /></a>

<!-- badges: start -->

[![repo
status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Release](https://img.shields.io/github/release/b-cubed-eu/dubicube.svg)](https://github.com/b-cubed-eu/dubicube/releases)
[![dubicube status
badge](https://b-cubed-eu.r-universe.dev/dubicube/badges/version)](https://b-cubed-eu.r-universe.dev/dubicube)
[![CRAN
status](https://www.r-pkg.org/badges/version/dubicube)](https://CRAN.R-project.org/package=dubicube)
[![R-CMD-check](https://github.com/b-cubed-eu/dubicube/actions/workflows/check_on_different_r_os.yml/badge.svg)](https://github.com/b-cubed-eu/dubicube/actions/workflows/check_on_different_r_os.yml)
[![codecov](https://codecov.io/gh/b-cubed-eu/dubicube/branch/main/graph/badge.svg)](https://app.codecov.io/gh/b-cubed-eu/dubicube/)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14850237.svg)](https://doi.org/10.5281/zenodo.14850237)
[![name status
badge](https://b-cubed-eu.r-universe.dev/badges/:name?color=6CDDB4)](https://b-cubed-eu.r-universe.dev/)
[![funder](https://badgen.net/static/funder/European%20Union/f2a)](https://doi.org/10.3030/101059592)

<!-- badges: end -->

The **dubicube** package aims to deliver measures for assessing the
applicability of biodiversity data cubes, whether for general use or
specific biodiversity indicators. These measures facilitate data
exploration by providing insights into data quality and reliability.
Additionally, the package includes functions for calculating indicator
uncertainty using bootstrapping, as well as tools for interpreting and
visualising uncertainty in biodiversity indicators derived from
occurrence cubes.

## Installation

Install **dubicube** in R:

```r
install.packages("dubicube", repos = c("https://b-cubed-eu.r-universe.dev", "https://cloud.r-project.org"))
```

You can install the development version from
[GitHub](https://github.com/) with:

```r
# install.packages("remotes")
remotes::install_github("b-cubed-eu/dubicube")
```

## The role of dubicube in the indicator calculation workflow

The functionality of the **dubicube** package is useful throughout the occurrence cube indicator calculation workflow.
Occurrence cubes can be created from GBIF data using the [**rgbif**](https://github.com/ropensci/rgbif/) package.
They are processed using the process_cube() function from the [**b3gbi**](https://github.com/b-cubed-eu/b3gbi) package.
This ensures data standardisation and verifies that the cube's format is correct.
**dubicube** facilitates data exploration and filtering (1) which is an iterative process with cube generation and processing.
After a number of iterations, data evaluation is successful and the final data cube can be used for indicator calculation.
Indicator calculation packages can use **dubicube** as a dependency for uncertainty interval calculation via bootstrapping (2) but the package can also be used on its own.
Finally, the package provides tools and tutorials to help with indicator visualisation and interpretation (3).

<img src="man/figures/dubicube-flow.png" alt="dubicube indicator calculation workflow." width="80%">

## Key Features

The **dubicube** package offers:

### 🔍 1. Data Exploration & Variability Assessment

Gain insights into the structure and sensitivity of biodiversity data
cubes.

- `diagnose_cube()`  
  Evaluate the quality of a processed data cube by applying a
  set of diagnostic rules. 
- `filter_cube()`  
  Remove observations from a processed cube that do not meet
  defined quality criteria.  
  📘 [Read the tutorial
  →](https://b-cubed-eu.github.io/dubicube/articles/diagnostics-and-filtering.html)

- `cross_validate_cube()`  
  Perform cross-validation (leave-one-out or k-fold) to assess
  group-level sensitivity of indicators and evaluate how individual
  categories influence results.  
  📘 [Read the tutorial
  →](https://b-cubed-eu.github.io/dubicube/articles/group-level-sensitivity.html)

### 📈 2. Estimating Indicator Uncertainty

Use bootstrap methods to understand variability, bias, and confidence in
your indicators.

- `bootstrap_cube()`  
  Create bootstrap replicates to estimate indicator variability, bias,
  and standard error.  
  📘 [Read the tutorial
  →](https://b-cubed-eu.github.io/dubicube/articles/bootstrap-method-cubes.html)

- `calculate_bootstrap_ci()`  
  Compute confidence intervals (percentile, BCa, normal, basic), with
  optional transformations and bias correction.  
  📘 [Read the tutorial
  →](https://b-cubed-eu.github.io/dubicube/articles/bootstrap-interval-calculation.html)

### 🧠 3. Interpretation & Visualisation

Put your results in context with reference values and uncertainty
thresholds.

- `add_effect_classification()`  
  Classify indicator trends (e.g. increase, stable, decrease) by
  comparing confidence intervals to thresholds.  
  📘 [Read the tutorial
  →](https://b-cubed-eu.github.io/dubicube/articles/effect-classification.html)

- **Spatial and temporal interpretation**  
  Learn how to visualise and assess patterns across space and time using
  indicator uncertainty.  
  📘 [Best practices for temporal trends
  →](https://b-cubed-eu.github.io/dubicube/articles/visualising-temporal-trends.html)  
  📘 [Best practices for spatial trends
  →](https://b-cubed-eu.github.io/dubicube/articles/visualising-spatial-trends.html)

---

🔗 Learn more at our [website](https://b-cubed-eu.github.io/dubicube/) or explore the [documentation](https://b-cubed-eu.github.io/dubicube/reference/).
