
<!-- markdownlint-disable-file -->

<!-- README.md is generated from README.Rmd. Please edit that file -->

# `crmPack`

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/crmPack)](https://CRAN.R-project.org/package=crmPack)
[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![License](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](https://opensource.org/license/apache-2-0)
[![Check
🛠](https://github.com/openpharma/crmPack/actions/workflows/check.yaml/badge.svg)](https://github.com/openpharma/crmPack/actions/workflows/check.yaml)
[![Test
Coverage](https://raw.githubusercontent.com/openpharma/crmPack/_xml_coverage_reports/data/main/badge.svg)](https://raw.githubusercontent.com/openpharma/crmPack/_xml_coverage_reports/data/main/badge.svg)
<!-- badges: end -->

<p align="center">

<img src='man/figures/logo.png' align="right" height="131.5" alt="crmPack-logo"/>
</p>

The goal of `crmPack` is to implement a wide range of model-based dose
escalation designs, ranging from classical and modern continual
reassessment methods (CRMs) based on dose-limiting toxicity endpoints to
dual-endpoint designs taking into account a biomarker/efficacy outcome.
The focus is on Bayesian inference, making it very easy to setup a new
design with your own JAGS code. However, it is also possible to
implement 3+3 designs for comparison or models with non-Bayesian
estimation. The whole package is written in a modular form in the S4
class system, making it very flexible for adaptation to new models,
escalation or stopping rules.

## Installation

You can install the development version of `crmPack` from GitHub with:

``` r
devtools::install_github("openpharma/crmPack")
```

You can install the stable release version of `crmPack` from CRAN with:

``` r
install.packages("crmPack")
```

## Examples

The package vignettes provide information on various aspects of CRM
trial design, implementation, simulation and analysis:

- [Trial
  definition](https://openpharma.github.io/crmPack/main/articles/trial_definition.html)
- [Trial
  analysis](https://openpharma.github.io/crmPack/main/articles/trial_analysis.html)
- [Sanity
  checking](https://openpharma.github.io/crmPack/main/articles/trial_sanity_checks.html)
- [Simulation of operating
  characteristics](https://openpharma.github.io/crmPack/main/articles/trial_simulation.html)
- [Backfill cohort
  simulations](https://openpharma.github.io/crmPack/main/articles/trial_backfill.html)
- [Ordinal CRM
  models](https://openpharma.github.io/crmPack/main/articles/ordinal-crm.html)
- [Extending
  `crmPack`](https://openpharma.github.io/crmPack/main/articles/parallel_computing_with_extensions.html)
- [Tidy `crmPack`
  data](https://openpharma.github.io/crmPack/main/articles/tidy_method.html)
- [Describing `crmPack` objects in Markdown and Quarto
  documents](https://openpharma.github.io/crmPack/main/articles/knit_print.html)
- [Upgrading from `crmPack` version
  1.0](https://openpharma.github.io/crmPack/main/articles/upgrading_from_the_old_crmPack.html)
- Sabanés Bové et al (2019) Model-based Dose Escalation Designs in R
  with `crmPack`. JSS 89:10 [DOI
  10.18637/jss.v089.i10](https://www.jstatsoft.org/article/view/v089i10)
