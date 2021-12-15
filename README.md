
<!-- markdownlint-disable-file -->
<!-- README.md is generated from README.Rmd. Please edit that file -->
# crmPack
<p align="center">
<img src='man/figures/logo.png' align="right" height="131.5" alt="crmPack-logo"/>
</p>

[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)  

The goal of crmPack is to implement a wide range of model-based dose
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

You can install the development version of crmPack from github with:

``` r
devtools::install_github("Roche/crmPack")
```

You can install the stable release version of crmPack from CRAN with:

``` r
install.packages("crmPack")
```

## Example

This is a basic example which shows how to run simulations from a CRM
with a 2-parameter logistic regression model, using a log normal prior
distribution, and custom cohort size, stopping and maximum increments
rules:

``` r
library(crmPack)

# Define the dose grid.
empty_data <- Data(doseGrid = c(1, 3, 5, 10, 15, 20, 25, 40, 50, 80, 100))

# Initialize the CRM model.
model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  refDose = 56
)

# Choose the rule for selecting the next dose.
my_next_best <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  maxOverdoseProb = 0.25
)

# Choose the rule for the cohort size.
my_size_1 <- CohortSizeRange(
  intervals = c(0, 30),
  cohortSize = c(1, 3)
)
my_size_2 <- CohortSizeDLT(
  DLTintervals = c(0, 1),
  cohortSize = c(1, 3)
)
my_size <- maxSize(my_size_1, my_size_2)

# Choose the rule for stopping.
my_stopping_1 <- StoppingMinCohorts(nCohorts = 3)
my_stopping_2 <- StoppingTargetProb(
  target = c(0.2, 0.35),
  prob = 0.5
)
my_stopping_3 <- StoppingMinPatients(nPatients = 20)
my_stopping <- (my_stopping_1 & my_stopping_2) | my_stopping_3

# Choose the rule for dose increments.
my_increments <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)

# Initialize the design.
design <- Design(
  model = model,
  nextBest = my_next_best,
  stopping = my_stopping,
  increments = my_increments,
  cohortSize = my_size,
  data = empty_data,
  startingDose = 3
)

# Define the true function.
my_truth <- function(dose) {
  alpha0 <- 7
  alpha1 <- 8
  ref_dose <- 56
  stand_log_dose <- log(dose / ref_dose)
  plogis(alpha0 + alpha1 * stand_log_dose)
}

# Run the simulation on the desired design.
# We only generate 1 trial outcome here for illustration, for the actual study
# this should be increased of course.
options <- McmcOptions(
  burnin = 100,
  step = 1,
  samples = 2000
)
time <- system.time(my_sims <- simulate(design,
  args = NULL,
  truth = my_truth,
  nsim = 1,
  seed = 819,
  mcmcOptions = options,
  parallel = FALSE
))[3]
```
