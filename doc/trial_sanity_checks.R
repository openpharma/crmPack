## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(dplyr)
library(ggplot2)
library(kableExtra)
library(knitr)
library(magrittr)
library(tibble)
library(tidyr)

## ----function-definitions, echo=FALSE-----------------------------------------
library(broom)

setMethod(
  f = "tidy",
  signature = signature(x = "Samples"),
  def = function(x, data = NULL, ...) {
    rv <- lapply(
      names(x@data),
      function(col) {
        tibble(!!as.symbol(col) := x@data[[col]])
      }
    ) %>% bind_cols()
    if (!is.null(data)) {
      rv <- rv %>%
        expand(!!!x@data, Dose = data@doseGrid) %>%
        mutate(
          Z = exp(alpha0 + alpha1 * log(Dose / 56)),
          Prob = Z / (1 + Z)
        )
    }
    rv
  }
)

## ----example, message = FALSE-------------------------------------------------
library(crmPack)

# Define the dose grid and an empty data object
dose_grid <- c(1, 3, 5, 10, 15, 20, 25, 40, 50, 80, 100)
empty_data <- Data(doseGrid = dose_grid)

# Initialize the CRM model.
initial_model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56
)

# Choose the rule for selecting the next dose.
next_best <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Choose the rule for the cohort size.
cohort_size1 <- CohortSizeRange(
  intervals = c(0, 30),
  cohort_size = c(1, 3)
)
cohort_size2 <- CohortSizeDLT(
  dlt_intervals = c(0, 1),
  cohort_size = c(1, 3)
)
cohort_size <- maxSize(cohort_size1, cohort_size2)

# Choose the rule for stopping.
stopping_success1 <- StoppingMinCohorts(nCohorts = 3)
stopping_success2 <- StoppingTargetProb(
  target = c(0.2, 0.35),
  prob = 0.5
)
stopping_futility <- StoppingMinPatients(nPatients = 20)
stopping_trial <- (stopping_success1 & stopping_success2) | stopping_futility

# Choose the rule for dose increments.
increment_rule <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)

# Initialize the design.
design <- Design(
  model = initial_model,
  nextBest = next_best,
  stopping = stopping_trial,
  increments = increment_rule,
  cohortSize = cohort_size,
  data = empty_data,
  startingDose = 3
)

## -----------------------------------------------------------------------------
examine(design) %>% kable()

## -----------------------------------------------------------------------------
no_tox_below_25_data <- Data(
  doseGrid = dose_grid,
  x = c(1, 3, 5, 10, 15, 20, 25),
  y = rep(0, 7),
  ID = 1L:7L,
  cohort = 1L:7L
)

next_dose <- maxDose(increment_rule, no_tox_below_25_data)
next_dose

## -----------------------------------------------------------------------------
revised_increment_rule <- IncrementsRelative(
  intervals = c(0, 20, 80),
  increments = c(1, 0.67, 0.33)
)
tibble(
  HighestDoseUsed = dose_grid,
  MaxPermittedDose = c(
    NA,
    sapply(
      seq_along(dose_grid[-1]),
      function(n) {
        tmp <- dose_grid[-1]
        d <- Data(
          doseGrid = dose_grid,
          x = tmp[1:n],
          y = rep(0, n),
          ID = as.integer(1:n),
          cohort = as.integer(1:n)
        )
        maxDose(revised_increment_rule, d)
      }
    )
  )
) %>%
  mutate(EscalationPermitted = lead(HighestDoseUsed < MaxPermittedDose)) %>%
  kable()

## -----------------------------------------------------------------------------
revised_design <- Design(
  model = initial_model,
  nextBest = next_best,
  stopping = stopping_trial,
  increments = revised_increment_rule,
  cohortSize = cohort_size,
  data = empty_data,
  startingDose = 3
)

## -----------------------------------------------------------------------------
examine(revised_design) %>% kable()

## -----------------------------------------------------------------------------
no_tox_below_50_data <- Data(
  doseGrid = dose_grid,
  x = c(1, 3, 5, 10, 15, 20, 25, 40, 40, 40, 50, 50, 50),
  y = rep(0, 13),
  ID = 1L:13L,
  cohort = c(1L:7L, rep(8L:9L, each = 3))
)

default_mcmc_options <- McmcOptions(burnin = 1000, step = 2, samples = 1000)
no_tox_50_samples <- mcmc(no_tox_below_50_data, initial_model, default_mcmc_options)
recommended_dose <- nextBest(
  next_best,
  doselimit = Inf,
  samples = no_tox_50_samples,
  model = initial_model,
  data = no_tox_below_50_data
)
recommended_dose$value
recommended_dose$probs

maxDose(revised_increment_rule, no_tox_below_50_data)

## ---- error=TRUE--------------------------------------------------------------
# Fitting the min_inf_model is slow.
if (file.exists("minInfModel.Rds")) {
  min_inf_model <- readRDS("minInfModel.Rds")
} else {
  min_inf_model <- MinimalInformative(
    dose_grid,
    56,
    threshmin = 0.1,
    threshmax = 0.4,
    probmin = 0.05,
    probmax = 0.05
  )

  as_tibble(min_inf_model$required) %>%
    add_column(Dose = dose_grid) %>%
    add_column(Type = "Required") %>%
    bind_rows(
      as_tibble(min_inf_model$quantiles) %>%
        add_column(Dose = dose_grid) %>%
        add_column(Type = "Fitted")
    ) %>%
    ggplot() +
      geom_line(aes(x = Dose, y = median, colour = Type), linetype = "solid") +
      geom_line(aes(x = Dose, y = lower, colour = Type), linetype = "dotted") +
      geom_line(aes(x = Dose, y = upper, colour = Type), linetype = "dotted")

  saveRDS(min_inf_model, "minInfModel.Rds")
}

min_inf_model$model@params

## -----------------------------------------------------------------------------
revised_model <-  min_inf_model$model

revised_design1 <- Design(
  model = revised_model,
  nextBest = next_best,
  stopping = stopping_trial,
  increments = revised_increment_rule,
  cohortSize = cohort_size,
  data = empty_data,
  startingDose = 3
)

examine(revised_design1) %>% kable()

## -----------------------------------------------------------------------------
no_tox_data <- Data(
  doseGrid = c(1, 3, 5, 10, 15, 20, 25, 40, 50, 80, 100),
  x = c(c(1, 3, 5, 10, 15, 20), rep(c(25, 40, 50, 80, 100), each = 3)),
  y = rep(0, 21),
  cohort = as.integer(c(1:6, rep(7:11, each = 3))),
  ID = as.integer(1:21)
)

no_tox_samples <- mcmc(no_tox_data, revised_model, default_mcmc_options)

recommended_dose <- nextBest(
  next_best,
  doselimit = Inf,
  samples = no_tox_samples,
  model = initial_model,
  data = no_tox_data
)
recommended_dose$probs

stopTrial(stopping_trial, recommended_dose$value, no_tox_samples, initial_model, no_tox_data)

## -----------------------------------------------------------------------------
revised_stopping_futility <- StoppingMinPatients(nPatients = 40)
revised_stopping_trial <- (stopping_success1 & stopping_success2) | revised_stopping_futility

revised_design2 <- Design(
  model = revised_model,
  nextBest = next_best,
  stopping = revised_stopping_trial,
  increments = revised_increment_rule,
  cohortSize = cohort_size,
  data = empty_data,
  startingDose = 3
)

