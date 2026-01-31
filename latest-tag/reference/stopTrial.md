# Stop the trial?

**\[stable\]**

This function returns whether to stop the trial.

**\[experimental\]**

**\[stable\]**

**\[stable\]**

**\[stable\]**

**\[stable\]**

**\[stable\]**

**\[stable\]**

**\[stable\]**

**\[stable\]**

Stopping rule based precision of the MTD estimation. The trial is
stopped, when the MTD can be estimated with sufficient precision. The
criteria is based on the robust coefficient of variation (CV) calculated
from the posterior distribution. The robust CV is defined
`mad(MTD) / median(MTD)`, where `mad` is the median absolute deviation.

Stopping based based on the lowest non placebo dose. The trial is
stopped when the lowest non placebo dose meets the Hard Safety Rule,
i.e. it is deemed to be overly toxic. Stopping is based on the observed
data at the lowest dose level using a Bin-Beta model based on DLT
probability.

**\[stable\]**

**\[experimental\]**

**\[experimental\]**

**\[experimental\]**

**\[stable\]**

**\[stable\]**

**\[stable\]**

**\[stable\]**

## Usage

``` r
stopTrial(stopping, dose, samples, model, data, ...)

# S4 method for class 'StoppingMissingDose,numeric,ANY,ANY,Data'
stopTrial(stopping, dose, samples, model, data, ...)

# S4 method for class 'StoppingList,ANY,ANY,ANY,ANY'
stopTrial(stopping, dose, samples, model, data, ...)

# S4 method for class 'StoppingAll,ANY,ANY,ANY,ANY'
stopTrial(stopping, dose, samples, model, data, ...)

# S4 method for class 'StoppingAny,ANY,ANY,ANY,ANY'
stopTrial(stopping, dose, samples, model, data, ...)

# S4 method for class 'StoppingCohortsNearDose,numeric,ANY,ANY,Data'
stopTrial(stopping, dose, samples, model, data, ...)

# S4 method for class 'StoppingPatientsNearDose,numeric,ANY,ANY,Data'
stopTrial(stopping, dose, samples, model, data, ...)

# S4 method for class 'StoppingMinCohorts,ANY,ANY,ANY,Data'
stopTrial(stopping, dose, samples, model, data, ...)

# S4 method for class 'StoppingMinPatients,ANY,ANY,ANY,Data'
stopTrial(stopping, dose, samples, model, data, ...)

# S4 method for class 'StoppingTargetProb,numeric,Samples,GeneralModel,ANY'
stopTrial(stopping, dose, samples, model, data, ...)

# S4 method for class 'StoppingMTDdistribution,numeric,Samples,GeneralModel,ANY'
stopTrial(stopping, dose, samples, model, data, ...)

# S4 method for class 'StoppingMTDCV,numeric,Samples,GeneralModel,ANY'
stopTrial(stopping, dose, samples, model, data, ...)

# S4 method for class 'StoppingLowestDoseHSRBeta,numeric,Samples,ANY,ANY'
stopTrial(stopping, dose, samples, model, data, ...)

# S4 method for class 'StoppingTargetBiomarker,numeric,Samples,DualEndpoint,ANY'
stopTrial(stopping, dose, samples, model, data, ...)

# S4 method for class 'StoppingSpecificDose,numeric,ANY,ANY,Data'
stopTrial(stopping, dose, samples, model, data, ...)

# S4 method for class 'StoppingHighestDose,numeric,ANY,ANY,Data'
stopTrial(stopping, dose, samples, model, data, ...)

# S4 method for class 'StoppingOrdinal,numeric,ANY,LogisticLogNormalOrdinal,DataOrdinal'
stopTrial(stopping, dose, samples, model, data, ...)

# S4 method for class 'StoppingOrdinal,numeric,ANY,ANY,ANY'
stopTrial(stopping, dose, samples, model, data, ...)

# S4 method for class 'StoppingExternal,numeric,ANY,ANY,ANY'
stopTrial(stopping, dose, samples, model, data, external, ...)

# S4 method for class 'StoppingTDCIRatio,ANY,Samples,ModelTox,ANY'
stopTrial(stopping, dose, samples, model, data, ...)

# S4 method for class 'StoppingTDCIRatio,ANY,missing,ModelTox,ANY'
stopTrial(stopping, dose, samples, model, data, ...)

# S4 method for class 'StoppingMaxGainCIRatio,ANY,Samples,ModelTox,DataDual'
stopTrial(
  stopping,
  dose,
  samples,
  model,
  data,
  TDderive,
  Effmodel,
  Effsamples,
  Gstarderive,
  ...
)

# S4 method for class 'StoppingMaxGainCIRatio,ANY,missing,ModelTox,DataDual'
stopTrial(stopping, dose, model, data, Effmodel, ...)
```

## Arguments

- stopping:

  (`Stopping`)  
  the rule for stopping the trial.

- dose:

  the recommended next best dose.

- samples:

  (`Samples`)  
  the mcmc samples.

- model:

  (`GeneralModel`)  
  the model.

- data:

  (`Data`)  
  input data.

- ...:

  additional arguments without method dispatch.

- external:

  (`flag`)  
  whether to stop based on the external result or not.

- TDderive:

  (`function`)  
  the function which derives from the input, a vector of the posterior
  samples called `TDsamples` of the dose which has the probability of
  the occurrence of DLE equals to either the targetDuringTrial or
  targetEndOfTrial, the final next best TDtargetDuringTrial (the dose
  with probability of the occurrence of DLE equals to the
  targetDuringTrial) and TDtargetEndOfTrial estimate.

- Effmodel:

  (`ModelEff`)  
  the efficacy model.

- Effsamples:

  (`Samples`)  
  the efficacy samples.

- Gstarderive:

  (`function`)  
  the function which derives from the input, a vector of the posterior
  Gstar (the dose which gives the maximum gain value) samples called
  `Gstarsamples`, the final next best Gstar estimate.

## Value

logical value: `TRUE` if the trial can be stopped, `FALSE` otherwise. It
should have an attribute `message` which gives the reason for the
decision.

## Functions

- `stopTrial( stopping = StoppingMissingDose, dose = numeric, samples = ANY, model = ANY, data = Data )`:
  Stop based on value returned by next best dose.

- `stopTrial( stopping = StoppingList, dose = ANY, samples = ANY, model = ANY, data = ANY )`:
  Stop based on multiple stopping rules.

- `stopTrial( stopping = StoppingAll, dose = ANY, samples = ANY, model = ANY, data = ANY )`:
  Stop based on fulfillment of all multiple stopping rules.

- `stopTrial( stopping = StoppingAny, dose = ANY, samples = ANY, model = ANY, data = ANY )`:
  Stop based on fulfillment of any stopping rule.

- `stopTrial( stopping = StoppingCohortsNearDose, dose = numeric, samples = ANY, model = ANY, data = Data )`:
  Stop based on number of cohorts near to next best dose.

- `stopTrial( stopping = StoppingPatientsNearDose, dose = numeric, samples = ANY, model = ANY, data = Data )`:
  Stop based on number of patients near to next best dose.

- `stopTrial( stopping = StoppingMinCohorts, dose = ANY, samples = ANY, model = ANY, data = Data )`:
  Stop based on minimum number of cohorts.

- `stopTrial( stopping = StoppingMinPatients, dose = ANY, samples = ANY, model = ANY, data = Data )`:
  Stop based on minimum number of patients.

- `stopTrial( stopping = StoppingTargetProb, dose = numeric, samples = Samples, model = GeneralModel, data = ANY )`:
  Stop based on probability of target tox interval

- `stopTrial( stopping = StoppingMTDdistribution, dose = numeric, samples = Samples, model = GeneralModel, data = ANY )`:
  Stop based on MTD distribution.

- `stopTrial( stopping = StoppingTargetBiomarker, dose = numeric, samples = Samples, model = DualEndpoint, data = ANY )`:
  Stop based on probability of targeting biomarker

- `stopTrial( stopping = StoppingSpecificDose, dose = numeric, samples = ANY, model = ANY, data = Data )`:
  if Stopping rule is met for specific dose of the planned dose grid and
  not just for the default next best dose.

- `stopTrial( stopping = StoppingHighestDose, dose = numeric, samples = ANY, model = ANY, data = Data )`:
  Stop when the highest dose is reached.

- `stopTrial( stopping = StoppingOrdinal, dose = numeric, samples = ANY, model = LogisticLogNormalOrdinal, data = DataOrdinal )`:
  Stop based on value returned by next best dose.

- `stopTrial( stopping = StoppingOrdinal, dose = numeric, samples = ANY, model = ANY, data = ANY )`:
  Stop based on value returned by next best dose.

- `stopTrial( stopping = StoppingExternal, dose = numeric, samples = ANY, model = ANY, data = ANY )`:
  Stop based on an external flag.

- `stopTrial( stopping = StoppingTDCIRatio, dose = ANY, samples = Samples, model = ModelTox, data = ANY )`:
  Stop based on
  [`StoppingTDCIRatio`](https://openpharma.github.io/crmPack/reference/StoppingTDCIRatio-class.md)
  class when reaching the target ratio of the upper to the lower 95%
  credibility interval of the estimate (TDtargetEndOfTrial). This is a
  stopping rule which incorporates only DLE responses and DLE samples
  are given.

- `stopTrial( stopping = StoppingTDCIRatio, dose = ANY, samples = missing, model = ModelTox, data = ANY )`:
  Stop based on
  [`StoppingTDCIRatio`](https://openpharma.github.io/crmPack/reference/StoppingTDCIRatio-class.md)
  class when reaching the target ratio of the upper to the lower 95%
  credibility interval of the estimate (TDtargetEndOfTrial). This is a
  stopping rule which incorporates only DLE responses and no DLE samples
  are involved.

- `stopTrial( stopping = StoppingMaxGainCIRatio, dose = ANY, samples = Samples, model = ModelTox, data = DataDual )`:
  Stop based on reaching the target ratio of the upper to the lower 95%
  credibility interval of the estimate (the minimum of Gstar and
  TDtargetEndOfTrial). This is a stopping rule which incorporates DLE
  and efficacy responses and DLE and efficacy samples are also used.

- `stopTrial( stopping = StoppingMaxGainCIRatio, dose = ANY, samples = missing, model = ModelTox, data = DataDual )`:
  Stop based on reaching the target ratio of the upper to the lower 95%
  credibility interval of the estimate (the minimum of Gstar and
  TDtargetEndOfTrial). This is a stopping rule which incorporates DLE
  and efficacy responses without DLE and efficacy samples involved.

## Examples

``` r
## Example of combining stopping rules with '&' and/or '|' operators

myStopping1 <- StoppingMinCohorts(nCohorts = 3)
myStopping2 <- StoppingTargetProb(target = c(0.2, 0.35), prob = 0.5)
myStopping3 <- StoppingMinPatients(nPatients = 20)

myStopping <- (myStopping1 & myStopping2) | myStopping3
# Example of usage for `StoppingMissingDose` StopTrial class.

# Create the data.
my_data <- Data(
  x = c(0.01, 0.1, 0.5, 3, 6, 10, 10, 10),
  y = c(0, 1, 1, 0, 0, 0, 0, 1),
  cohort = c(1, 1, 2, 3, 4, 5, 5, 5),
  ID = 1:8,
  doseGrid = c(
    0.01,
    0.1,
    0.5,
    1.5,
    3,
    6,
    seq(from = 10, to = 80, by = 2)
  ),
  placebo = TRUE
)

# Initialize the CRM model used to model the data.
my_model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56
)

# Set-up some MCMC parameters and generate samples from the posterior.
my_options <- McmcOptions(burnin = 5, step = 1, samples = 10)
my_samples <- mcmc(my_data, my_model, my_options)

# Define the rule for dose increments and calculate the maximum dose allowed.
my_increments <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)

next_max_dose <- maxDose(my_increments, data = my_data)

# Define the rule which will be used to select the next best dose based
# on the class 'NextBestNCRM'.
my_next_best <- NextBestNCRM(
  target = c(0.1, 0.25),
  overdose = c(0.2, 1),
  max_overdose_prob = 0.25
)

# Calculate the next best dose.
dose_recommendation <- nextBest(
  my_next_best,
  doselimit = next_max_dose,
  samples = my_samples,
  model = my_model,
  data = my_data
)

# Define the stopping rule such that the study would be stopped if there is
# no safe active dose returned from dose_recommendation.
my_stopping <- StoppingMissingDose()
my_stopping <- StoppingAny(
  stop_list = c(
    StoppingMinPatients(nPatients = 16),
    StoppingMissingDose()
  )
)

# Evaluate if to stop the trial.
stopTrial(
  stopping = my_stopping,
  dose = dose_recommendation$value,
  data = my_data,
  model = my_model
)
#> [1] TRUE
#> attr(,"message")
#> attr(,"message")[[1]]
#> [1] "Number of patients is 8 and thus below the prespecified minimum number 16"
#> 
#> attr(,"message")[[2]]
#> [1] "Next dose is NA , i.e., no active dose is safe enough according to the NextBest rule."
#> 
#> attr(,"individual")
#> attr(,"individual")[[1]]
#> [1] FALSE
#> attr(,"message")
#> [1] "Number of patients is 8 and thus below the prespecified minimum number 16"
#> attr(,"report_label")
#> [1] "≥ 16 patients dosed"
#> 
#> attr(,"individual")[[2]]
#> [1] TRUE
#> attr(,"message")
#> [1] "Next dose is NA , i.e., no active dose is safe enough according to the NextBest rule."
#> attr(,"report_label")
#> [1] "Stopped because of missing dose"
#> 
#> attr(,"report_label")
#> [1] NA
# nolint start

# Create some data
data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)
#> Used default patient IDs!

# Initialize the CRM model used to model the data
model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56
)

# Set-up some MCMC parameters and generate samples from the posterior
options <- McmcOptions(burnin = 5, step = 1, samples = 10)
set.seed(94)
samples <- mcmc(data, model, options)

# Define the rule for dose increments and calculate the maximum dose allowed
myIncrements <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)
nextMaxDose <- maxDose(myIncrements, data = data)

# Define the rule which will be used to select the next best dose
# based on the class 'NextBestNCRM'
myNextBest <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Calculate the next best dose
doseRecommendation <- nextBest(
  myNextBest,
  doselimit = nextMaxDose,
  samples = samples,
  model = model,
  data = data
)

# Define the stopping rules
myStopping1 <- StoppingMinCohorts(nCohorts = 3)
myStopping2 <- StoppingTargetProb(target = c(0.2, 0.35), prob = 0.5)
myStopping3 <- StoppingMinPatients(nPatients = 20)

# Create a list of stopping rules (of class 'StoppingList') which will then be
# summarized (in this specific example) with the 'any' function, meaning that the study
# would be stopped if 'any' of the single stopping rules is TRUE.
mystopping <- StoppingList(
  stop_list = c(myStopping1, myStopping2, myStopping3),
  summary = any
)

# Evaluate if to stop the Trial
stopTrial(
  stopping = myStopping,
  dose = doseRecommendation$value,
  samples = samples,
  model = model,
  data = data
)
#> [1] FALSE
#> attr(,"message")
#> attr(,"message")[[1]]
#> attr(,"message")[[1]][[1]]
#> [1] "Number of cohorts is 6 and thus reached the prespecified minimum number 3"
#> 
#> attr(,"message")[[1]][[2]]
#> [1] "Probability for target toxicity is 10 % for dose 14 and thus below the required 50 %"
#> 
#> 
#> attr(,"message")[[2]]
#> [1] "Number of patients is 8 and thus below the prespecified minimum number 20"
#> 
#> attr(,"individual")
#> attr(,"individual")[[1]]
#> [1] FALSE
#> attr(,"message")
#> attr(,"message")[[1]]
#> [1] "Number of cohorts is 6 and thus reached the prespecified minimum number 3"
#> 
#> attr(,"message")[[2]]
#> [1] "Probability for target toxicity is 10 % for dose 14 and thus below the required 50 %"
#> 
#> attr(,"individual")
#> attr(,"individual")[[1]]
#> [1] TRUE
#> attr(,"message")
#> [1] "Number of cohorts is 6 and thus reached the prespecified minimum number 3"
#> attr(,"report_label")
#> [1] "≥ 3 cohorts dosed"
#> 
#> attr(,"individual")[[2]]
#> [1] FALSE
#> attr(,"message")
#> [1] "Probability for target toxicity is 10 % for dose 14 and thus below the required 50 %"
#> attr(,"report_label")
#> [1] "P(0.2 ≤ prob(DLE | NBD) ≤ 0.35) ≥ 0.5"
#> 
#> attr(,"report_label")
#> [1] NA
#> 
#> attr(,"individual")[[2]]
#> [1] FALSE
#> attr(,"message")
#> [1] "Number of patients is 8 and thus below the prespecified minimum number 20"
#> attr(,"report_label")
#> [1] "≥ 20 patients dosed"
#> 
#> attr(,"report_label")
#> [1] NA

# nolint end
# nolint start

# Create some data
data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)
#> Used default patient IDs!

# Initialize the CRM model used to model the data
model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56
)

# Set-up some MCMC parameters and generate samples from the posterior
options <- McmcOptions(burnin = 5, step = 1, samples = 10)
set.seed(94)
samples <- mcmc(data, model, options)

# Define the rule for dose increments and calculate the maximum dose allowed
myIncrements <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)
nextMaxDose <- maxDose(myIncrements, data = data)

# Define the rule which will be used to select the next best dose
# based on the class 'NextBestNCRM'
myNextBest <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Calculate the next best dose
doseRecommendation <- nextBest(
  myNextBest,
  doselimit = nextMaxDose,
  samples = samples,
  model = model,
  data = data
)

# Define the stopping rules
myStopping1 <- StoppingMinCohorts(nCohorts = 3)
myStopping2 <- StoppingTargetProb(target = c(0.2, 0.35), prob = 0.5)
myStopping3 <- StoppingMinPatients(nPatients = 20)

# Combine the stopping rules, obtaining (in this specific example) a list of stopping
# rules of class 'StoppingAll'
myStopping <- (myStopping1 | myStopping2) & myStopping3

# Evaluate if to stop the Trial
stopTrial(
  stopping = myStopping,
  dose = doseRecommendation$value,
  samples = samples,
  model = model,
  data = data
)
#> [1] FALSE
#> attr(,"message")
#> attr(,"message")[[1]]
#> attr(,"message")[[1]][[1]]
#> [1] "Number of cohorts is 6 and thus reached the prespecified minimum number 3"
#> 
#> attr(,"message")[[1]][[2]]
#> [1] "Probability for target toxicity is 20 % for dose 10 and thus below the required 50 %"
#> 
#> 
#> attr(,"message")[[2]]
#> [1] "Number of patients is 8 and thus below the prespecified minimum number 20"
#> 
#> attr(,"individual")
#> attr(,"individual")[[1]]
#> [1] TRUE
#> attr(,"message")
#> attr(,"message")[[1]]
#> [1] "Number of cohorts is 6 and thus reached the prespecified minimum number 3"
#> 
#> attr(,"message")[[2]]
#> [1] "Probability for target toxicity is 20 % for dose 10 and thus below the required 50 %"
#> 
#> attr(,"individual")
#> attr(,"individual")[[1]]
#> [1] TRUE
#> attr(,"message")
#> [1] "Number of cohorts is 6 and thus reached the prespecified minimum number 3"
#> attr(,"report_label")
#> [1] "≥ 3 cohorts dosed"
#> 
#> attr(,"individual")[[2]]
#> [1] FALSE
#> attr(,"message")
#> [1] "Probability for target toxicity is 20 % for dose 10 and thus below the required 50 %"
#> attr(,"report_label")
#> [1] "P(0.2 ≤ prob(DLE | NBD) ≤ 0.35) ≥ 0.5"
#> 
#> attr(,"report_label")
#> [1] NA
#> 
#> attr(,"individual")[[2]]
#> [1] FALSE
#> attr(,"message")
#> [1] "Number of patients is 8 and thus below the prespecified minimum number 20"
#> attr(,"report_label")
#> [1] "≥ 20 patients dosed"
#> 
#> attr(,"report_label")
#> [1] NA

# nolint end
# nolint start

# Create some data
data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)
#> Used default patient IDs!

# Initialize the CRM model used to model the data
model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56
)

# Set-up some MCMC parameters and generate samples from the posterior
options <- McmcOptions(burnin = 5, step = 1, samples = 10)
set.seed(94)
samples <- mcmc(data, model, options)

# Define the rule for dose increments and calculate the maximum dose allowed
myIncrements <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)
nextMaxDose <- maxDose(myIncrements, data = data)

# Define the rule which will be used to select the next best dose
# based on the class 'NextBestNCRM'
myNextBest <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Calculate the next best dose
doseRecommendation <- nextBest(
  myNextBest,
  doselimit = nextMaxDose,
  samples = samples,
  model = model,
  data = data
)

# Define the stopping rules
myStopping1 <- StoppingMinCohorts(nCohorts = 3)
myStopping2 <- StoppingTargetProb(target = c(0.2, 0.35), prob = 0.5)
myStopping3 <- StoppingMinPatients(nPatients = 20)

# Combine the stopping rules, obtaining (in this specific example) a list of stopping
# rules of class 'StoppingAny'
myStopping <- (myStopping1 | myStopping2) | myStopping3

# Evaluate if to stop the Trial
stopTrial(
  stopping = myStopping,
  dose = doseRecommendation$value,
  samples = samples,
  model = model,
  data = data
)
#> [1] TRUE
#> attr(,"message")
#> attr(,"message")[[1]]
#> [1] "Number of cohorts is 6 and thus reached the prespecified minimum number 3"
#> 
#> attr(,"message")[[2]]
#> [1] "Probability for target toxicity is 40 % for dose 10 and thus below the required 50 %"
#> 
#> attr(,"message")[[3]]
#> [1] "Number of patients is 8 and thus below the prespecified minimum number 20"
#> 
#> attr(,"individual")
#> attr(,"individual")[[1]]
#> [1] TRUE
#> attr(,"message")
#> [1] "Number of cohorts is 6 and thus reached the prespecified minimum number 3"
#> attr(,"report_label")
#> [1] "≥ 3 cohorts dosed"
#> 
#> attr(,"individual")[[2]]
#> [1] FALSE
#> attr(,"message")
#> [1] "Probability for target toxicity is 40 % for dose 10 and thus below the required 50 %"
#> attr(,"report_label")
#> [1] "P(0.2 ≤ prob(DLE | NBD) ≤ 0.35) ≥ 0.5"
#> 
#> attr(,"individual")[[3]]
#> [1] FALSE
#> attr(,"message")
#> [1] "Number of patients is 8 and thus below the prespecified minimum number 20"
#> attr(,"report_label")
#> [1] "≥ 20 patients dosed"
#> 
#> attr(,"report_label")
#> [1] NA

# nolint end
# nolint start

# Create the data
data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)
#> Used default patient IDs!

# Initialize the CRM model used to model the data
model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56
)

# Set-up some MCMC parameters and generate samples from the posterior
options <- McmcOptions(burnin = 5, step = 1, samples = 10)
set.seed(94)
samples <- mcmc(data, model, options)

# Define the rule for dose increments and calculate the maximum dose allowed
myIncrements <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)
nextMaxDose <- maxDose(myIncrements, data = data)

# Define the rule which will be used to select the next best dose
# based on the class 'NextBestNCRM'
myNextBest <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Calculate the next best dose
doseRecommendation <- nextBest(
  myNextBest,
  doselimit = nextMaxDose,
  samples = samples,
  model = model,
  data = data
)

# Define the stopping rule such that the study would be stopped if at least 3
# cohorts were already dosed within 1 +/- 0.2 of the next best dose
myStopping <- StoppingCohortsNearDose(nCohorts = 3, percentage = 0.2)

# Evaluate if to stop the trial
stopTrial(stopping = myStopping, dose = doseRecommendation$value, data = data)
#> [1] FALSE
#> attr(,"message")
#> [1] "1 cohorts lie within 0.2% of the next best dose 6. This is below the required 3 cohorts"
#> attr(,"report_label")
#> [1] "≥ 3 cohorts dosed in 0.2 % dose range around NBD"

# nolint end
# nolint start

# Create the data
data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)
#> Used default patient IDs!

# Initialize the CRM model used to model the data
model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56
)

# Set-up some MCMC parameters and generate samples from the posterior
options <- McmcOptions(burnin = 5, step = 1, samples = 10)
set.seed(94)
samples <- mcmc(data, model, options)

# Define the rule for dose increments and calculate the maximum dose allowed
myIncrements <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)
nextMaxDose <- maxDose(myIncrements, data = data)

# Define the rule which will be used to select the next best dose
# based on the class 'NextBestNCRM'
myNextBest <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Calculate the next best dose
doseRecommendation <- nextBest(
  myNextBest,
  doselimit = nextMaxDose,
  samples = samples,
  model = model,
  data = data
)

# Define the stopping rule such that the study would be stopped if at least 9
# patients were already dosed within 1 +/- 0.2 of the next best dose
myStopping <- StoppingPatientsNearDose(nPatients = 9, percentage = 0.2)

# Evaluate if to stop the trial
stopTrial(stopping = myStopping, dose = doseRecommendation$value, data = data)
#> [1] FALSE
#> attr(,"message")
#> [1] "3 patients lie within 0.2% of the next best dose 10. This is below the required 9 patients"
#> attr(,"report_label")
#> [1] "≥ 9 patients dosed in 0.2 % dose range around NBD"

# nolint end
# nolint start

# Create the data
data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)
#> Used default patient IDs!

# Initialize the CRM model used to model the data
model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56
)

# Set-up some MCMC parameters and generate samples from the posterior
options <- McmcOptions(burnin = 5, step = 1, samples = 10)
set.seed(94)
samples <- mcmc(data, model, options)

# Define the rule for dose increments and calculate the maximum dose allowed
myIncrements <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)
nextMaxDose <- maxDose(myIncrements, data = data)

# Define the rule which will be used to select the next best dose
# based on the class 'NextBestNCRM'
myNextBest <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Calculate the next best dose
doseRecommendation <- nextBest(
  myNextBest,
  doselimit = nextMaxDose,
  samples = samples,
  model = model,
  data = data
)

# Define the stopping rule such that the study would be stopped if at least 6
# cohorts were already dosed
myStopping <- StoppingMinCohorts(nCohorts = 6)

# Evaluate if to stop the trial
stopTrial(stopping = myStopping, dose = doseRecommendation$value, data = data)
#> [1] TRUE
#> attr(,"message")
#> [1] "Number of cohorts is 6 and thus reached the prespecified minimum number 6"
#> attr(,"report_label")
#> [1] "≥ 6 cohorts dosed"

# nolint end
# nolint start

# Create the data
data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)
#> Used default patient IDs!

# Initialize the CRM model used to model the data
model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56
)

# Set-up some MCMC parameters and generate samples from the posterior
options <- McmcOptions(burnin = 5, step = 1, samples = 10)
set.seed(94)
samples <- mcmc(data, model, options)

# Define the rule for dose increments and calculate the maximum dose allowed
myIncrements <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)
nextMaxDose <- maxDose(myIncrements, data = data)

# Define the rule which will be used to select the next best dose
# based on the class 'NextBestNCRM'
myNextBest <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Calculate the next best dose
doseRecommendation <- nextBest(
  myNextBest,
  doselimit = nextMaxDose,
  samples = samples,
  model = model,
  data = data
)

# Define the stopping rule such that the study would be stopped if at least 20
# patients were already dosed
myStopping <- StoppingMinPatients(nPatients = 20)

# Evaluate if to stop the trial
stopTrial(stopping = myStopping, dose = doseRecommendation$value, data = data)
#> [1] FALSE
#> attr(,"message")
#> [1] "Number of patients is 8 and thus below the prespecified minimum number 20"
#> attr(,"report_label")
#> [1] "≥ 20 patients dosed"

# nolint end
# nolint start

# Create the data
data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)
#> Used default patient IDs!

# Initialize the CRM model used to model the data
model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56
)

# Set-up some MCMC parameters and generate samples from the posterior
options <- McmcOptions(burnin = 5, step = 1, samples = 10)
set.seed(94)
samples <- mcmc(data, model, options)

# Define the rule for dose increments and calculate the maximum dose allowed
myIncrements <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)
nextMaxDose <- maxDose(myIncrements, data = data)

# Define the rule which will be used to select the next best dose
# based on the class 'NextBestNCRM'
myNextBest <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Calculate the next best dose
doseRecommendation <- nextBest(
  myNextBest,
  doselimit = nextMaxDose,
  samples = samples,
  model = model,
  data = data
)

# Define the stopping rule such that the study would be stopped if there is at least
# 0.5 posterior probability that [0.2 =< Prob(DLT | next-best-dose) <= 0.35]
myStopping <- StoppingTargetProb(target = c(0.2, 0.35), prob = 0.5)

# Evaluate if to stop the trial
stopTrial(
  stopping = myStopping,
  dose = doseRecommendation$value,
  samples = samples,
  model = model,
  data = data
)
#> [1] FALSE
#> attr(,"message")
#> [1] "Probability for target toxicity is 10 % for dose 6 and thus below the required 50 %"
#> attr(,"report_label")
#> [1] "P(0.2 ≤ prob(DLE | NBD) ≤ 0.35) ≥ 0.5"

# nolint end
# nolint start

# Create the data
data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)
#> Used default patient IDs!

# Initialize the CRM model used to model the data
model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56
)

# Set-up some MCMC parameters and generate samples from the posterior
options <- McmcOptions(burnin = 5, step = 1, samples = 10)
set.seed(94)
samples <- mcmc(data, model, options)

# Define the rule for dose increments and calculate the maximum dose allowed
myIncrements <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)
nextMaxDose <- maxDose(myIncrements, data = data)

# Define the rule which will be used to select the next best dose
# based on the class 'NextBestNCRM'
myNextBest <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Calculate the next best dose
doseRecommendation <- nextBest(
  myNextBest,
  doselimit = nextMaxDose,
  samples = samples,
  model = model,
  data = data
)

# Define the stopping rule such that the study would be stopped if there is at least
# 0.9 probability that MTD > 0.5*next_best_dose. Here MTD is defined as the dose for
# which prob(DLE)=0.33
myStopping <- StoppingMTDdistribution(target = 0.33, thresh = 0.5, prob = 0.9)

# Evaluate if to stop the trial
stopTrial(
  stopping = myStopping,
  dose = doseRecommendation$value,
  samples = samples,
  model = model,
  data = data
)
#> [1] TRUE
#> attr(,"message")
#> [1] "Probability of MTD above 50 % of current dose 18 is 100 % and thus greater than or equal to the required 90 %"
#> attr(,"report_label")
#> [1] "P(MTD > 0.5 * NBD | P(DLE) = 0.33) ≥ 0.9"

# nolint end
# Create the data.
my_data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)
#> Used default patient IDs!

# Initialize the CRM model used to model the data.
my_model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56
)

# Set-up some MCMC parameters and generate samples from the posterior.
my_options <- McmcOptions(burnin = 5, step = 1, samples = 10)
my_samples <- mcmc(my_data, my_model, my_options)

# Define the rule for dose increments and calculate the maximum dose allowed.
my_increments <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)
next_max_dose <- maxDose(my_increments, data = my_data)

# Define the rule which will be used to select the next best dose
# based on the class 'NextBestNCRM'.
my_next_best <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Calculate the next best dose.
dose_recommendation <- nextBest(
  my_next_best,
  doselimit = next_max_dose,
  samples = my_samples,
  model = my_model,
  data = my_data
)

# Define the stopping rule such that the study would be stopped if the
# the MTD can be estimated with sufficient precision, i.e. if robust coefficient
# of variation is below 40%.
my_stopping <- StoppingMTDCV(target = 0.3, thresh_cv = 40)

# Evaluate if to stop the trial.
stopTrial(
  stopping = my_stopping,
  dose = dose_recommendation$value,
  samples = my_samples,
  model = my_model,
  data = my_data
)
#> [1] FALSE
#> attr(,"message")
#> [1] "CV of MTD is 76 % and thus above the required precision threshold of 40 %"
#> attr(,"report_label")
#> [1] "CV(MTD) > 0.3"
# Create the data.
data <- Data(
  x = c(0.1, 0.1, 0.1),
  y = c(0, 0, 1),
  cohort = c(1, 1, 1),
  doseGrid = c(
    0.1,
    0.5,
    1.5,
    3,
    6,
    seq(from = 10, to = 80, by = 2)
  ),
  ID = 1:3
)

# Initialize the CRM model used to model the data.
model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56
)

# Set-up some MCMC parameters and generate samples from the posterior.
options <- McmcOptions(burnin = 5, step = 1, samples = 10)
set.seed(94)
# \donttest{
samples <- mcmc(data, model, options)

# Define the rule for dose increments and calculate the maximum dose allowed.
my_increments <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)

next_max_dose <- maxDose(my_increments, data = data)

# Define the rule which will be used to select the next best dose
# based on the class 'NextBestNCRM'.
my_next_best <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Calculate the next best dose.
dose_recommendation <- nextBest(
  my_next_best,
  doselimit = next_max_dose,
  samples = samples,
  model = model,
  data = data
)

# Define the stopping rule such that the study would be stopped if first dose
# is toxic based on a Beta posterior distribution with Beta(1,1) prior.
my_stopping <- StoppingLowestDoseHSRBeta(
  target = 0.3,
  prob = 0.9
)

# Evaluate if the trial will be stopped.
stopTrial(
  stopping = my_stopping,
  dose = dose_recommendation$value,
  samples = samples,
  model = model,
  data = data
)
#> [1] FALSE
#> attr(,"message")
#> [1] "Probability that the lowest active dose of 0.1 being toxic based on posterior Beta distribution using a Beta(1,1) prior is 65% and thus below the required 90% threshold."
#> attr(,"report_label")
#> [1] "Pβ(lowest dose > P(DLE) = 0.3) > 0.9"
# }
# nolint start

# Create the data
data <- DataDual(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10, 20, 20, 20, 40, 40, 40, 50, 50, 50),
  y = c(0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1),
  w = c(
    0.31,
    0.42,
    0.59,
    0.45,
    0.6,
    0.7,
    0.55,
    0.6,
    0.52,
    0.54,
    0.56,
    0.43,
    0.41,
    0.39,
    0.34,
    0.38,
    0.21
  ),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)
#> Used default patient IDs!
#> Used best guess cohort indices!

# Initialize the Dual-Endpoint model (in this case RW1)
model <- DualEndpointRW(
  mean = c(0, 1),
  cov = matrix(c(1, 0, 0, 1), nrow = 2),
  sigma2betaW = 0.01,
  sigma2W = c(a = 0.1, b = 0.1),
  rho = c(a = 1, b = 1),
  rw1 = TRUE
)

# Set-up some MCMC parameters and generate samples from the posterior
options <- McmcOptions(burnin = 5, step = 1, samples = 10)
set.seed(94)
# \donttest{
samples <- mcmc(data, model, options)

# Define the rule for dose increments and calculate the maximum dose allowed
myIncrements <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)
nextMaxDose <- maxDose(myIncrements, data = data)

# Define the rule which will be used to select the next best dose
# In this case target a dose achieving at least 0.9 of maximum biomarker level (efficacy)
# and with a probability below 0.25 that prob(DLT)>0.35 (safety)
myNextBest <- NextBestDualEndpoint(
  target = c(0.9, 1),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Calculate the next best dose
doseRecommendation <- nextBest(
  myNextBest,
  doselimit = nextMaxDose,
  samples = samples,
  model = model,
  data = data
)

# Define the stopping rule such that the study would be stopped if if there is at
# least 0.5 posterior probability that the biomarker (efficacy) is within the
# biomarker target range of [0.9, 1.0] (relative to the maximum for the biomarker).
myStopping <- StoppingTargetBiomarker(target = c(0.9, 1), prob = 0.5)

# Evaluate if to stop the trial
stopTrial(
  stopping = myStopping,
  dose = doseRecommendation$value,
  samples = samples,
  model = model,
  data = data
)
#> [1] FALSE
#> attr(,"message")
#> [1] "Probability for target biomarker is 0 % for dose NA and thus below the required 50 %"
#> attr(,"report_label")
#> [1] "P(0.9 ≤ Biomarker ≤ 1) ≥ 0.5 (relative)"
# }
# nolint end
# Create some data.
my_data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 0, 0, 0, 0, 1, 0),
  ID = 1:8,
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)

# Initialize the CRM model used to model the data.
my_model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 50
)

# Set-up some MCMC parameters and generate samples from the posterior.
my_options <- McmcOptions(burnin = 5, step = 1, samples = 10)
my_samples <- mcmc(my_data, my_model, my_options)

# Define the rule which will be used to select the next best dose
# based on the class 'NextBestNCRM'.
my_nb_ncrm <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Calculate the next best dose.
my_dose_recommendation <- nextBest(
  nextBest = my_nb_ncrm,
  doselimit = 100,
  samples = my_samples,
  model = my_model,
  data = my_data
)

# Define the stopping rules.
highest_dose_safe <- StoppingSpecificDose(
  rule = StoppingTargetProb(target = c(0, 0.3), prob = 0.8),
  dose = 80
)
max_patients <- StoppingMinPatients(nPatients = 20)
patients_near_dose <- StoppingPatientsNearDose(nPatients = 3, percentage = 0)

# Create a list of stopping rules (of class 'StoppingList') which will then be
# summarized (in this specific example) with the 'any' function, meaning that
# the study would be stopped if 'any' of the single stopping rules is TRUE.
my_stopping <- highest_dose_safe | max_patients | patients_near_dose

# Evaluate if to stop the Trial
stopTrial(
  stopping = my_stopping,
  dose = my_dose_recommendation$value,
  samples = my_samples,
  model = my_model,
  data = my_data
)
#> [1] FALSE
#> attr(,"message")
#> attr(,"message")[[1]]
#> [1] "Probability for target toxicity is 0 % for dose 80 and thus below the required 80 %"
#> 
#> attr(,"message")[[2]]
#> [1] "Number of patients is 8 and thus below the prespecified minimum number 20"
#> 
#> attr(,"message")[[3]]
#> [1] "0 patients lie within 0% of the next best dose 32. This is below the required 3 patients"
#> 
#> attr(,"individual")
#> attr(,"individual")[[1]]
#> [1] FALSE
#> attr(,"message")
#> [1] "Probability for target toxicity is 0 % for dose 80 and thus below the required 80 %"
#> attr(,"report_label")
#> [1] "Dose 80 used for testing a stopping rule"
#> 
#> attr(,"individual")[[2]]
#> [1] FALSE
#> attr(,"message")
#> [1] "Number of patients is 8 and thus below the prespecified minimum number 20"
#> attr(,"report_label")
#> [1] "≥ 20 patients dosed"
#> 
#> attr(,"individual")[[3]]
#> [1] FALSE
#> attr(,"message")
#> [1] "0 patients lie within 0% of the next best dose 32. This is below the required 3 patients"
#> attr(,"report_label")
#> [1] "≥ 3 patients dosed in 0 % dose range around NBD"
#> 
#> attr(,"report_label")
#> [1] NA
# nolint start

# Create the data
data <- Data(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10, 20, 20, 20, 40, 40, 40, 80, 80, 80),
  y = c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  cohort = c(0, 1, 2, 3, 4, 5, 5, 5, 6, 6, 6, 7, 7, 7, 8, 8, 8),
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2))
)
#> Used default patient IDs!

# Initialize the CRM model used to model the data
model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56
)

# Set-up some MCMC parameters and generate samples from the posterior
options <- McmcOptions(burnin = 5, step = 1, samples = 10)
set.seed(94)
samples <- mcmc(data, model, options)

# Define the rule for dose increments and calculate the maximum dose allowed
myIncrements <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)
nextMaxDose <- maxDose(myIncrements, data = data)

# Define the rule which will be used to select the next best dose
# based on the class 'NextBestNCRM'
myNextBest <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Calculate the next best dose
doseRecommendation <- nextBest(
  myNextBest,
  doselimit = nextMaxDose,
  samples = samples,
  model = model,
  data = data
)

# Define the stopping rule such that the study would be stopped if there is at least
# 0.5 posterior probability that [0.2 =< Prob(DLT | next-best-dose) <= 0.35]
stopTarget <- StoppingTargetProb(target = c(0.2, 0.35), prob = 0.5)

## now use the StoppingHighestDose rule:
stopHigh <-
  StoppingHighestDose() &
  StoppingPatientsNearDose(nPatients = 3, percentage = 0) &
  StoppingTargetProb(target = c(0, 0.2), prob = 0.5)

## and combine everything:
myStopping <- stopTarget | stopHigh

# Then evaluate if to stop the trial
stopTrial(
  stopping = myStopping,
  dose = doseRecommendation$value,
  samples = samples,
  model = model,
  data = data
)
#> [1] TRUE
#> attr(,"message")
#> attr(,"message")[[1]]
#> [1] "Probability for target toxicity is 70 % for dose 44 and thus above the required 50 %"
#> 
#> attr(,"message")[[2]]
#> attr(,"message")[[2]][[1]]
#> [1] "Next best dose is 44 and thus not the highest dose"
#> 
#> attr(,"message")[[2]][[2]]
#> [1] "0 patients lie within 0% of the next best dose 44. This is below the required 3 patients"
#> 
#> attr(,"message")[[2]][[3]]
#> [1] "Probability for target toxicity is 30 % for dose 44 and thus below the required 50 %"
#> 
#> 
#> attr(,"individual")
#> attr(,"individual")[[1]]
#> [1] TRUE
#> attr(,"message")
#> [1] "Probability for target toxicity is 70 % for dose 44 and thus above the required 50 %"
#> attr(,"report_label")
#> [1] "P(0.2 ≤ prob(DLE | NBD) ≤ 0.35) ≥ 0.5"
#> 
#> attr(,"individual")[[2]]
#> [1] FALSE
#> attr(,"message")
#> attr(,"message")[[1]]
#> [1] "Next best dose is 44 and thus not the highest dose"
#> 
#> attr(,"message")[[2]]
#> [1] "0 patients lie within 0% of the next best dose 44. This is below the required 3 patients"
#> 
#> attr(,"message")[[3]]
#> [1] "Probability for target toxicity is 30 % for dose 44 and thus below the required 50 %"
#> 
#> attr(,"individual")
#> attr(,"individual")[[1]]
#> [1] FALSE
#> attr(,"message")
#> [1] "Next best dose is 44 and thus not the highest dose"
#> attr(,"report_label")
#> [1] "NBD is the highest dose"
#> 
#> attr(,"individual")[[2]]
#> [1] FALSE
#> attr(,"message")
#> [1] "0 patients lie within 0% of the next best dose 44. This is below the required 3 patients"
#> attr(,"report_label")
#> [1] "≥ 3 patients dosed in 0 % dose range around NBD"
#> 
#> attr(,"individual")[[3]]
#> [1] FALSE
#> attr(,"message")
#> [1] "Probability for target toxicity is 30 % for dose 44 and thus below the required 50 %"
#> attr(,"report_label")
#> [1] "P(0 ≤ prob(DLE | NBD) ≤ 0.2) ≥ 0.5"
#> 
#> attr(,"report_label")
#> [1] NA
#> 
#> attr(,"report_label")
#> [1] NA

# nolint end
data <- .DefaultDataOrdinal()
model <- .DefaultLogisticLogNormalOrdinal()
options <- McmcOptions(burnin = 5, step = 1, samples = 10)
samples <- mcmc(data, model, options)

myIncrements <- .DefaultIncrementsOrdinal()
nextMaxDose <- maxDose(myIncrements, data = data)

myNextBest <- .DefaultNextBestOrdinal()

doseRecommendation <- nextBest(
  myNextBest,
  doselimit = nextMaxDose,
  samples = samples,
  model = model,
  data = data
)

myStopping <- .DefaultStoppingOrdinal()

stopTrial(
  stopping = myStopping,
  dose = doseRecommendation$value,
  samples = samples,
  model = model,
  data = data
)
#> [1] FALSE
#> attr(,"message")
#> [1] "Probability for target toxicity is 40 % for dose 50 and thus below the required 60 %"
#> attr(,"report_label")
#> [1] "P(0.2 ≤ prob(DLE | NBD) ≤ 0.35) ≥ 0.6"
data <- .DefaultDataOrdinal()
model <- .DefaultLogisticLogNormalOrdinal()
options <- McmcOptions(burnin = 5, step = 1, samples = 10)
samples <- mcmc(data, model, options)

myIncrements <- .DefaultIncrementsOrdinal()
nextMaxDose <- maxDose(myIncrements, data = data)

myNextBest <- .DefaultNextBestOrdinal()

doseRecommendation <- nextBest(
  myNextBest,
  doselimit = nextMaxDose,
  samples = samples,
  model = model,
  data = data
)

myStopping <- .DefaultStoppingOrdinal()

stopTrial(
  stopping = myStopping,
  dose = doseRecommendation$value,
  samples = samples,
  model = model,
  data = data
)
#> [1] FALSE
#> attr(,"message")
#> [1] "Probability for target toxicity is 30 % for dose 40 and thus below the required 60 %"
#> attr(,"report_label")
#> [1] "P(0.2 ≤ prob(DLE | NBD) ≤ 0.35) ≥ 0.6"
my_rule <- StoppingExternal(report_label = "Based on combo stop")

stopTrial(
  my_rule,
  5,
  .DefaultSamples(),
  .DefaultModelLogNormal(),
  .DefaultData(),
  external = TRUE
)
#> [1] TRUE
#> attr(,"message")
#> [1] "Based on external result stop"
#> attr(,"report_label")
#> [1] "Based on combo stop"
# nolint start

##define the stopping rules based on the 'StoppingTDCIRatio' class
##Using only DLE responses with samples
## we need a data object with doses >= 1:
data <- Data(
  x = c(25, 50, 50, 75, 150, 200, 225, 300),
  y = c(0, 0, 0, 0, 1, 1, 1, 1),
  doseGrid = seq(from = 25, to = 300, by = 25)
)
#> Used default patient IDs!
#> Used best guess cohort indices!

##model can be specified of 'Model' or 'ModelTox' class
##For example, the 'logisticIndepBeta' class model
model <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEweights = c(3, 3),
  DLEdose = c(25, 300),
  data = data
)
##define MCMC options
## for illustration purposes, in reality larger number of burnin and samples shoud be used
options <- McmcOptions(burnin = 5, step = 1, samples = 10)
##samples of 'Samples' class
samples <- mcmc(data, model, options)
##define the 'StoppingTDCIRatio' class
myStopping <- StoppingTDCIRatio(target_ratio = 5, prob_target = 0.3)
##Find the next Recommend dose using the nextBest method (plesae refer to nextbest examples)
tdNextBest <- NextBestTDsamples(
  prob_target_drt = 0.35,
  prob_target_eot = 0.3,
  derive = function(samples) {
    as.numeric(quantile(samples, probs = 0.3))
  }
)

RecommendDose <- nextBest(
  tdNextBest,
  doselimit = max(data@doseGrid),
  samples = samples,
  model = model,
  data = data
)
##use 'stopTrial' to determine if the rule has been fulfilled
##use 0.3 as the target proability of DLE at the end of the trial

stopTrial(
  stopping = myStopping,
  dose = RecommendDose$next_dose_drt,
  samples = samples,
  model = model,
  data = data
)
#> [1] TRUE
#> attr(,"message")
#> [1] "95% CI is (28.5026089176956, 28.5026089176956), Ratio = 1 is less than or equal to target_ratio = 5"
#> attr(,"report_label")
#> [1] "TD 5 for 0.3 target prob"

# nolint end
# nolint start

##define the stopping rules based on the 'StoppingTDCIRatio' class
##Using only DLE responses
## we need a data object with doses >= 1:
data <- Data(
  x = c(25, 50, 50, 75, 150, 200, 225, 300),
  y = c(0, 0, 0, 0, 1, 1, 1, 1),
  doseGrid = seq(from = 25, to = 300, by = 25)
)
#> Used default patient IDs!
#> Used best guess cohort indices!

##model must be of 'ModelTox' class
##For example, the 'logisticIndepBeta' class model
model <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEweights = c(3, 3),
  DLEdose = c(25, 300),
  data = data
)
##define the 'StoppingTDCIRatio' class
myStopping <- StoppingTDCIRatio(target_ratio = 5, prob_target = 0.3)
##Find the next Recommend dose using the nextBest method (plesae refer to nextbest examples)
tdNextBest <- NextBestTD(prob_target_drt = 0.35, prob_target_eot = 0.3)


RecommendDose <- nextBest(
  tdNextBest,
  doselimit = max(data@doseGrid),
  model = model,
  data = data
)
##use 'stopTrial' to determine if the rule has been fulfilled
##use 0.3 as the target proability of DLE at the end of the trial

stopTrial(
  stopping = myStopping,
  dose = RecommendDose$next_dose_drt,
  model = model,
  data = data
)
#> [1] FALSE
#> attr(,"message")
#> [1] "95% CI is ( 15.2619 , 173.8695 ), Ratio = 11.3924 is  greater than target_ratio = 5"
#> attr(,"report_label")
#> [1] "TD 5 for 0.3 target prob"

# nolint end
# nolint start
##define the stopping rules based on the 'StoppingMaxGainCIRatio' class
##Using both DLE and efficacy responses
## we need a data object with doses >= 1:
data <- DataDual(
  x = c(25, 50, 25, 50, 75, 300, 250, 150),
  y = c(0, 0, 0, 0, 0, 1, 1, 0),
  w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
  doseGrid = seq(25, 300, 25),
  placebo = FALSE
)
#> Used default patient IDs!
#> Used best guess cohort indices!

##DLEmodel must be of 'ModelTox' class
##For example, the 'logisticIndepBeta' class model
DLEmodel <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEweights = c(3, 3),
  DLEdose = c(25, 300),
  data = data
)

##Effmodel must be  of 'ModelEff' class
##For example, the 'Effloglog' class model
Effmodel <- Effloglog(
  eff = c(1.223, 2.513),
  eff_dose = c(25, 300),
  nu = c(a = 1, b = 0.025),
  data = data
)
##for illustration purposes, in reality larger number of burnin and samples shoud be used
options <- McmcOptions(burnin = 5, step = 1, samples = 10)
##DLE and efficacy samples must be of 'Samples' class
# \donttest{
DLEsamples <- mcmc(data, DLEmodel, options)
Effsamples <- mcmc(data, Effmodel, options)

##define the 'StoppingMaxGainCIRatio' class
myStopping <- StoppingMaxGainCIRatio(target_ratio = 5, prob_target = 0.3)
##Find the next Recommend dose using the nextBest method (plesae refer to nextbest examples)
mynextbest <- NextBestMaxGainSamples(
  prob_target_drt = 0.35,
  prob_target_eot = 0.3,
  derive = function(samples) {
    as.numeric(quantile(samples, prob = 0.3))
  },
  mg_derive = function(mg_samples) {
    as.numeric(quantile(mg_samples, prob = 0.5))
  }
)

RecommendDose <- nextBest(
  mynextbest,
  doselimit = max(data@doseGrid),
  samples = DLEsamples,
  model = DLEmodel,
  data = data,
  model_eff = Effmodel,
  samples_eff = Effsamples
)
##use 'stopTrial' to determine if the rule has been fulfilled
##use 0.3 as the target proability of DLE at the end of the trial

stopTrial(
  stopping = myStopping,
  dose = RecommendDose$next_dose,
  samples = DLEsamples,
  model = DLEmodel,
  data = data,
  TDderive = function(TDsamples) {
    quantile(TDsamples, prob = 0.3)
  },
  Effmodel = Effmodel,
  Effsamples = Effsamples,
  Gstarderive = function(Gstarsamples) {
    quantile(Gstarsamples, prob = 0.5)
  }
)
#> [1] FALSE
#> attr(,"message")
#> [1] "Gstar estimate is 150 with 95% CI ( 61.25 , 300 ) and its ratio = 4.898"                              
#> [2] "TDtargetEndOfTrial estimate is  28.5027 with 95% CI ( 28.5027 , 11629.1764 ) and its ratio= 408.0026" 
#> [3] "TDtargetEndOfTrial estimate is smaller with ratio = 408.0026  which is  greater than target_ratio = 5"
#> attr(,"report_label")
#> [1] "GStar 5 for 0.3 target prob"
# }
# nolint end
# nolint start

##define the stopping rules based on the 'StoppingMaxGainCIRatio' class
##Using both DLE and efficacy responses
## we need a data object with doses >= 1:
data <- DataDual(
  x = c(25, 50, 25, 50, 75, 300, 250, 150),
  y = c(0, 0, 0, 0, 0, 1, 1, 0),
  w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
  doseGrid = seq(25, 300, 25),
  placebo = FALSE
)
#> Used default patient IDs!
#> Used best guess cohort indices!

##DLEmodel must be of 'ModelTox' class
##For example, the 'logisticIndepBeta' class model
DLEmodel <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEweights = c(3, 3),
  DLEdose = c(25, 300),
  data = data
)

##Effmodel must be  of 'ModelEff' class
##For example, the 'Effloglog' class model
Effmodel <- Effloglog(
  eff = c(1.223, 2.513),
  eff_dose = c(25, 300),
  nu = c(a = 1, b = 0.025),
  data = data
)


##define the 'StoppingMaxGainCIRatio' class
myStopping <- StoppingMaxGainCIRatio(target_ratio = 5, prob_target = 0.3)
##Find the next Recommend dose using the nextBest method (plesae refer to nextbest examples)
mynextbest <- NextBestMaxGain(prob_target_drt = 0.35, prob_target_eot = 0.3)

RecommendDose <- nextBest(
  mynextbest,
  doselimit = max(data@doseGrid),
  model = DLEmodel,
  model_eff = Effmodel,
  data = data
)

##use 'stopTrial' to determine if the rule has been fulfilled
##use 0.3 as the target proability of DLE at the end of the trial

stopTrial(
  stopping = myStopping,
  dose = RecommendDose$next_dose,
  model = DLEmodel,
  data = data,
  Effmodel = Effmodel
)
#> [1] FALSE
#> attr(,"message")
#> [1] "Gstar estimate is 94.942 with 95% CI ( 16.771 , 537.4745 ) and its ratio = 32.0479"                 
#> [2] "TDtargetEndOfTrial estimate is  81.4924 with 95% CI ( 24.3435 , 272.804 ) and its ratio= 11.2064"   
#> [3] "TDtargetEndOfTrial estimate is smaller with ratio = 11.2064 which is  greater than target_ratio = 5"
#> attr(,"report_label")
#> [1] "GStar 5 for 0.3 target prob"

# nolint end
```
