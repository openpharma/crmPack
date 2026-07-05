# Finding the Next Best Dose

**\[stable\]**

A function that computes the recommended next best dose based on the
corresponding rule `nextBest`, the posterior `samples` from the `model`
and the underlying `data`.

## Usage

``` r
nextBest(nextBest, doselimit, samples, model, data, ...)

# S4 method for class 'NextBestEWOC,numeric,Samples,GeneralModel,Data'
nextBest(nextBest, doselimit = Inf, samples, model, data, ...)

# S4 method for class 'NextBestMTD,numeric,Samples,GeneralModel,Data'
nextBest(nextBest, doselimit = Inf, samples, model, data, ...)

# S4 method for class 'NextBestNCRM,numeric,Samples,GeneralModel,Data'
nextBest(nextBest, doselimit = Inf, samples, model, data, ...)

# S4 method for class 'NextBestNCRM,matrix,Samples,TwoDrugsCombo,DataCombo'
nextBest(nextBest, doselimit, samples, model, data, ...)

# S4 method for class 'NextBestNCRM,numeric,Samples,GeneralModel,DataParts'
nextBest(nextBest, doselimit = Inf, samples, model, data, ...)

# S4 method for class 'NextBestNCRMLoss,numeric,Samples,GeneralModel,Data'
nextBest(nextBest, doselimit = Inf, samples, model, data, ...)

# S4 method for class 'NextBestThreePlusThree,missing,missing,missing,Data'
nextBest(nextBest, doselimit, samples, model, data, ...)

# S4 method for class 'NextBestDualEndpoint,numeric,Samples,DualEndpoint,Data'
nextBest(nextBest, doselimit = Inf, samples, model, data, ...)

# S4 method for class 'NextBestMinDist,numeric,Samples,GeneralModel,Data'
nextBest(nextBest, doselimit = Inf, samples, model, data, ...)

# S4 method for class 'NextBestInfTheory,numeric,Samples,GeneralModel,Data'
nextBest(nextBest, doselimit = Inf, samples, model, data, ...)

# S4 method for class 'NextBestTD,numeric,missing,LogisticIndepBeta,Data'
nextBest(nextBest, doselimit = Inf, model, data, in_sim = FALSE, ...)

# S4 method for class 'NextBestTDsamples,numeric,Samples,LogisticIndepBeta,Data'
nextBest(nextBest, doselimit = Inf, samples, model, data, in_sim, ...)

# S4 method for class 'NextBestMaxGain,numeric,missing,ModelTox,DataDual'
nextBest(
  nextBest,
  doselimit = Inf,
  model,
  data,
  model_eff,
  in_sim = FALSE,
  ...
)

# S4 method for class 'NextBestMaxGainSamples,numeric,Samples,ModelTox,DataDual'
nextBest(
  nextBest,
  doselimit = Inf,
  samples,
  model,
  data,
  model_eff,
  samples_eff,
  in_sim = FALSE,
  ...
)

# S4 method for class 'NextBestProbMTDLTE,numeric,Samples,GeneralModel,Data'
nextBest(nextBest, doselimit, samples, model, data, ...)

# S4 method for class 'NextBestProbMTDMinDist,numeric,Samples,GeneralModel,Data'
nextBest(nextBest, doselimit, samples, model, data, ...)

# S4 method for class 'NextBestOrdinal,numeric,Samples,GeneralModel,Data'
nextBest(nextBest, doselimit = Inf, samples, model, data, ...)

# S4 method for class 'NextBestOrdinal,numeric,Samples,LogisticLogNormalOrdinal,DataOrdinal'
nextBest(nextBest, doselimit = Inf, samples, model, data, ...)
```

## Arguments

- nextBest:

  (`NextBest`)\
  the rule for the next best dose.

- doselimit:

  (`number` or `matrix`)\
  the maximum allowed next dose. If it is infinity (default), then
  essentially no dose limit will be applied in the course of dose
  recommendation calculation. A `matrix` must be used for two drug
  combinations.

- samples:

  (`Samples`)\
  posterior samples from `model` parameters given `data`.

- model:

  (`ModelTox`)\
  the DLT model.

- data:

  (`Data` or `DataCombo`)\
  data that was used to generate the samples.

- ...:

  additional arguments without method dispatch.

- in_sim:

  (`flag`)\
  is this method used in simulations? Default as `FALSE`. If this flag
  is `TRUE` and target dose estimates (during trial and end-of-trial)
  are outside of the dose grid range, the information message is printed
  by this method.

- model_eff:

  (`Effloglog` or `EffFlexi`)\
  the efficacy model.

- samples_eff:

  (`Samples`)\
  posterior samples from `model_eff` parameters given `data`.

## Value

A list with the next best dose recommendation (element named `value`)
from the grid defined in `data`, and a plot depicting this
recommendation (element named `plot`). In case of multiple plots also an
element named `singlePlots` is included. The `singlePlots` is itself a
list with single plots. An additional list with elements describing the
outcome of the rule can be contained too.

## Functions

- `nextBest( nextBest = NextBestEWOC, doselimit = numeric, samples = Samples, model = GeneralModel, data = Data )`:
  find the next best dose based on the EWOC rule.

- `nextBest( nextBest = NextBestMTD, doselimit = numeric, samples = Samples, model = GeneralModel, data = Data )`:
  find the next best dose based on the MTD rule.

- `nextBest( nextBest = NextBestNCRM, doselimit = numeric, samples = Samples, model = GeneralModel, data = Data )`:
  find the next best dose based on the NCRM method. The additional
  element `probs` in the output's list contains the target and
  overdosing probabilities (across all doses in the dose grid) used in
  the derivation of the next best dose.

- `nextBest( nextBest = NextBestNCRM, doselimit = matrix, samples = Samples, model = TwoDrugsCombo, data = DataCombo )`:
  find the next best dose combination based on the NCRM method.

- `nextBest( nextBest = NextBestNCRM, doselimit = numeric, samples = Samples, model = GeneralModel, data = DataParts )`:
  find the next best dose based on the NCRM method when two parts trial
  is used.

- `nextBest( nextBest = NextBestNCRMLoss, doselimit = numeric, samples = Samples, model = GeneralModel, data = Data )`:
  find the next best dose based on the NCRM method and loss function.

- `nextBest( nextBest = NextBestThreePlusThree, doselimit = missing, samples = missing, model = missing, data = Data )`:
  find the next best dose based on the 3+3 method.

- `nextBest( nextBest = NextBestDualEndpoint, doselimit = numeric, samples = Samples, model = DualEndpoint, data = Data )`:
  find the next best dose based on the dual endpoint model. The
  additional list element `probs` contains the target and overdosing
  probabilities (across all doses in the dose grid) used in the
  derivation of the next best dose.

- `nextBest( nextBest = NextBestMinDist, doselimit = numeric, samples = Samples, model = GeneralModel, data = Data )`:
  gives the dose which is below the dose limit and has an estimated DLT
  probability which is closest to the target dose.

- `nextBest( nextBest = NextBestInfTheory, doselimit = numeric, samples = Samples, model = GeneralModel, data = Data )`:
  gives the appropriate dose within an information theoretic framework.

- `nextBest( nextBest = NextBestTD, doselimit = numeric, samples = missing, model = LogisticIndepBeta, data = Data )`:
  find the next best dose based only on the DLT responses and for
  [`LogisticIndepBeta`](https://docs.crmpack.org/reference/LogisticIndepBeta-class.md)
  model class object without DLT samples.

- `nextBest( nextBest = NextBestTDsamples, doselimit = numeric, samples = Samples, model = LogisticIndepBeta, data = Data )`:
  find the next best dose based only on the DLT responses and for
  [`LogisticIndepBeta`](https://docs.crmpack.org/reference/LogisticIndepBeta-class.md)
  model class object involving DLT samples.

- `nextBest( nextBest = NextBestMaxGain, doselimit = numeric, samples = missing, model = ModelTox, data = DataDual )`:
  find the next best dose based only on pseudo DLT model
  [`ModelTox`](https://docs.crmpack.org/reference/ModelTox-class.md) and
  [`Effloglog`](https://docs.crmpack.org/reference/Effloglog-class.md)
  efficacy model without samples.

- `nextBest( nextBest = NextBestMaxGainSamples, doselimit = numeric, samples = Samples, model = ModelTox, data = DataDual )`:
  find the next best dose based on DLT and efficacy responses with DLT
  and efficacy samples.

- `nextBest( nextBest = NextBestProbMTDLTE, doselimit = numeric, samples = Samples, model = GeneralModel, data = Data )`:
  find the next best dose based with the highest probability of having a
  toxicity rate less or equal to the target toxicity level.

- `nextBest( nextBest = NextBestProbMTDMinDist, doselimit = numeric, samples = Samples, model = GeneralModel, data = Data )`:
  find the next best dose based with the highest probability of having a
  toxicity rate with minimum distance to the target toxicity level.

- `nextBest( nextBest = NextBestOrdinal, doselimit = numeric, samples = Samples, model = GeneralModel, data = Data )`:
  find the next best dose for ordinal CRM models.

- `nextBest( nextBest = NextBestOrdinal, doselimit = numeric, samples = Samples, model = LogisticLogNormalOrdinal, data = DataOrdinal )`:
  find the next best dose for ordinal CRM models.

## Examples

``` r
# Example of usage for `NextBestEWOC` NextBest class.

# Create the data.
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
# based on the 'NextBestEWOC' class.
ewoc_next_best <- NextBestEWOC(
  target = 0.30,
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Calculate the next best dose.
dose_recommendation <- nextBest(
  nextBest = ewoc_next_best,
  doselimit = next_max_dose,
  samples = my_samples,
  model = my_model,
  data = my_data
)

# See the probabilities.
dose_recommendation$probs
#>       dose overdose
#>  [1,]  0.1        0
#>  [2,]  0.5        0
#>  [3,]  1.5        0
#>  [4,]  3.0        0
#>  [5,]  6.0        0
#>  [6,] 10.0        0
#>  [7,] 12.0        0
#>  [8,] 14.0        0
#>  [9,] 16.0        0
#> [10,] 18.0        0
#> [11,] 20.0        0
#> [12,] 22.0        0
#> [13,] 24.0        0
#> [14,] 26.0        0
#> [15,] 28.0        0
#> [16,] 30.0        0
#> [17,] 32.0        0
#> [18,] 34.0        0
#> [19,] 36.0        0
#> [20,] 38.0        0
#> [21,] 40.0        0
#> [22,] 42.0        0
#> [23,] 44.0        0
#> [24,] 46.0        0
#> [25,] 48.0        0
#> [26,] 50.0        0
#> [27,] 52.0        0
#> [28,] 54.0        0
#> [29,] 56.0        0
#> [30,] 58.0        0
#> [31,] 60.0        0
#> [32,] 62.0        0
#> [33,] 64.0        0
#> [34,] 66.0        0
#> [35,] 68.0        0
#> [36,] 70.0        0
#> [37,] 72.0        0
#> [38,] 74.0        0
#> [39,] 76.0        0
#> [40,] 78.0        0
#> [41,] 80.0        0
# Example of usage for `NextBestMTD` NextBest class.

# Create the data.
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
# based on the 'NextBestMTD' class.
mtd_next_best <- NextBestMTD(
  target = 0.33,
  derive = function(mtd_samples) {
    quantile(mtd_samples, probs = 0.25)
  }
)

# Calculate the next best dose.
dose_recommendation <- nextBest(
  nextBest = mtd_next_best,
  doselimit = next_max_dose,
  samples = my_samples,
  model = my_model,
  data = my_data
)
# Example of usage for `NextBestNCRM` NextBest class.

# Create the data.
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
# based on the 'NextBestNCRM' class.
nrcm_next_best <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Calculate the next best dose.
dose_recommendation <- nextBest(
  nextBest = nrcm_next_best,
  doselimit = next_max_dose,
  samples = my_samples,
  model = my_model,
  data = my_data
)

# See the probabilities.
dose_recommendation$probs
#>       dose target overdose
#>  [1,]  0.1    0.0        0
#>  [2,]  0.5    0.0        0
#>  [3,]  1.5    0.0        0
#>  [4,]  3.0    0.0        0
#>  [5,]  6.0    0.0        0
#>  [6,] 10.0    0.0        0
#>  [7,] 12.0    0.0        0
#>  [8,] 14.0    0.0        0
#>  [9,] 16.0    0.0        0
#> [10,] 18.0    0.0        0
#> [11,] 20.0    0.0        0
#> [12,] 22.0    0.0        0
#> [13,] 24.0    0.0        0
#> [14,] 26.0    0.0        0
#> [15,] 28.0    0.0        0
#> [16,] 30.0    0.0        0
#> [17,] 32.0    0.0        0
#> [18,] 34.0    0.0        0
#> [19,] 36.0    0.0        0
#> [20,] 38.0    0.0        0
#> [21,] 40.0    0.0        0
#> [22,] 42.0    0.0        0
#> [23,] 44.0    0.0        0
#> [24,] 46.0    0.0        0
#> [25,] 48.0    0.0        0
#> [26,] 50.0    0.0        0
#> [27,] 52.0    0.0        0
#> [28,] 54.0    0.0        0
#> [29,] 56.0    0.0        0
#> [30,] 58.0    0.5        0
#> [31,] 60.0    0.5        0
#> [32,] 62.0    0.5        0
#> [33,] 64.0    0.5        0
#> [34,] 66.0    0.5        0
#> [35,] 68.0    0.5        0
#> [36,] 70.0    0.5        0
#> [37,] 72.0    0.5        0
#> [38,] 74.0    0.5        0
#> [39,] 76.0    0.5        0
#> [40,] 78.0    0.5        0
#> [41,] 80.0    0.5        0
# Example of usage for `NextBestNCRM` nextBest method with two-drug `DataCombo`.

# Create two-drug combination data.
my_data <- DataCombo(
  x = cbind(
    drug1 = c(10, 10, 10, 20, 20, 20),
    drug2 = c(20, 20, 20, 20, 20, 20)
  ),
  y = c(0L, 0L, 0L, 0L, 1L, 0L),
  ID = 1L:6L,
  cohort = c(1L, 1L, 1L, 2L, 2L, 2L),
  doseGrid = list(
    drug1 = c(10, 20, 30),
    drug2 = c(20, 40, 60)
  )
)

# Initialize the two-drug combination model.
my_model <- TwoDrugsCombo(
  single_models = list(
    drug1 = LogisticLogNormal(
      mean = c(-0.85, 1),
      cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
      ref_dose = 10
    ),
    drug2 = LogisticLogNormal(
      mean = c(-0.7, 0.8),
      cov = matrix(c(1.1, -0.3, -0.3, 0.9), nrow = 2),
      ref_dose = 20
    )
  ),
  gamma = 0,
  tau = 1
)

# Set-up MCMC parameters and generate samples from the posterior.
my_options <- McmcOptions(burnin = 5, step = 1, samples = 10)
my_samples <- mcmc(my_data, my_model, my_options)

# Define the dose increment rules for two-drug combination.
rule_one <- IncrementsComboOneDrugOnly()
rule_two <- IncrementsComboCartesian(
  drug1 = IncrementsRelative(intervals = c(0), increments = c(2)),
  drug2 = IncrementsRelative(intervals = c(0), increments = c(1))
)
my_increments <- IncrementsMin(increments_list = list(rule_one, rule_two))
next_max_dose <- maxDose(my_increments, data = my_data)

# Define the next best rule based on the NextBestNCRM class.
my_next_best <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Calculate the next best dose combination.
dose_recommendation <- nextBest(
  nextBest = my_next_best,
  doselimit = next_max_dose,
  samples = my_samples,
  model = my_model,
  data = my_data
)

# See the next best dose combination.
dose_recommendation$value
#> [1] NA

# See the target and overdose probabilities.
dose_recommendation$probs
#>   drug1 drug2 target_prob overdose_prob not_eligible
#> 1    10    20         0.1           0.5         TRUE
#> 2    20    20         0.0           1.0         TRUE
#> 3    30    20         0.1           0.9         TRUE
#> 4    10    40         0.1           0.9         TRUE
#> 5    20    40         0.2           0.6         TRUE
#> 6    30    40          NA            NA        FALSE
#> 7    10    60          NA            NA        FALSE
#> 8    20    60          NA            NA        FALSE
#> 9    30    60          NA            NA        FALSE

# Look at the plot.
dose_recommendation$plot
# Example of usage for `NextBestNCRM-DataParts` NextBest class.

# Create the data.
my_data <- DataParts(
  x = c(0.1, 0.5, 1.5),
  y = c(0, 0, 0),
  ID = 1:3,
  cohort = 1:3,
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2)),
  part = c(1L, 1L, 1L),
  nextPart = 1L,
  part1Ladder = c(0.1, 0.5, 1.5, 3, 6, 10)
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
my_increments <- IncrementsRelativeParts(
  dlt_start = 0,
  clean_start = 1
)
next_max_dose <- maxDose(my_increments, data = my_data)

# Define the rule which will be used to select the next best dose
# based on the 'NextBestNCRM' class.
nrcm_next_best <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Calculate the next best dose.
dose_recommendation <- nextBest(
  nextBest = nrcm_next_best,
  doselimit = next_max_dose,
  samples = my_samples,
  model = my_model,
  data = my_data
)

dose_recommendation
#> $value
#> [1] 3
#> 
#> $plot
#> NULL
#> 
# Example of usage for `NextBestNCRMLoss` NextBest class.

# Create the data.
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
# based on the class `NextBestNCRMLoss`.
nrcm_loss_next_best <- NextBestNCRMLoss(
  target = c(0.2, 0.35),
  overdose = c(0.35, 0.6),
  unacceptable = c(0.6, 1),
  max_overdose_prob = 0.999,
  losses = c(1, 0, 1, 2)
)

# Calculate the next best dose.
dose_recommendation <- nextBest(
  nextBest = nrcm_loss_next_best,
  doselimit = next_max_dose,
  samples = my_samples,
  model = my_model,
  data = my_data
)

# Next best dose.
dose_recommendation$value
#> [1] 10

# Look at the probabilities.
dose_recommendation$probs
#>     dose underdosing target excessive unacceptable         mean     std_dev
#> 0.1  0.1         1.0    0.0       0.0          0.0 0.0006475208 0.001116105
#> 0.5  0.5         1.0    0.0       0.0          0.0 0.0036572058 0.006268202
#> 1.5  1.5         1.0    0.0       0.0          0.0 0.0118548119 0.019780260
#> 3    3.0         1.0    0.0       0.0          0.0 0.0245514051 0.039404381
#> 6    6.0         1.0    0.0       0.0          0.0 0.0496928677 0.073922627
#> 10  10.0         0.8    0.2       0.0          0.0 0.0819548470 0.110007949
#> 12  12.0         0.8    0.2       0.0          0.0 0.0976255202 0.124343907
#> 14  14.0         0.8    0.0       0.2          0.0 0.1130747483 0.136568529
#> 16  16.0         0.8    0.0       0.2          0.0 0.1283581178 0.146924146
#> 18  18.0         0.8    0.0       0.2          0.0 0.1435193390 0.155626391
#> 20  20.0         0.8    0.0       0.2          0.0 0.1585902385 0.162866127
#> 22  22.0         0.8    0.0       0.2          0.0 0.1735918828 0.168812464
#> 24  24.0         0.8    0.0       0.2          0.0 0.1885360803 0.173615832
#> 26  26.0         0.8    0.0       0.2          0.0 0.2034269377 0.177410706
#> 28  28.0         0.6    0.2       0.2          0.0 0.2182623329 0.180317897
#> 30  30.0         0.6    0.2       0.2          0.0 0.2330352509 0.182446416
#> 32  32.0         0.6    0.2       0.2          0.0 0.2477349632 0.183894974
#> 34  34.0         0.6    0.2       0.0          0.2 0.2623480529 0.184753163
#> 36  36.0         0.5    0.3       0.0          0.2 0.2768592935 0.185102414
#> 38  38.0         0.5    0.3       0.0          0.2 0.2912523901 0.185016748
#> 40  40.0         0.5    0.3       0.0          0.2 0.3055105978 0.184563397
#> 42  42.0         0.1    0.7       0.0          0.2 0.3196172278 0.183803311
#> 44  44.0         0.1    0.7       0.0          0.2 0.3335560544 0.182791590
#> 46  46.0         0.1    0.7       0.0          0.2 0.3473116330 0.181577858
#> 48  48.0         0.1    0.7       0.0          0.2 0.3608695423 0.180206602
#> 50  50.0         0.1    0.5       0.2          0.2 0.3742165584 0.178717478
#> 52  52.0         0.1    0.5       0.2          0.2 0.3873407726 0.177145604
#> 54  54.0         0.1    0.4       0.3          0.2 0.4002316609 0.175521842
#> 56  56.0         0.1    0.4       0.3          0.2 0.4128801132 0.173873068
#> 58  58.0         0.1    0.4       0.3          0.2 0.4252784296 0.172222446
#> 60  60.0         0.1    0.0       0.7          0.2 0.4374202909 0.170589687
#> 62  62.0         0.0    0.1       0.7          0.2 0.4493007083 0.168991324
#> 64  64.0         0.0    0.1       0.7          0.2 0.4609159578 0.167440964
#> 66  66.0         0.0    0.1       0.7          0.2 0.4722635035 0.165949551
#> 68  68.0         0.0    0.1       0.7          0.2 0.4833419133 0.164525620
#> 70  70.0         0.0    0.1       0.7          0.2 0.4941507705 0.163175544
#> 72  72.0         0.0    0.1       0.7          0.2 0.5046905833 0.161903777
#> 74  74.0         0.0    0.1       0.7          0.2 0.5149626948 0.160713083
#> 76  76.0         0.0    0.1       0.7          0.2 0.5249691942 0.159604757
#> 78  78.0         0.0    0.1       0.7          0.2 0.5347128315 0.158578838
#> 80  80.0         0.0    0.1       0.7          0.2 0.5441969360 0.157634299
#>     posterior_loss
#> 0.1            1.0
#> 0.5            1.0
#> 1.5            1.0
#> 3              1.0
#> 6              1.0
#> 10             0.8
#> 12             0.8
#> 14             1.0
#> 16             1.0
#> 18             1.0
#> 20             1.0
#> 22             1.0
#> 24             1.0
#> 26             1.0
#> 28             0.8
#> 30             0.8
#> 32             0.8
#> 34             1.0
#> 36             0.9
#> 38             0.9
#> 40             0.9
#> 42             0.5
#> 44             0.5
#> 46             0.5
#> 48             0.5
#> 50             0.7
#> 52             0.7
#> 54             0.8
#> 56             0.8
#> 58             0.8
#> 60             1.2
#> 62             1.1
#> 64             1.1
#> 66             1.1
#> 68             1.1
#> 70             1.1
#> 72             1.1
#> 74             1.1
#> 76             1.1
#> 78             1.1
#> 80             1.1

# Define another rule (loss function of 3 elements).
nrcm_loss_next_best_losses_3 <- NextBestNCRMLoss(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.30,
  losses = c(1, 0, 2)
)

# Calculate the next best dose.
dose_recommendation_losses_3 <- nextBest(
  nextBest = nrcm_loss_next_best_losses_3,
  doselimit = next_max_dose,
  samples = my_samples,
  model = my_model,
  data = my_data
)

# Next best dose.
dose_recommendation_losses_3$value
#> [1] 10

# Look at the probabilities.
dose_recommendation_losses_3$probs
#>     dose underdosing target overdose         mean     std_dev posterior_loss
#> 0.1  0.1         1.0    0.0      0.0 0.0006475208 0.001116105            1.0
#> 0.5  0.5         1.0    0.0      0.0 0.0036572058 0.006268202            1.0
#> 1.5  1.5         1.0    0.0      0.0 0.0118548119 0.019780260            1.0
#> 3    3.0         1.0    0.0      0.0 0.0245514051 0.039404381            1.0
#> 6    6.0         1.0    0.0      0.0 0.0496928677 0.073922627            1.0
#> 10  10.0         0.8    0.2      0.0 0.0819548470 0.110007949            0.8
#> 12  12.0         0.8    0.2      0.0 0.0976255202 0.124343907            0.8
#> 14  14.0         0.8    0.0      0.2 0.1130747483 0.136568529            1.2
#> 16  16.0         0.8    0.0      0.2 0.1283581178 0.146924146            1.2
#> 18  18.0         0.8    0.0      0.2 0.1435193390 0.155626391            1.2
#> 20  20.0         0.8    0.0      0.2 0.1585902385 0.162866127            1.2
#> 22  22.0         0.8    0.0      0.2 0.1735918828 0.168812464            1.2
#> 24  24.0         0.8    0.0      0.2 0.1885360803 0.173615832            1.2
#> 26  26.0         0.8    0.0      0.2 0.2034269377 0.177410706            1.2
#> 28  28.0         0.6    0.2      0.2 0.2182623329 0.180317897            1.0
#> 30  30.0         0.6    0.2      0.2 0.2330352509 0.182446416            1.0
#> 32  32.0         0.6    0.2      0.2 0.2477349632 0.183894974            1.0
#> 34  34.0         0.6    0.2      0.2 0.2623480529 0.184753163            1.0
#> 36  36.0         0.5    0.3      0.2 0.2768592935 0.185102414            0.9
#> 38  38.0         0.5    0.3      0.2 0.2912523901 0.185016748            0.9
#> 40  40.0         0.5    0.3      0.2 0.3055105978 0.184563397            0.9
#> 42  42.0         0.1    0.7      0.2 0.3196172278 0.183803311            0.5
#> 44  44.0         0.1    0.7      0.2 0.3335560544 0.182791590            0.5
#> 46  46.0         0.1    0.7      0.2 0.3473116330 0.181577858            0.5
#> 48  48.0         0.1    0.7      0.2 0.3608695423 0.180206602            0.5
#> 50  50.0         0.1    0.5      0.4 0.3742165584 0.178717478            0.9
#> 52  52.0         0.1    0.5      0.4 0.3873407726 0.177145604            0.9
#> 54  54.0         0.1    0.4      0.5 0.4002316609 0.175521842            1.1
#> 56  56.0         0.1    0.4      0.5 0.4128801132 0.173873068            1.1
#> 58  58.0         0.1    0.4      0.5 0.4252784296 0.172222446            1.1
#> 60  60.0         0.1    0.0      0.9 0.4374202909 0.170589687            1.9
#> 62  62.0         0.0    0.1      0.9 0.4493007083 0.168991324            1.8
#> 64  64.0         0.0    0.1      0.9 0.4609159578 0.167440964            1.8
#> 66  66.0         0.0    0.1      0.9 0.4722635035 0.165949551            1.8
#> 68  68.0         0.0    0.1      0.9 0.4833419133 0.164525620            1.8
#> 70  70.0         0.0    0.1      0.9 0.4941507705 0.163175544            1.8
#> 72  72.0         0.0    0.1      0.9 0.5046905833 0.161903777            1.8
#> 74  74.0         0.0    0.1      0.9 0.5149626948 0.160713083            1.8
#> 76  76.0         0.0    0.1      0.9 0.5249691942 0.159604757            1.8
#> 78  78.0         0.0    0.1      0.9 0.5347128315 0.158578838            1.8
#> 80  80.0         0.0    0.1      0.9 0.5441969360 0.157634299            1.8
# Example of usage for `NextBestThreePlusThree` NextBest class.

# Create the data.
my_data <- Data(
  x = c(5, 5, 5, 10, 10, 10),
  y = c(0, 0, 0, 0, 1, 0),
  ID = 1:6,
  cohort = c(0, 0, 0, 1, 1, 1),
  doseGrid = c(0.1, 0.5, 1.5, 3, 5, seq(from = 10, to = 80, by = 2))
)

# The rule to select the next best dose will be based on the 3+3 method.
my_next_best <- NextBestThreePlusThree()

# Calculate the next best dose.
dose_recommendation <- nextBest(my_next_best, data = my_data)
# Example of usage for `NextBestDualEndpoint` NextBest class.

# Create the data.
my_data <- DataDual(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10, 20, 20, 20, 40, 40, 40, 50, 50, 50),
  y = c(0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1),
  ID = 1:17,
  cohort = c(
    1L,
    2L,
    3L,
    4L,
    5L,
    6L,
    6L,
    6L,
    7L,
    7L,
    7L,
    8L,
    8L,
    8L,
    9L,
    9L,
    9L
  ),
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

# Initialize the Dual-Endpoint model (in this case RW1).
my_model <- DualEndpointRW(
  mean = c(0, 1),
  cov = matrix(c(1, 0, 0, 1), nrow = 2),
  sigma2betaW = 0.01,
  sigma2W = c(a = 0.1, b = 0.1),
  rho = c(a = 1, b = 1),
  rw1 = TRUE
)

# Set-up some MCMC parameters and generate samples from the posterior.
my_options <- McmcOptions(burnin = 5, step = 1, samples = 10)

# \donttest{
my_samples <- mcmc(my_data, my_model, my_options)

# Define the rule for dose increments and calculate the maximum dose allowed.
my_increments <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)
next_max_dose <- maxDose(my_increments, data = my_data)

# Define the rule which will be used to select the next best dose. In this case,
# target a dose achieving at least 0.9 of maximum biomarker level (efficacy)
# and with a probability below 0.25 that prob(DLT)>0.35 (safety).
de_next_best <- NextBestDualEndpoint(
  target = c(0.9, 1),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Calculate the next best dose.
dose_recommendation <- nextBest(
  nextBest = de_next_best,
  doselimit = next_max_dose,
  samples = my_samples,
  model = my_model,
  data = my_data
)

# See the probabilities.
dose_recommendation$probs
#>       dose target overdose
#>  [1,]  0.1    0.1      0.1
#>  [2,]  0.5    0.0      0.1
#>  [3,]  1.5    0.0      0.1
#>  [4,]  3.0    0.0      0.1
#>  [5,]  6.0    0.4      0.1
#>  [6,] 10.0    0.3      0.6
#>  [7,] 12.0    0.1      0.6
#>  [8,] 14.0    0.0      0.6
#>  [9,] 16.0    0.0      1.0
#> [10,] 18.0    0.0      1.0
#> [11,] 20.0    0.0      1.0
#> [12,] 22.0    0.0      1.0
#> [13,] 24.0    0.0      1.0
#> [14,] 26.0    0.0      1.0
#> [15,] 28.0    0.0      1.0
#> [16,] 30.0    0.0      1.0
#> [17,] 32.0    0.0      1.0
#> [18,] 34.0    0.0      1.0
#> [19,] 36.0    0.0      1.0
#> [20,] 38.0    0.0      1.0
#> [21,] 40.0    0.0      1.0
#> [22,] 42.0    0.0      1.0
#> [23,] 44.0    0.0      1.0
#> [24,] 46.0    0.0      1.0
#> [25,] 48.0    0.0      1.0
#> [26,] 50.0    0.0      1.0
#> [27,] 52.0    0.0      1.0
#> [28,] 54.0    0.0      1.0
#> [29,] 56.0    0.0      1.0
#> [30,] 58.0    0.0      1.0
#> [31,] 60.0    0.0      1.0
#> [32,] 62.0    0.0      1.0
#> [33,] 64.0    0.0      1.0
#> [34,] 66.0    0.1      1.0
#> [35,] 68.0    0.0      1.0
#> [36,] 70.0    0.0      1.0
#> [37,] 72.0    0.0      1.0
#> [38,] 74.0    0.0      1.0
#> [39,] 76.0    0.0      1.0
#> [40,] 78.0    0.0      1.0
#> [41,] 80.0    0.0      1.0

# Joint plot.
print(dose_recommendation$plot)


# Show customization of single plot.
variant1 <- dose_recommendation$singlePlots$plot1 + xlim(0, 20)
print(variant1)
#> Warning: Removed 31 rows containing missing values or values outside the scale range
#> (`geom_bar()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_vline()`).

# }
# Example of usage for `NextBestTD` NextBest class.
my_data <- Data(
  x = c(25, 50, 50, 75, 150, 200, 225, 300),
  y = c(0, 0, 0, 0, 1, 1, 1, 1),
  ID = 1:8,
  cohort = c(1L, 2L, 2L, 3L, 4L, 5L, 6L, 7L),
  doseGrid = seq(from = 25, to = 300, by = 25)
)

my_model <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEweights = c(3, 3),
  DLEdose = c(25, 300),
  data = my_data
)

# Target probabilities of the occurrence of a DLT during trial and
# at the end of the trial are defined as 0.35 and 0.3, respectively.
td_next_best <- NextBestTD(prob_target_drt = 0.35, prob_target_eot = 0.3)

# doselimit is the maximum allowable dose level to be given to subjects.
dose_recommendation <- nextBest(
  nextBest = td_next_best,
  doselimit = max(my_data@doseGrid),
  model = my_model,
  data = my_data
)

dose_recommendation$next_dose_drt
#> [1] 50
dose_recommendation$plot

# Example of usage for `NextBestTDsamples` NextBest class.
my_data <- Data(
  x = c(25, 50, 50, 75, 150, 200, 225, 300),
  y = c(0, 0, 0, 0, 1, 1, 1, 1),
  ID = 1:8,
  cohort = c(1L, 2L, 2L, 3L, 4L, 5L, 6L, 7L),
  doseGrid = seq(from = 25, to = 300, by = 25)
)

my_model <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEweights = c(3, 3),
  DLEdose = c(25, 300),
  data = my_data
)

# Set-up some MCMC parameters and generate samples.
my_options <- McmcOptions(burnin = 100, step = 2, samples = 800)
my_samples <- mcmc(my_data, my_model, my_options)

# Target probabilities of the occurrence of a DLT during trial and
# at the end of the trial are defined as 0.35 and 0.3, respectively.
# 'derive' is specified such that the 30% posterior quantile of the TD35 and
# TD30 samples will be used as TD35 and TD30 estimates.
tds_next_best <- NextBestTDsamples(
  prob_target_drt = 0.35,
  prob_target_eot = 0.3,
  derive = function(samples) {
    as.numeric(quantile(samples, probs = 0.3))
  }
)

# doselimit is the maximum allowable dose level to be given to subjects.
dose_recommendation <- nextBest(
  nextBest = tds_next_best,
  doselimit = max(my_data@doseGrid),
  samples = my_samples,
  model = my_model,
  data = my_data
)

dose_recommendation$next_dose_drt
#> [1] 25
dose_recommendation$plot
#> Warning: Some data points are outside of `bounds`. Removing them.
#> Warning: Some data points are outside of `bounds`. Removing them.

# Example of usage for `NextBestMaxGain` NextBest class.

# Create the data.
my_data <- DataDual(
  x = c(25, 50, 25, 50, 75, 300, 250, 150),
  y = c(0, 0, 0, 0, 0, 1, 1, 0),
  ID = 1:8,
  cohort = 1:8,
  w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
  doseGrid = seq(25, 300, 25),
  placebo = FALSE
)

# 'ModelTox' DLT model, e.g 'LogisticIndepBeta'.
my_model_dlt <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEweights = c(3, 3),
  DLEdose = c(25, 300),
  data = my_data
)

# 'ModelEff' efficacy model, e.g. 'Effloglog'.
my_model_eff <- Effloglog(
  eff = c(1.223, 2.513),
  eff_dose = c(25, 300),
  nu = c(a = 1, b = 0.025),
  data = my_data
)

# Target probabilities of the occurrence of a DLT during trial and at the
# end of trial are defined as 0.35 and 0.3, respectively.
mg_next_best <- NextBestMaxGain(
  prob_target_drt = 0.35,
  prob_target_eot = 0.3
)

# doselimit is the maximum allowable dose level to be given to subjects.
dose_recommendation <- nextBest(
  nextBest = mg_next_best,
  doselimit = 300,
  model = my_model_dlt,
  model_eff = my_model_eff,
  data = my_data
)

dose_recommendation$next_dose
#> [1] 75
dose_recommendation$plot

# Example of usage for `NextBestMaxGainSamples` NextBest class.

# Create the data.
my_data <- DataDual(
  x = c(25, 50, 25, 50, 75, 300, 250, 150),
  y = c(0, 0, 0, 0, 0, 1, 1, 0),
  w = c(0.31, 0.42, 0.59, 0.45, 0.6, 0.7, 0.6, 0.52),
  ID = 1:8,
  cohort = 1:8,
  doseGrid = seq(25, 300, 25),
  placebo = FALSE
)

# 'ModelTox' DLT model, e.g 'LogisticIndepBeta'.
my_model_dlt <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEweights = c(3, 3),
  DLEdose = c(25, 300),
  data = my_data
)

# 'ModelEff' efficacy model, e.g 'Effloglog'.
my_model_effll <- Effloglog(
  eff = c(1.223, 2.513),
  eff_dose = c(25, 300),
  nu = c(a = 1, b = 0.025),
  data = my_data
)

# Set-up some MCMC parameters and generate samples from the posterior.
my_options <- McmcOptions(burnin = 5, step = 1, samples = 10)
my_samples_dlt <- mcmc(my_data, my_model_dlt, my_options)
my_samples_effll <- mcmc(my_data, my_model_effll, my_options)

# Target probabilities of the occurrence of a DLT during trial and at the end of
# trial are defined as 0.35 and 0.3, respectively.
# Use 30% posterior quantile of the TD35 and TD30 samples as estimates of TD35
# and TD30.
# Use 50% posterior quantile of the Gstar (the dose which gives the maxim gain value)
# samples as Gstar estimate.
mgs_next_best <- NextBestMaxGainSamples(
  prob_target_drt = 0.35,
  prob_target_eot = 0.3,
  derive = function(samples) {
    as.numeric(quantile(samples, prob = 0.3))
  },
  mg_derive = function(mg_samples) {
    as.numeric(quantile(mg_samples, prob = 0.5))
  }
)

dose_recommendation <- nextBest(
  nextBest = mgs_next_best,
  doselimit = max(my_data@doseGrid),
  samples = my_samples_dlt,
  model = my_model_dlt,
  data = my_data,
  model_eff = my_model_effll,
  samples_eff = my_samples_effll
)
#> [1] "Estimated TD 35 = 18.5931775531197 not within dose grid"
#> [1] "Estimated TD 30 = 13.4232117251262 not within dose grid"

dose_recommendation$next_dose
#> [1] NA
dose_recommendation$plot
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_vline()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_text()`).


# Now using the 'EffFlexi' class efficacy model:

my_model_effflexi <- EffFlexi(
  eff = c(1.223, 2.513),
  eff_dose = c(25, 300),
  sigma2W = c(a = 0.1, b = 0.1),
  sigma2betaW = c(a = 20, b = 50),
  rw1 = FALSE,
  data = my_data
)

# \donttest{
my_samples_effflexi <- mcmc(my_data, my_model_effflexi, my_options)

dose_recommendation <- nextBest(
  nextBest = mgs_next_best,
  doselimit = max(my_data@doseGrid),
  samples = my_samples_dlt,
  model = my_model_dlt,
  data = my_data,
  model_eff = my_model_effflexi,
  samples_eff = my_samples_effflexi
)
#> [1] "Estimated TD 35 = 18.5931775531197 not within dose grid"
#> [1] "Estimated TD 30 = 13.4232117251262 not within dose grid"

dose_recommendation$next_dose
#> [1] NA
dose_recommendation$plot
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_vline()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_text()`).
# }
# Example of usage for `NextBestProbMTDLTE` NextBest class.

# Create the data.
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
# based on the 'NextBestProbMTDLTE' class.
nb_mtd_lte <- NextBestProbMTDLTE(target = 0.33)

# Calculate the next best dose.
dose_recommendation <- nextBest(
  nextBest = nb_mtd_lte,
  doselimit = next_max_dose,
  samples = my_samples,
  model = my_model,
  data = my_data
)
# Example of usage for `NextBestProbMTDMinDist` NextBest class.

# Create the data.
my_data <- Data(
  x = c(1.5, 1.5, 1.5, 2.5, 2.5, 2.5, 3.5, 3.5, 3.5),
  y = c(0, 0, 0, 0, 0, 0, 1, 1, 0),
  ID = 1:9,
  cohort = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
  doseGrid = c(1.5, 2.5, 3.5, 4.5, 6, 7)
)

# Initialize the CRM model used to model the data.
my_model <- my_model <- LogisticKadaneBetaGamma(
  theta = 0.3,
  xmin = 1.5,
  xmax = 7,
  alpha = 1,
  beta = 19,
  shape = 0.5625,
  rate = 0.125
)

# Set-up some MCMC parameters and generate samples from the posterior.
my_options <- McmcOptions(burnin = 5, step = 1, samples = 10)
my_samples <- mcmc(my_data, my_model, my_options)

# Define the rule for dose increments and calculate the maximum dose allowed.
my_increments <- IncrementsDoseLevels(levels = 1)

next_max_dose <- maxDose(my_increments, data = my_data)

# Define the rule which will be used to select the next best dose
# based on the 'NextBestProbMTDMinDist' class.
nb_mtd_min_dist <- NextBestProbMTDMinDist(target = 0.3)

# Calculate the next best dose.
dose_recommendation <- nextBest(
  nextBest = nb_mtd_min_dist,
  doselimit = next_max_dose,
  samples = my_samples,
  model = my_model,
  data = my_data
)
ordinal_data <- .DefaultDataOrdinal()
ordinal_model <- .DefaultLogisticLogNormalOrdinal()
options <- .DefaultMcmcOptions()

# \donttest{
ordinal_samples <- mcmc(ordinal_data, ordinal_model, options)

nextBest(
  nextBest = NextBestOrdinal(2L, .DefaultNextBestNCRM()),
  samples = ordinal_samples,
  doselimit = Inf,
  model = ordinal_model,
  data = ordinal_data
)

#> $value
#> [1] 60
#> 
#> $plot
#> 
#> $singlePlots
#> $singlePlots$plot1

#> 
#> $singlePlots$plot2
#> 
#> 
#> $probs
#>       dose target overdose
#>  [1,]   10  0.000    0.000
#>  [2,]   20  0.000    0.000
#>  [3,]   30  0.000    0.000
#>  [4,]   40  0.004    0.000
#>  [5,]   50  0.024    0.002
#>  [6,]   60  0.225    0.136
#>  [7,]   70  0.174    0.493
#>  [8,]   80  0.132    0.650
#>  [9,]   90  0.088    0.741
#> [10,]  100  0.076    0.792
#> 
# }
ordinal_data <- .DefaultDataOrdinal()
ordinal_model <- .DefaultLogisticLogNormalOrdinal()
options <- .DefaultMcmcOptions()

# \donttest{
ordinal_samples <- mcmc(ordinal_data, ordinal_model, options)

nextBest(
  nextBest = NextBestOrdinal(2L, .DefaultNextBestNCRM()),
  samples = ordinal_samples,
  doselimit = Inf,
  model = ordinal_model,
  data = ordinal_data
)

#> $value
#> [1] 60
#> 
#> $plot
#> 
#> $singlePlots
#> $singlePlots$plot1

#> 
#> $singlePlots$plot2

#> 
#> 
#> $probs
#>       dose target overdose
#>  [1,]   10  0.000    0.000
#>  [2,]   20  0.000    0.000
#>  [3,]   30  0.000    0.000
#>  [4,]   40  0.002    0.000
#>  [5,]   50  0.018    0.002
#>  [6,]   60  0.253    0.147
#>  [7,]   70  0.160    0.524
#>  [8,]   80  0.129    0.672
#>  [9,]   90  0.087    0.755
#> [10,]  100  0.083    0.798
#> 
# }
```
