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
#>  [1,]  0.1      0.0
#>  [2,]  0.5      0.0
#>  [3,]  1.5      0.0
#>  [4,]  3.0      0.0
#>  [5,]  6.0      0.0
#>  [6,] 10.0      0.0
#>  [7,] 12.0      0.0
#>  [8,] 14.0      0.0
#>  [9,] 16.0      0.0
#> [10,] 18.0      0.0
#> [11,] 20.0      0.0
#> [12,] 22.0      0.0
#> [13,] 24.0      0.9
#> [14,] 26.0      0.9
#> [15,] 28.0      0.9
#> [16,] 30.0      0.9
#> [17,] 32.0      0.9
#> [18,] 34.0      0.9
#> [19,] 36.0      0.9
#> [20,] 38.0      1.0
#> [21,] 40.0      1.0
#> [22,] 42.0      1.0
#> [23,] 44.0      1.0
#> [24,] 46.0      1.0
#> [25,] 48.0      1.0
#> [26,] 50.0      1.0
#> [27,] 52.0      1.0
#> [28,] 54.0      1.0
#> [29,] 56.0      1.0
#> [30,] 58.0      1.0
#> [31,] 60.0      1.0
#> [32,] 62.0      1.0
#> [33,] 64.0      1.0
#> [34,] 66.0      1.0
#> [35,] 68.0      1.0
#> [36,] 70.0      1.0
#> [37,] 72.0      1.0
#> [38,] 74.0      1.0
#> [39,] 76.0      1.0
#> [40,] 78.0      1.0
#> [41,] 80.0      1.0
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
#>  [1,]  0.1    0.0      0.0
#>  [2,]  0.5    0.0      0.0
#>  [3,]  1.5    0.0      0.0
#>  [4,]  3.0    0.0      0.0
#>  [5,]  6.0    0.3      0.0
#>  [6,] 10.0    0.5      0.0
#>  [7,] 12.0    0.2      0.3
#>  [8,] 14.0    0.5      0.3
#>  [9,] 16.0    0.5      0.3
#> [10,] 18.0    0.3      0.5
#> [11,] 20.0    0.3      0.5
#> [12,] 22.0    0.3      0.5
#> [13,] 24.0    0.0      0.8
#> [14,] 26.0    0.0      0.8
#> [15,] 28.0    0.2      0.8
#> [16,] 30.0    0.2      0.8
#> [17,] 32.0    0.2      0.8
#> [18,] 34.0    0.2      0.8
#> [19,] 36.0    0.2      0.8
#> [20,] 38.0    0.2      0.8
#> [21,] 40.0    0.2      0.8
#> [22,] 42.0    0.2      0.8
#> [23,] 44.0    0.2      0.8
#> [24,] 46.0    0.2      0.8
#> [25,] 48.0    0.2      0.8
#> [26,] 50.0    0.2      0.8
#> [27,] 52.0    0.2      0.8
#> [28,] 54.0    0.2      0.8
#> [29,] 56.0    0.2      0.8
#> [30,] 58.0    0.2      0.8
#> [31,] 60.0    0.2      0.8
#> [32,] 62.0    0.2      0.8
#> [33,] 64.0    0.2      0.8
#> [34,] 66.0    0.2      0.8
#> [35,] 68.0    0.2      0.8
#> [36,] 70.0    0.2      0.8
#> [37,] 72.0    0.2      0.8
#> [38,] 74.0    0.2      0.8
#> [39,] 76.0    0.2      0.8
#> [40,] 78.0    0.2      0.8
#> [41,] 80.0    0.2      0.8
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
#> 1    10    20         0.4           0.5         TRUE
#> 2    20    20         0.1           0.5         TRUE
#> 3    30    20         0.1           0.4         TRUE
#> 4    10    40         0.2           0.3         TRUE
#> 5    20    40         0.0           0.3         TRUE
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
#> [1] 0.1

# Look at the probabilities.
dose_recommendation$probs
#>     dose underdosing target excessive unacceptable         mean      std_dev
#> 0.1  0.1         1.0    0.0       0.0            0 0.0009807846 0.0006808776
#> 0.5  0.5         1.0    0.0       0.0            0 0.0041335093 0.0024421622
#> 1.5  1.5         1.0    0.0       0.0            0 0.0111679700 0.0054812188
#> 3    3.0         1.0    0.0       0.0            0 0.0209896989 0.0086676693
#> 6    6.0         1.0    0.0       0.0            0 0.0394481438 0.0127452740
#> 10  10.0         1.0    0.0       0.0            0 0.0625805966 0.0156412036
#> 12  12.0         1.0    0.0       0.0            0 0.0736654023 0.0164106826
#> 14  14.0         1.0    0.0       0.0            0 0.0844716846 0.0168564776
#> 16  16.0         1.0    0.0       0.0            0 0.0950193927 0.0170423127
#> 18  18.0         1.0    0.0       0.0            0 0.1053237441 0.0170174541
#> 20  20.0         1.0    0.0       0.0            0 0.1153969652 0.0168214579
#> 22  22.0         1.0    0.0       0.0            0 0.1252492802 0.0164871064
#> 24  24.0         1.0    0.0       0.0            0 0.1348895115 0.0160423649
#> 26  26.0         1.0    0.0       0.0            0 0.1443254609 0.0155117769
#> 28  28.0         1.0    0.0       0.0            0 0.1535641619 0.0149175253
#> 30  30.0         1.0    0.0       0.0            0 0.1626120525 0.0142802917
#> 32  32.0         1.0    0.0       0.0            0 0.1714750967 0.0136199862
#> 34  34.0         1.0    0.0       0.0            0 0.1801588711 0.0129563836
#> 36  36.0         1.0    0.0       0.0            0 0.1886686297 0.0123096577
#> 38  38.0         0.4    0.6       0.0            0 0.1970093511 0.0117007561
#> 40  40.0         0.1    0.9       0.0            0 0.2051857756 0.0111514962
#> 42  42.0         0.1    0.9       0.0            0 0.2132024332 0.0106841986
#> 44  44.0         0.1    0.9       0.0            0 0.2210636656 0.0103206626
#> 46  46.0         0.0    1.0       0.0            0 0.2287736445 0.0100804007
#> 48  48.0         0.0    1.0       0.0            0 0.2363363852 0.0099783556
#> 50  50.0         0.0    1.0       0.0            0 0.2437557594 0.0100227368
#> 52  52.0         0.0    1.0       0.0            0 0.2510355040 0.0102138332
#> 54  54.0         0.0    1.0       0.0            0 0.2581792304 0.0105443714
#> 56  56.0         0.0    1.0       0.0            0 0.2651904307 0.0110012917
#> 58  58.0         0.0    1.0       0.0            0 0.2720724851 0.0115682111
#> 60  60.0         0.0    1.0       0.0            0 0.2788286661 0.0122277467
#> 62  62.0         0.0    1.0       0.0            0 0.2854621446 0.0129632042
#> 64  64.0         0.0    1.0       0.0            0 0.2919759932 0.0137595376
#> 66  66.0         0.0    1.0       0.0            0 0.2983731913 0.0146037285
#> 68  68.0         0.0    1.0       0.0            0 0.3046566277 0.0154847978
#> 70  70.0         0.0    1.0       0.0            0 0.3108291050 0.0163936267
#> 72  72.0         0.0    1.0       0.0            0 0.3168933420 0.0173227046
#> 74  74.0         0.0    1.0       0.0            0 0.3228519771 0.0182658685
#> 76  76.0         0.0    0.7       0.3            0 0.3287075713 0.0192180660
#> 78  78.0         0.0    0.7       0.3            0 0.3344626100 0.0201751509
#> 80  80.0         0.0    0.7       0.3            0 0.3401195067 0.0211337152
#>     posterior_loss
#> 0.1            1.0
#> 0.5            1.0
#> 1.5            1.0
#> 3              1.0
#> 6              1.0
#> 10             1.0
#> 12             1.0
#> 14             1.0
#> 16             1.0
#> 18             1.0
#> 20             1.0
#> 22             1.0
#> 24             1.0
#> 26             1.0
#> 28             1.0
#> 30             1.0
#> 32             1.0
#> 34             1.0
#> 36             1.0
#> 38             0.4
#> 40             0.1
#> 42             0.1
#> 44             0.1
#> 46             0.0
#> 48             0.0
#> 50             0.0
#> 52             0.0
#> 54             0.0
#> 56             0.0
#> 58             0.0
#> 60             0.0
#> 62             0.0
#> 64             0.0
#> 66             0.0
#> 68             0.0
#> 70             0.0
#> 72             0.0
#> 74             0.0
#> 76             0.3
#> 78             0.3
#> 80             0.3

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
#> [1] 0.1

# Look at the probabilities.
dose_recommendation_losses_3$probs
#>     dose underdosing target overdose         mean      std_dev posterior_loss
#> 0.1  0.1         1.0    0.0      0.0 0.0009807846 0.0006808776            1.0
#> 0.5  0.5         1.0    0.0      0.0 0.0041335093 0.0024421622            1.0
#> 1.5  1.5         1.0    0.0      0.0 0.0111679700 0.0054812188            1.0
#> 3    3.0         1.0    0.0      0.0 0.0209896989 0.0086676693            1.0
#> 6    6.0         1.0    0.0      0.0 0.0394481438 0.0127452740            1.0
#> 10  10.0         1.0    0.0      0.0 0.0625805966 0.0156412036            1.0
#> 12  12.0         1.0    0.0      0.0 0.0736654023 0.0164106826            1.0
#> 14  14.0         1.0    0.0      0.0 0.0844716846 0.0168564776            1.0
#> 16  16.0         1.0    0.0      0.0 0.0950193927 0.0170423127            1.0
#> 18  18.0         1.0    0.0      0.0 0.1053237441 0.0170174541            1.0
#> 20  20.0         1.0    0.0      0.0 0.1153969652 0.0168214579            1.0
#> 22  22.0         1.0    0.0      0.0 0.1252492802 0.0164871064            1.0
#> 24  24.0         1.0    0.0      0.0 0.1348895115 0.0160423649            1.0
#> 26  26.0         1.0    0.0      0.0 0.1443254609 0.0155117769            1.0
#> 28  28.0         1.0    0.0      0.0 0.1535641619 0.0149175253            1.0
#> 30  30.0         1.0    0.0      0.0 0.1626120525 0.0142802917            1.0
#> 32  32.0         1.0    0.0      0.0 0.1714750967 0.0136199862            1.0
#> 34  34.0         1.0    0.0      0.0 0.1801588711 0.0129563836            1.0
#> 36  36.0         1.0    0.0      0.0 0.1886686297 0.0123096577            1.0
#> 38  38.0         0.4    0.6      0.0 0.1970093511 0.0117007561            0.4
#> 40  40.0         0.1    0.9      0.0 0.2051857756 0.0111514962            0.1
#> 42  42.0         0.1    0.9      0.0 0.2132024332 0.0106841986            0.1
#> 44  44.0         0.1    0.9      0.0 0.2210636656 0.0103206626            0.1
#> 46  46.0         0.0    1.0      0.0 0.2287736445 0.0100804007            0.0
#> 48  48.0         0.0    1.0      0.0 0.2363363852 0.0099783556            0.0
#> 50  50.0         0.0    1.0      0.0 0.2437557594 0.0100227368            0.0
#> 52  52.0         0.0    1.0      0.0 0.2510355040 0.0102138332            0.0
#> 54  54.0         0.0    1.0      0.0 0.2581792304 0.0105443714            0.0
#> 56  56.0         0.0    1.0      0.0 0.2651904307 0.0110012917            0.0
#> 58  58.0         0.0    1.0      0.0 0.2720724851 0.0115682111            0.0
#> 60  60.0         0.0    1.0      0.0 0.2788286661 0.0122277467            0.0
#> 62  62.0         0.0    1.0      0.0 0.2854621446 0.0129632042            0.0
#> 64  64.0         0.0    1.0      0.0 0.2919759932 0.0137595376            0.0
#> 66  66.0         0.0    1.0      0.0 0.2983731913 0.0146037285            0.0
#> 68  68.0         0.0    1.0      0.0 0.3046566277 0.0154847978            0.0
#> 70  70.0         0.0    1.0      0.0 0.3108291050 0.0163936267            0.0
#> 72  72.0         0.0    1.0      0.0 0.3168933420 0.0173227046            0.0
#> 74  74.0         0.0    1.0      0.0 0.3228519771 0.0182658685            0.0
#> 76  76.0         0.0    0.7      0.3 0.3287075713 0.0192180660            0.6
#> 78  78.0         0.0    0.7      0.3 0.3344626100 0.0201751509            0.6
#> 80  80.0         0.0    0.7      0.3 0.3401195067 0.0211337152            0.6
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
#>  [1,]  0.1    0.2      0.9
#>  [2,]  0.5    0.1      1.0
#>  [3,]  1.5    0.1      1.0
#>  [4,]  3.0    0.0      1.0
#>  [5,]  6.0    0.0      1.0
#>  [6,] 10.0    0.0      1.0
#>  [7,] 12.0    0.1      1.0
#>  [8,] 14.0    0.0      1.0
#>  [9,] 16.0    0.1      1.0
#> [10,] 18.0    0.0      1.0
#> [11,] 20.0    0.0      1.0
#> [12,] 22.0    0.0      1.0
#> [13,] 24.0    0.0      1.0
#> [14,] 26.0    0.2      1.0
#> [15,] 28.0    0.1      1.0
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
#> [34,] 66.0    0.0      1.0
#> [35,] 68.0    0.0      1.0
#> [36,] 70.0    0.0      1.0
#> [37,] 72.0    0.0      1.0
#> [38,] 74.0    0.0      1.0
#> [39,] 76.0    0.1      1.0
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
#>  [5,]   50  0.029    0.002
#>  [6,]   60  0.225    0.125
#>  [7,]   70  0.192    0.474
#>  [8,]   80  0.139    0.646
#>  [9,]   90  0.105    0.733
#> [10,]  100  0.076    0.797
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
#>  [2,]   20  0.001    0.000
#>  [3,]   30  0.001    0.000
#>  [4,]   40  0.003    0.000
#>  [5,]   50  0.013    0.004
#>  [6,]   60  0.201    0.166
#>  [7,]   70  0.169    0.505
#>  [8,]   80  0.129    0.661
#>  [9,]   90  0.108    0.738
#> [10,]  100  0.084    0.789
#> 
# }
```
