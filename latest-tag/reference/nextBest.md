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

  (`NextBest`)  
  the rule for the next best dose.

- doselimit:

  (`number`)  
  the maximum allowed next dose. If it is an infinity (default), then
  essentially no dose limit will be applied in the course of dose
  recommendation calculation.

- samples:

  (`Samples`)  
  posterior samples from `model` parameters given `data`.

- model:

  (`ModelTox`)  
  the DLT model.

- data:

  (`Data`)  
  data that was used to generate the samples.

- ...:

  additional arguments without method dispatch.

- in_sim:

  (`flag`)  
  is this method used in simulations? Default as `FALSE`. If this flag
  is `TRUE` and target dose estimates (during trial and end-of-trial)
  are outside of the dose grid range, the information message is printed
  by this method.

- model_eff:

  (`Effloglog` or `EffFlexi`)  
  the efficacy model.

- samples_eff:

  (`Samples`)  
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
  [`LogisticIndepBeta`](https://openpharma.github.io/crmPack/reference/LogisticIndepBeta-class.md)
  model class object without DLT samples.

- `nextBest( nextBest = NextBestTDsamples, doselimit = numeric, samples = Samples, model = LogisticIndepBeta, data = Data )`:
  find the next best dose based only on the DLT responses and for
  [`LogisticIndepBeta`](https://openpharma.github.io/crmPack/reference/LogisticIndepBeta-class.md)
  model class object involving DLT samples.

- `nextBest( nextBest = NextBestMaxGain, doselimit = numeric, samples = missing, model = ModelTox, data = DataDual )`:
  find the next best dose based only on pseudo DLT model
  [`ModelTox`](https://openpharma.github.io/crmPack/reference/ModelTox-class.md)
  and
  [`Effloglog`](https://openpharma.github.io/crmPack/reference/Effloglog-class.md)
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
#>  [7,] 12.0      0.7
#>  [8,] 14.0      0.9
#>  [9,] 16.0      0.9
#> [10,] 18.0      0.9
#> [11,] 20.0      0.9
#> [12,] 22.0      0.9
#> [13,] 24.0      0.9
#> [14,] 26.0      1.0
#> [15,] 28.0      1.0
#> [16,] 30.0      1.0
#> [17,] 32.0      1.0
#> [18,] 34.0      1.0
#> [19,] 36.0      1.0
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
#>  [3,]  1.5    0.2      0.0
#>  [4,]  3.0    0.3      0.2
#>  [5,]  6.0    0.8      0.2
#>  [6,] 10.0    0.5      0.5
#>  [7,] 12.0    0.0      1.0
#>  [8,] 14.0    0.0      1.0
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
#> [34,] 66.0    0.0      1.0
#> [35,] 68.0    0.0      1.0
#> [36,] 70.0    0.0      1.0
#> [37,] 72.0    0.0      1.0
#> [38,] 74.0    0.0      1.0
#> [39,] 76.0    0.0      1.0
#> [40,] 78.0    0.0      1.0
#> [41,] 80.0    0.0      1.0
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
#> [1] 16

# Look at the probabilities.
dose_recommendation$probs
#>     dose underdosing target excessive unacceptable        mean     std_dev
#> 0.1  0.1         1.0    0.0       0.0          0.0 0.003085460 0.006295566
#> 0.5  0.5         1.0    0.0       0.0          0.0 0.007917626 0.011747142
#> 1.5  1.5         1.0    0.0       0.0          0.0 0.017421597 0.017321106
#> 3    3.0         1.0    0.0       0.0          0.0 0.031197444 0.021698711
#> 6    6.0         1.0    0.0       0.0          0.0 0.059468263 0.030459010
#> 10  10.0         1.0    0.0       0.0          0.0 0.097725112 0.048675950
#> 12  12.0         1.0    0.0       0.0          0.0 0.116558576 0.059557822
#> 14  14.0         0.9    0.1       0.0          0.0 0.134977608 0.070873085
#> 16  16.0         0.6    0.4       0.0          0.0 0.152879306 0.082259989
#> 18  18.0         0.6    0.4       0.0          0.0 0.170196874 0.093478027
#> 20  20.0         0.6    0.4       0.0          0.0 0.186890999 0.104370053
#> 22  22.0         0.6    0.3       0.1          0.0 0.202942929 0.114836014
#> 24  24.0         0.6    0.3       0.1          0.0 0.218349041 0.124815355
#> 26  26.0         0.6    0.0       0.4          0.0 0.233116624 0.134275099
#> 28  28.0         0.6    0.0       0.4          0.0 0.247260641 0.143201586
#> 30  30.0         0.4    0.2       0.4          0.0 0.260801277 0.151594652
#> 32  32.0         0.4    0.2       0.4          0.0 0.273762082 0.159463464
#> 34  34.0         0.4    0.2       0.4          0.0 0.286168595 0.166823510
#> 36  36.0         0.4    0.2       0.4          0.0 0.298047318 0.173694416
#> 38  38.0         0.4    0.2       0.4          0.0 0.309424981 0.180098359
#> 40  40.0         0.4    0.2       0.4          0.0 0.320328014 0.186058916
#> 42  42.0         0.4    0.2       0.4          0.0 0.330782174 0.191600235
#> 44  44.0         0.4    0.2       0.4          0.0 0.340812300 0.196746445
#> 46  46.0         0.4    0.2       0.4          0.0 0.350442153 0.201521242
#> 48  48.0         0.3    0.3       0.3          0.1 0.359694326 0.205947600
#> 50  50.0         0.3    0.3       0.0          0.4 0.368590199 0.210047590
#> 52  52.0         0.3    0.3       0.0          0.4 0.377149935 0.213842261
#> 54  54.0         0.3    0.3       0.0          0.4 0.385392491 0.217351574
#> 56  56.0         0.3    0.3       0.0          0.4 0.393335658 0.220594381
#> 58  58.0         0.2    0.4       0.0          0.4 0.400996104 0.223588425
#> 60  60.0         0.2    0.4       0.0          0.4 0.408389427 0.226350359
#> 62  62.0         0.0    0.6       0.0          0.4 0.415530212 0.228895783
#> 64  64.0         0.0    0.6       0.0          0.4 0.422432091 0.231239283
#> 66  66.0         0.0    0.6       0.0          0.4 0.429107802 0.233394486
#> 68  68.0         0.0    0.6       0.0          0.4 0.435569244 0.235374103
#> 70  70.0         0.0    0.6       0.0          0.4 0.441827534 0.237189988
#> 72  72.0         0.0    0.6       0.0          0.4 0.447893062 0.238853186
#> 74  74.0         0.0    0.6       0.0          0.4 0.453775541 0.240373987
#> 76  76.0         0.0    0.6       0.0          0.4 0.459484054 0.241761974
#> 78  78.0         0.0    0.6       0.0          0.4 0.465027099 0.243026069
#> 80  80.0         0.0    0.6       0.0          0.4 0.470412631 0.244174583
#>     posterior_loss
#> 0.1            1.0
#> 0.5            1.0
#> 1.5            1.0
#> 3              1.0
#> 6              1.0
#> 10             1.0
#> 12             1.0
#> 14             0.9
#> 16             0.6
#> 18             0.6
#> 20             0.6
#> 22             0.7
#> 24             0.7
#> 26             1.0
#> 28             1.0
#> 30             0.8
#> 32             0.8
#> 34             0.8
#> 36             0.8
#> 38             0.8
#> 40             0.8
#> 42             0.8
#> 44             0.8
#> 46             0.8
#> 48             0.8
#> 50             1.1
#> 52             1.1
#> 54             1.1
#> 56             1.1
#> 58             1.0
#> 60             1.0
#> 62             0.8
#> 64             0.8
#> 66             0.8
#> 68             0.8
#> 70             0.8
#> 72             0.8
#> 74             0.8
#> 76             0.8
#> 78             0.8
#> 80             0.8

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
#> [1] 16

# Look at the probabilities.
dose_recommendation_losses_3$probs
#>     dose underdosing target overdose        mean     std_dev posterior_loss
#> 0.1  0.1         1.0    0.0      0.0 0.003085460 0.006295566            1.0
#> 0.5  0.5         1.0    0.0      0.0 0.007917626 0.011747142            1.0
#> 1.5  1.5         1.0    0.0      0.0 0.017421597 0.017321106            1.0
#> 3    3.0         1.0    0.0      0.0 0.031197444 0.021698711            1.0
#> 6    6.0         1.0    0.0      0.0 0.059468263 0.030459010            1.0
#> 10  10.0         1.0    0.0      0.0 0.097725112 0.048675950            1.0
#> 12  12.0         1.0    0.0      0.0 0.116558576 0.059557822            1.0
#> 14  14.0         0.9    0.1      0.0 0.134977608 0.070873085            0.9
#> 16  16.0         0.6    0.4      0.0 0.152879306 0.082259989            0.6
#> 18  18.0         0.6    0.4      0.0 0.170196874 0.093478027            0.6
#> 20  20.0         0.6    0.4      0.0 0.186890999 0.104370053            0.6
#> 22  22.0         0.6    0.3      0.1 0.202942929 0.114836014            0.8
#> 24  24.0         0.6    0.3      0.1 0.218349041 0.124815355            0.8
#> 26  26.0         0.6    0.0      0.4 0.233116624 0.134275099            1.4
#> 28  28.0         0.6    0.0      0.4 0.247260641 0.143201586            1.4
#> 30  30.0         0.4    0.2      0.4 0.260801277 0.151594652            1.2
#> 32  32.0         0.4    0.2      0.4 0.273762082 0.159463464            1.2
#> 34  34.0         0.4    0.2      0.4 0.286168595 0.166823510            1.2
#> 36  36.0         0.4    0.2      0.4 0.298047318 0.173694416            1.2
#> 38  38.0         0.4    0.2      0.4 0.309424981 0.180098359            1.2
#> 40  40.0         0.4    0.2      0.4 0.320328014 0.186058916            1.2
#> 42  42.0         0.4    0.2      0.4 0.330782174 0.191600235            1.2
#> 44  44.0         0.4    0.2      0.4 0.340812300 0.196746445            1.2
#> 46  46.0         0.4    0.2      0.4 0.350442153 0.201521242            1.2
#> 48  48.0         0.3    0.3      0.4 0.359694326 0.205947600            1.1
#> 50  50.0         0.3    0.3      0.4 0.368590199 0.210047590            1.1
#> 52  52.0         0.3    0.3      0.4 0.377149935 0.213842261            1.1
#> 54  54.0         0.3    0.3      0.4 0.385392491 0.217351574            1.1
#> 56  56.0         0.3    0.3      0.4 0.393335658 0.220594381            1.1
#> 58  58.0         0.2    0.4      0.4 0.400996104 0.223588425            1.0
#> 60  60.0         0.2    0.4      0.4 0.408389427 0.226350359            1.0
#> 62  62.0         0.0    0.6      0.4 0.415530212 0.228895783            0.8
#> 64  64.0         0.0    0.6      0.4 0.422432091 0.231239283            0.8
#> 66  66.0         0.0    0.6      0.4 0.429107802 0.233394486            0.8
#> 68  68.0         0.0    0.6      0.4 0.435569244 0.235374103            0.8
#> 70  70.0         0.0    0.6      0.4 0.441827534 0.237189988            0.8
#> 72  72.0         0.0    0.6      0.4 0.447893062 0.238853186            0.8
#> 74  74.0         0.0    0.6      0.4 0.453775541 0.240373987            0.8
#> 76  76.0         0.0    0.6      0.4 0.459484054 0.241761974            0.8
#> 78  78.0         0.0    0.6      0.4 0.465027099 0.243026069            0.8
#> 80  80.0         0.0    0.6      0.4 0.470412631 0.244174583            0.8
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
#>  [1,]  0.1    0.1        1
#>  [2,]  0.5    0.1        1
#>  [3,]  1.5    0.0        1
#>  [4,]  3.0    0.0        1
#>  [5,]  6.0    0.0        1
#>  [6,] 10.0    0.1        1
#>  [7,] 12.0    0.2        1
#>  [8,] 14.0    0.0        1
#>  [9,] 16.0    0.0        1
#> [10,] 18.0    0.0        1
#> [11,] 20.0    0.0        1
#> [12,] 22.0    0.0        1
#> [13,] 24.0    0.0        1
#> [14,] 26.0    0.0        1
#> [15,] 28.0    0.0        1
#> [16,] 30.0    0.0        1
#> [17,] 32.0    0.0        1
#> [18,] 34.0    0.0        1
#> [19,] 36.0    0.0        1
#> [20,] 38.0    0.0        1
#> [21,] 40.0    0.0        1
#> [22,] 42.0    0.0        1
#> [23,] 44.0    0.0        1
#> [24,] 46.0    0.0        1
#> [25,] 48.0    0.0        1
#> [26,] 50.0    0.0        1
#> [27,] 52.0    0.0        1
#> [28,] 54.0    0.0        1
#> [29,] 56.0    0.0        1
#> [30,] 58.0    0.0        1
#> [31,] 60.0    0.0        1
#> [32,] 62.0    0.1        1
#> [33,] 64.0    0.1        1
#> [34,] 66.0    0.0        1
#> [35,] 68.0    0.1        1
#> [36,] 70.0    0.0        1
#> [37,] 72.0    0.0        1
#> [38,] 74.0    0.0        1
#> [39,] 76.0    0.1        1
#> [40,] 78.0    0.1        1
#> [41,] 80.0    0.0        1

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
#> [1] 50
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
#> [1] "Estimated max gain dose = 300 not within dose grid"

dose_recommendation$next_dose
#> [1] 275
dose_recommendation$plot


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

dose_recommendation$next_dose
#> [1] 250
dose_recommendation$plot
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
#>  [4,]   40  0.003    0.000
#>  [5,]   50  0.019    0.001
#>  [6,]   60  0.228    0.130
#>  [7,]   70  0.156    0.488
#>  [8,]   80  0.116    0.638
#>  [9,]   90  0.100    0.715
#> [10,]  100  0.106    0.763
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
#>  [5,]   50  0.017    0.001
#>  [6,]   60  0.203    0.133
#>  [7,]   70  0.183    0.484
#>  [8,]   80  0.105    0.661
#>  [9,]   90  0.095    0.735
#> [10,]  100  0.073    0.780
#> 
# }
```
