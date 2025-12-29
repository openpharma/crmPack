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
my_options <- McmcOptions(burnin = 10, step = 2, samples = 50)
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
#>  [1,]  0.1     0.00
#>  [2,]  0.5     0.00
#>  [3,]  1.5     0.00
#>  [4,]  3.0     0.00
#>  [5,]  6.0     0.08
#>  [6,] 10.0     0.12
#>  [7,] 12.0     0.12
#>  [8,] 14.0     0.14
#>  [9,] 16.0     0.14
#> [10,] 18.0     0.16
#> [11,] 20.0     0.16
#> [12,] 22.0     0.20
#> [13,] 24.0     0.28
#> [14,] 26.0     0.28
#> [15,] 28.0     0.28
#> [16,] 30.0     0.28
#> [17,] 32.0     0.28
#> [18,] 34.0     0.28
#> [19,] 36.0     0.40
#> [20,] 38.0     0.40
#> [21,] 40.0     0.42
#> [22,] 42.0     0.46
#> [23,] 44.0     0.46
#> [24,] 46.0     0.46
#> [25,] 48.0     0.46
#> [26,] 50.0     0.46
#> [27,] 52.0     0.46
#> [28,] 54.0     0.46
#> [29,] 56.0     0.46
#> [30,] 58.0     0.46
#> [31,] 60.0     0.46
#> [32,] 62.0     0.46
#> [33,] 64.0     0.48
#> [34,] 66.0     0.50
#> [35,] 68.0     0.50
#> [36,] 70.0     0.54
#> [37,] 72.0     0.54
#> [38,] 74.0     0.54
#> [39,] 76.0     0.54
#> [40,] 78.0     0.64
#> [41,] 80.0     0.64
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
my_options <- McmcOptions(burnin = 10, step = 2, samples = 50)
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
my_options <- McmcOptions(burnin = 10, step = 2, samples = 50)
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
#>  [1,]  0.1   0.00     0.00
#>  [2,]  0.5   0.00     0.00
#>  [3,]  1.5   0.04     0.00
#>  [4,]  3.0   0.04     0.00
#>  [5,]  6.0   0.04     0.04
#>  [6,] 10.0   0.10     0.06
#>  [7,] 12.0   0.24     0.06
#>  [8,] 14.0   0.24     0.06
#>  [9,] 16.0   0.40     0.06
#> [10,] 18.0   0.42     0.06
#> [11,] 20.0   0.46     0.08
#> [12,] 22.0   0.40     0.16
#> [13,] 24.0   0.36     0.20
#> [14,] 26.0   0.40     0.22
#> [15,] 28.0   0.54     0.22
#> [16,] 30.0   0.44     0.38
#> [17,] 32.0   0.54     0.38
#> [18,] 34.0   0.48     0.46
#> [19,] 36.0   0.48     0.46
#> [20,] 38.0   0.44     0.50
#> [21,] 40.0   0.42     0.56
#> [22,] 42.0   0.42     0.56
#> [23,] 44.0   0.42     0.56
#> [24,] 46.0   0.32     0.66
#> [25,] 48.0   0.24     0.74
#> [26,] 50.0   0.24     0.74
#> [27,] 52.0   0.24     0.74
#> [28,] 54.0   0.08     0.90
#> [29,] 56.0   0.04     0.94
#> [30,] 58.0   0.04     0.94
#> [31,] 60.0   0.04     0.94
#> [32,] 62.0   0.04     0.94
#> [33,] 64.0   0.04     0.94
#> [34,] 66.0   0.04     0.94
#> [35,] 68.0   0.04     0.94
#> [36,] 70.0   0.00     0.98
#> [37,] 72.0   0.00     0.98
#> [38,] 74.0   0.00     0.98
#> [39,] 76.0   0.00     0.98
#> [40,] 78.0   0.00     0.98
#> [41,] 80.0   0.00     0.98
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
my_options <- McmcOptions(burnin = 10, step = 2, samples = 50)
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
my_options <- McmcOptions(burnin = 10, step = 2, samples = 50)
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
#> [1] 12

# Look at the probabilities.
dose_recommendation$probs
#>     dose underdosing target excessive unacceptable       mean    std_dev
#> 0.1  0.1        1.00   0.00      0.00         0.00 0.01204861 0.03037815
#> 0.5  0.5        0.94   0.06      0.00         0.00 0.02985726 0.05349151
#> 1.5  1.5        0.94   0.06      0.00         0.00 0.05906023 0.07487166
#> 3    3.0        0.94   0.00      0.06         0.00 0.09301703 0.09106614
#> 6    6.0        0.72   0.22      0.06         0.00 0.14772018 0.11033685
#> 10  10.0        0.54   0.34      0.12         0.00 0.20707717 0.12618617
#> 12  12.0        0.38   0.50      0.12         0.00 0.23312708 0.13166303
#> 14  14.0        0.36   0.36      0.28         0.00 0.25737268 0.13597543
#> 16  16.0        0.22   0.48      0.30         0.00 0.28008852 0.13937675
#> 18  18.0        0.18   0.46      0.36         0.00 0.30146982 0.14206986
#> 20  20.0        0.18   0.46      0.30         0.06 0.32166141 0.14421068
#> 22  22.0        0.12   0.52      0.30         0.06 0.34077550 0.14591284
#> 24  24.0        0.12   0.48      0.34         0.06 0.35890287 0.14725527
#> 26  26.0        0.10   0.38      0.46         0.06 0.37611987 0.14829129
#> 28  28.0        0.10   0.38      0.46         0.06 0.39249271 0.14905750
#> 30  30.0        0.10   0.22      0.56         0.12 0.40808009 0.14958104
#> 32  32.0        0.10   0.22      0.46         0.22 0.42293478 0.14988477
#> 34  34.0        0.10   0.22      0.46         0.22 0.43710457 0.14999052
#> 36  36.0        0.04   0.28      0.40         0.28 0.45063294 0.14992061
#> 38  38.0        0.02   0.30      0.40         0.28 0.46355954 0.14969833
#> 40  40.0        0.00   0.32      0.34         0.34 0.47592056 0.14934760
#> 42  42.0        0.00   0.16      0.50         0.34 0.48774908 0.14889237
#> 44  44.0        0.00   0.16      0.50         0.34 0.49907544 0.14835591
#> 46  46.0        0.00   0.16      0.50         0.34 0.50992746 0.14776008
#> 48  48.0        0.00   0.16      0.50         0.34 0.52033080 0.14712479
#> 50  50.0        0.00   0.10      0.48         0.42 0.53030917 0.14646763
#> 52  52.0        0.00   0.04      0.54         0.42 0.53988456 0.14580366
#> 54  54.0        0.00   0.04      0.54         0.42 0.54907745 0.14514532
#> 56  56.0        0.00   0.04      0.54         0.42 0.55790698 0.14450251
#> 58  58.0        0.00   0.02      0.56         0.42 0.56639111 0.14388275
#> 60  60.0        0.00   0.02      0.56         0.42 0.57454670 0.14329141
#> 62  62.0        0.00   0.02      0.54         0.44 0.58238969 0.14273193
#> 64  64.0        0.00   0.02      0.54         0.44 0.58993509 0.14220617
#> 66  66.0        0.00   0.02      0.54         0.44 0.59719716 0.14171464
#> 68  68.0        0.00   0.02      0.54         0.44 0.60418936 0.14125677
#> 70  70.0        0.00   0.02      0.54         0.44 0.61092450 0.14083119
#> 72  72.0        0.00   0.02      0.54         0.44 0.61741472 0.14043591
#> 74  74.0        0.00   0.02      0.48         0.50 0.62367155 0.14006851
#> 76  76.0        0.00   0.02      0.48         0.50 0.62970594 0.13972634
#> 78  78.0        0.00   0.02      0.48         0.50 0.63552831 0.13940659
#> 80  80.0        0.00   0.02      0.48         0.50 0.64114853 0.13910645
#>     posterior_loss
#> 0.1           1.00
#> 0.5           0.94
#> 1.5           0.94
#> 3             1.00
#> 6             0.78
#> 10            0.66
#> 12            0.50
#> 14            0.64
#> 16            0.52
#> 18            0.54
#> 20            0.60
#> 22            0.54
#> 24            0.58
#> 26            0.68
#> 28            0.68
#> 30            0.90
#> 32            1.00
#> 34            1.00
#> 36            1.00
#> 38            0.98
#> 40            1.02
#> 42            1.18
#> 44            1.18
#> 46            1.18
#> 48            1.18
#> 50            1.32
#> 52            1.38
#> 54            1.38
#> 56            1.38
#> 58            1.40
#> 60            1.40
#> 62            1.42
#> 64            1.42
#> 66            1.42
#> 68            1.42
#> 70            1.42
#> 72            1.42
#> 74            1.48
#> 76            1.48
#> 78            1.48
#> 80            1.48

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
#> [1] 12

# Look at the probabilities.
dose_recommendation_losses_3$probs
#>     dose underdosing target overdose       mean    std_dev posterior_loss
#> 0.1  0.1        1.00   0.00     0.00 0.01204861 0.03037815           1.00
#> 0.5  0.5        0.94   0.06     0.00 0.02985726 0.05349151           0.94
#> 1.5  1.5        0.94   0.06     0.00 0.05906023 0.07487166           0.94
#> 3    3.0        0.94   0.00     0.06 0.09301703 0.09106614           1.06
#> 6    6.0        0.72   0.22     0.06 0.14772018 0.11033685           0.84
#> 10  10.0        0.54   0.34     0.12 0.20707717 0.12618617           0.78
#> 12  12.0        0.38   0.50     0.12 0.23312708 0.13166303           0.62
#> 14  14.0        0.36   0.36     0.28 0.25737268 0.13597543           0.92
#> 16  16.0        0.22   0.48     0.30 0.28008852 0.13937675           0.82
#> 18  18.0        0.18   0.46     0.36 0.30146982 0.14206986           0.90
#> 20  20.0        0.18   0.46     0.36 0.32166141 0.14421068           0.90
#> 22  22.0        0.12   0.52     0.36 0.34077550 0.14591284           0.84
#> 24  24.0        0.12   0.48     0.40 0.35890287 0.14725527           0.92
#> 26  26.0        0.10   0.38     0.52 0.37611987 0.14829129           1.14
#> 28  28.0        0.10   0.38     0.52 0.39249271 0.14905750           1.14
#> 30  30.0        0.10   0.22     0.68 0.40808009 0.14958104           1.46
#> 32  32.0        0.10   0.22     0.68 0.42293478 0.14988477           1.46
#> 34  34.0        0.10   0.22     0.68 0.43710457 0.14999052           1.46
#> 36  36.0        0.04   0.28     0.68 0.45063294 0.14992061           1.40
#> 38  38.0        0.02   0.30     0.68 0.46355954 0.14969833           1.38
#> 40  40.0        0.00   0.32     0.68 0.47592056 0.14934760           1.36
#> 42  42.0        0.00   0.16     0.84 0.48774908 0.14889237           1.68
#> 44  44.0        0.00   0.16     0.84 0.49907544 0.14835591           1.68
#> 46  46.0        0.00   0.16     0.84 0.50992746 0.14776008           1.68
#> 48  48.0        0.00   0.16     0.84 0.52033080 0.14712479           1.68
#> 50  50.0        0.00   0.10     0.90 0.53030917 0.14646763           1.80
#> 52  52.0        0.00   0.04     0.96 0.53988456 0.14580366           1.92
#> 54  54.0        0.00   0.04     0.96 0.54907745 0.14514532           1.92
#> 56  56.0        0.00   0.04     0.96 0.55790698 0.14450251           1.92
#> 58  58.0        0.00   0.02     0.98 0.56639111 0.14388275           1.96
#> 60  60.0        0.00   0.02     0.98 0.57454670 0.14329141           1.96
#> 62  62.0        0.00   0.02     0.98 0.58238969 0.14273193           1.96
#> 64  64.0        0.00   0.02     0.98 0.58993509 0.14220617           1.96
#> 66  66.0        0.00   0.02     0.98 0.59719716 0.14171464           1.96
#> 68  68.0        0.00   0.02     0.98 0.60418936 0.14125677           1.96
#> 70  70.0        0.00   0.02     0.98 0.61092450 0.14083119           1.96
#> 72  72.0        0.00   0.02     0.98 0.61741472 0.14043591           1.96
#> 74  74.0        0.00   0.02     0.98 0.62367155 0.14006851           1.96
#> 76  76.0        0.00   0.02     0.98 0.62970594 0.13972634           1.96
#> 78  78.0        0.00   0.02     0.98 0.63552831 0.13940659           1.96
#> 80  80.0        0.00   0.02     0.98 0.64114853 0.13910645           1.96
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
my_options <- McmcOptions(burnin = 10, step = 2, samples = 50)

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
#>  [1,]  0.1   0.00     0.16
#>  [2,]  0.5   0.00     0.16
#>  [3,]  1.5   0.02     0.30
#>  [4,]  3.0   0.00     0.30
#>  [5,]  6.0   0.14     0.30
#>  [6,] 10.0   0.00     0.46
#>  [7,] 12.0   0.06     0.46
#>  [8,] 14.0   0.00     0.46
#>  [9,] 16.0   0.02     0.46
#> [10,] 18.0   0.06     0.46
#> [11,] 20.0   0.02     0.90
#> [12,] 22.0   0.08     0.90
#> [13,] 24.0   0.10     1.00
#> [14,] 26.0   0.00     1.00
#> [15,] 28.0   0.08     1.00
#> [16,] 30.0   0.10     1.00
#> [17,] 32.0   0.04     1.00
#> [18,] 34.0   0.04     1.00
#> [19,] 36.0   0.06     1.00
#> [20,] 38.0   0.02     1.00
#> [21,] 40.0   0.00     1.00
#> [22,] 42.0   0.00     1.00
#> [23,] 44.0   0.02     1.00
#> [24,] 46.0   0.00     1.00
#> [25,] 48.0   0.00     1.00
#> [26,] 50.0   0.00     1.00
#> [27,] 52.0   0.00     1.00
#> [28,] 54.0   0.00     1.00
#> [29,] 56.0   0.00     1.00
#> [30,] 58.0   0.02     1.00
#> [31,] 60.0   0.02     1.00
#> [32,] 62.0   0.00     1.00
#> [33,] 64.0   0.04     1.00
#> [34,] 66.0   0.00     1.00
#> [35,] 68.0   0.00     1.00
#> [36,] 70.0   0.00     1.00
#> [37,] 72.0   0.02     1.00
#> [38,] 74.0   0.00     1.00
#> [39,] 76.0   0.02     1.00
#> [40,] 78.0   0.02     1.00
#> [41,] 80.0   0.00     1.00

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
my_options <- McmcOptions(burnin = 10, step = 2, samples = 50)
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
#> [1] 50
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
#> [1] 50
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
my_options <- McmcOptions(burnin = 10, step = 2, samples = 50)
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
my_options <- McmcOptions(burnin = 10, step = 2, samples = 50)
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
#>  [4,]   40  0.001    0.000
#>  [5,]   50  0.026    0.001
#>  [6,]   60  0.222    0.156
#>  [7,]   70  0.179    0.520
#>  [8,]   80  0.111    0.692
#>  [9,]   90  0.081    0.772
#> [10,]  100  0.090    0.806
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
#>  [4,]   40  0.001    0.000
#>  [5,]   50  0.020    0.000
#>  [6,]   60  0.225    0.126
#>  [7,]   70  0.168    0.494
#>  [8,]   80  0.130    0.643
#>  [9,]   90  0.110    0.715
#> [10,]  100  0.091    0.779
#> 
# }
```
