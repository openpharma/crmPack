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
my_options <- McmcOptions(burnin = 100, step = 2, samples = 500)
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
#>  [1,]  0.1    0.000
#>  [2,]  0.5    0.004
#>  [3,]  1.5    0.006
#>  [4,]  3.0    0.012
#>  [5,]  6.0    0.038
#>  [6,] 10.0    0.090
#>  [7,] 12.0    0.106
#>  [8,] 14.0    0.158
#>  [9,] 16.0    0.180
#> [10,] 18.0    0.210
#> [11,] 20.0    0.264
#> [12,] 22.0    0.282
#> [13,] 24.0    0.310
#> [14,] 26.0    0.372
#> [15,] 28.0    0.404
#> [16,] 30.0    0.414
#> [17,] 32.0    0.440
#> [18,] 34.0    0.454
#> [19,] 36.0    0.490
#> [20,] 38.0    0.528
#> [21,] 40.0    0.548
#> [22,] 42.0    0.586
#> [23,] 44.0    0.620
#> [24,] 46.0    0.630
#> [25,] 48.0    0.654
#> [26,] 50.0    0.668
#> [27,] 52.0    0.716
#> [28,] 54.0    0.734
#> [29,] 56.0    0.736
#> [30,] 58.0    0.750
#> [31,] 60.0    0.760
#> [32,] 62.0    0.772
#> [33,] 64.0    0.782
#> [34,] 66.0    0.784
#> [35,] 68.0    0.806
#> [36,] 70.0    0.812
#> [37,] 72.0    0.816
#> [38,] 74.0    0.822
#> [39,] 76.0    0.824
#> [40,] 78.0    0.826
#> [41,] 80.0    0.838
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
my_options <- McmcOptions(burnin = 100, step = 2, samples = 500)
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
my_options <- McmcOptions(burnin = 100, step = 2, samples = 500)
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
#>  [1,]  0.1  0.000    0.000
#>  [2,]  0.5  0.002    0.000
#>  [3,]  1.5  0.024    0.000
#>  [4,]  3.0  0.062    0.004
#>  [5,]  6.0  0.134    0.034
#>  [6,] 10.0  0.198    0.072
#>  [7,] 12.0  0.236    0.088
#>  [8,] 14.0  0.274    0.108
#>  [9,] 16.0  0.308    0.150
#> [10,] 18.0  0.314    0.178
#> [11,] 20.0  0.310    0.216
#> [12,] 22.0  0.390    0.252
#> [13,] 24.0  0.360    0.308
#> [14,] 26.0  0.350    0.348
#> [15,] 28.0  0.342    0.368
#> [16,] 30.0  0.360    0.404
#> [17,] 32.0  0.332    0.436
#> [18,] 34.0  0.318    0.474
#> [19,] 36.0  0.290    0.532
#> [20,] 38.0  0.280    0.552
#> [21,] 40.0  0.280    0.568
#> [22,] 42.0  0.282    0.602
#> [23,] 44.0  0.282    0.610
#> [24,] 46.0  0.246    0.656
#> [25,] 48.0  0.236    0.676
#> [26,] 50.0  0.196    0.722
#> [27,] 52.0  0.190    0.736
#> [28,] 54.0  0.192    0.738
#> [29,] 56.0  0.174    0.760
#> [30,] 58.0  0.158    0.778
#> [31,] 60.0  0.150    0.786
#> [32,] 62.0  0.142    0.796
#> [33,] 64.0  0.122    0.818
#> [34,] 66.0  0.112    0.832
#> [35,] 68.0  0.098    0.850
#> [36,] 70.0  0.102    0.856
#> [37,] 72.0  0.098    0.860
#> [38,] 74.0  0.100    0.862
#> [39,] 76.0  0.098    0.864
#> [40,] 78.0  0.098    0.864
#> [41,] 80.0  0.088    0.876
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
my_options <- McmcOptions(burnin = 100, step = 2, samples = 500)
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
my_options <- McmcOptions(burnin = 100, step = 2, samples = 500)
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
#> [1] 20

# Look at the probabilities.
dose_recommendation$probs
#>     dose underdosing target excessive unacceptable       mean    std_dev
#> 0.1  0.1       1.000  0.000     0.000        0.000 0.01113643 0.02737368
#> 0.5  0.5       0.984  0.016     0.000        0.000 0.02399649 0.04327796
#> 1.5  1.5       0.968  0.026     0.006        0.000 0.04345256 0.06074000
#> 3    3.0       0.954  0.036     0.010        0.000 0.06542590 0.07586900
#> 6    6.0       0.854  0.118     0.028        0.000 0.10142416 0.09461586
#> 10  10.0       0.710  0.246     0.042        0.002 0.14262908 0.11035644
#> 12  12.0       0.676  0.266     0.056        0.002 0.16155377 0.11624127
#> 14  14.0       0.604  0.294     0.098        0.004 0.17963650 0.12129370
#> 16  16.0       0.538  0.324     0.134        0.004 0.19698518 0.12571515
#> 18  18.0       0.494  0.352     0.150        0.004 0.21367245 0.12963965
#> 20  20.0       0.454  0.354     0.188        0.004 0.22975083 0.13315880
#> 22  22.0       0.416  0.364     0.214        0.006 0.24526078 0.13633705
#> 24  24.0       0.344  0.398     0.250        0.008 0.26023508 0.13922166
#> 26  26.0       0.330  0.368     0.288        0.014 0.27470144 0.14184950
#> 28  28.0       0.298  0.364     0.324        0.014 0.28868380 0.14425150
#> 30  30.0       0.270  0.368     0.342        0.020 0.30220329 0.14645535
#> 32  32.0       0.252  0.366     0.354        0.028 0.31527864 0.14848666
#> 34  34.0       0.208  0.374     0.390        0.028 0.32792665 0.15036913
#> 36  36.0       0.190  0.326     0.444        0.040 0.34016255 0.15212408
#> 38  38.0       0.182  0.328     0.448        0.042 0.35200030 0.15376976
#> 40  40.0       0.170  0.296     0.478        0.056 0.36345298 0.15532093
#> 42  42.0       0.158  0.258     0.506        0.078 0.37453307 0.15678878
#> 44  44.0       0.154  0.230     0.504        0.112 0.38525267 0.15818124
#> 46  46.0       0.148  0.224     0.494        0.134 0.39562371 0.15950370
#> 48  48.0       0.126  0.228     0.512        0.134 0.40565803 0.16075974
#> 50  50.0       0.126  0.222     0.512        0.140 0.41536736 0.16195186
#> 52  52.0       0.104  0.226     0.508        0.162 0.42476338 0.16308206
#> 54  54.0       0.090  0.228     0.496        0.186 0.43385762 0.16415224
#> 56  56.0       0.084  0.206     0.512        0.198 0.44266142 0.16516439
#> 58  58.0       0.074  0.210     0.494        0.222 0.45118588 0.16612067
#> 60  60.0       0.072  0.212     0.494        0.222 0.45944183 0.16702343
#> 62  62.0       0.060  0.214     0.502        0.224 0.46743975 0.16787517
#> 64  64.0       0.060  0.204     0.502        0.234 0.47518981 0.16867844
#> 66  66.0       0.058  0.196     0.504        0.242 0.48270177 0.16943579
#> 68  68.0       0.048  0.184     0.502        0.266 0.48998505 0.17014977
#> 70  70.0       0.048  0.182     0.492        0.278 0.49704869 0.17082282
#> 72  72.0       0.044  0.166     0.480        0.310 0.50390134 0.17145729
#> 74  74.0       0.044  0.156     0.482        0.318 0.51055131 0.17205539
#> 76  76.0       0.044  0.142     0.476        0.338 0.51700654 0.17261922
#> 78  78.0       0.044  0.130     0.462        0.364 0.52327464 0.17315074
#> 80  80.0       0.044  0.116     0.472        0.368 0.52936287 0.17365179
#>     posterior_loss
#> 0.1          1.000
#> 0.5          0.984
#> 1.5          0.974
#> 3            0.964
#> 6            0.882
#> 10           0.756
#> 12           0.736
#> 14           0.710
#> 16           0.680
#> 18           0.652
#> 20           0.650
#> 22           0.642
#> 24           0.610
#> 26           0.646
#> 28           0.650
#> 30           0.652
#> 32           0.662
#> 34           0.654
#> 36           0.714
#> 38           0.714
#> 40           0.760
#> 42           0.820
#> 44           0.882
#> 46           0.910
#> 48           0.906
#> 50           0.918
#> 52           0.936
#> 54           0.958
#> 56           0.992
#> 58           1.012
#> 60           1.010
#> 62           1.010
#> 64           1.030
#> 66           1.046
#> 68           1.082
#> 70           1.096
#> 72           1.144
#> 74           1.162
#> 76           1.196
#> 78           1.234
#> 80           1.252

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
#> 0.1  0.1       1.000  0.000    0.000 0.01113643 0.02737368          1.000
#> 0.5  0.5       0.984  0.016    0.000 0.02399649 0.04327796          0.984
#> 1.5  1.5       0.968  0.026    0.006 0.04345256 0.06074000          0.980
#> 3    3.0       0.954  0.036    0.010 0.06542590 0.07586900          0.974
#> 6    6.0       0.854  0.118    0.028 0.10142416 0.09461586          0.910
#> 10  10.0       0.710  0.246    0.044 0.14262908 0.11035644          0.798
#> 12  12.0       0.676  0.266    0.058 0.16155377 0.11624127          0.792
#> 14  14.0       0.604  0.294    0.102 0.17963650 0.12129370          0.808
#> 16  16.0       0.538  0.324    0.138 0.19698518 0.12571515          0.814
#> 18  18.0       0.494  0.352    0.154 0.21367245 0.12963965          0.802
#> 20  20.0       0.454  0.354    0.192 0.22975083 0.13315880          0.838
#> 22  22.0       0.416  0.364    0.220 0.24526078 0.13633705          0.856
#> 24  24.0       0.344  0.398    0.258 0.26023508 0.13922166          0.860
#> 26  26.0       0.330  0.368    0.302 0.27470144 0.14184950          0.934
#> 28  28.0       0.298  0.364    0.338 0.28868380 0.14425150          0.974
#> 30  30.0       0.270  0.368    0.362 0.30220329 0.14645535          0.994
#> 32  32.0       0.252  0.366    0.382 0.31527864 0.14848666          1.016
#> 34  34.0       0.208  0.374    0.418 0.32792665 0.15036913          1.044
#> 36  36.0       0.190  0.326    0.484 0.34016255 0.15212408          1.158
#> 38  38.0       0.182  0.328    0.490 0.35200030 0.15376976          1.162
#> 40  40.0       0.170  0.296    0.534 0.36345298 0.15532093          1.238
#> 42  42.0       0.158  0.258    0.584 0.37453307 0.15678878          1.326
#> 44  44.0       0.154  0.230    0.616 0.38525267 0.15818124          1.386
#> 46  46.0       0.148  0.224    0.628 0.39562371 0.15950370          1.404
#> 48  48.0       0.126  0.228    0.646 0.40565803 0.16075974          1.418
#> 50  50.0       0.126  0.222    0.652 0.41536736 0.16195186          1.430
#> 52  52.0       0.104  0.226    0.670 0.42476338 0.16308206          1.444
#> 54  54.0       0.090  0.228    0.682 0.43385762 0.16415224          1.454
#> 56  56.0       0.084  0.206    0.710 0.44266142 0.16516439          1.504
#> 58  58.0       0.074  0.210    0.716 0.45118588 0.16612067          1.506
#> 60  60.0       0.072  0.212    0.716 0.45944183 0.16702343          1.504
#> 62  62.0       0.060  0.214    0.726 0.46743975 0.16787517          1.512
#> 64  64.0       0.060  0.204    0.736 0.47518981 0.16867844          1.532
#> 66  66.0       0.058  0.196    0.746 0.48270177 0.16943579          1.550
#> 68  68.0       0.048  0.184    0.768 0.48998505 0.17014977          1.584
#> 70  70.0       0.048  0.182    0.770 0.49704869 0.17082282          1.588
#> 72  72.0       0.044  0.166    0.790 0.50390134 0.17145729          1.624
#> 74  74.0       0.044  0.156    0.800 0.51055131 0.17205539          1.644
#> 76  76.0       0.044  0.142    0.814 0.51700654 0.17261922          1.672
#> 78  78.0       0.044  0.130    0.826 0.52327464 0.17315074          1.696
#> 80  80.0       0.044  0.116    0.840 0.52936287 0.17365179          1.724
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
my_options <- McmcOptions(burnin = 100, step = 2, samples = 500)
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
#>  [1,]  0.1  0.018    0.020
#>  [2,]  0.5  0.012    0.020
#>  [3,]  1.5  0.038    0.026
#>  [4,]  3.0  0.024    0.030
#>  [5,]  6.0  0.112    0.036
#>  [6,] 10.0  0.100    0.056
#>  [7,] 12.0  0.072    0.132
#>  [8,] 14.0  0.050    0.264
#>  [9,] 16.0  0.054    0.340
#> [10,] 18.0  0.026    0.432
#> [11,] 20.0  0.008    0.522
#> [12,] 22.0  0.032    0.650
#> [13,] 24.0  0.042    0.716
#> [14,] 26.0  0.030    0.794
#> [15,] 28.0  0.038    0.840
#> [16,] 30.0  0.028    0.868
#> [17,] 32.0  0.018    0.904
#> [18,] 34.0  0.012    0.956
#> [19,] 36.0  0.004    0.966
#> [20,] 38.0  0.000    0.968
#> [21,] 40.0  0.000    0.994
#> [22,] 42.0  0.000    0.998
#> [23,] 44.0  0.002    1.000
#> [24,] 46.0  0.000    1.000
#> [25,] 48.0  0.000    1.000
#> [26,] 50.0  0.000    1.000
#> [27,] 52.0  0.000    1.000
#> [28,] 54.0  0.004    1.000
#> [29,] 56.0  0.006    1.000
#> [30,] 58.0  0.008    1.000
#> [31,] 60.0  0.020    1.000
#> [32,] 62.0  0.016    1.000
#> [33,] 64.0  0.012    1.000
#> [34,] 66.0  0.014    1.000
#> [35,] 68.0  0.020    1.000
#> [36,] 70.0  0.020    1.000
#> [37,] 72.0  0.018    1.000
#> [38,] 74.0  0.028    1.000
#> [39,] 76.0  0.038    1.000
#> [40,] 78.0  0.032    1.000
#> [41,] 80.0  0.044    1.000

# Joint plot.
print(dose_recommendation$plot)


# Show customization of single plot.
variant1 <- dose_recommendation$singlePlots$plot1 + xlim(0, 20)
print(variant1)
#> Warning: Removed 31 rows containing missing values or values outside the scale range
#> (`geom_bar()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_vline()`).

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
my_options <- McmcOptions(burnin = 100, step = 2, samples = 500)
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

dose_recommendation$next_dose
#> [1] 75
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
#> [1] 75
dose_recommendation$plot
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
my_options <- McmcOptions(burnin = 100, step = 2, samples = 500)
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
my_options <- McmcOptions(burnin = 100, step = 2, samples = 500)
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
#>  [5,]   50  0.020    0.000
#>  [6,]   60  0.191    0.139
#>  [7,]   70  0.170    0.465
#>  [8,]   80  0.132    0.625
#>  [9,]   90  0.098    0.719
#> [10,]  100  0.084    0.770
#> 
ordinal_data <- .DefaultDataOrdinal()
ordinal_model <- .DefaultLogisticLogNormalOrdinal()
options <- .DefaultMcmcOptions()
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
#>  [5,]   50  0.030    0.001
#>  [6,]   60  0.227    0.141
#>  [7,]   70  0.158    0.509
#>  [8,]   80  0.115    0.655
#>  [9,]   90  0.098    0.736
#> [10,]  100  0.092    0.781
#> 
```
