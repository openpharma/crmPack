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
#>  [1,]  0.1    0.002
#>  [2,]  0.5    0.004
#>  [3,]  1.5    0.010
#>  [4,]  3.0    0.016
#>  [5,]  6.0    0.050
#>  [6,] 10.0    0.080
#>  [7,] 12.0    0.150
#>  [8,] 14.0    0.188
#>  [9,] 16.0    0.204
#> [10,] 18.0    0.226
#> [11,] 20.0    0.252
#> [12,] 22.0    0.300
#> [13,] 24.0    0.318
#> [14,] 26.0    0.356
#> [15,] 28.0    0.368
#> [16,] 30.0    0.386
#> [17,] 32.0    0.442
#> [18,] 34.0    0.456
#> [19,] 36.0    0.502
#> [20,] 38.0    0.562
#> [21,] 40.0    0.594
#> [22,] 42.0    0.630
#> [23,] 44.0    0.636
#> [24,] 46.0    0.646
#> [25,] 48.0    0.664
#> [26,] 50.0    0.672
#> [27,] 52.0    0.676
#> [28,] 54.0    0.678
#> [29,] 56.0    0.698
#> [30,] 58.0    0.718
#> [31,] 60.0    0.734
#> [32,] 62.0    0.744
#> [33,] 64.0    0.760
#> [34,] 66.0    0.772
#> [35,] 68.0    0.784
#> [36,] 70.0    0.788
#> [37,] 72.0    0.792
#> [38,] 74.0    0.794
#> [39,] 76.0    0.796
#> [40,] 78.0    0.798
#> [41,] 80.0    0.800
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
#>  [1,]  0.1  0.000    0.002
#>  [2,]  0.5  0.008    0.002
#>  [3,]  1.5  0.020    0.002
#>  [4,]  3.0  0.068    0.008
#>  [5,]  6.0  0.148    0.012
#>  [6,] 10.0  0.206    0.080
#>  [7,] 12.0  0.230    0.084
#>  [8,] 14.0  0.234    0.128
#>  [9,] 16.0  0.308    0.160
#> [10,] 18.0  0.338    0.198
#> [11,] 20.0  0.350    0.226
#> [12,] 22.0  0.396    0.238
#> [13,] 24.0  0.368    0.286
#> [14,] 26.0  0.388    0.318
#> [15,] 28.0  0.366    0.376
#> [16,] 30.0  0.380    0.390
#> [17,] 32.0  0.338    0.434
#> [18,] 34.0  0.320    0.464
#> [19,] 36.0  0.338    0.476
#> [20,] 38.0  0.314    0.514
#> [21,] 40.0  0.328    0.542
#> [22,] 42.0  0.320    0.570
#> [23,] 44.0  0.306    0.588
#> [24,] 46.0  0.278    0.616
#> [25,] 48.0  0.274    0.622
#> [26,] 50.0  0.224    0.682
#> [27,] 52.0  0.224    0.690
#> [28,] 54.0  0.194    0.732
#> [29,] 56.0  0.196    0.736
#> [30,] 58.0  0.188    0.744
#> [31,] 60.0  0.170    0.762
#> [32,] 62.0  0.182    0.764
#> [33,] 64.0  0.158    0.788
#> [34,] 66.0  0.152    0.796
#> [35,] 68.0  0.128    0.820
#> [36,] 70.0  0.118    0.834
#> [37,] 72.0  0.116    0.836
#> [38,] 74.0  0.118    0.842
#> [39,] 76.0  0.116    0.844
#> [40,] 78.0  0.118    0.850
#> [41,] 80.0  0.114    0.854
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
#> 0.1  0.1       1.000  0.000     0.000        0.000 0.01081839 0.02190350
#> 0.5  0.5       0.998  0.002     0.000        0.000 0.02522947 0.03912338
#> 1.5  1.5       0.962  0.038     0.000        0.000 0.04722097 0.05902893
#> 3    3.0       0.910  0.082     0.008        0.000 0.07183613 0.07598839
#> 6    6.0       0.822  0.152     0.026        0.000 0.11162413 0.09586289
#> 10  10.0       0.700  0.220     0.080        0.000 0.15653065 0.11104905
#> 12  12.0       0.656  0.254     0.090        0.000 0.17697631 0.11621491
#> 14  14.0       0.582  0.306     0.112        0.000 0.19642345 0.12036599
#> 16  16.0       0.506  0.350     0.144        0.000 0.21500689 0.12375910
#> 18  18.0       0.452  0.360     0.188        0.000 0.23281774 0.12657343
#> 20  20.0       0.414  0.376     0.202        0.008 0.24992150 0.12893898
#> 22  22.0       0.384  0.370     0.236        0.010 0.26636791 0.13095291
#> 24  24.0       0.316  0.378     0.296        0.010 0.28219652 0.13268977
#> 26  26.0       0.268  0.406     0.314        0.012 0.29744013 0.13420805
#> 28  28.0       0.240  0.378     0.368        0.014 0.31212684 0.13555454
#> 30  30.0       0.224  0.356     0.394        0.026 0.32628141 0.13676720
#> 32  32.0       0.158  0.390     0.424        0.028 0.33992616 0.13787699
#> 34  34.0       0.138  0.378     0.452        0.032 0.35308154 0.13890904
#> 36  36.0       0.122  0.366     0.450        0.062 0.36576663 0.13988342
#> 38  38.0       0.102  0.334     0.488        0.076 0.37799940 0.14081569
#> 40  40.0       0.078  0.306     0.520        0.096 0.38979698 0.14171737
#> 42  42.0       0.066  0.288     0.540        0.106 0.40117584 0.14259646
#> 44  44.0       0.060  0.266     0.552        0.122 0.41215193 0.14345792
#> 46  46.0       0.060  0.264     0.540        0.136 0.42274075 0.14430426
#> 48  48.0       0.052  0.254     0.548        0.146 0.43295738 0.14513616
#> 50  50.0       0.044  0.224     0.574        0.158 0.44281658 0.14595297
#> 52  52.0       0.044  0.216     0.564        0.176 0.45233272 0.14675320
#> 54  54.0       0.042  0.200     0.572        0.186 0.46151977 0.14753497
#> 56  56.0       0.038  0.196     0.546        0.220 0.47039135 0.14829623
#> 58  58.0       0.028  0.204     0.534        0.234 0.47896065 0.14903505
#> 60  60.0       0.022  0.198     0.510        0.270 0.48724041 0.14974971
#> 62  62.0       0.020  0.176     0.518        0.286 0.49524295 0.15043880
#> 64  64.0       0.018  0.158     0.532        0.292 0.50298010 0.15110123
#> 66  66.0       0.014  0.152     0.530        0.304 0.51046324 0.15173625
#> 68  68.0       0.014  0.150     0.514        0.322 0.51770328 0.15234338
#> 70  70.0       0.014  0.138     0.518        0.330 0.52471064 0.15292244
#> 72  72.0       0.014  0.134     0.502        0.350 0.53149530 0.15347346
#> 74  74.0       0.014  0.114     0.512        0.360 0.53806679 0.15399666
#> 76  76.0       0.014  0.108     0.484        0.394 0.54443419 0.15449244
#> 78  78.0       0.012  0.100     0.490        0.398 0.55060614 0.15496129
#> 80  80.0       0.006  0.102     0.482        0.410 0.55659090 0.15540382
#>     posterior_loss
#> 0.1          1.000
#> 0.5          0.998
#> 1.5          0.962
#> 3            0.918
#> 6            0.848
#> 10           0.780
#> 12           0.746
#> 14           0.694
#> 16           0.650
#> 18           0.640
#> 20           0.632
#> 22           0.640
#> 24           0.632
#> 26           0.606
#> 28           0.636
#> 30           0.670
#> 32           0.638
#> 34           0.654
#> 36           0.696
#> 38           0.742
#> 40           0.790
#> 42           0.818
#> 44           0.856
#> 46           0.872
#> 48           0.892
#> 50           0.934
#> 52           0.960
#> 54           0.986
#> 56           1.024
#> 58           1.030
#> 60           1.072
#> 62           1.110
#> 64           1.134
#> 66           1.152
#> 68           1.172
#> 70           1.192
#> 72           1.216
#> 74           1.246
#> 76           1.286
#> 78           1.298
#> 80           1.308

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
#>     dose underdosing target overdose       mean    std_dev posterior_loss
#> 0.1  0.1       1.000  0.000    0.000 0.01081839 0.02190350          1.000
#> 0.5  0.5       0.998  0.002    0.000 0.02522947 0.03912338          0.998
#> 1.5  1.5       0.962  0.038    0.000 0.04722097 0.05902893          0.962
#> 3    3.0       0.910  0.082    0.008 0.07183613 0.07598839          0.926
#> 6    6.0       0.822  0.152    0.026 0.11162413 0.09586289          0.874
#> 10  10.0       0.700  0.220    0.080 0.15653065 0.11104905          0.860
#> 12  12.0       0.656  0.254    0.090 0.17697631 0.11621491          0.836
#> 14  14.0       0.582  0.306    0.112 0.19642345 0.12036599          0.806
#> 16  16.0       0.506  0.350    0.144 0.21500689 0.12375910          0.794
#> 18  18.0       0.452  0.360    0.188 0.23281774 0.12657343          0.828
#> 20  20.0       0.414  0.376    0.210 0.24992150 0.12893898          0.834
#> 22  22.0       0.384  0.370    0.246 0.26636791 0.13095291          0.876
#> 24  24.0       0.316  0.378    0.306 0.28219652 0.13268977          0.928
#> 26  26.0       0.268  0.406    0.326 0.29744013 0.13420805          0.920
#> 28  28.0       0.240  0.378    0.382 0.31212684 0.13555454          1.004
#> 30  30.0       0.224  0.356    0.420 0.32628141 0.13676720          1.064
#> 32  32.0       0.158  0.390    0.452 0.33992616 0.13787699          1.062
#> 34  34.0       0.138  0.378    0.484 0.35308154 0.13890904          1.106
#> 36  36.0       0.122  0.366    0.512 0.36576663 0.13988342          1.146
#> 38  38.0       0.102  0.334    0.564 0.37799940 0.14081569          1.230
#> 40  40.0       0.078  0.306    0.616 0.38979698 0.14171737          1.310
#> 42  42.0       0.066  0.288    0.646 0.40117584 0.14259646          1.358
#> 44  44.0       0.060  0.266    0.674 0.41215193 0.14345792          1.408
#> 46  46.0       0.060  0.264    0.676 0.42274075 0.14430426          1.412
#> 48  48.0       0.052  0.254    0.694 0.43295738 0.14513616          1.440
#> 50  50.0       0.044  0.224    0.732 0.44281658 0.14595297          1.508
#> 52  52.0       0.044  0.216    0.740 0.45233272 0.14675320          1.524
#> 54  54.0       0.042  0.200    0.758 0.46151977 0.14753497          1.558
#> 56  56.0       0.038  0.196    0.766 0.47039135 0.14829623          1.570
#> 58  58.0       0.028  0.204    0.768 0.47896065 0.14903505          1.564
#> 60  60.0       0.022  0.198    0.780 0.48724041 0.14974971          1.582
#> 62  62.0       0.020  0.176    0.804 0.49524295 0.15043880          1.628
#> 64  64.0       0.018  0.158    0.824 0.50298010 0.15110123          1.666
#> 66  66.0       0.014  0.152    0.834 0.51046324 0.15173625          1.682
#> 68  68.0       0.014  0.150    0.836 0.51770328 0.15234338          1.686
#> 70  70.0       0.014  0.138    0.848 0.52471064 0.15292244          1.710
#> 72  72.0       0.014  0.134    0.852 0.53149530 0.15347346          1.718
#> 74  74.0       0.014  0.114    0.872 0.53806679 0.15399666          1.758
#> 76  76.0       0.014  0.108    0.878 0.54443419 0.15449244          1.770
#> 78  78.0       0.012  0.100    0.888 0.55060614 0.15496129          1.788
#> 80  80.0       0.006  0.102    0.892 0.55659090 0.15540382          1.790
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
#>  [1,]  0.1  0.010    0.000
#>  [2,]  0.5  0.000    0.000
#>  [3,]  1.5  0.048    0.000
#>  [4,]  3.0  0.016    0.000
#>  [5,]  6.0  0.078    0.004
#>  [6,] 10.0  0.142    0.042
#>  [7,] 12.0  0.094    0.096
#>  [8,] 14.0  0.076    0.120
#>  [9,] 16.0  0.036    0.260
#> [10,] 18.0  0.022    0.342
#> [11,] 20.0  0.004    0.460
#> [12,] 22.0  0.036    0.556
#> [13,] 24.0  0.024    0.724
#> [14,] 26.0  0.030    0.844
#> [15,] 28.0  0.030    0.854
#> [16,] 30.0  0.016    0.946
#> [17,] 32.0  0.028    0.948
#> [18,] 34.0  0.014    0.948
#> [19,] 36.0  0.018    0.966
#> [20,] 38.0  0.004    0.980
#> [21,] 40.0  0.000    0.992
#> [22,] 42.0  0.002    0.992
#> [23,] 44.0  0.004    0.992
#> [24,] 46.0  0.002    1.000
#> [25,] 48.0  0.002    1.000
#> [26,] 50.0  0.000    1.000
#> [27,] 52.0  0.000    1.000
#> [28,] 54.0  0.002    1.000
#> [29,] 56.0  0.004    1.000
#> [30,] 58.0  0.006    1.000
#> [31,] 60.0  0.002    1.000
#> [32,] 62.0  0.014    1.000
#> [33,] 64.0  0.034    1.000
#> [34,] 66.0  0.026    1.000
#> [35,] 68.0  0.020    1.000
#> [36,] 70.0  0.022    1.000
#> [37,] 72.0  0.018    1.000
#> [38,] 74.0  0.032    1.000
#> [39,] 76.0  0.026    1.000
#> [40,] 78.0  0.024    1.000
#> [41,] 80.0  0.034    1.000

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
#>  [5,]   50  0.018    0.001
#>  [6,]   60  0.230    0.131
#>  [7,]   70  0.154    0.517
#>  [8,]   80  0.118    0.662
#>  [9,]   90  0.090    0.746
#> [10,]  100  0.087    0.790
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
#>  [3,]   30  0.001    0.000
#>  [4,]   40  0.003    0.000
#>  [5,]   50  0.028    0.001
#>  [6,]   60  0.195    0.144
#>  [7,]   70  0.181    0.465
#>  [8,]   80  0.142    0.635
#>  [9,]   90  0.092    0.739
#> [10,]  100  0.097    0.778
#> 
```
