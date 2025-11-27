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
#>  [1,]  0.1    0.004
#>  [2,]  0.5    0.004
#>  [3,]  1.5    0.008
#>  [4,]  3.0    0.012
#>  [5,]  6.0    0.038
#>  [6,] 10.0    0.102
#>  [7,] 12.0    0.138
#>  [8,] 14.0    0.158
#>  [9,] 16.0    0.228
#> [10,] 18.0    0.282
#> [11,] 20.0    0.298
#> [12,] 22.0    0.348
#> [13,] 24.0    0.382
#> [14,] 26.0    0.406
#> [15,] 28.0    0.424
#> [16,] 30.0    0.442
#> [17,] 32.0    0.458
#> [18,] 34.0    0.508
#> [19,] 36.0    0.530
#> [20,] 38.0    0.560
#> [21,] 40.0    0.600
#> [22,] 42.0    0.618
#> [23,] 44.0    0.642
#> [24,] 46.0    0.672
#> [25,] 48.0    0.698
#> [26,] 50.0    0.722
#> [27,] 52.0    0.756
#> [28,] 54.0    0.758
#> [29,] 56.0    0.770
#> [30,] 58.0    0.788
#> [31,] 60.0    0.794
#> [32,] 62.0    0.814
#> [33,] 64.0    0.826
#> [34,] 66.0    0.834
#> [35,] 68.0    0.834
#> [36,] 70.0    0.838
#> [37,] 72.0    0.856
#> [38,] 74.0    0.868
#> [39,] 76.0    0.868
#> [40,] 78.0    0.874
#> [41,] 80.0    0.884
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
#>  [2,]  0.5  0.000    0.000
#>  [3,]  1.5  0.010    0.000
#>  [4,]  3.0  0.048    0.000
#>  [5,]  6.0  0.122    0.010
#>  [6,] 10.0  0.186    0.050
#>  [7,] 12.0  0.224    0.064
#>  [8,] 14.0  0.286    0.084
#>  [9,] 16.0  0.324    0.114
#> [10,] 18.0  0.324    0.158
#> [11,] 20.0  0.338    0.190
#> [12,] 22.0  0.332    0.230
#> [13,] 24.0  0.354    0.252
#> [14,] 26.0  0.356    0.304
#> [15,] 28.0  0.366    0.330
#> [16,] 30.0  0.382    0.346
#> [17,] 32.0  0.332    0.404
#> [18,] 34.0  0.330    0.426
#> [19,] 36.0  0.318    0.454
#> [20,] 38.0  0.342    0.472
#> [21,] 40.0  0.326    0.498
#> [22,] 42.0  0.316    0.522
#> [23,] 44.0  0.310    0.550
#> [24,] 46.0  0.284    0.588
#> [25,] 48.0  0.282    0.596
#> [26,] 50.0  0.258    0.630
#> [27,] 52.0  0.252    0.654
#> [28,] 54.0  0.246    0.668
#> [29,] 56.0  0.234    0.694
#> [30,] 58.0  0.222    0.716
#> [31,] 60.0  0.224    0.726
#> [32,] 62.0  0.216    0.740
#> [33,] 64.0  0.212    0.748
#> [34,] 66.0  0.198    0.766
#> [35,] 68.0  0.176    0.790
#> [36,] 70.0  0.180    0.794
#> [37,] 72.0  0.172    0.802
#> [38,] 74.0  0.170    0.804
#> [39,] 76.0  0.158    0.818
#> [40,] 78.0  0.138    0.840
#> [41,] 80.0  0.132    0.852
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
#>     dose underdosing target excessive unacceptable        mean    std_dev
#> 0.1  0.1       0.998  0.002     0.000        0.000 0.008768162 0.02310763
#> 0.5  0.5       0.990  0.008     0.002        0.000 0.021072253 0.04084322
#> 1.5  1.5       0.970  0.028     0.002        0.000 0.041074027 0.06204160
#> 3    3.0       0.906  0.082     0.012        0.000 0.064600251 0.08136268
#> 6    6.0       0.834  0.130     0.036        0.000 0.103994009 0.10597657
#> 10  10.0       0.750  0.146     0.104        0.000 0.149525162 0.12646242
#> 12  12.0       0.688  0.176     0.132        0.004 0.170529557 0.13370585
#> 14  14.0       0.616  0.230     0.146        0.008 0.190672673 0.13952971
#> 16  16.0       0.546  0.288     0.158        0.008 0.210087867 0.14419418
#> 18  18.0       0.468  0.326     0.192        0.014 0.228867163 0.14789472
#> 20  20.0       0.416  0.356     0.198        0.030 0.247075389 0.15078858
#> 22  22.0       0.378  0.356     0.230        0.036 0.264758288 0.15300937
#> 24  24.0       0.348  0.370     0.246        0.036 0.281947610 0.15467531
#> 26  26.0       0.306  0.362     0.280        0.052 0.298664634 0.15589354
#> 28  28.0       0.246  0.400     0.288        0.066 0.314922809 0.15676180
#> 30  30.0       0.220  0.384     0.324        0.072 0.330729889 0.15736842
#> 32  32.0       0.204  0.368     0.352        0.076 0.346089725 0.15779154
#> 34  34.0       0.176  0.304     0.442        0.078 0.361003787 0.15809807
#> 36  36.0       0.142  0.294     0.472        0.092 0.375472425 0.15834298
#> 38  38.0       0.132  0.284     0.482        0.102 0.389495878 0.15856915
#> 40  40.0       0.110  0.288     0.472        0.130 0.403075032 0.15880785
#> 42  42.0       0.100  0.252     0.516        0.132 0.416211948 0.15907983
#> 44  44.0       0.084  0.224     0.552        0.140 0.428910175 0.15939671
#> 46  46.0       0.060  0.232     0.562        0.146 0.441174895 0.15976269
#> 48  48.0       0.050  0.234     0.536        0.180 0.453012932 0.16017623
#> 50  50.0       0.044  0.214     0.548        0.194 0.464432651 0.16063168
#> 52  52.0       0.042  0.202     0.536        0.220 0.475443802 0.16112071
#> 54  54.0       0.042  0.192     0.532        0.234 0.486057312 0.16163354
#> 56  56.0       0.040  0.176     0.548        0.236 0.496285060 0.16215986
#> 58  58.0       0.038  0.144     0.552        0.266 0.506139656 0.16268961
#> 60  60.0       0.036  0.130     0.554        0.280 0.515634223 0.16321345
#> 62  62.0       0.036  0.124     0.506        0.334 0.524782196 0.16372310
#> 64  64.0       0.034  0.124     0.456        0.386 0.533597150 0.16421149
#> 66  66.0       0.030  0.122     0.436        0.412 0.542092644 0.16467283
#> 68  68.0       0.022  0.122     0.416        0.440 0.550282095 0.16510261
#> 70  70.0       0.022  0.122     0.396        0.460 0.558178678 0.16549745
#> 72  72.0       0.010  0.124     0.394        0.472 0.565795240 0.16585504
#> 74  74.0       0.006  0.112     0.396        0.486 0.573144240 0.16617396
#> 76  76.0       0.006  0.102     0.370        0.522 0.580237706 0.16645358
#> 78  78.0       0.006  0.100     0.350        0.544 0.587087205 0.16669390
#> 80  80.0       0.000  0.106     0.298        0.596 0.593703823 0.16689544
#>     posterior_loss
#> 0.1          0.998
#> 0.5          0.992
#> 1.5          0.972
#> 3            0.918
#> 6            0.870
#> 10           0.854
#> 12           0.828
#> 14           0.778
#> 16           0.720
#> 18           0.688
#> 20           0.674
#> 22           0.680
#> 24           0.666
#> 26           0.690
#> 28           0.666
#> 30           0.688
#> 32           0.708
#> 34           0.774
#> 36           0.798
#> 38           0.818
#> 40           0.842
#> 42           0.880
#> 44           0.916
#> 46           0.914
#> 48           0.946
#> 50           0.980
#> 52           1.018
#> 54           1.042
#> 56           1.060
#> 58           1.122
#> 60           1.150
#> 62           1.210
#> 64           1.262
#> 66           1.290
#> 68           1.318
#> 70           1.338
#> 72           1.348
#> 74           1.374
#> 76           1.420
#> 78           1.444
#> 80           1.490

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
#> [1] 20

# Look at the probabilities.
dose_recommendation_losses_3$probs
#>     dose underdosing target overdose        mean    std_dev posterior_loss
#> 0.1  0.1       0.998  0.002    0.000 0.008768162 0.02310763          0.998
#> 0.5  0.5       0.990  0.008    0.002 0.021072253 0.04084322          0.994
#> 1.5  1.5       0.970  0.028    0.002 0.041074027 0.06204160          0.974
#> 3    3.0       0.906  0.082    0.012 0.064600251 0.08136268          0.930
#> 6    6.0       0.834  0.130    0.036 0.103994009 0.10597657          0.906
#> 10  10.0       0.750  0.146    0.104 0.149525162 0.12646242          0.958
#> 12  12.0       0.688  0.176    0.136 0.170529557 0.13370585          0.960
#> 14  14.0       0.616  0.230    0.154 0.190672673 0.13952971          0.924
#> 16  16.0       0.546  0.288    0.166 0.210087867 0.14419418          0.878
#> 18  18.0       0.468  0.326    0.206 0.228867163 0.14789472          0.880
#> 20  20.0       0.416  0.356    0.228 0.247075389 0.15078858          0.872
#> 22  22.0       0.378  0.356    0.266 0.264758288 0.15300937          0.910
#> 24  24.0       0.348  0.370    0.282 0.281947610 0.15467531          0.912
#> 26  26.0       0.306  0.362    0.332 0.298664634 0.15589354          0.970
#> 28  28.0       0.246  0.400    0.354 0.314922809 0.15676180          0.954
#> 30  30.0       0.220  0.384    0.396 0.330729889 0.15736842          1.012
#> 32  32.0       0.204  0.368    0.428 0.346089725 0.15779154          1.060
#> 34  34.0       0.176  0.304    0.520 0.361003787 0.15809807          1.216
#> 36  36.0       0.142  0.294    0.564 0.375472425 0.15834298          1.270
#> 38  38.0       0.132  0.284    0.584 0.389495878 0.15856915          1.300
#> 40  40.0       0.110  0.288    0.602 0.403075032 0.15880785          1.314
#> 42  42.0       0.100  0.252    0.648 0.416211948 0.15907983          1.396
#> 44  44.0       0.084  0.224    0.692 0.428910175 0.15939671          1.468
#> 46  46.0       0.060  0.232    0.708 0.441174895 0.15976269          1.476
#> 48  48.0       0.050  0.234    0.716 0.453012932 0.16017623          1.482
#> 50  50.0       0.044  0.214    0.742 0.464432651 0.16063168          1.528
#> 52  52.0       0.042  0.202    0.756 0.475443802 0.16112071          1.554
#> 54  54.0       0.042  0.192    0.766 0.486057312 0.16163354          1.574
#> 56  56.0       0.040  0.176    0.784 0.496285060 0.16215986          1.608
#> 58  58.0       0.038  0.144    0.818 0.506139656 0.16268961          1.674
#> 60  60.0       0.036  0.130    0.834 0.515634223 0.16321345          1.704
#> 62  62.0       0.036  0.124    0.840 0.524782196 0.16372310          1.716
#> 64  64.0       0.034  0.124    0.842 0.533597150 0.16421149          1.718
#> 66  66.0       0.030  0.122    0.848 0.542092644 0.16467283          1.726
#> 68  68.0       0.022  0.122    0.856 0.550282095 0.16510261          1.734
#> 70  70.0       0.022  0.122    0.856 0.558178678 0.16549745          1.734
#> 72  72.0       0.010  0.124    0.866 0.565795240 0.16585504          1.742
#> 74  74.0       0.006  0.112    0.882 0.573144240 0.16617396          1.770
#> 76  76.0       0.006  0.102    0.892 0.580237706 0.16645358          1.790
#> 78  78.0       0.006  0.100    0.894 0.587087205 0.16669390          1.794
#> 80  80.0       0.000  0.106    0.894 0.593703823 0.16689544          1.788
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
#>  [1,]  0.1  0.004    0.024
#>  [2,]  0.5  0.016    0.024
#>  [3,]  1.5  0.050    0.024
#>  [4,]  3.0  0.020    0.024
#>  [5,]  6.0  0.090    0.028
#>  [6,] 10.0  0.110    0.066
#>  [7,] 12.0  0.086    0.104
#>  [8,] 14.0  0.074    0.140
#>  [9,] 16.0  0.036    0.160
#> [10,] 18.0  0.024    0.222
#> [11,] 20.0  0.002    0.342
#> [12,] 22.0  0.034    0.560
#> [13,] 24.0  0.044    0.624
#> [14,] 26.0  0.022    0.656
#> [15,] 28.0  0.032    0.794
#> [16,] 30.0  0.016    0.830
#> [17,] 32.0  0.022    0.876
#> [18,] 34.0  0.018    0.966
#> [19,] 36.0  0.004    0.974
#> [20,] 38.0  0.004    0.986
#> [21,] 40.0  0.000    0.992
#> [22,] 42.0  0.000    0.998
#> [23,] 44.0  0.000    0.998
#> [24,] 46.0  0.004    0.998
#> [25,] 48.0  0.000    1.000
#> [26,] 50.0  0.000    1.000
#> [27,] 52.0  0.000    1.000
#> [28,] 54.0  0.002    1.000
#> [29,] 56.0  0.012    1.000
#> [30,] 58.0  0.002    1.000
#> [31,] 60.0  0.010    1.000
#> [32,] 62.0  0.012    1.000
#> [33,] 64.0  0.016    1.000
#> [34,] 66.0  0.022    1.000
#> [35,] 68.0  0.018    1.000
#> [36,] 70.0  0.014    1.000
#> [37,] 72.0  0.026    1.000
#> [38,] 74.0  0.030    1.000
#> [39,] 76.0  0.030    1.000
#> [40,] 78.0  0.052    1.000
#> [41,] 80.0  0.042    1.000

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
#>  [2,]   20  0.001    0.000
#>  [3,]   30  0.001    0.000
#>  [4,]   40  0.000    0.001
#>  [5,]   50  0.016    0.001
#>  [6,]   60  0.229    0.137
#>  [7,]   70  0.180    0.499
#>  [8,]   80  0.134    0.663
#>  [9,]   90  0.091    0.748
#> [10,]  100  0.063    0.798
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
#>  [5,]   50  0.029    0.001
#>  [6,]   60  0.192    0.152
#>  [7,]   70  0.191    0.475
#>  [8,]   80  0.124    0.660
#>  [9,]   90  0.096    0.743
#> [10,]  100  0.078    0.790
#> 
```
