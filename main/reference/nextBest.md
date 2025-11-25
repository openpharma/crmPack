# Finding the Next Best Dose

**\[stable\]**

A function that computes the recommended next best dose based on the
corresponding rule `nextBest`, the posterior `samples` from the `model`
and the underlying `data`.

## Usage

``` r
nextBest(nextBest, doselimit, samples, model, data, ...)

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
#>  [2,]  0.5  0.006    0.000
#>  [3,]  1.5  0.012    0.004
#>  [4,]  3.0  0.038    0.006
#>  [5,]  6.0  0.122    0.022
#>  [6,] 10.0  0.250    0.058
#>  [7,] 12.0  0.230    0.112
#>  [8,] 14.0  0.272    0.136
#>  [9,] 16.0  0.296    0.184
#> [10,] 18.0  0.334    0.212
#> [11,] 20.0  0.332    0.240
#> [12,] 22.0  0.374    0.280
#> [13,] 24.0  0.362    0.316
#> [14,] 26.0  0.360    0.334
#> [15,] 28.0  0.356    0.376
#> [16,] 30.0  0.370    0.422
#> [17,] 32.0  0.358    0.458
#> [18,] 34.0  0.320    0.512
#> [19,] 36.0  0.326    0.526
#> [20,] 38.0  0.316    0.548
#> [21,] 40.0  0.266    0.612
#> [22,] 42.0  0.250    0.636
#> [23,] 44.0  0.252    0.656
#> [24,] 46.0  0.268    0.668
#> [25,] 48.0  0.240    0.702
#> [26,] 50.0  0.228    0.726
#> [27,] 52.0  0.220    0.738
#> [28,] 54.0  0.194    0.764
#> [29,] 56.0  0.178    0.780
#> [30,] 58.0  0.178    0.786
#> [31,] 60.0  0.176    0.788
#> [32,] 62.0  0.172    0.796
#> [33,] 64.0  0.178    0.796
#> [34,] 66.0  0.170    0.804
#> [35,] 68.0  0.154    0.820
#> [36,] 70.0  0.144    0.836
#> [37,] 72.0  0.144    0.838
#> [38,] 74.0  0.144    0.838
#> [39,] 76.0  0.140    0.844
#> [40,] 78.0  0.132    0.852
#> [41,] 80.0  0.124    0.866
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
#> [1] 18

# Look at the probabilities.
dose_recommendation$probs
#>     dose underdosing target excessive unacceptable       mean    std_dev
#> 0.1  0.1       0.992  0.008     0.000        0.000 0.01361883 0.02937670
#> 0.5  0.5       0.990  0.010     0.000        0.000 0.02833469 0.04870685
#> 1.5  1.5       0.944  0.048     0.008        0.000 0.04964781 0.06919452
#> 3    3.0       0.890  0.102     0.008        0.000 0.07337968 0.08585899
#> 6    6.0       0.832  0.118     0.050        0.000 0.11226840 0.10542754
#> 10  10.0       0.698  0.218     0.084        0.000 0.15683616 0.12173355
#> 12  12.0       0.644  0.250     0.106        0.000 0.17725961 0.12792521
#> 14  14.0       0.546  0.318     0.130        0.006 0.19672635 0.13327878
#> 16  16.0       0.498  0.338     0.158        0.006 0.21535257 0.13797540
#> 18  18.0       0.454  0.356     0.172        0.018 0.23321952 0.14214027
#> 20  20.0       0.436  0.342     0.202        0.020 0.25038709 0.14586797
#> 22  22.0       0.386  0.332     0.250        0.032 0.26690105 0.14923428
#> 24  24.0       0.350  0.326     0.292        0.032 0.28279742 0.15230109
#> 26  26.0       0.308  0.332     0.322        0.038 0.29810550 0.15511792
#> 28  28.0       0.288  0.304     0.368        0.040 0.31285005 0.15772262
#> 30  30.0       0.246  0.296     0.418        0.040 0.32705304 0.16014233
#> 32  32.0       0.222  0.306     0.426        0.046 0.34073478 0.16239550
#> 34  34.0       0.194  0.290     0.434        0.082 0.35391479 0.16449449
#> 36  36.0       0.178  0.286     0.440        0.096 0.36661214 0.16644818
#> 38  38.0       0.168  0.282     0.424        0.126 0.37884565 0.16826420
#> 40  40.0       0.158  0.272     0.418        0.152 0.39063384 0.16995035
#> 42  42.0       0.152  0.256     0.438        0.154 0.40199483 0.17151525
#> 44  44.0       0.128  0.250     0.466        0.156 0.41294627 0.17296849
#> 46  46.0       0.124  0.238     0.452        0.186 0.42350516 0.17432032
#> 48  48.0       0.118  0.230     0.454        0.198 0.43368784 0.17558124
#> 50  50.0       0.100  0.238     0.450        0.212 0.44350997 0.17676151
#> 52  52.0       0.096  0.212     0.466        0.226 0.45298649 0.17787075
#> 54  54.0       0.084  0.206     0.484        0.226 0.46213164 0.17891772
#> 56  56.0       0.080  0.202     0.482        0.236 0.47095903 0.17991008
#> 58  58.0       0.074  0.196     0.476        0.254 0.47948168 0.18085438
#> 60  60.0       0.070  0.172     0.490        0.268 0.48771201 0.18175607
#> 62  62.0       0.068  0.152     0.488        0.292 0.49566192 0.18261954
#> 64  64.0       0.066  0.152     0.476        0.306 0.50334283 0.18344828
#> 66  66.0       0.060  0.144     0.468        0.328 0.51076569 0.18424495
#> 68  68.0       0.052  0.152     0.442        0.354 0.51794102 0.18501157
#> 70  70.0       0.052  0.150     0.438        0.360 0.52487890 0.18574961
#> 72  72.0       0.046  0.150     0.414        0.390 0.53158906 0.18646011
#> 74  74.0       0.044  0.150     0.410        0.396 0.53808080 0.18714379
#> 76  76.0       0.044  0.140     0.402        0.414 0.54436308 0.18780114
#> 78  78.0       0.042  0.134     0.368        0.456 0.55044451 0.18843248
#> 80  80.0       0.040  0.130     0.356        0.474 0.55633333 0.18903803
#>     posterior_loss
#> 0.1          0.992
#> 0.5          0.990
#> 1.5          0.952
#> 3            0.898
#> 6            0.882
#> 10           0.782
#> 12           0.750
#> 14           0.688
#> 16           0.668
#> 18           0.662
#> 20           0.678
#> 22           0.700
#> 24           0.706
#> 26           0.706
#> 28           0.736
#> 30           0.744
#> 32           0.740
#> 34           0.792
#> 36           0.810
#> 38           0.844
#> 40           0.880
#> 42           0.898
#> 44           0.906
#> 46           0.948
#> 48           0.968
#> 50           0.974
#> 52           1.014
#> 54           1.020
#> 56           1.034
#> 58           1.058
#> 60           1.096
#> 62           1.140
#> 64           1.154
#> 66           1.184
#> 68           1.202
#> 70           1.210
#> 72           1.240
#> 74           1.246
#> 76           1.274
#> 78           1.322
#> 80           1.344

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
#> [1] 14

# Look at the probabilities.
dose_recommendation_losses_3$probs
#>     dose underdosing target overdose       mean    std_dev posterior_loss
#> 0.1  0.1       0.992  0.008    0.000 0.01361883 0.02937670          0.992
#> 0.5  0.5       0.990  0.010    0.000 0.02833469 0.04870685          0.990
#> 1.5  1.5       0.944  0.048    0.008 0.04964781 0.06919452          0.960
#> 3    3.0       0.890  0.102    0.008 0.07337968 0.08585899          0.906
#> 6    6.0       0.832  0.118    0.050 0.11226840 0.10542754          0.932
#> 10  10.0       0.698  0.218    0.084 0.15683616 0.12173355          0.866
#> 12  12.0       0.644  0.250    0.106 0.17725961 0.12792521          0.856
#> 14  14.0       0.546  0.318    0.136 0.19672635 0.13327878          0.818
#> 16  16.0       0.498  0.338    0.164 0.21535257 0.13797540          0.826
#> 18  18.0       0.454  0.356    0.190 0.23321952 0.14214027          0.834
#> 20  20.0       0.436  0.342    0.222 0.25038709 0.14586797          0.880
#> 22  22.0       0.386  0.332    0.282 0.26690105 0.14923428          0.950
#> 24  24.0       0.350  0.326    0.324 0.28279742 0.15230109          0.998
#> 26  26.0       0.308  0.332    0.360 0.29810550 0.15511792          1.028
#> 28  28.0       0.288  0.304    0.408 0.31285005 0.15772262          1.104
#> 30  30.0       0.246  0.296    0.458 0.32705304 0.16014233          1.162
#> 32  32.0       0.222  0.306    0.472 0.34073478 0.16239550          1.166
#> 34  34.0       0.194  0.290    0.516 0.35391479 0.16449449          1.226
#> 36  36.0       0.178  0.286    0.536 0.36661214 0.16644818          1.250
#> 38  38.0       0.168  0.282    0.550 0.37884565 0.16826420          1.268
#> 40  40.0       0.158  0.272    0.570 0.39063384 0.16995035          1.298
#> 42  42.0       0.152  0.256    0.592 0.40199483 0.17151525          1.336
#> 44  44.0       0.128  0.250    0.622 0.41294627 0.17296849          1.372
#> 46  46.0       0.124  0.238    0.638 0.42350516 0.17432032          1.400
#> 48  48.0       0.118  0.230    0.652 0.43368784 0.17558124          1.422
#> 50  50.0       0.100  0.238    0.662 0.44350997 0.17676151          1.424
#> 52  52.0       0.096  0.212    0.692 0.45298649 0.17787075          1.480
#> 54  54.0       0.084  0.206    0.710 0.46213164 0.17891772          1.504
#> 56  56.0       0.080  0.202    0.718 0.47095903 0.17991008          1.516
#> 58  58.0       0.074  0.196    0.730 0.47948168 0.18085438          1.534
#> 60  60.0       0.070  0.172    0.758 0.48771201 0.18175607          1.586
#> 62  62.0       0.068  0.152    0.780 0.49566192 0.18261954          1.628
#> 64  64.0       0.066  0.152    0.782 0.50334283 0.18344828          1.630
#> 66  66.0       0.060  0.144    0.796 0.51076569 0.18424495          1.652
#> 68  68.0       0.052  0.152    0.796 0.51794102 0.18501157          1.644
#> 70  70.0       0.052  0.150    0.798 0.52487890 0.18574961          1.648
#> 72  72.0       0.046  0.150    0.804 0.53158906 0.18646011          1.654
#> 74  74.0       0.044  0.150    0.806 0.53808080 0.18714379          1.656
#> 76  76.0       0.044  0.140    0.816 0.54436308 0.18780114          1.676
#> 78  78.0       0.042  0.134    0.824 0.55044451 0.18843248          1.690
#> 80  80.0       0.040  0.130    0.830 0.55633333 0.18903803          1.700
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
#>  [1,]  0.1  0.010    0.014
#>  [2,]  0.5  0.002    0.014
#>  [3,]  1.5  0.028    0.014
#>  [4,]  3.0  0.048    0.016
#>  [5,]  6.0  0.066    0.026
#>  [6,] 10.0  0.094    0.104
#>  [7,] 12.0  0.094    0.136
#>  [8,] 14.0  0.080    0.154
#>  [9,] 16.0  0.058    0.224
#> [10,] 18.0  0.030    0.292
#> [11,] 20.0  0.006    0.412
#> [12,] 22.0  0.062    0.516
#> [13,] 24.0  0.028    0.624
#> [14,] 26.0  0.058    0.746
#> [15,] 28.0  0.030    0.838
#> [16,] 30.0  0.008    0.858
#> [17,] 32.0  0.010    0.904
#> [18,] 34.0  0.018    0.920
#> [19,] 36.0  0.000    0.950
#> [20,] 38.0  0.000    0.966
#> [21,] 40.0  0.000    1.000
#> [22,] 42.0  0.006    1.000
#> [23,] 44.0  0.004    1.000
#> [24,] 46.0  0.006    1.000
#> [25,] 48.0  0.000    1.000
#> [26,] 50.0  0.000    1.000
#> [27,] 52.0  0.000    1.000
#> [28,] 54.0  0.000    1.000
#> [29,] 56.0  0.002    1.000
#> [30,] 58.0  0.008    1.000
#> [31,] 60.0  0.008    1.000
#> [32,] 62.0  0.020    1.000
#> [33,] 64.0  0.008    1.000
#> [34,] 66.0  0.010    1.000
#> [35,] 68.0  0.008    1.000
#> [36,] 70.0  0.022    1.000
#> [37,] 72.0  0.036    1.000
#> [38,] 74.0  0.030    1.000
#> [39,] 76.0  0.032    1.000
#> [40,] 78.0  0.034    1.000
#> [41,] 80.0  0.036    1.000

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
#>  [4,]   40  0.005    0.000
#>  [5,]   50  0.029    0.002
#>  [6,]   60  0.238    0.164
#>  [7,]   70  0.162    0.532
#>  [8,]   80  0.104    0.691
#>  [9,]   90  0.091    0.758
#> [10,]  100  0.065    0.808
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
#>  [5,]   50  0.022    0.002
#>  [6,]   60  0.215    0.150
#>  [7,]   70  0.176    0.498
#>  [8,]   80  0.113    0.673
#>  [9,]   90  0.092    0.745
#> [10,]  100  0.073    0.799
#> 
```
