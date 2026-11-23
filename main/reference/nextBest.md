# Finding the Next Best Dose

**\[stable\]**

A function that computes the recommended next best dose based on the
corresponding rule `nextBest`, the posterior `samples` from the `model`
and the underlying `data`.

## Usage

``` r
nextBest(nextBest, doselimit, samples, model, data, ...)

# S4 method for class 'NextBestEWOC,numeric,Samples,GeneralModel,Data'
nextBest(
  nextBest,
  doselimit = Inf,
  samples,
  model,
  data,
  prob_plot_type = c("lollipop", "bar"),
  dose_scale = c("linear", "log", "factor"),
  axis_ticks = c("dosegrid", "regular"),
  axis_text_angle = ifelse(match.arg(axis_ticks) == "dosegrid", 45, 0),
  safe_dose_marker = c("line", "triangle"),
  ...
)

# S4 method for class 'NextBestMTD,numeric,Samples,GeneralModel,Data'
nextBest(nextBest, doselimit = Inf, samples, model, data, ...)

# S4 method for class 'NextBestNCRM,numeric,Samples,GeneralModel,Data'
nextBest(
  nextBest,
  doselimit = Inf,
  samples,
  model,
  data,
  prob_plot_type = c("lollipop", "bar"),
  dose_scale = c("linear", "log", "factor"),
  axis_ticks = c("dosegrid", "regular"),
  axis_text_angle = ifelse(match.arg(axis_ticks) == "dosegrid", 45, 0),
  safe_dose_marker = c("line", "triangle"),
  ...
)

# S4 method for class 'NextBestNCRM,matrix,Samples,TwoDrugsCombo,DataCombo'
nextBest(nextBest, doselimit, samples, model, data, ...)

# S4 method for class 'NextBestNCRM,numeric,Samples,GeneralModel,DataParts'
nextBest(
  nextBest,
  doselimit = Inf,
  samples,
  model,
  data,
  prob_plot_type = c("lollipop", "bar"),
  dose_scale = c("linear", "log", "factor"),
  axis_ticks = c("dosegrid", "regular"),
  axis_text_angle = ifelse(match.arg(axis_ticks) == "dosegrid", 45, 0),
  safe_dose_marker = c("line", "triangle"),
  ...
)

# S4 method for class 'NextBestNCRMLoss,numeric,Samples,GeneralModel,Data'
nextBest(
  nextBest,
  doselimit = Inf,
  samples,
  model,
  data,
  prob_plot_type = c("lollipop", "bar"),
  dose_scale = c("linear", "log", "factor"),
  axis_ticks = c("dosegrid", "regular"),
  axis_text_angle = ifelse(match.arg(axis_ticks) == "dosegrid", 45, 0),
  safe_dose_marker = c("line", "triangle"),
  ...
)

# S4 method for class 'NextBestThreePlusThree,missing,missing,missing,Data'
nextBest(nextBest, doselimit, samples, model, data, ...)

# S4 method for class 'NextBestDualEndpoint,numeric,Samples,DualEndpoint,Data'
nextBest(
  nextBest,
  doselimit = Inf,
  samples,
  model,
  data,
  prob_plot_type = c("lollipop", "bar"),
  dose_scale = c("linear", "log", "factor"),
  axis_ticks = c("dosegrid", "regular"),
  axis_text_angle = ifelse(match.arg(axis_ticks) == "dosegrid", 45, 0),
  safe_dose_marker = c("line", "triangle"),
  ...
)

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

- prob_plot_type:

  (`string`)\
  for one-dimensional target or overdose probability plots, use
  `"lollipop"` (default) or the legacy `"bar"` geometry.

- dose_scale:

  (`string`)\
  for one-dimensional target or overdose probability plots, use a
  `"linear"` (default), `"log"`, or `"factor"` dose axis. The factor
  scale spaces dose levels equally. The log scale requires all doses to
  be strictly positive.

- axis_ticks:

  (`string`)\
  for one-dimensional target or overdose probability plots, place x-axis
  ticks at each dose-grid value (`"dosegrid"`, the default) or at
  regular positions selected by `ggplot2` (`"regular"`).

- axis_text_angle:

  (`number`)\
  rotation angle for x-axis tick labels. Defaults to 45 degrees for
  `axis_ticks = "dosegrid"` and 0 degrees for `axis_ticks = "regular"`.

- safe_dose_marker:

  (`string`)\
  in overdose probability plots, mark the maximum dose passing the
  overdose criterion with a red dashed `"line"` (default) or a red
  `"triangle"` above its probability geometry.

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
#> [11,] 20.0      0.4
#> [12,] 22.0      0.4
#> [13,] 24.0      0.4
#> [14,] 26.0      0.4
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
#>  [3,]  1.5    0.1      0.0
#>  [4,]  3.0    0.6      0.1
#>  [5,]  6.0    0.4      0.4
#>  [6,] 10.0    0.4      0.4
#>  [7,] 12.0    0.4      0.4
#>  [8,] 14.0    0.2      0.6
#>  [9,] 16.0    0.2      0.6
#> [10,] 18.0    0.2      0.6
#> [11,] 20.0    0.2      0.6
#> [12,] 22.0    0.1      0.7
#> [13,] 24.0    0.1      0.7
#> [14,] 26.0    0.1      0.7
#> [15,] 28.0    0.2      0.7
#> [16,] 30.0    0.2      0.7
#> [17,] 32.0    0.2      0.7
#> [18,] 34.0    0.3      0.7
#> [19,] 36.0    0.3      0.7
#> [20,] 38.0    0.3      0.7
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
#> drug1 drug2 
#>    30    20 

# See the target and overdose probabilities.
dose_recommendation$probs
#>   drug1 drug2 target_prob overdose_prob not_eligible
#> 1    10    20         0.5           0.4         TRUE
#> 2    20    20         0.3           0.4         TRUE
#> 3    30    20         0.3           0.2        FALSE
#> 4    10    40         0.3           0.3         TRUE
#> 5    20    40         0.3           0.1        FALSE
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
#> [1] 12

# Look at the probabilities.
dose_recommendation$probs
#>     dose underdosing target excessive unacceptable       mean    std_dev
#> 0.1  0.1         1.0    0.0       0.0            0 0.01238557 0.01019605
#> 0.5  0.5         1.0    0.0       0.0            0 0.03194384 0.02179917
#> 1.5  1.5         1.0    0.0       0.0            0 0.06147294 0.03418908
#> 3    3.0         1.0    0.0       0.0            0 0.09294175 0.04353893
#> 6    6.0         1.0    0.0       0.0            0 0.13992889 0.05327592
#> 10  10.0         0.5    0.5       0.0            0 0.18794132 0.05976134
#> 12  12.0         0.1    0.9       0.0            0 0.20837003 0.06157675
#> 14  14.0         0.1    0.9       0.0            0 0.22711947 0.06272319
#> 16  16.0         0.1    0.9       0.0            0 0.24449951 0.06331727
#> 18  18.0         0.1    0.9       0.0            0 0.26073172 0.06344652
#> 20  20.0         0.1    0.9       0.0            0 0.27598124 0.06318296
#> 22  22.0         0.1    0.9       0.0            0 0.29037512 0.06258945
#> 24  24.0         0.1    0.9       0.0            0 0.30401358 0.06172281
#> 26  26.0         0.1    0.9       0.0            0 0.31697732 0.06063544
#> 28  28.0         0.1    0.5       0.4            0 0.32933247 0.05937619
#> 30  30.0         0.1    0.5       0.4            0 0.34113404 0.05799088
#> 32  32.0         0.1    0.0       0.9            0 0.35242844 0.05652257
#> 34  34.0         0.0    0.1       0.9            0 0.36325526 0.05501176
#> 36  36.0         0.0    0.1       0.9            0 0.37364867 0.05349644
#> 38  38.0         0.0    0.1       0.9            0 0.38363847 0.05201209
#> 40  40.0         0.0    0.1       0.9            0 0.39325090 0.05059154
#> 42  42.0         0.0    0.1       0.9            0 0.40250924 0.04926480
#> 44  44.0         0.0    0.1       0.9            0 0.41143433 0.04805867
#> 46  46.0         0.0    0.1       0.9            0 0.42004495 0.04699640
#> 48  48.0         0.0    0.1       0.9            0 0.42835816 0.04609721
#> 50  50.0         0.0    0.1       0.9            0 0.43638951 0.04537585
#> 52  52.0         0.0    0.0       1.0            0 0.44415329 0.04484225
#> 54  54.0         0.0    0.0       1.0            0 0.45166268 0.04450138
#> 56  56.0         0.0    0.0       1.0            0 0.45892988 0.04435327
#> 58  58.0         0.0    0.0       1.0            0 0.46596625 0.04439336
#> 60  60.0         0.0    0.0       1.0            0 0.47278237 0.04461304
#> 62  62.0         0.0    0.0       1.0            0 0.47938815 0.04500047
#> 64  64.0         0.0    0.0       1.0            0 0.48579289 0.04554139
#> 66  66.0         0.0    0.0       1.0            0 0.49200533 0.04622001
#> 68  68.0         0.0    0.0       1.0            0 0.49803373 0.04701984
#> 70  70.0         0.0    0.0       1.0            0 0.50388584 0.04792438
#> 72  72.0         0.0    0.0       1.0            0 0.50956902 0.04891764
#> 74  74.0         0.0    0.0       1.0            0 0.51509024 0.04998450
#> 76  76.0         0.0    0.0       1.0            0 0.52045609 0.05111105
#> 78  78.0         0.0    0.0       1.0            0 0.52567282 0.05228462
#> 80  80.0         0.0    0.0       1.0            0 0.53074637 0.05349388
#>     posterior_loss
#> 0.1            1.0
#> 0.5            1.0
#> 1.5            1.0
#> 3              1.0
#> 6              1.0
#> 10             0.5
#> 12             0.1
#> 14             0.1
#> 16             0.1
#> 18             0.1
#> 20             0.1
#> 22             0.1
#> 24             0.1
#> 26             0.1
#> 28             0.5
#> 30             0.5
#> 32             1.0
#> 34             0.9
#> 36             0.9
#> 38             0.9
#> 40             0.9
#> 42             0.9
#> 44             0.9
#> 46             0.9
#> 48             0.9
#> 50             0.9
#> 52             1.0
#> 54             1.0
#> 56             1.0
#> 58             1.0
#> 60             1.0
#> 62             1.0
#> 64             1.0
#> 66             1.0
#> 68             1.0
#> 70             1.0
#> 72             1.0
#> 74             1.0
#> 76             1.0
#> 78             1.0
#> 80             1.0

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
#> 0.1  0.1         1.0    0.0      0.0 0.01238557 0.01019605            1.0
#> 0.5  0.5         1.0    0.0      0.0 0.03194384 0.02179917            1.0
#> 1.5  1.5         1.0    0.0      0.0 0.06147294 0.03418908            1.0
#> 3    3.0         1.0    0.0      0.0 0.09294175 0.04353893            1.0
#> 6    6.0         1.0    0.0      0.0 0.13992889 0.05327592            1.0
#> 10  10.0         0.5    0.5      0.0 0.18794132 0.05976134            0.5
#> 12  12.0         0.1    0.9      0.0 0.20837003 0.06157675            0.1
#> 14  14.0         0.1    0.9      0.0 0.22711947 0.06272319            0.1
#> 16  16.0         0.1    0.9      0.0 0.24449951 0.06331727            0.1
#> 18  18.0         0.1    0.9      0.0 0.26073172 0.06344652            0.1
#> 20  20.0         0.1    0.9      0.0 0.27598124 0.06318296            0.1
#> 22  22.0         0.1    0.9      0.0 0.29037512 0.06258945            0.1
#> 24  24.0         0.1    0.9      0.0 0.30401358 0.06172281            0.1
#> 26  26.0         0.1    0.9      0.0 0.31697732 0.06063544            0.1
#> 28  28.0         0.1    0.5      0.4 0.32933247 0.05937619            0.9
#> 30  30.0         0.1    0.5      0.4 0.34113404 0.05799088            0.9
#> 32  32.0         0.1    0.0      0.9 0.35242844 0.05652257            1.9
#> 34  34.0         0.0    0.1      0.9 0.36325526 0.05501176            1.8
#> 36  36.0         0.0    0.1      0.9 0.37364867 0.05349644            1.8
#> 38  38.0         0.0    0.1      0.9 0.38363847 0.05201209            1.8
#> 40  40.0         0.0    0.1      0.9 0.39325090 0.05059154            1.8
#> 42  42.0         0.0    0.1      0.9 0.40250924 0.04926480            1.8
#> 44  44.0         0.0    0.1      0.9 0.41143433 0.04805867            1.8
#> 46  46.0         0.0    0.1      0.9 0.42004495 0.04699640            1.8
#> 48  48.0         0.0    0.1      0.9 0.42835816 0.04609721            1.8
#> 50  50.0         0.0    0.1      0.9 0.43638951 0.04537585            1.8
#> 52  52.0         0.0    0.0      1.0 0.44415329 0.04484225            2.0
#> 54  54.0         0.0    0.0      1.0 0.45166268 0.04450138            2.0
#> 56  56.0         0.0    0.0      1.0 0.45892988 0.04435327            2.0
#> 58  58.0         0.0    0.0      1.0 0.46596625 0.04439336            2.0
#> 60  60.0         0.0    0.0      1.0 0.47278237 0.04461304            2.0
#> 62  62.0         0.0    0.0      1.0 0.47938815 0.04500047            2.0
#> 64  64.0         0.0    0.0      1.0 0.48579289 0.04554139            2.0
#> 66  66.0         0.0    0.0      1.0 0.49200533 0.04622001            2.0
#> 68  68.0         0.0    0.0      1.0 0.49803373 0.04701984            2.0
#> 70  70.0         0.0    0.0      1.0 0.50388584 0.04792438            2.0
#> 72  72.0         0.0    0.0      1.0 0.50956902 0.04891764            2.0
#> 74  74.0         0.0    0.0      1.0 0.51509024 0.04998450            2.0
#> 76  76.0         0.0    0.0      1.0 0.52045609 0.05111105            2.0
#> 78  78.0         0.0    0.0      1.0 0.52567282 0.05228462            2.0
#> 80  80.0         0.0    0.0      1.0 0.53074637 0.05349388            2.0
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
#>  [1,]  0.1    0.0      0.1
#>  [2,]  0.5    0.1      0.1
#>  [3,]  1.5    0.1      0.1
#>  [4,]  3.0    0.1      0.2
#>  [5,]  6.0    0.1      0.2
#>  [6,] 10.0    0.0      0.9
#>  [7,] 12.0    0.0      0.9
#>  [8,] 14.0    0.0      0.9
#>  [9,] 16.0    0.0      0.9
#> [10,] 18.0    0.0      0.9
#> [11,] 20.0    0.0      1.0
#> [12,] 22.0    0.0      1.0
#> [13,] 24.0    0.1      1.0
#> [14,] 26.0    0.1      1.0
#> [15,] 28.0    0.1      1.0
#> [16,] 30.0    0.0      1.0
#> [17,] 32.0    0.1      1.0
#> [18,] 34.0    0.0      1.0
#> [19,] 36.0    0.1      1.0
#> [20,] 38.0    0.0      1.0
#> [21,] 40.0    0.0      1.0
#> [22,] 42.0    0.0      1.0
#> [23,] 44.0    0.0      1.0
#> [24,] 46.0    0.1      1.0
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

# Joint plot.
print(dose_recommendation$plot)


# Show customization of single plot.
variant1 <- dose_recommendation$singlePlots$plot1 + xlim(0, 20)
#> Scale for x is already present.
#> Adding another scale for x, which will replace the existing scale.
print(variant1)
#> Warning: Removed 30 rows containing missing values or values outside the scale range
#> (`geom_segment()`).
#> Warning: Removed 30 rows containing missing values or values outside the scale range
#> (`geom_point()`).

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
#>  [3,]   30  0.001    0.000
#>  [4,]   40  0.003    0.000
#>  [5,]   50  0.021    0.003
#>  [6,]   60  0.223    0.136
#>  [7,]   70  0.201    0.466
#>  [8,]   80  0.132    0.655
#>  [9,]   90  0.116    0.745
#> [10,]  100  0.061    0.823
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
#>  [5,]   50  0.016    0.001
#>  [6,]   60  0.203    0.142
#>  [7,]   70  0.186    0.495
#>  [8,]   80  0.108    0.674
#>  [9,]   90  0.083    0.755
#> [10,]  100  0.076    0.796
#> 
# }
```
