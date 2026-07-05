# Evaluate a Hypothetical Data Scenario for a Design

**\[experimental\]**

`scenario()` is a convenience wrapper for evaluating a CRM design at a
user-supplied hypothetical data scenario. It runs the model, summarizes
the posterior fit, calculates the next dose recommendation, and
evaluates the stopping rule for the supplied `data`.

## Usage

``` r
scenario(object, data, mcmcOptions, ...)

# S4 method for class 'Design,Data,McmcOptions'
scenario(object, data, mcmcOptions = McmcOptions(), ...)

# S4 method for class 'DesignCombo,DataCombo,McmcOptions'
scenario(object, data, mcmcOptions = McmcOptions(), ...)

# S4 method for class 'DADesign,DataDA,McmcOptions'
scenario(object, data, mcmcOptions = McmcOptions(), ...)

# S4 method for class 'HierarchicalDesign,HierarchicalData,McmcOptions'
scenario(object, data, mcmcOptions = McmcOptions(), ...)
```

## Arguments

- object:

  (`Design`, `DesignCombo`, `DADesign`, or `HierarchicalDesign`)\
  the design to evaluate.

- data:

  (`Data`, `DataCombo`, `DataDA`, or `HierarchicalData`)\
  hypothetical data scenario to evaluate.

- mcmcOptions:

  (`McmcOptions`)\
  MCMC options for the model fit.

- ...:

  additional arguments without method dispatch.

## Value

A named list containing:

- `data`: the evaluated data scenario.

- `samples`: posterior samples from
  [`mcmc()`](https://docs.crmpack.org/reference/mcmc.md).

- `fit`: posterior model fit summary from
  [`fit()`](https://docs.crmpack.org/reference/fit.md).

- `dose_limit`: maximum allowed next dose from the design's increment
  rule.

- `next_best`: full next best dose recommendation from
  [`nextBest()`](https://docs.crmpack.org/reference/nextBest.md).

- `next_dose`: recommended dose value for the next cohort.

- `cohort_size`: active treatment cohort size at `next_dose`.

- `placebo_cohort_size`: placebo cohort size at `next_dose`, if
  applicable.

- `stop`: logical stop decision from
  [`stopTrial()`](https://docs.crmpack.org/reference/stopTrial.md).

- `stop_report`: named logical vector with stopping rule results.

- `stop_reason`: stopping-rule message.

## Functions

- `scenario(object = Design, data = Data, mcmcOptions = McmcOptions)`:
  Evaluate a hypothetical scenario for a CRM design.

- `scenario(object = DesignCombo, data = DataCombo, mcmcOptions = McmcOptions)`:
  Evaluate a hypothetical scenario for a two-drug combination CRM
  design.

- `scenario(object = DADesign, data = DataDA, mcmcOptions = McmcOptions)`:
  Evaluate a hypothetical scenario for a time-to-DLT augmented CRM
  design.

- `scenario( object = HierarchicalDesign, data = HierarchicalData, mcmcOptions = McmcOptions )`:
  Evaluate a hypothetical scenario for a hierarchical CRM design.

## Examples

``` r
# nolint start

# Define the dose-grid and a hypothetical observed data scenario.
data <- Data(
  x = c(1, 3, 3, 5, 5, 5),
  y = c(0, 0, 0, 0, 1, 0),
  cohort = c(1, 2, 2, 3, 3, 3),
  doseGrid = c(1, 3, 5, 10, 15, 20, 25)
)
#> Used default patient IDs!

# Initialize the CRM model.
model <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56
)

# Choose the rule for selecting the next dose.
next_best <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)

# Choose the rule for stopping.
stopping <- StoppingMinPatients(nPatients = 20) | StoppingMissingDose()

# Choose the rule for dose increments.
increments <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)

# Initialize the design.
design <- Design(
  model = model,
  nextBest = next_best,
  stopping = stopping,
  increments = increments,
  cohort_size = CohortSizeConst(3),
  data = Data(doseGrid = data@doseGrid), # empty data here.
  startingDose = 1
)

options <- McmcOptions(
  burnin = 10,
  step = 1,
  samples = 20,
  rng_kind = "Super-Duper",
  rng_seed = 94
)

# \donttest{
result <- scenario(design, data, options)
result$fit
#>   dose     middle        lower     upper
#> 1    1 0.04926272 6.590058e-05 0.1014921
#> 2    3 0.09716721 9.426422e-04 0.1859010
#> 3    5 0.13137236 3.241759e-03 0.2421466
#> 4   10 0.19483316 1.713708e-02 0.3367197
#> 5   15 0.24409058 4.449022e-02 0.3996054
#> 6   20 0.28652432 8.548459e-02 0.4464690
#> 7   25 0.32488893 1.348516e-01 0.4835322
result$next_dose
#> [1] 5
result$cohort_size
#> [1] 3
result$stop
#> [1] FALSE
#> attr(,"message")
#> attr(,"message")[[1]]
#> [1] "Number of patients is 6 and thus below the prespecified minimum number 20"
#> 
#> attr(,"message")[[2]]
#> [1] "Next dose is available at the dose grid."
#> 
#> attr(,"individual")
#> attr(,"individual")[[1]]
#> [1] FALSE
#> attr(,"message")
#> [1] "Number of patients is 6 and thus below the prespecified minimum number 20"
#> attr(,"report_label")
#> [1] "≥ 20 patients dosed"
#> 
#> attr(,"individual")[[2]]
#> [1] FALSE
#> attr(,"message")
#> [1] "Next dose is available at the dose grid."
#> attr(,"report_label")
#> [1] "Stopped because of missing dose"
#> 
#> attr(,"report_label")
#> [1] NA
# }

# nolint end
# nolint start

# Define a hypothetical two-drug scenario.
data <- DataCombo(
  x = cbind(
    drug1 = c(10, 10, 10, 20, 20, 20),
    drug2 = c(20, 20, 20, 20, 20, 20)
  ),
  y = c(0, 0, 1, 0, 0, 0),
  doseGrid = list(drug1 = c(10, 20, 30), drug2 = c(20, 40, 60))
)
#> Used default patient IDs!
#> Used best guess cohort indices!

model <- TwoDrugsCombo(
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

increments <- IncrementsMin(
  increments_list = list(
    IncrementsComboOneDrugOnly(),
    IncrementsComboCartesian(
      drug1 = IncrementsRelative(intervals = c(0), increments = c(1)),
      drug2 = IncrementsRelative(intervals = c(0), increments = c(1))
    )
  )
)

design <- DesignCombo(
  model = model,
  nextBest = NextBestNCRM(
    target = c(0.2, 0.35),
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25
  ),
  stopping = StoppingMinPatients(nPatients = 20),
  increments = increments,
  cohort_size = CohortSizeConst(3),
  data = DataCombo(doseGrid = data@doseGrid),
  startingDose = c(drug1 = 10, drug2 = 20)
)

options <- McmcOptions(
  burnin = 10,
  step = 1,
  samples = 20,
  rng_kind = "Super-Duper",
  rng_seed = 94
)

# \donttest{
result <- scenario(design, data, options)
result$fit
#>   drug1 drug2     middle        lower     upper
#> 1    10    20 0.28942914 8.924512e-02 0.5428955
#> 2    20    20 0.21807179 1.593610e-02 0.7144056
#> 3    30    20 0.19095116 1.822571e-03 0.8190516
#> 4    10    40 0.20394617 1.372769e-02 0.5917367
#> 5    20    40 0.11930164 1.406389e-04 0.6659382
#> 6    30    40 0.09602554 8.663292e-07 0.7027910
#> 7    10    60 0.14785469 1.582785e-03 0.5949093
#> 8    20    60 0.07707668 8.068742e-07 0.5820876
#> 9    30    60 0.06004360 3.183482e-10 0.5541390
result$next_dose
#> drug1 drug2 
#>    10    40 
result$cohort_size
#> [1] 3
result$stop
#> [1] FALSE
#> attr(,"message")
#> [1] "Number of patients is 6 and thus below the prespecified minimum number 20"
#> attr(,"report_label")
#> [1] "≥ 20 patients dosed"
# }

# nolint end
# nolint start

# Define a hypothetical time-to-DLT scenario.
data <- DataDA(
  x = c(0.1, 0.5, 1.5, 3, 6, 10, 10, 10),
  y = c(0, 0, 1, 1, 0, 0, 1, 0),
  u = c(42, 30, 15, 5, 20, 25, 30, 60),
  t0 = c(0, 15, 30, 40, 55, 70, 75, 85),
  Tmax = 60,
  doseGrid = c(0.1, 0.5, 1.5, 3, 6, seq(from = 10, to = 80, by = 2)),
  ID = 1L:8L,
  cohort = as.integer(c(1, 2, 3, 4, 5, 6, 6, 6))
)

npiece <- 10
t_max <- 60
lambda_prior <- function(k) {
  npiece / (t_max * (npiece - k + 0.5))
}

model <- DALogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 56,
  npiece = npiece,
  l = as.numeric(t(apply(as.matrix(c(1:npiece), 1, npiece), 2, lambda_prior))),
  c_par = 2
)

size1 <- CohortSizeRange(
  intervals = c(0, 30),
  cohort_size = c(1, 3)
)
size2 <- CohortSizeDLT(
  intervals = c(0, 1),
  cohort_size = c(1, 3)
)

design <- DADesign(
  model = model,
  increments = IncrementsRelative(
    intervals = c(0, 20),
    increments = c(1, 0.33)
  ),
  nextBest = NextBestNCRM(
    target = c(0.2, 0.35),
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25
  ),
  stopping = StoppingTargetProb(
    target = c(0.2, 0.35),
    prob = 0.5
  ) | StoppingMinPatients(nPatients = 50) | StoppingMissingDose(),
  cohort_size = maxSize(size1, size2),
  data = DataDA(doseGrid = data@doseGrid, Tmax = data@Tmax),
  safetyWindow = SafetyWindowConst(c(6, 2), 7, 7),
  startingDose = 3
)

options <- McmcOptions(
  burnin = 10,
  step = 1,
  samples = 20,
  rng_kind = "Super-Duper",
  rng_seed = 94
)

# \donttest{
result <- scenario(design, data, options)
result$fit
#>    dose     middle      lower     upper
#> 1   0.1 0.09822444 0.02625257 0.1731539
#> 2   0.5 0.19134460 0.07553898 0.3039279
#> 3   1.5 0.28895352 0.14834123 0.4189463
#> 4   3.0 0.36573376 0.21923579 0.4973375
#> 5   6.0 0.45226622 0.31161495 0.5758599
#> 6  10.0 0.51990994 0.39158714 0.6315829
#> 7  12.0 0.54434764 0.42188629 0.6507303
#> 8  14.0 0.56498469 0.44798084 0.6665511
#> 9  16.0 0.58278053 0.47082429 0.6799604
#> 10 18.0 0.59837360 0.49107878 0.6915472
#> 11 20.0 0.61221063 0.50922388 0.7017121
#> 12 22.0 0.62461645 0.52494193 0.7107392
#> 13 24.0 0.63583470 0.53874039 0.7188372
#> 14 26.0 0.64605274 0.55138204 0.7261635
#> 15 28.0 0.65541768 0.56302784 0.7328396
#> 16 30.0 0.66404711 0.57380798 0.7389610
#> 17 32.0 0.67203646 0.58382923 0.7446045
#> 18 34.0 0.67946419 0.59318021 0.7498322
#> 19 36.0 0.68639557 0.60193524 0.7546953
#> 20 38.0 0.69288544 0.61015730 0.7592362
#> 21 40.0 0.69898031 0.61790015 0.7634909
#> 22 42.0 0.70471996 0.62521006 0.7674894
#> 23 44.0 0.71013862 0.63212711 0.7712578
#> 24 46.0 0.71526600 0.63868627 0.7748184
#> 25 48.0 0.72012800 0.64491818 0.7785820
#> 26 50.0 0.72474735 0.65084986 0.7827161
#> 27 52.0 0.72914407 0.65650522 0.7866353
#> 28 54.0 0.73333592 0.66190554 0.7903576
#> 29 56.0 0.73733870 0.66706985 0.7938992
#> 30 58.0 0.74116650 0.67201517 0.7972744
#> 31 60.0 0.74483199 0.67675686 0.8004957
#> 32 62.0 0.74834655 0.68130876 0.8035748
#> 33 64.0 0.75172046 0.68568341 0.8065217
#> 34 66.0 0.75496302 0.68989221 0.8093458
#> 35 68.0 0.75808270 0.69394553 0.8120553
#> 36 70.0 0.76108720 0.69785284 0.8146579
#> 37 72.0 0.76398354 0.70162283 0.8171604
#> 38 74.0 0.76677818 0.70526344 0.8195690
#> 39 76.0 0.76947700 0.70878201 0.8218896
#> 40 78.0 0.77208546 0.71218526 0.8241274
#> 41 80.0 0.77460853 0.71547944 0.8262872
result$next_dose
#> [1] 0.5
result$cohort_size
#> [1] 3
result$stop
#> [1] FALSE
#> attr(,"message")
#> attr(,"message")[[1]]
#> [1] "Probability for target toxicity is 45 % for dose 0.5 and thus below the required 50 %"
#> 
#> attr(,"message")[[2]]
#> [1] "Number of patients is 8 and thus below the prespecified minimum number 50"
#> 
#> attr(,"message")[[3]]
#> [1] "Next dose is available at the dose grid."
#> 
#> attr(,"individual")
#> attr(,"individual")[[1]]
#> [1] FALSE
#> attr(,"message")
#> [1] "Probability for target toxicity is 45 % for dose 0.5 and thus below the required 50 %"
#> attr(,"report_label")
#> [1] "P(0.2 ≤ prob(DLE | NBD) ≤ 0.35) ≥ 0.5"
#> 
#> attr(,"individual")[[2]]
#> [1] FALSE
#> attr(,"message")
#> [1] "Number of patients is 8 and thus below the prespecified minimum number 50"
#> attr(,"report_label")
#> [1] "≥ 50 patients dosed"
#> 
#> attr(,"individual")[[3]]
#> [1] FALSE
#> attr(,"message")
#> [1] "Next dose is available at the dose grid."
#> attr(,"report_label")
#> [1] "Stopped because of missing dose"
#> 
#> attr(,"report_label")
#> [1] NA
# }

# nolint end
# nolint start

dose_grid <- c(1, 3, 5, 10, 15, 20, 25)

# Define hypothetical observed data for two related arms.
data <- HierarchicalData(
  arm_a = Data(
    x = c(1, 3, 3, 5),
    y = c(0, 0, 0, 1),
    cohort = c(1, 2, 2, 3),
    doseGrid = dose_grid
  ),
  arm_b = Data(
    x = c(1, 1, 3, 3),
    y = c(0, 0, 0, 0),
    cohort = c(1, 1, 2, 2),
    doseGrid = dose_grid
  )
)
#> Used default patient IDs!
#> Used default patient IDs!

model_a <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 10
)
model_b <- LogisticLogNormal(
  mean = c(-0.85, 1),
  cov = matrix(c(1, -0.5, -0.5, 1), nrow = 2),
  ref_dose = 10
)

next_best <- NextBestNCRM(
  target = c(0.2, 0.35),
  overdose = c(0.35, 1),
  max_overdose_prob = 0.25
)
stopping <- StoppingMinPatients(nPatients = 20) | StoppingMissingDose()
increments <- IncrementsRelative(
  intervals = c(0, 20),
  increments = c(1, 0.33)
)

design_a <- Design(
  model = model_a,
  nextBest = next_best,
  stopping = stopping,
  increments = increments,
  cohort_size = CohortSizeConst(3),
  data = Data(doseGrid = dose_grid),
  startingDose = 1
)
design_b <- Design(
  model = model_b,
  nextBest = next_best,
  stopping = stopping,
  increments = increments,
  cohort_size = CohortSizeConst(3),
  data = Data(doseGrid = dose_grid),
  startingDose = 1
)

design <- HierarchicalDesign(
  DesignArm(
    name = "arm_a",
    design = design_a
  ),
  DesignArm(
    name = "arm_b",
    design = design_b
  ),
  exchangeable_parameters = list(
    intercept = list(
      arm_a = "alpha0",
      arm_b = "alpha0"
    ),
    slope = list(
      arm_a = "alpha1",
      arm_b = "alpha1"
    )
  )
)

options <- McmcOptions(
  burnin = 10,
  step = 1,
  samples = 20,
  rng_kind = "Super-Duper",
  rng_seed = 94
)

# \donttest{
result <- scenario(design, data, options)
result$fit
#> $arm_a
#>   dose      middle        lower      upper
#> 1    1 0.009015542 0.0002003288 0.05053552
#> 2    3 0.049504308 0.0026090075 0.20553050
#> 3    5 0.107489878 0.0071844675 0.35051715
#> 4   10 0.279224179 0.0290598561 0.59539318
#> 5   15 0.424343925 0.0647742684 0.76432119
#> 6   20 0.524344299 0.1044492235 0.85543179
#> 7   25 0.594015652 0.1479300118 0.91777810
#> 
#> $arm_b
#>   dose      middle        lower      upper
#> 1    1 0.004103667 0.0001336878 0.01970931
#> 2    3 0.031622702 0.0031985377 0.09473569
#> 3    5 0.084165568 0.0112488454 0.20411870
#> 4   10 0.274758148 0.0428855057 0.54179321
#> 5   15 0.451451854 0.0908390873 0.75978901
#> 6   20 0.573851809 0.1499623936 0.86811111
#> 7   25 0.657177101 0.2150887663 0.92303032
#> 
result$next_dose
#> $arm_a
#> [1] 5
#> 
#> $arm_b
#> [1] 5
#> 
result$cohort_size
#> $arm_a
#> [1] 3
#> 
#> $arm_b
#> [1] 3
#> 
result$stop
#> $arm_a
#> [1] FALSE
#> attr(,"message")
#> attr(,"message")[[1]]
#> [1] "Number of patients is 4 and thus below the prespecified minimum number 20"
#> 
#> attr(,"message")[[2]]
#> [1] "Next dose is available at the dose grid."
#> 
#> attr(,"individual")
#> attr(,"individual")[[1]]
#> [1] FALSE
#> attr(,"message")
#> [1] "Number of patients is 4 and thus below the prespecified minimum number 20"
#> attr(,"report_label")
#> [1] "≥ 20 patients dosed"
#> 
#> attr(,"individual")[[2]]
#> [1] FALSE
#> attr(,"message")
#> [1] "Next dose is available at the dose grid."
#> attr(,"report_label")
#> [1] "Stopped because of missing dose"
#> 
#> attr(,"report_label")
#> [1] NA
#> 
#> $arm_b
#> [1] FALSE
#> attr(,"message")
#> attr(,"message")[[1]]
#> [1] "Number of patients is 4 and thus below the prespecified minimum number 20"
#> 
#> attr(,"message")[[2]]
#> [1] "Next dose is available at the dose grid."
#> 
#> attr(,"individual")
#> attr(,"individual")[[1]]
#> [1] FALSE
#> attr(,"message")
#> [1] "Number of patients is 4 and thus below the prespecified minimum number 20"
#> attr(,"report_label")
#> [1] "≥ 20 patients dosed"
#> 
#> attr(,"individual")[[2]]
#> [1] FALSE
#> attr(,"message")
#> [1] "Next dose is available at the dose grid."
#> attr(,"report_label")
#> [1] "Stopped because of missing dose"
#> 
#> attr(,"report_label")
#> [1] NA
#> 
# }

# nolint end
```
