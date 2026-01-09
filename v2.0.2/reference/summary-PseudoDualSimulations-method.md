# Summarize `PseudoDualSimulations`

**\[stable\]**

Summary for Pseudo Dual responses simulations, relative to a given
pseudo DLE and efficacy model (except the EffFlexi class model).

## Usage

``` r
# S4 method for class 'PseudoDualSimulations'
summary(
  object,
  trueDLE,
  trueEff,
  targetEndOfTrial = 0.3,
  targetDuringTrial = 0.35,
  ...
)
```

## Arguments

- object:

  (`PseudoDualSimulations`)  
  the object we want to summarize.

- trueDLE:

  (`function`)  
  a function which takes as input a dose (vector) and returns the true
  probability (vector) of DLE.

- trueEff:

  (`function`)  
  a function which takes as input a dose (vector) and returns the mean
  efficacy value(s) (vector).

- targetEndOfTrial:

  (`number`)  
  the target probability of DLE that are used at the end of a trial.
  Default at 0.3.

- targetDuringTrial:

  (`number`)  
  the target probability of DLE that are used during the trial. Default
  at 0.35.

- ...:

  additional arguments can be supplied here for `trueDLE` and `trueEff`.

## Value

An object of class
[`PseudoDualSimulationsSummary`](https://openpharma.github.io/crmPack/reference/PseudoDualSimulationsSummary-class.md).

## Examples

``` r
# Obtain the plot for the simulation results if DLE and efficacy responses
# are considered in the simulations.

# Specified simulations when no samples are used.
emptydata <- DataDual(doseGrid = seq(25, 300, 25))

# The DLE model must be of 'ModelTox' (e.g 'LogisticIndepBeta') class.
dle_model <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEweights = c(3, 3),
  DLEdose = c(25, 300),
  data = emptydata
)

# The efficacy model of 'ModelEff' (e.g 'Effloglog') class.
eff_model <- Effloglog(
  eff = c(1.223, 2.513),
  eff_dose = c(25, 300),
  nu = c(a = 1, b = 0.025),
  data = emptydata
)

# The escalation rule using the 'NextBestMaxGain' class.
my_next_best <- NextBestMaxGain(
  prob_target_drt = 0.35,
  prob_target_eot = 0.3
)

# Allow increase of 200%.
my_increments <- IncrementsRelative(intervals = 0, increments = 2)

# Cohort size of 3.
my_size <- CohortSizeConst(size = 3)

# Stop when 36 subjects are treated or next dose is NA.
my_stopping <- StoppingMinPatients(nPatients = 36) | StoppingMissingDose()

# Specify the design. (For details please refer to the 'DualResponsesDesign' example.)
my_design <- DualResponsesDesign(
  nextBest = my_next_best,
  model = dle_model,
  eff_model = eff_model,
  stopping = my_stopping,
  increments = my_increments,
  cohort_size = my_size,
  data = emptydata,
  startingDose = 25
)

# Specify the true DLE and efficacy curves.
my_truth_dle <- probFunction(dle_model, phi1 = -53.66584, phi2 = 10.50499)
my_truth_eff <- efficacyFunction(eff_model, theta1 = -4.818429, theta2 = 3.653058)

# \donttest{
# Specify the simulations and generate the 2 trials.
my_sim <- simulate(
  object = my_design,
  args = NULL,
  trueDLE = my_truth_dle,
  trueEff = my_truth_eff,
  trueNu = 1 / 0.025,
  nsim = 2,
  seed = 819,
  parallel = FALSE
)

# Produce a summary of the simulations.
summary(
  my_sim,
  trueDLE = my_truth_dle,
  trueEff = my_truth_eff
)
#> Summary of 2 simulations
#> 
#> Target probability of DLE p(DLE) used at the end of a trial was 30 %
#> The dose level corresponds to the target p(DLE) used at the end of a trial, TDEOT, was 152.6195 
#> TDEOT at dose Grid was 150 
#> Target p(DLE) used during a trial was 35 %
#> The dose level corresponds to the target p(DLE) used during a trial, TDDT, was 155.972 
#> TDDT at dose Grid was 150 
#> Number of patients overall : mean 36 (36, 36) 
#> Number of patients treated above the target p(DLE) used at the end of a trial : mean 6 (6, 6) 
#> Number of patients treated above the target p(DLE) used during a trial : mean 6 (6, 6) 
#> Proportions of observed DLT in the trials : mean 22 % (20 %, 24 %) 
#> Mean toxicity risks for the patients : mean 21 % (20 %, 22 %) 
#> Doses selected as TDEOT : mean 125 (125, 125) 
#> True toxicity at TDEOT : mean 5 % (5 %, 5 %) 
#> Proportion of trials selecting the TDEOT: 0 %
#> Proportion of trials selecting the TDDT: 0 %
#> Dose most often selected as TDEOT: 125 
#> Observed toxicity rate at dose most often selected: 7 %
#> Fitted probabilities of DLE at dose most often selected : mean 23 % (21 %, 26 %) 
#> The summary table of the final TDEOT across all simulations
#>     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
#>    137.6   143.9   150.2   150.2   156.5   162.8  
#> The summary table of the final ratios of the TDEOT across all simulations
#>     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
#>    2.206   2.209   2.212   2.212   2.215   2.218  
#> The summary table of the final TDDT across all simulations
#>     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
#>    155.6   162.5   169.3   169.3   176.2   183.1  
#> The summary table of dose levels, the optimal dose
#>  to recommend for subsequent study across all simulations
#>     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
#>    137.6   140.5   143.5   143.5   146.4   149.4  
#> The summary table of the final ratios of the optimal dose for stopping across
#>                   all simulations
#>     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
#>    2.206   2.606   3.006   3.006   3.406   3.806  
#> 
#> Stop reason triggered:
#>  ≥ 36 patients dosed :  100 %
#>  Stopped because of missing dose :  0 %
#> Target Gstar, the dose which gives the maximum gain value was 130.0097 
#> Target Gstar at dose Grid was 125 
#> The summary table of the final Gstar across all simulations
#>     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
#>    146.2   147.0   147.8   147.8   148.6   149.4  
#> The summary table of the final ratios of the Gstar across all simulations
#>     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
#>    3.799   3.801   3.802   3.802   3.804   3.806  
#> The summary table of dose levels, the optimal dose
#>  to recommend for subsequent study across all simulations
#>     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
#>    137.6   140.5   143.5   143.5   146.4   149.4  
#> The summary table of the final ratios of the optimal dose for stopping across
#>         all simulations
#>     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
#>    2.206   2.606   3.006   3.006   3.406   3.806  
#> Fitted expected efficacy level at dose most often selected : mean 1 (1, 1) 
#> Stop reason triggered:
#>  ≥ 36 patients dosed :  100 %
#>  Stopped because of missing dose :  0 %
# }

# Example where DLE and efficacy samples are involved.
# Please refer to design-method 'simulate DualResponsesSamplesDesign' examples for details.

# Specify the next best rule.
my_next_best <- NextBestMaxGainSamples(
  prob_target_drt = 0.35,
  prob_target_eot = 0.3,
  derive = function(samples) {
    as.numeric(quantile(samples, prob = 0.3))
  },
  mg_derive = function(mg_samples) {
    as.numeric(quantile(mg_samples, prob = 0.5))
  }
)

# Specify the design.
my_design <- DualResponsesSamplesDesign(
  nextBest = my_next_best,
  cohort_size = my_size,
  startingDose = 25,
  model = dle_model,
  eff_model = eff_model,
  data = emptydata,
  stopping = my_stopping,
  increments = my_increments
)

# For illustration purpose 50 burn-ins to generate 200 samples are used.
my_options <- McmcOptions(burnin = 50, step = 2, samples = 200)

# fmt: skip
# \donttest{
# For illustration purpose 2 simulation are created.
my_sim <- simulate(
  object = my_design,
  args = NULL,
  trueDLE = my_truth_dle,
  trueEff = my_truth_eff,
  trueNu = 1 / 0.025,
  nsim = 2,
  mcmcOptions = my_options,
  seed = 819,
  parallel = FALSE
)

# Produce a summary of the simulations.
summary(
  my_sim,
  trueDLE = my_truth_dle,
  trueEff = my_truth_eff
)
#> Summary of 2 simulations
#> 
#> Target probability of DLE p(DLE) used at the end of a trial was 30 %
#> The dose level corresponds to the target p(DLE) used at the end of a trial, TDEOT, was 152.6195 
#> TDEOT at dose Grid was 150 
#> Target p(DLE) used during a trial was 35 %
#> The dose level corresponds to the target p(DLE) used during a trial, TDDT, was 155.972 
#> TDDT at dose Grid was 150 
#> Number of patients overall : mean 24 (14, 34) 
#> Number of patients treated above the target p(DLE) used at the end of a trial : mean 3 (1, 5) 
#> Number of patients treated above the target p(DLE) used during a trial : mean 3 (1, 5) 
#> Proportions of observed DLT in the trials : mean 7 % (1 %, 12 %) 
#> Mean toxicity risks for the patients : mean 10 % (2 %, 18 %) 
#> Doses selected as TDEOT : mean 100 (20, 180) 
#> True toxicity at TDEOT : mean 44 % (9 %, 79 %) 
#> Proportion of trials selecting the TDEOT: 0 %
#> Proportion of trials selecting the TDDT: 0 %
#> Dose most often selected as TDEOT: 0 
#> Observed toxicity rate at dose most often selected: NaN %
#> Fitted probabilities of DLE at dose most often selected : mean NA % (NA %, NA %) 
#> The summary table of the final TDEOT across all simulations
#>     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
#>    1.736  55.402 109.068 109.068 162.734 216.400  
#> The summary table of the final ratios of the TDEOT across all simulations
#>     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
#>        1       1       1       1       1       1  
#> The summary table of the final TDDT across all simulations
#>      Min.  1st Qu.   Median     Mean  3rd Qu.     Max.  
#>    0.1401  75.4949 150.8497 150.8497 226.2044 301.5592  
#> The summary table of dose levels, the optimal dose
#>  to recommend for subsequent study across all simulations
#>     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
#>    1.736  55.402 109.068 109.068 162.734 216.400  
#> The summary table of the final ratios of the optimal dose for stopping across
#>                   all simulations
#>     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
#>        1       1       1       1       1       1  
#> 
#> Stop reason triggered:
#>  ≥ 36 patients dosed :  50 %
#>  Stopped because of missing dose :  50 %
#> Target Gstar, the dose which gives the maximum gain value was 130.0097 
#> Target Gstar at dose Grid was 125 
#> The summary table of the final Gstar across all simulations
#>     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
#>      300     300     300     300     300     300  
#> The summary table of the final ratios of the Gstar across all simulations
#>     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
#>     1.00    1.35    1.70    1.70    2.05    2.40  
#> The summary table of dose levels, the optimal dose
#>  to recommend for subsequent study across all simulations
#>     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
#>    1.736  55.402 109.068 109.068 162.734 216.400  
#> The summary table of the final ratios of the optimal dose for stopping across
#>         all simulations
#>     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
#>        1       1       1       1       1       1  
#> Fitted expected efficacy level at dose most often selected : mean NA (NA, NA) 
#> Stop reason triggered:
#>  ≥ 36 patients dosed :  50 %
#>  Stopped because of missing dose :  50 %
# }
```
