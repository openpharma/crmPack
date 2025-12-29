# Show the Summary of `PseudoSimulations`

**\[stable\]**

Display a summary of pseudo simulation results.

## Usage

``` r
# S4 method for class 'PseudoSimulationsSummary'
show(object)
```

## Arguments

- object:

  (`PseudoSimulationsSummary`)  
  the object we want to print.

## Value

Invisibly returns a data frame of the results with one row and
appropriate column names.

## Examples

``` r
# Obtain the plot for the simulation results if only DLE responses are
# considered in the simulations.

# Specified simulations when no DLE samples are used.
emptydata <- Data(doseGrid = seq(25, 300, 25))

# The design only incorporate DLE responses and DLE samples are involved.
# Specify the model of 'ModelTox' class eg 'LogisticIndepBeta' class model.
my_model <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEweights = c(3, 3),
  DLEdose = c(25, 300),
  data = emptydata
)

# The escalation rule.
td_next_best <- NextBestTD(
  prob_target_drt = 0.35,
  prob_target_eot = 0.3
)

# The cohort size is 3 subjects.
my_size <- CohortSizeConst(size = 3)

# Allow increase of 200%.
my_increments <- IncrementsRelative(intervals = 0, increments = 2)

# Specify the stopping rule with maximum sample size of 36 patients or when the
# next dose is NA.
my_stopping <- StoppingMinPatients(nPatients = 36) | StoppingMissingDose()

# Specify the design. (For details please refer to the 'TDDesign' example.)
my_design <- TDDesign(
  model = my_model,
  nextBest = td_next_best,
  stopping = my_stopping,
  increments = my_increments,
  cohort_size = my_size,
  data = emptydata,
  startingDose = 25
)

# Specify the truth of the DLE responses.
my_truth <- probFunction(my_model, phi1 = -53.66584, phi2 = 10.50499)

# For illustration purpose only 1 simulation is produced.
my_sim <- simulate(
  object = my_design,
  args = NULL,
  truth = my_truth,
  nsim = 1,
  seed = 819,
  parallel = FALSE
)

# Summary of the simulations.
my_sum <- summary(
  my_sim,
  truth = my_truth
)

# Show the summary of the simulated results in a data frame.
show(my_sum)
#> Summary of 1 simulations
#> 
#> Target probability of DLE p(DLE) used at the end of a trial was 30 %
#> The dose level corresponds to the target p(DLE) used at the end of a trial, TDEOT, was 152.6195 
#> TDEOT at dose Grid was 150 
#> Target p(DLE) used during a trial was 35 %
#> The dose level corresponds to the target p(DLE) used during a trial, TDDT, was 155.972 
#> TDDT at dose Grid was 150 
#> Number of patients overall : mean 36 (36, 36) 
#> Number of patients treated above the target p(DLE) used at the end of a trial : mean 9 (9, 9) 
#> Number of patients treated above the target p(DLE) used during a trial : mean 9 (9, 9) 
#> Proportions of observed DLT in the trials : mean 22 % (22 %, 22 %) 
#> Mean toxicity risks for the patients : mean 27 % (27 %, 27 %) 
#> Doses selected as TDEOT : mean 150 (150, 150) 
#> True toxicity at TDEOT : mean 26 % (26 %, 26 %) 
#> Proportion of trials selecting the TDEOT: 100 %
#> Proportion of trials selecting the TDDT: 100 %
#> Dose most often selected as TDEOT: 150 
#> Observed toxicity rate at dose most often selected: 33 %
#> Fitted probabilities of DLE at dose most often selected : mean 29 % (29 %, 29 %) 
#> The summary table of the final TDEOT across all simulations
#>     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
#>    155.1   155.1   155.1   155.1   155.1   155.1  
#> The summary table of the final ratios of the TDEOT across all simulations
#>     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
#>    2.405   2.405   2.405   2.405   2.405   2.405  
#> The summary table of the final TDDT across all simulations
#>     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
#>    177.8   177.8   177.8   177.8   177.8   177.8  
#> The summary table of dose levels, the optimal dose
#>  to recommend for subsequent study across all simulations
#>     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
#>    155.1   155.1   155.1   155.1   155.1   155.1  
#> The summary table of the final ratios of the optimal dose for stopping across
#>                   all simulations
#>     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
#>    2.405   2.405   2.405   2.405   2.405   2.405  
#> 
#> Stop reason triggered:
#>  ≥ 36 patients dosed :  100 %
#>  Stopped because of missing dose :  0 %

# Example where DLE samples are involved.

# The escalation rule.
td_next_best <- NextBestTDsamples(
  prob_target_drt = 0.35,
  prob_target_eot = 0.3,
  derive = function(samples) {
    as.numeric(quantile(samples, probs = 0.3))
  }
)

# The design.
my_design <- TDsamplesDesign(
  model = my_model,
  nextBest = td_next_best,
  stopping = my_stopping,
  increments = my_increments,
  cohort_size = my_size,
  data = emptydata,
  startingDose = 25
)

# For illustration purposes 2 trails are simulated with 50 burn-ins to generate
# 200 samples.
my_options <- McmcOptions(burnin = 50, step = 2, samples = 200)

my_sim <- simulate(
  object = my_design,
  args = NULL,
  truth = my_truth,
  nsim = 2,
  seed = 819,
  mcmcOptions = my_options,
  parallel = FALSE
)

# Produce a summary of the simulations.
my_sum <- summary(
  my_sim,
  truth = my_truth
)

# Show the summary of the simulated results in a data frame.
show(my_sum)
#> Summary of 2 simulations
#> 
#> Target probability of DLE p(DLE) used at the end of a trial was 30 %
#> The dose level corresponds to the target p(DLE) used at the end of a trial, TDEOT, was 152.6195 
#> TDEOT at dose Grid was 150 
#> Target p(DLE) used during a trial was 35 %
#> The dose level corresponds to the target p(DLE) used during a trial, TDDT, was 155.972 
#> TDDT at dose Grid was 150 
#> Number of patients overall : mean 20 (6, 33) 
#> Number of patients treated above the target p(DLE) used at the end of a trial : mean 6 (1, 11) 
#> Number of patients treated above the target p(DLE) used during a trial : mean 6 (1, 11) 
#> Proportions of observed DLT in the trials : mean 11 % (2 %, 20 %) 
#> Mean toxicity risks for the patients : mean 16 % (3 %, 28 %) 
#> Doses selected as TDEOT : mean 50 (10, 90) 
#> True toxicity at TDEOT : mean 0 % (0 %, 0 %) 
#> Proportion of trials selecting the TDEOT: 0 %
#> Proportion of trials selecting the TDDT: 0 %
#> Dose most often selected as TDEOT: 0 
#> Observed toxicity rate at dose most often selected: NaN %
#> Fitted probabilities of DLE at dose most often selected : mean NA % (NA %, NA %) 
#> The summary table of the final TDEOT across all simulations
#>     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
#>    17.95   43.51   69.07   69.07   94.63  120.19  
#> The summary table of the final ratios of the TDEOT across all simulations
#>     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
#>    473.5   781.3  1089.2  1089.2  1397.0  1704.8  
#> The summary table of the final TDDT across all simulations
#>     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
#>    24.92   56.13   87.34   87.34  118.56  149.77  
#> The summary table of dose levels, the optimal dose
#>  to recommend for subsequent study across all simulations
#>     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
#>    17.95   43.51   69.07   69.07   94.63  120.19  
#> The summary table of the final ratios of the optimal dose for stopping across
#>                   all simulations
#>     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
#>    473.5   781.3  1089.2  1089.2  1397.0  1704.8  
#> 
#> Stop reason triggered:
#>  ≥ 36 patients dosed :  50 %
#>  Stopped because of missing dose :  50 %
```
