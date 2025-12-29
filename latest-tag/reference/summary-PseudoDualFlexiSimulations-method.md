# Summarize `PseudoDualFlexiSimulations`

**\[stable\]**

Summary for `PseudoDualFlexiSimulations` given a pseudo DLE model and
the flexible efficacy model.

## Usage

``` r
# S4 method for class 'PseudoDualFlexiSimulations'
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

  (`PseudoDualFlexiSimulations`)  
  the object we want to summarize.

- trueDLE:

  (`function`)  
  a function which takes as input a dose (vector) and returns the true
  probability of DLE (vector).

- trueEff:

  (`numeric`)  
  a vector which takes as input the true mean efficacy values at all
  dose levels (in order).

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
# nolint start

## If DLE and efficacy responses are considered in the simulations and the 'EffFlexi' class is used
## we need a data object with doses >= 1:
data <- DataDual(doseGrid = seq(25, 300, 25))
## First for the DLE model
## The DLE model must be of 'ModelTox' (e.g 'LogisticIndepBeta') class
DLEmodel <- LogisticIndepBeta(
  binDLE = c(1.05, 1.8),
  DLEweights = c(3, 3),
  DLEdose = c(25, 300),
  data = data
)

## for the efficacy model
Effmodel <- EffFlexi(
  eff = c(1.223, 2.513),
  eff_dose = c(25, 300),
  sigma2W = c(a = 0.1, b = 0.1),
  sigma2betaW = c(a = 20, b = 50),
  rw1 = FALSE,
  data = data
)


## specified the next best
mynextbest <- NextBestMaxGainSamples(
  prob_target_drt = 0.35,
  prob_target_eot = 0.3,
  derive = function(samples) {
    as.numeric(quantile(samples, prob = 0.3))
  },
  mg_derive = function(mg_samples) {
    as.numeric(quantile(mg_samples, prob = 0.5))
  }
)

## The increments (see Increments class examples)
## 200% allowable increase for dose below 300 and 200% increase for dose above 300
myIncrements <- IncrementsRelative(
  intervals = c(25, 300),
  increments = c(2, 2)
)
## cohort size of 3
mySize <- CohortSizeConst(size = 3)
## Stop only when 10 subjects are treated:
## very low sample size is just for illustration here
myStopping <- StoppingMinPatients(nPatients = 10)

## Specified the design
design <- DualResponsesSamplesDesign(
  nextBest = mynextbest,
  cohort_size = mySize,
  startingDose = 25,
  model = DLEmodel,
  eff_model = Effmodel,
  data = data,
  stopping = myStopping,
  increments = myIncrements
)
## specified the true DLE curve and the true expected efficacy values at all dose levels
myTruthDLE <- probFunction(DLEmodel, phi1 = -53.66584, phi2 = 10.50499)

myTruthEff <- c(
  -0.5478867,
  0.1645417,
  0.5248031,
  0.7604467,
  0.9333009,
  1.0687031,
  1.1793942,
  1.2726408,
  1.3529598,
  1.4233411,
  1.4858613,
  1.5420182
)


## specify the options for MCMC
# For illustration purpose, we use 10 burn-in and generate 100 samples
options <- McmcOptions(burnin = 10, step = 1, samples = 100)
## The simulation
## For illustration purpose only 1 simulation is produced (nsim=1).
mySim <- simulate(
  object = design,
  args = NULL,
  trueDLE = myTruthDLE,
  trueEff = myTruthEff,
  trueSigma2 = 0.025,
  trueSigma2betaW = 1,
  nsim = 1,
  seed = 819,
  parallel = FALSE,
  mcmcOptions = options
)
## summarize the simulation results
summary(mySim, trueDLE = myTruthDLE, trueEff = myTruthEff)
#> Summary of 1 simulations
#> 
#> Target probability of DLE p(DLE) used at the end of a trial was 30 %
#> The dose level corresponds to the target p(DLE) used at the end of a trial, TDEOT, was 152.6195 
#> TDEOT at dose Grid was 150 
#> Target p(DLE) used during a trial was 35 %
#> The dose level corresponds to the target p(DLE) used during a trial, TDDT, was 155.972 
#> TDDT at dose Grid was 150 
#> Number of patients overall : mean 12 (12, 12) 
#> Number of patients treated above the target p(DLE) used at the end of a trial : mean 0 (0, 0) 
#> Number of patients treated above the target p(DLE) used during a trial : mean 0 (0, 0) 
#> Proportions of observed DLT in the trials : mean 0 % (0 %, 0 %) 
#> Mean toxicity risks for the patients : mean 0 % (0 %, 0 %) 
#> Doses selected as TDEOT : mean 0 (0, 0) 
#> True toxicity at TDEOT : mean 0 % (0 %, 0 %) 
#> Proportion of trials selecting the TDEOT: 0 %
#> Proportion of trials selecting the TDDT: 0 %
#> Dose most often selected as TDEOT: 0 
#> Observed toxicity rate at dose most often selected: NaN %
#> Fitted probabilities of DLE at dose most often selected : mean NA % (NA %, NA %) 
#> The summary table of the final TDEOT across all simulations
#>     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
#>    10.12   10.12   10.12   10.12   10.12   10.12  
#> The summary table of the final ratios of the TDEOT across all simulations
#>     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
#>        1       1       1       1       1       1  
#> The summary table of the final TDDT across all simulations
#>     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
#>    6.222   6.222   6.222   6.222   6.222   6.222  
#> The summary table of dose levels, the optimal dose
#>  to recommend for subsequent study across all simulations
#>     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
#>    10.12   10.12   10.12   10.12   10.12   10.12  
#> The summary table of the final ratios of the optimal dose for stopping across
#>                   all simulations
#>     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
#>        1       1       1       1       1       1  
#> 
#> Stop reason triggered:
#>  ≥ 10 patients dosed :  100 %
#> Target Gstar, the dose which gives the maximum gain value was 125 
#> Target Gstar at dose Grid was 125 
#> The summary table of the final Gstar across all simulations
#>     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
#>      250     250     250     250     250     250  
#> The summary table of the final ratios of the Gstar across all simulations
#>     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
#>    2.192   2.192   2.192   2.192   2.192   2.192  
#> The summary table of dose levels, the optimal dose
#>  to recommend for subsequent study across all simulations
#>     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
#>    10.12   10.12   10.12   10.12   10.12   10.12  
#> The summary table of the final ratios of the optimal dose for stopping across
#>         all simulations
#>     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.  
#>        1       1       1       1       1       1  
#> Fitted expected efficacy level at dose most often selected : mean NA (NA, NA) 
#> Stop reason triggered:
#>  ≥ 10 patients dosed :  100 %

# nolint end
```
