# Version 0.2.9

* By default only use 5 cores and not all available cores on a machine. Note that
  this value can also be changed by the user.
  
* Change of maintainer


# Version 0.2.8

### Bugfixes:

* PLcohortSize now defaults to 0 placebo patients upon Design class 
  initialization (instead of 1 before - but note that this did not have
  effect on erroneous simulations, due to option being set in Data class)

* The "examine" function also stops when the stopping rules are fulfilled 
  already in case of no DLTs occurring. This was not the case beforehand and 
  could lead to infinite looping (thanks to John Kirkpatrick for reporting 
  the bug)
  
* Removed RW2 warnings in "DualEndpointRW" - it seems to work nicely now 
  (thanks to Charles Warne for reporting!)
  
* Removed WinBUGS since it was not used anyway (and paper does not describe
  it)
  

### New features:
  
* The "examine" function now counts the number of times the same dose is 
  recommended contiguously and break after e.g. the default 100 times 
  (can be specified in a new option of "examine") to further avoid infinite 
  loops and issues a corresponding warning if this condition is met
  
* New "Increments" class "IncrementsNumDoseLevels" that works directly on
  the number of dose levels in the dose grid that can be incremented to
  from the current to the next cohort (thanks to John Kirkpatrick for the 
  suggestion). This can for example be used in order to force the design 
  not to skip any dose level when escalating.
  
* Included the JSS manuscript as a new vignette.

* It is now possible to specify how many cores should be used when parallel
  computations are used.
  
  

# Version 0.2.7

### Bugfixes:

* LogisticNormal now works again - "prec" was not found before.


# Version 0.2.6

### Bugfixes:

* Replaced BayesLogit dependency by JAGS code, since BayesLogit was taken off CRAN.

* Speed up one example to pass CRAN check.


# Version 0.2.5

### New features:

* matching of doses with the dose grid now includes a tolerance of 1e-10, in order
  to make it more user-friendly (thanks to YJ Choi and Giuseppe for investigating)

### Bugfixes:

* documentation: 
  - minor fix for alpha1 description in LogisticLogNormal-class
  
* minor fix on scale_colour_manual import from ggplot2 reported by R-Core


# Version 0.2.4

### New features:

* In case of multiple nextBest plots these are now also returned as original plots
  in the list singlePlots, to allow for further customization, before jointly plotting them. 
* ProbitLogNormal: Now also this model allows for a reference dose and a log transformation
  of the (standardized) dose. This can be specified with options refDose and useLogDose.
* DualEndpoint: Same additional options as for ProbitLogNormal are now available for the
  DualEndpoint models. As a consequence, the parameter "refDose" for class 
  DualEndpointBeta needed to be renamed to "refDoseBeta", and the parameter "refDose" for
  class DualEndpointEmax renamed to "refDoseEmax".


### Bugfixes:

* documentation: in the DualEndpoint description fixed the \left problem in the
  formula 

  
# Version 0.2.3

### New features:

* New increment class "IncrementMin" has been added which allows to combine multiple 
  increment rules with the MIN operation


# Version 0.2.1

### New features:

* Option targetThresh for NextBestDualEndpoint allows to tune from which target probability
  onwards it will be used to derive the next best dose (before this was fixed to 0.05)

* Added ProbitLogNormal model

* In the NextBestDualEndpoint class, the additional option "scale" now allows to also
  specify absolute biomarker target ranges. In the corresponding method evaluation,
  the safety samples are now no longer included in the evaluation of the biomarker
  target probability, such that now the description is consistent with the computations.
  
* NextBestNCRM and NextBestDualEndpoint now return the matrix of target and overdosing 
  probabilities as additional list element "probs" in the result of "nextBest" applied.
  
* Note that in the StoppingTargetBiomarker evaluation, the toxicity is no longer
  a part of the biomarker target probability.
  

### Bugfixes:

* Added back the example vignette, so that it can be opened with crmPackExample()

* Clarified that for the DualEndpointRW model samples from the prior cannot be 
  obtained due to impropriety of the RW prior (added to model class description).
  
* For DualEndpointRW models, it is now possible to have non-equidistant grid points,
  and obtain sensible results. (But still needs to be thouroughly tested though.)
  
* For DualEndpointBeta model, it is now possible to have negative E0 and Emax parameters.

* Cohort size of 0 for placebo is now possible - e.g. to only start with patients and
  then later move to larger cohorts also including placebo subjects.
  
* When simulating with firstSeparate=TRUE and placebo, now the first (sentinel) cohort
  includes one active and one placebo patients, and the next patients use the cohort
  size for the active and placebo arms, respectively.
  
* Barplots work now also when there was only one observed value in all simulations

* NextBestDualEndpoint now only takes into account active doses when optimizing the biomarker
  outcome for the next best dose among admissible doses, thus avoiding early stopping
  at the placebo dose level.
  
* If DataMixture objects are used, mcmc now correctly sets fromPrior to FALSE if
  the shared data object contains any data.
  

# Version 0.2.0

* Added arguments probmin and probmax to MinimalInformative in order to control
  the probability threshold at the minimum and maximum dose for the minimally 
  informative prior

* Values of 95% CI and the corresponding ratio of the upper to the lower limit of this CI
  are displayed in results when using 'nextBest'

* The six- number summary tables  including the values of the lowest, 25th percentile,
  50th percentile or the median, the mean, the 75th precentile and the highest of the 
  final (at stopping) estimates of the 
  1) dose levels corresponds to the target probability of DLE used at the end of a trial, TDEOT
  2) ratios of the upper to the lower 95% credibility intervals (CI) of TDEOT
  3) dose levels corresponds to the target probability of DLE used during a trial
  4) dose levels corresponds to the maximum gain value, Gstar
  5) ratios of the upper to the lower 95% CI of the final estimates of Gstar
  6) optimal doses, either the TDEOT (for DLE response only) or the minimum of TDEOT and Gstar 
     (for DLE and efficacy response)
  7) ratios of the optimal dose

  across all simulations will also be displayed when using 'summary' for simulations.
  


# Version 0.1.8

* The value of the 95% CI of the final estimates 
  will be displayed in results when using 'stopTrial'  
  
* Bugfixes for dual endpoint designs: 
  - Improved graphical display in plots for nextBest dose 
  - Improved methodology to compute Gstar
  - Warnings are removed when using nextBest in simulations
  - Stopping rules can now also be freely combined using the and/or operators    
    with the dual endpoint design stopping rules not using MCMC samples.

# Version 0.1.6

* New model class "LogisticLogNormalMixture" has been added, for use with the 
  new data class "DataMixture".

* New stopping rule "StoppingHighestDose" has been added.

* The "examine" method no longer stops when two consecutive cohorts start with 
  the same dose. This is important e.g. for the two-parts study designs, where 
  part 1 can end with the same dose as part 2 starts.
  
* The contents of the "datanames" slot of new models are no longer restricted to 
  a specific set, which was previously enforced by the validation function of 
  the GeneralModel and AllModels classes.
  
* Sampling from the prior can now be enabled/disabled by the user for the
  mcmc function, which is necessary for models where it might not be from the
  prior even though nObs == 0.

* Bugfix: The results from the MinimalInformative function were not reproducible
  beforehand. Now a seed parameter can be supplied, which ensures 
  reproducibility.

* Bugfix: Compatibility of help file links with new ggplot2 package version.

# Version 0.1.5

* Bugfix: In newer versions of grid the plotting of simulation objects did no 
  longer work. This was fixed.

# Version 0.1.2

* Bugfix: The MinimalInformative function previously produced too uninformative
  prior quantiles, which were not fulfilling the requirements in the function's
  documentation. With this bugfix, the correct (as per the Neuenschwander et al
  (2008) publication) prior quantiles are specified and then approximated with
  logistic (log) normal priors.

# Version 0.1.1

* Bugfix: Previously, it could happen with NextBestNCRM rule, that higher doses
  lead to decreasing probability of overdosing, only because for some doses
  there was numerically probability 1 of having a DLT. With this bugfix, it was
  clarified in the rules documentation and fixed in the rule method, that the
  right limit of the overdose interval vector will be inclusive.

# Version 0.1.0

* Added examine function to generate a table of hypothetical trial courses
  for model-based and rule-based DLT-endpoint designs

* Made results from mcmc() (works with the usual set.seed in earlier user code)
  and simulate() (as previously already promised) reproducible. See help file
  for mcmc for more details. Additional improvements to reduce confusing warning
  messages / notes from mcmc() and higher-level functions.

* Made simulate with parallel=TRUE work on r.roche.com (Linux server),
  using the same parallelization method as for laptops (Windows)

* Passing an empty (zero length) vector as the doselimit parameter of the
  nextBest function is now considered as requesting a dose recommendation
  without a strict dose limit, and a corresponding warning is printed.

* Introduced GeneralModel class, from which then the class Model for single
  agent dose escalation derives. Another branch will be the ComboLogistic model
  for multiple agent combinations (in a future version). Similarly introduced
  GeneralData class, from which the class Data for single agent derives,
  separately from that will be the subclass DataCombo (in a future version).

# Version 0.0.23

* Fixed bug in mcmc function which led to error
  "all data elements must have as many rows as the sample size was" and slightly
  changed JAGS way of handling burnin / thinning (which should not have a user
  impact).

* Reduced number of MCMC samples for dual-endpoint example in vignette to be
  able to plot the vignette

# Version 0.0.22

* simulate function has been fixed (specification of arguments)

* Dual-endpoint model-based design has been added.

* 3+3 design simulation is now possible, see ?ThreePlusThreeDesign

* Welcome message on attaching crmPack, i.e. when library("crmPack") is
  run

* crmPackUpgrade() function for easy upgrade of crmPack to the latest version

* Rule-based designs now can be specified with the class RuleDesign, while the
  model-based designs stay with the class Design. An even more special class is
  the DualDesign class, for dual-endpoint model-based designs. Corresponding
  classes GeneralSimulations, Simulations and DualSimulations capture the output
  of the trial simulations for rule-based, model-based and dual-endpoint
  designs.

* The class Simulations-summary has been renamed to SimulationsSummary,
  similarly for the classes GeneralSimulationsSummary and
  DualSimulationsSummary.

* All Stopping and CohortSize rules that are based on intervals
  (IncrementsRelative, IncrementsRelativeDLT, CohortSizeRange, CohortSizeDLT)
  now use a different intervals definition. Now the "intervals" slots only contain
  the left bounds of the intervals. Before, the last element needed to be
  infinity. See the vignette for examples.

* StoppingMaxPatients class has been removed, as it was redundant with the class
  StoppingMinPatients. Please just use the StoppingMinPatients class instead.

* Initialization methods have been replaced by dedicated initialization
  functions. Please now use these Class(...) functions instead of new("Class",
  ...) calls to obtain the correct objects. This change is also reflected in the
  vignette.

* The extract function for extracting parameter samples from Samples objects has
  been removed (due to a name conflict with ggmcmc dependency packages). Please
  now use instead the "get" method for Samples objects (see the vignette for an
  example) to obtain data in the ggmcmc format.

* crmPack now needs the package httr (it's now in the "Imports" field). Packages
  Rcpp and RcppArmadillo have been moved from "Depends" to "Suggests" packages.
  Currently we are not using them at all.

* showLegend argument for model fit plotting functions, in order to show the
  legend or not.

# Version 0.0.21

no NEWS until this version
