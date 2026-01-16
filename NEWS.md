# Version 3.0.0

* This release marks yet another (breaking) major update of the package: Backfill cohort simulations are now supported in the `Design` class, powered by the new `Backfill` class. This also leads to breaking changes in the `Data` class, which gains new slots, too. Please regenerate any existing `Data` or `Design` as well as resulting `Simulation` objects; seralized objects from previous versions will not be compatible.

### New Features

* A new vignette `trial_backfill` has been added which describes how to use backfill cohorts in `crmPack`.
* Added new `Backfill` class which allows to specify backfill cohorts as a new slot in the `Design` objects.
* Added new slots `backfilled` and `response` in the `Data` class to capture the information whether each patient has been backfilled, and whether a (binary) efficacy response was observed (in order to support backfilling rules which depend on a minimum number of efficacy responses).
* The `plot` method for `Data` objects allows to optionally exclude backfill patients, and mark backfill and response information.
* Added new virtual classes `Opening` and `Recruitment` and corresponding instantiable subclasses to allow for flexible specification of backfill cohort opening and recruitment rules.
* Added new `CohortSizeRandom` class to allow for random cohort sizes.
* Simulations of `Design` objects now support backfill cohorts.
* `StoppingPatientsNearDose` got a new slot `include_backfill` to allow for optionally excluding backfilled patients from the patient count.

# Version 2.0.2

### Miscellaneous

* Fixed logging tests which had been broken by recent `futile.logger` updates.

# Version 2.0.1

### Bugfixes

* The validation of `LogisticNormalFixedMixture` objects did not allow for a floating point calculation deviation in the sum of the weights when comparing them to 1, which was caught by the `M1mac` check on CRAN. This has been fixed now.

# Version 2.0.0

* This release signifies a major breaking revamp of the package. Over several years, the package has undergone significant cleanup, tests writing and refactoring efforts. The package has thereby reached a major milestone and currently includes:
  - 15 different dose-toxicity models
  - 15 different dose recommendation rules
  - 19 different stopping rules
  - 9 cohort size rules
  - 9 increment rules
  - Reporting functionality (using `knitr`)
  - 13 vignettes (documentation)
* Users are advised to carefully review the release notes and documentation for detailed information on the changes and any necessary updates to their existing code using a previous CRAN release of `crmPack`. In particular, there is a new vignette which describes how to use certain functions and features of `crmPack` after the major refactoring.

### New Features

* Added new `NextBestNCRMLoss` and `NextBestEWOC` classes and corresponding `nextBest` methods.
* Added new `DataGrouped` and `DesignGrouped` classes with corresponding model `LogisticLogNormalGrouped` to support simultaneous dose escalation with monotherapy and combination therapy arms.
* Provided basic support for ordinal CRM models in `DesignOrdinal` (simulation is not yet supported). See the vignette for more details.
* Included rolling CRM design implemented in `DADesign`. See the vignette for more details.
* Created new `ProbitLogNormalRel` model class to support the (standardized) dose.
* Implemented `knit_print` methods for almost all `crmPack` classes to improve rendering in Markdown and Quarto documents. See the vignette for more details.
* Implemented `broom`-like `tidy` methods for all concrete `crmPack` classes. See the vignette for more details.
* Added logging via the `futile.logger` package, the user interface consists of four functions: `enable_logging`, `disable_logging`, `is_logging_enabled`, `log_trace`.
* Created the `CrmPackClass` class as the ultimate ancestor of all other `crmPack` classes to allow identification of crmPack classes and simpler definition of generic methods.
* Added no-parameter constructor functions named `.Default<class name>` to provide usable instances of all concrete subclasses of `Increments`, `Model`, `NextBest` and `Stopping`.
* Included `additional_stats` to add reporting of additional parameters to method `simulate` to summarize MTD.
* `report_label` can be added to stopping rules for individual or combined stopping rule reporting.
* Implemented the `IncrementsMaxToxProb` class.

### Enhancements

* `approximate` now returns a `list` containing the fitted model and, optionally, a `ggplot` object of the approximated dose/toxicity curve.
* Modified `efficacy-EffFlexi` method: allowed for vectorized dose; `NA` is now returned for doses from outside of the dose grid range (and the warning is thrown).
* `doselimit` argument in `nextBest` method is now specified as `Inf` instead of `numeric(0)`.
* Warning message not printed anymore by `nextBest` methods when `doselimit` not specified.
* Allowed for `from_prior` flag - argument to `modelspecs` function at `GeneralModel` class.
* Introduced validation of the updated object for `update` methods for `Data`-like classes. Added `check` flag to possibly omit the validation of the updated object.
* Added new functions `dose_grid_range` and `ngrid`, which return the range and the number of doses in the dose grid, respectively.
* Added methods `names` and `size` for objects of class `Samples`.

### Bugfixes

* Corrected the spelling of the name of the `messgae` [sic] attribute of the return value of `stopTrial` with signature `stopping = "StoppingTDCIRatio"`.

### Refactoring

* Set up the package to use `testthat` and added unit and integration tests.
* Started using the `lifecycle` package to manage deprecations and breaking changes.
* Changed `ProbitLogNormal` so that it supports the log of (standardized) dose only.
* Replaced warning with message when no `cohort` or `ID` is provided to the user constructor `Data`.
* Re-factored `sampleSize` function so that it returns `0` if `burnin > iterations`.

### Miscellaneous

* Removed `multiplot` function. Please use equivalent functionality in other packages, such as `cowplot` or `ggpubr`.

# Version 1.0.0

* Reference JSS publication.

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

* `LogisticNormal` now works again - `prec` was not found before.


# Version 0.2.6

### Bugfixes:

* Replaced `BayesLogit` dependency by JAGS code, since `BayesLogit` was taken off CRAN.

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
  and obtain sensible results. (But still needs to be thoroughly tested though.)
  
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
