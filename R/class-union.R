#' @include ModelParams-class.R
#' @include Data-class.R
#' @include Design-class.R
#' @include McmcOptions-class.R
#' @include Model-class.R
#' @include Rules-class.R
#' @include Samples-class.R
#' @include Simulations-class.R

# Children must be added to the union before their parents.  In each call to
# `useClassUnion`, the classes are added atomically.  Therefore, a stepwise
# approach is required.

# Temporary: needed because of references to DataOrdinal in
# /design/ordinal-crm-*.Rmd files.
setClass(
  Class = "DataOrdinal"
)

# CrmPackClass-classunion

#CohortSize
setClassUnion(
  "CrmPackCohortSize1",
  c(
    "CohortSizeRange",
    "CohortSizeDLT",
    "CohortSizeConst",
    "CohortSizeParts",
    "CohortSizeMax",
    "CohortSizeMin"
  )
)
setClassUnion("CrmPackCohortSize", c("CrmPackCohortSize1", "CohortSize"))

# Data
setClassUnion(
  "CrmPackData1",
  c(
    "DataDual",
    "DataDA",
    "DataMixture",
    "DataParts",
    "DataOrdinal"
  )
)
setClassUnion("CrmPackData2", c("Data", "CrmPackData1"))
setClassUnion("CrmPackData", c("GeneralData", "CrmPackData2"))

#Design
setClassUnion(
  "CrmPackDesign1",
  c(
    "DualDesign",
    "DualResponsesDesign",
    "DualResponsesSamplesDesign",
    "DADesign"
  )
)
setClassUnion(
  "CrmPackDesign2",
  c(
    "Design",
    "TDDesign",
    "TDsamplesDesign",
    "CrmPackDesign1"
  )
)
setClassUnion("CrmPackDesign", c("RuleDesign", "CrmPackDesign2"))

#Increments
setClassUnion(
  "CrmPackIncrements1",
  c(
    "IncrementsRelativeParts",
    "IncrementsRelativeDLTCurrent",
    "IncrementsDoseLevels",
    "IncrementsHSRBeta",
    "IncrementsMin"
  )
)
setClassUnion(
  "CrmPackIncrements2",
  c(
    "IncrementsRelativeDLT",
    "IncrementsRelative",
    "CrmPackIncrements1"
  )
)
setClassUnion("CrmPackIncrements", c("Increments", "CrmPackIncrements2"))

#Models
setClassUnion(
  "CrmPackModel1",
  c(
    "LogisticNormal",
    "LogisticLogNormalSub",
    "ProbitLogNormal",
    "ProbitLogNormalRel",
    "LogisticNormalMixture",
    "LogisticLogNormalMixture",
    "LogisticNormalFixedMixture",
    "DualEndpointBeta",
    "DualEndpointEmax",
    "DualEndpointRW",
    "LogisticIndepBeta",
    "Effloglog",
    "EffFlexi",
    "DALogisticLogNormal",
    "TITELogisticLogNormal",
    "OneParExpPrior",
    "FractionalCRM"
  )
)
setClassUnion(
  "CrmPackModel2",
  c(
    "DualEndpoint",
    "ModelTox",
    "ModelEff",
    "OneParLogNormalPrior",
    "LogisticKadaneBetaGamma",
    "CrmPackModel1"
  )
)
setClassUnion(
  "CrmPackModel3",
  c(
    "ModelLogNormal",
    "LogisticLogNormal",
    "LogisticKadane",
    "CrmPackModel2"
  )
)
setClassUnion("CrmPackModel", c("ModelPseudo", "GeneralModel", "CrmPackModel3"))

#NextBest
setClassUnion(
  "CrmPackNextBest1",
  c(
    "NextBestMTD",
    "NextBestNCRMLoss",
    "NextBestThreePlusThree",
    "NextBestDualEndpoint",
    "NextBestMinDist",
    "NextBestInfTheory",
    "NextBestMaxGainSamples",
    "NextBestProbMTDLTE",
    "NextBestProbMTDMinDist",
    "NextBestTDsamples"
  )
)
setClassUnion(
  "CrmPackNextBest",
  c(
    "NextBestTD",
    "NextBestMaxGain",
    "NextBestNCRM",
    "NextBest",
    "CrmPackNextBest1"
  )
)

#SafetyWindow
setClassUnion("CrmPackSafetyWindow1", c("SafetyWindowConst", "SafetyWindowSize"))
setClassUnion("CrmPackSafetyWindow", c("SafetyWindow",  "CrmPackSafetyWindow1"))

#Simulations
setClassUnion("CrmPackSimulations1", c("DualSimulations"))
setClassUnion("CrmPackSimulations2", c("Simulations", "CrmPackSimulations1"))
setClassUnion("CrmPackSimulations", c("GeneralSimulations", "CrmPackSimulations2"))

#Stopping
setClassUnion(
  "CrmPackStopping1",
  c(
    "StoppingMissingDose",
    "StoppingCohortsNearDose",
    "StoppingPatientsNearDose",
    "StoppingMinCohorts",
    "StoppingTargetProb",
    "StoppingMTDdistribution",
    "StoppingMTDCV",
    "StoppingLowestDoseHSRBeta",
    "StoppingTargetBiomarker",
    "StoppingSpecificDose",
    "StoppingHighestDose",
    "StoppingList",
    "StoppingAll",
    "StoppingAny",
    "StoppingTDCIRatio"
  )
)

setClassUnion("CrmPackStopping", c("CrmPackStopping1", "Stopping"))

#' A Class Union to Identify All Package Classes
#' @rdname CrmPackClass
#' @export
setClassUnion(
  "CrmPackClass",
  c(
    "CrmPackCohortSize",
    "CrmPackData",
    "CrmPackDesign",
    "CrmPackIncrements",
    "ModelParamsNormal",
    "McmcOptions",
    "CrmPackModel",
    "CrmPackNextBest",
    "CrmPackSafetyWindow",
    "Samples",
    "CrmPackSimulations",
    "CrmPackStopping"
  )
)

