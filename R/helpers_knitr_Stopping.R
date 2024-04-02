# StoppingMissingDose         Done
# StoppingCohortsNearDose     Done
# StoppingPatientsNearDose    Done
# StoppingMinCohorts
# StoppingMinPatients
# StoppingTargetProb
# StoppingMTDdistribution
# StoppingMTDCV
# StoppingLowestDoseHSRBeta
# StoppingTargetBiomarker
# StoppingSpecificDose
# StoppingHighestDose
# StoppingTDCIRatio
# StoppingMaxGainCIRatio
# StoppingList
# StoppingAll
# StoppingAny
# StoppingOrdinal

#' @description `r lifecycle::badge("experimental")`
#' @param label (`character`)\cr the term used to label participants
#' @param asis (`flag`)\cr Not used at present
#' @rdname knit_print
#' @export
#' @method knit_print StoppingPatientsNearDose
knit_print.StoppingPatientsNearDose <- function(
    x,
    ...,
    label = "participants",
    asis = TRUE
) {
  rv <- paste0(
    ifelse(is.na(x@report_label), "", paste0(x@report_label, ": ")),
    "If ",
    x@nPatients,
    paste0(" or more ", label, " have been treated "),
    ifelse(
      x@percentage == 0,
      "at ",
      paste0("within ", x@percentage, "% of ")
    ),
    "the next best dose."
  )
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

#' @description `r lifecycle::badge("experimental")`
#' @param asis (`flag`)\cr Not used at present
#' @rdname knit_print
#' @export
#' @method knit_print StoppingCohortsNearDose
knit_print.StoppingCohortsNearDose <- function(
    x,
    ...,
    asis = TRUE
) {
  rv <- paste0(
    ifelse(is.na(x@report_label), "", paste0(x@report_label, ": ")),
    "If ",
    x@nCohorts,
    " or more cohorts have been treated ",
    ifelse(
      x@percentage == 0,
      "at ",
      paste0("within ", x@percentage, "% of ")
    ),
    "the next best dose."
  )
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

#' @description `r lifecycle::badge("experimental")`
#' @param asis (`flag`)\cr Not used at present
#' @rdname knit_print
#' @export
#' @method knit_print StoppingMissingDose
knit_print.StoppingMissingDose <- function(
    x,
    ...,
    asis = TRUE
) {
  rv <- paste0(
    ifelse(is.na(x@report_label), "", paste0(x@report_label, ": ")),
    "If the dose returned by <code>nextBest()</code> is ",
    "<code>NA</code>, or if the trial includes a placebo dose, the placebo dose."
  )
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}
