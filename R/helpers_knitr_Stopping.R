# StoppingMissingDose         Done
# StoppingCohortsNearDose     Done
# StoppingPatientsNearDose    Done
# StoppingMinCohorts          Done
# StoppingMinPatients         Done
# StoppingTargetProb          Done
# StoppingMTDdistribution     Done
# StoppingMTDCV               Done
# StoppingLowestDoseHSRBeta   Done
# StoppingTargetBiomarker     Done
# StoppingSpecificDose        Done
# StoppingHighestDose         Done
# StoppingTDCIRatio           Done
# StoppingMaxGainCIRatio
# StoppingList
# StoppingAll
# StoppingAny
# StoppingOrdinal

# StoppingTDCIRatio ---

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.StoppingTargetProb
#' @rdname knit_print
#' @export
#' @method knit_print StoppingMaxGainCIRatio
knit_print.StoppingMaxGainCIRatio <- function(
    x,
    ...,
    dose_label = "the next best dose",
    tox_label = "toxicity",
    fmt_string = paste0(
      "%sIf the ratio of the upper and lower limits of the posterior 95%% ",
      "credible interval for the probability of %s at the smaller of the doses ",
      "given by the end-of-trial TD estimate and the G* estimate is less than or ",
      "equal to "
    ),
    asis = TRUE
) {
  assert_flag(asis)
  assert_character(fmt_string, len = 1, any.missing = FALSE)
  assert_character(tox_label, len = 1, any.missing = FALSE)

  rv <- paste0(
    sprintf(
      fmt_string,
      ifelse(is.na(x@report_label), "", paste0(x@report_label, ": ")),
      tox_label
    ),
    x@target_ratio,
    "."
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# StoppingTDCIRatio ---

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.StoppingTargetProb
#' @rdname knit_print
#' @export
#' @method knit_print StoppingTDCIRatio
knit_print.StoppingTDCIRatio <- function(
    x,
    ...,
    dose_label = "the next best dose",
    tox_label = "toxicity",
    fmt_string = paste0(
      "%sIf, at %s, the ratio of the upper and lower limits of the posterior ",
      "95%% credible interval for the probability of %s is less than or equal to "
    ),
    asis = TRUE
) {
  assert_flag(asis)
  assert_character(fmt_string, len = 1, any.missing = FALSE)
  assert_character(tox_label, len = 1, any.missing = FALSE)
  assert_character(dose_label, len = 1, any.missing = FALSE)

  rv <- paste0(
    sprintf(
      fmt_string,
      ifelse(is.na(x@report_label), "", paste0(x@report_label, ": ")),
      dose_label,
      tox_label
    ),
    x@target_ratio,
    "."
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# StoppingTargetBiomarker ---

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.StoppingTargetProb
#' @param biomarker_label (`character`)\cr the term used to describe the biomarker
#' @rdname knit_print
#' @export
#' @method knit_print StoppingTargetBiomarker
knit_print.StoppingTargetBiomarker <- function(
    x,
    ...,
    dose_label = "the next best dose",
    biomarker_label = "the target biomarker",
    fmt_string = paste0(
      "%sIf, at %s, the posterior probability that %s is in the range (%.2f, %.2f)%s is %.0f%% or more."
    ),
    asis = TRUE
) {
  assert_flag(asis)
  assert_character(fmt_string, len = 1, any.missing = FALSE)
  assert_character(biomarker_label, len = 1, any.missing = FALSE)

  rv <- sprintf(
    fmt_string,
    ifelse(is.na(x@report_label), "", paste0(x@report_label, ": ")),
    dose_label,
    biomarker_label,
    x@target[1],
    x@target[2],
    ifelse(
      x@is_relative,
      paste0(", relative to the maximum value of ", biomarker_label, ","),
      ""
    ),
    100 * x@prob
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# StoppingLowestDoseHSRBeta ---

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.StoppingTargetProb
#' @rdname knit_print
#' @export
#' @method knit_print StoppingLowestDoseHSRBeta
knit_print.StoppingLowestDoseHSRBeta <- function(
    x,
    ...,
    tox_label = "toxicity",
    fmt_string = paste0(
      "%sIf, using a Hard Stopping Rule with a prior of Beta(%.0f, %.0f), the ",
      "lowest dose in the dose grid has a posterior probability of %s of ",
      "%.0f%% or more."
    ),
    asis = TRUE
) {
  assert_flag(asis)
  assert_character(fmt_string, len = 1, any.missing = FALSE)

  rv <- sprintf(
    fmt_string,
    ifelse(is.na(x@report_label), "", paste0(x@report_label, ": ")),
    x@a,
    x@b,
    tox_label,
    100 * x@prob
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# StoppingMTDCV ---

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.StoppingTargetProb
#' @rdname knit_print
#' @export
#' @method knit_print StoppingMTDCV
knit_print.StoppingMTDCV <- function(
    x,
    ...,
    fmt_string = paste0(
      "%sIf the posterior estimate of the robust coefficient of variation of the MTD, ",
      "expressed as a percentage, is both greater than zero and less than or ",
      "equal to %.0f%%."
    ),
    asis = TRUE
) {
  assert_flag(asis)
  assert_character(fmt_string, len = 1, any.missing = FALSE)

  rv <- sprintf(
    fmt_string,
    ifelse(is.na(x@report_label), "", paste0(x@report_label, ": ")),
    100 * x@thresh_cv
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# StoppingMTDdistribution ---

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.StoppingTargetProb
#' @rdname knit_print
#' @export
#' @method knit_print StoppingMTDdistribution
knit_print.StoppingMTDdistribution <- function(
    x,
    ...,
    fmt_string = "%sIf the mean posterior probability of %s at %.0f%% of %s is at least %4.2f.",
    dose_label = "the next best dose",
    tox_label = "toxicity",
    asis = TRUE
) {
  assert_flag(asis)
  assert_character(dose_label, len = 1, any.missing = FALSE)
  assert_character(tox_label, len = 1, any.missing = FALSE)
  assert_character(fmt_string, len = 1, any.missing = FALSE)

  rv <- sprintf(
    fmt_string,
    ifelse(is.na(x@report_label), "", paste0(x@report_label, ": ")),
    tox_label,
    100 * x@thresh,
    dose_label,
    x@prob
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# StoppingHighestDose ---

#' @description `r lifecycle::badge("experimental")`
#' @param asis (`flag`)\cr Not used at present
#' @rdname knit_print
#' @export
#' @method knit_print StoppingHighestDose
knit_print.StoppingHighestDose <- function(
    x,
    ...,
    dose_label = "the highest dose in the dose grid",
    asis = TRUE
) {
  rv <- paste0(
    ifelse(is.na(x@report_label), "", paste0(x@report_label, ": ")),
    "If the next best dose is ",
    dose_label, "."
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# StoppingSpecificDose ---

#' @description `r lifecycle::badge("experimental")`
#' @param asis (`flag`)\cr Not used at present
#' @inheritParams knit_print.StoppingTargetProb
#' @rdname knit_print
#' @export
#' @method knit_print StoppingSpecificDose
knit_print.StoppingSpecificDose <- function(
    x,
    ...,
    dose_label = as.character(x@dose),
    asis = TRUE
) {
  x@rule@report_label <- x@report_label
  knit_print(
    x@rule,
    ...,
    dose_label = dose_label,
    asis = asis
  )
}

# StoppingTargetProb ---

#' @description `r lifecycle::badge("experimental")`
#' @param asis (`flag`)\cr Not used at present
#' @param fmt_string (`character`)\cr the character string that defines the format
#' of the output
#' @param dose_label (`character`)\cr the term used to describe the target dose
#' @rdname knit_print
#' @export
#' @method knit_print StoppingTargetProb
knit_print.StoppingTargetProb <- function(
    x,
    ...,
    fmt_string = "%sIf the probability of %s at %s is in the range [%4.2f, %4.2f] is at least %4.2f.",
    dose_label = "the next best dose",
    tox_label = "toxicity",
    asis = TRUE
) {
  assert_flag(asis)
  assert_character(dose_label, len = 1, any.missing = FALSE)
  assert_character(tox_label, len = 1, any.missing = FALSE)
  assert_character(fmt_string, len = 1, any.missing = FALSE)

  rv <- sprintf(
    fmt_string,
    ifelse(is.na(x@report_label), "", paste0(x@report_label, ": ")),
    tox_label,
    dose_label,
    x@target[1],
    x@target[2],
    x@prob
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# StoppingMinCohorts ---

#' @description `r lifecycle::badge("experimental")`
#' @param asis (`flag`)\cr Not used at present
#' @rdname knit_print
#' @export
#' @method knit_print StoppingMinCohorts
knit_print.StoppingMinCohorts <- function(
    x,
    ...,
    asis = TRUE
) {
  assert_flag(asis)

  rv <- paste0(
    ifelse(is.na(x@report_label), "", paste0(x@report_label, ": ")),
    "If ",
    x@nCohorts,
    " or more cohorts have been treated."
  )
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# StoppingMinPatients ---

#' @description `r lifecycle::badge("experimental")`
#' @param label (`character`)\cr the term used to label participants
#' @param asis (`flag`)\cr Not used at present
#' @rdname knit_print
#' @export
#' @method knit_print StoppingMinPatients
knit_print.StoppingMinPatients <- function(
    x,
    ...,
    label = "participants",
    asis = TRUE
) {
  assert_flag(asis)
  assert_character(label, len = 1, any.missing = FALSE)

  rv <- paste0(
    ifelse(is.na(x@report_label), "", paste0(x@report_label, ": ")),
    "If ",
    x@nPatients,
    paste0(" or more ", label, " have been treated.")
  )
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# StoppingPatientsNearDose ---

#' @description `r lifecycle::badge("experimental")`
#' @param label (`character`)\cr the term used to label participants
#' @inheritParams knit_print.StoppingTargetProb
#' @param asis (`flag`)\cr Not used at present
#' @rdname knit_print
#' @export
#' @method knit_print StoppingPatientsNearDose
knit_print.StoppingPatientsNearDose <- function(
    x,
    ...,
    dose_label = "the next best dose",
    label = "participants",
    asis = TRUE
) {
  assert_flag(asis)
  assert_character(label, len = 1, any.missing = FALSE)
  assert_character(dose_label, len = 1, any.missing = FALSE)

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
    dose_label,
    "."
  )
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# StoppingCohortsNearDose ---

#' @description `r lifecycle::badge("experimental")`
#' @param asis (`flag`)\cr Not used at present
#' @inheritParams knit_print.StoppingTargetProb
#' @rdname knit_print
#' @export
#' @method knit_print StoppingCohortsNearDose
knit_print.StoppingCohortsNearDose <- function(
    x,
    ...,
    dose_label = "the next best dose",
    asis = TRUE
) {
  assert_flag(asis)
  assert_character(dose_label, len = 1, any.missing = FALSE)

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
    dose_label,
    "."
  )
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# StoppingMissingDose ---

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
  assert_flag(asis)

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
