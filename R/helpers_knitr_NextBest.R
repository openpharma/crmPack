# These functions will need to be amended to support the report_label slot if
# and when it is added to NextBest classes

# NextBestMTD ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.StoppingTargetProb
#' @param target_label (`character`)\cr the term used to describe the target
#' toxicity rate
#' @rdname knit_print
#' @export
#' @method knit_print NextBestMTD
knit_print.NextBestMTD <- function(
  x,
  ...,
  target_label = "the 25th centile",
  tox_label = "toxicity",
  asis = TRUE
) {
  # Validate
  assert_flag(asis)
  assert_character(target_label, len = 1, any.missing = FALSE)

  tox_label <- h_prepare_labels(tox_label)
  # Execute
  rv <- paste0(
    "The dose level recommended for the next cohort will be selected as follows:\n\n",
    "-  First, ",
    target_label,
    " of the posterior distribution of ",
    tox_label[1],
    " will be calculated for all dose levels that are eligible according to the ",
    " Increments rule.\n",
    "-  Next, the \"target dose\" (which may not be part of the dose grid) for which ",
    target_label,
    " of the posterior distribution of ",
    tox_label[1],
    " is exactly equal to the target rate of ",
    x@target,
    " will be determined.\n",
    "- Finally, the dose level whose absolute distance from the target dose ",
    "is smallest will be selected as the recommended dose for the next cohort\n\n"
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# NextBestNCRM ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.StoppingTargetProb
#' @rdname knit_print
#' @export
#' @method knit_print NextBestNCRM
knit_print.NextBestNCRM <- function(
  x,
  ...,
  tox_label = "toxicity",
  asis = TRUE
) {
  # Validate
  assert_flag(asis)
  assert_character(tox_label, max.len = 2, any.missing = FALSE)

  # Execute
  rv <- paste0(
    "The dose recommended for the next cohort will be chosen in the following ",
    "way.  First, doses that are ineligible according to the increments rule ",
    "will be discarded.  Next, any dose for which the mean posterior probability of ",
    tox_label,
    " being in the overdose range - (",
    x@overdose[1],
    ", ",
    x@overdose[2],
    "] - is ",
    x@max_overdose_prob,
    " or more will also be discarded.  Finally, the dose amongst those remaining ",
    "which has the highest chance that the mean posterior probability of ",
    tox_label,
    " is in the target ",
    tox_label,
    " range of ",
    x@target[1],
    " to ",
    x@target[2],
    " (inclusive) will be selected.\n\n"
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# NextBestThreePlusThree ----

#' @description `r lifecycle::badge("experimental")`
#' @param label (`character`)\cr The term used to label the participants.
#' @param tox_label (`character`)\cr the term used to describe toxicity.  See
#' Usage Notes below.
#' See Usage Notes below.
#' @section Usage Notes:
#' This section describes the use of `label` and `tox_label`, collectively
#' referred to as `label`s.
#' A `label` should be a scalar or a vector of length 2.  If a scalar, it is
#' converted by adding a second element that is equal to the first, suffixed by `s`.
#' For example, `tox_label = "DLT"` becomes `tox_label = c("DLT", "DLTs")`.  The
#' first element of the vector is used to describe a count of 1.  The second
#' is used in all other cases.
#' @rdname knit_print
#' @export
#' @method knit_print NextBestThreePlusThree
knit_print.NextBestThreePlusThree <- function(
  x,
  ...,
  tox_label = c("toxicity", "toxicities"),
  label = "participant",
  asis = TRUE
) {
  # Validate
  assert_flag(asis)

  # Prepare
  tox_label <- h_prepare_labels(tox_label)
  label <- h_prepare_labels(label)

  # Execute
  rv <- paste0(
    "The dose recommended for the next cohort will be chosen using the \"Three ",
    "Plus Three\" rule.\n\n- If no ",
    tox_label[2],
    " have been reported at the current dose level, escalate by one dose level.\n",
    "- If the observed ",
    tox_label[1],
    " rate at the current dose level is exactly 1/3 and no more than three ",
    label[2],
    " treated at the current dose level are evaluable, remain at the current ",
    "dose level.\n",
    "- Otherwise, recommend that the trial stops and identify the MTD as dose ",
    "level immediately below the current one.\n\n"
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# NextBestDualEndpoint ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.StoppingTargetProb
#' @param biomarker_label (`character`)\cr the term used to describe the biomarker
#' @param biomarker_units (`character`)\cr the units in which the biomarker is
#' measured
#' @rdname knit_print
#' @export
#' @method knit_print NextBestDualEndpoint
knit_print.NextBestDualEndpoint <- function(
  x,
  ...,
  tox_label = "toxicity",
  biomarker_label = "the biomarker",
  biomarker_units = ifelse(x@target_relative, "%", ""),
  asis = TRUE
) {
  assert_flag(asis)
  assert_character(tox_label, len = 1, any.missing = FALSE)
  assert_character(biomarker_label, len = 1, any.missing = FALSE)
  assert_character(biomarker_units, len = 1, any.missing = FALSE)

  rv <- paste0(
    "The dose recommended for the next cohort will be chosen in the following ",
    "way.  First, doses that are ineligible according to the increments rule ",
    "will be discarded.  Next, any dose for which the mean posterior probability of ",
    tox_label,
    " being in the overdose range - (",
    x@overdose[1],
    ", ",
    x@overdose[2],
    "] - is ",
    x@max_overdose_prob,
    " or more will also be discarded.  Finally, the dose amongst those remaining ",
    "which has the highest chance that the mean posterior probability that ",
    biomarker_label,
    " is in the target range for ",
    biomarker_label,
    ", which is ",
    x@target[1],
    ifelse(
      x@target_relative,
      "",
      stringr::str_squish(paste0(" ", biomarker_units))
    ),
    " to ",
    x@target[2],
    ifelse(
      x@target_relative,
      "",
      stringr::str_squish(paste0(" ", biomarker_units))
    ),
    " (inclusive),",
    ifelse(
      x@target_relative,
      paste0(" of the maximum ", biomarker_label, " value"),
      ""
    ),
    " will be selected, provided that this probability exceeds ",
    x@target_thresh,
    ".  If no dose meets this threshold, then the highest eligible dose will ",
    "be selected.\n\n"
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# NextBestMinDist ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.StoppingTargetProb
#' @rdname knit_print
#' @export
#' @method knit_print NextBestMinDist
knit_print.NextBestMinDist <- function(
  x,
  ...,
  tox_label = "toxicity",
  asis = TRUE
) {
  # Validate
  assert_flag(asis)
  assert_character(tox_label, len = 1, any.missing = FALSE)

  # Execute
  rv <- paste0(
    "The dose recommended for the next cohort will be the one which is both ",
    "eligible and which has the smallest absolute difference between ",
    "its mean posterior estimate of the probability of ",
    tox_label,
    " and the target ",
    tox_label,
    " rate [",
    x@target,
    "].\n\n"
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# NextBestInfTheory ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.StoppingTargetProb
#' @param citation_text (`character`)\cr the text used to cite Mozgunov & Jaki
#' @param citation_link (`character`)\cr the link to Mozgunov & Jaki
#' @section Usage Notes:
#' To use a BibTeX-style citation, specify (for example) `citation_text =
#' "@MOZGUNOV", citation_link = ""`.
#' @rdname knit_print
#' @export
#' @method knit_print NextBestInfTheory
knit_print.NextBestInfTheory <- function(
  x,
  ...,
  tox_label = "toxicity",
  citation_text = "Mozgunov & Jaki (2019)",
  citation_link = "https://doi.org/10.1002/sim.8450",
  asis = TRUE
) {
  # Validate
  assert_flag(asis)
  assert_character(tox_label, len = 1, any.missing = FALSE)
  assert_character(citation_text, len = 1, any.missing = FALSE)
  assert_character(citation_link, len = 1, any.missing = FALSE)

  # Execute
  rv <- paste0(
    "The recommended dose for the next cohort will be chosen using the ",
    "complex infinite bounds penalisation (CIBP) criterion of ",
    "[",
    citation_text,
    "]",
    ifelse(nchar(citation_link) > 0, paste0("(", citation_link, ")"), ""),
    ".  Let\n\n",
    "$$ \\delta(\\hat{p}_d, \\gamma) = \\frac{(\\hat{p}_d - \\gamma)^2}",
    "{\\hat{p}_d^a \\cdot (1 - \\hat{p}_d)^{2 - a}} $$\n\n",
    "where a is the non-centrality parameter with a value of ",
    x@asymmetry,
    ", &gamma; is the target ",
    tox_label,
    " rate with a value of ",
    x@target,
    " and $\\hat{p}_d$ is the mean posterior estimate of the probability of ",
    tox_label,
    " at dose level d.\n\n",
    "The recommended dose for the next cohort will be ",
    "the value of d that minimises $\\delta(\\hat{p}_d, \\gamma)$.\n\n"
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# NextBestTD ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.StoppingTargetProb
#' @rdname knit_print
#' @export
#' @method knit_print NextBestTD
knit_print.NextBestTD <- function(
  x,
  ...,
  tox_label = "toxicity",
  asis = TRUE
) {
  # Validate
  assert_flag(asis)
  assert_character(tox_label, len = 1, any.missing = FALSE)

  # Execute
  rv <- paste0(
    "The dose recommended for the next cohort will be the one which is both ",
    "eligible and which is the highest dose in the dose grid strictly less than ",
    "the dose (which may not be in the dose grid) that has a posterior plug-in ",
    "estimate of the probability of ",
    tox_label,
    " exactly equal to the target ",
    tox_label,
    " rate, either during [",
    x@prob_target_drt,
    "] or at the end of the trial [",
    x@prob_target_eot,
    "].\n\n"
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# NextBestMaxGain ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.StoppingTargetProb
#' @rdname knit_print
#' @export
#' @method knit_print NextBestMaxGain
knit_print.NextBestMaxGain <- function(
  x,
  ...,
  tox_label = "toxicity",
  asis = TRUE
) {
  # Validate
  assert_flag(asis)
  assert_character(tox_label, len = 1, any.missing = FALSE)

  # Execute
  rv <- paste0(
    "The dose recommended for the next cohort will be the one which is closest to ",
    "Gstar, the dose that maximises the gain for probability of ",
    tox_label,
    " exactly equal to the target ",
    tox_label,
    " rate, either during [",
    x@prob_target_drt,
    "] or at the end of the trial [",
    x@prob_target_eot,
    "].\n\n"
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# NextBestProbMTDLTE ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.StoppingTargetProb
#' @rdname knit_print
#' @export
#' @method knit_print NextBestProbMTDLTE
knit_print.NextBestProbMTDLTE <- function(
  x,
  ...,
  tox_label = "toxicity",
  asis = TRUE
) {
  # Validate
  assert_flag(asis)
  assert_character(tox_label, len = 1, any.missing = FALSE)

  # Execute
  rv <- paste0(
    "The dose recommended for the next cohort will be the dose level with ",
    "the highest probability of being the highest dose with an estimated ",
    "probability of ",
    tox_label,
    " less than or equal to ",
    x@target,
    ".\n\n"
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# NextBestProbMTDMinDist ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.StoppingTargetProb
#' @rdname knit_print
#' @export
#' @method knit_print NextBestProbMTDMinDist
knit_print.NextBestProbMTDMinDist <- function(
  x,
  ...,
  tox_label = "toxicity",
  asis = TRUE
) {
  # Validate
  assert_flag(asis)
  assert_character(tox_label, len = 1, any.missing = FALSE)

  # Execute
  rv <- paste0(
    "The dose recommended for the next cohort will be the dose level with ",
    "the highest probability of being the highest dose with an estimated ",
    "probability of ",
    tox_label,
    " closest to ",
    x@target,
    ".\n\n"
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# NextBestNCRMLoss ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.StoppingTargetProb
#' @param format_func (`function`)\cr The function used to format the range table.
#' @importFrom rlang .data
#' @rdname knit_print
#' @export
#' @method knit_print NextBestNCRMLoss
knit_print.NextBestNCRMLoss <- function(
  x,
  ...,
  tox_label = "toxicity",
  asis = TRUE,
  format_func = h_knit_format_func
) {
  # Validate
  assert_flag(asis)
  assert_character(tox_label, len = 1, any.missing = FALSE)

  # Execute
  param <- list(...)
  param[["x"]] <- x %>%
    tidy() %>%
    dplyr::select(-MaxOverdoseProb)
  param[["col.names"]] <- c("Range", "Lower", "Upper", "Loss Coefficient")
  rv <- paste0(
    "The dose recommended for the next cohort will be chosen in the following ",
    "way:\n\n-  First, the chance that the probability of ",
    tox_label,
    " falls into each of the underdose, target ",
    ifelse(
      any(x@unacceptable != c(1, 1)),
      ", overdose and unacceptable",
      " and overdose"
    ),
    " dose ranges is calculated for element of the dose grid.\n",
    "-  Next, the loss associated with each dose is calculated by multiplying ",
    "these probabilities by the corresponding loss coefficient and summing the result.\n",
    "-  Then ineligible doses, and those with a probability of being in the ",
    ifelse(
      length(x@losses) == 3,
      "overdose range",
      "overdose or unaccaptable ranges"
    ),
    " that is greater than ",
    x@max_overdose_prob,
    ", are discarded.\n",
    "-  Finally, the dose level with the smallest loss is selected as the ",
    "recommended dose for the next cohort.\n\n",
    ifelse(
      toupper(tox_label) == tox_label,
      tox_label,
      stringr::str_to_sentence(tox_label)
    ),
    " ranges and loss coefficients are given in the following table:\n\n",
    paste((do.call(knitr::kable, param)) %>% format_func(), collapse = "\n"),
    "\n\n"
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# NextBestTDsamples ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.StoppingTargetProb
#' @rdname knit_print
#' @export
#' @method knit_print NextBestTDsamples
knit_print.NextBestTDsamples <- function(
  x,
  ...,
  tox_label = "toxicity",
  asis = TRUE
) {
  # Validate
  assert_flag(asis)
  assert_character(tox_label, len = 1, any.missing = FALSE)

  # Execute
  rv <- paste0(
    "The dose recommended for the next cohort will be the one which is both ",
    "eligible and which is the highest dose in the dose grid strictly less than ",
    "the dose (which may not be in the dose grid) that has a full Bayes posterior ",
    "estimate of the probability of ",
    tox_label,
    " exactly equal to the target ",
    tox_label,
    " rate, either during [",
    x@prob_target_drt,
    "] or at the end of the trial [",
    x@prob_target_eot,
    "].\n\n"
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}


# NextBestMaxGainSamples ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.StoppingTargetProb
#' @rdname knit_print
#' @export
#' @method knit_print NextBestMaxGainSamples
knit_print.NextBestMaxGainSamples <- function(
  x,
  ...,
  tox_label = "toxicity",
  asis = TRUE
) {
  # Validate
  assert_flag(asis)
  assert_character(tox_label, len = 1, any.missing = FALSE)

  # Execute
  rv <- paste0(
    "The dose recommended for the next cohort will be the one which is closest to ",
    "Gstar, the dose for which the full Bayes posterior estimate of the probability of ",
    tox_label,
    " maximises the gain relative to the target ",
    tox_label,
    " rate, either during [",
    x@prob_target_drt,
    "] or at the end of the trial [",
    x@prob_target_eot,
    "].\n\n"
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# NextBestOrdinal ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.StoppingTargetProb
#' @rdname knit_print
#' @export
#' @method knit_print NextBestOrdinal
knit_print.NextBestOrdinal <- function(
  x,
  ...,
  tox_label = "toxicity",
  asis = TRUE
) {
  assert_flag(asis)
  assert_character(tox_label, max.len = 2, any.missing = FALSE)

  tox_label <- h_prepare_labels(tox_label)
  rv <- paste0(
    "Based on a ",
    tox_label[1],
    " grade of ",
    x@grade,
    ": ",
    paste0(
      knit_print(x@rule, asis = asis, tox_label = tox_label, ...),
      collapse = "\n"
    ),
    "\n\n"
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}
