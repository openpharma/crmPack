# Integration with knitr ----
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' We provide additional utility functions to allow human-friendly rendition of
#' crmPack objects in Markdown and Quarto files
#'
#' @return a character string that represents the object in markdown.
#' @name knit_print
NULL

# Recommendation ----

#' Render a `Recommendation` Object
#'
#' @description `r lifecycle::badge("experimental")`
#' @param x (`Recommendation`)\cr The object to knit_print.
#' @param asis (`flag`)\cr Should the return value be wrapped in a call to `knitr::asis_output`?
#' @param unit (`character`)\cr The dose unit.
#' @param ... Not used at present
#'
#' @return The markdown representation of the object, as a character string
#' @seealso [`knit_print`] for more details.
#'
#' @export
#' @method knit_print Recommendation
#' @rdname knit_print
knit_print.Recommendation <- function(
    x,
    ...,
    asis = TRUE,
    unit = ""
) {
  assert_flag(asis)

  rv <- paste0(
    "The suggested dose for the next cohort is ",
    x@value,
    ifelse(stringr::str_length(unit) > 0, " ", ""),
    unit,
    ".\n\n"
  )
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# Delete after running devtools::document()
registerS3method("knit_print", "Recommendation", knit_print.Recommendation)

# RecommendationProbs ----

#' Render a `RecommendationProbs` Object
#'
#' @description `r lifecycle::badge("experimental")`
#' @param x (`RecommendationProbs`)\cr The object to knit_print.
#' @param asis (`flag`)\cr Should the return value be wrapped in a call to `knitr::asis_output`?
#' @param caption (`character`)\cr The caption for the probs table.
#' @param unit (`character`)\cr The dose unit.
#' @param ... Not used at present
#'
#' @return The markdown representation of the object, as a character string
#' @seealso [`knit_print`] for more details.
#'
#' @export
#' @method knit_print RecommendationProbs
#' @rdname knit_print
knit_print.RecommendationProbs <- function(
    x,
    ...,
    asis = TRUE,
    unit = "",
    caption = "Probabilities by dose"
) {
  assert_flag(asis)
  assert_character(caption, len = 1, any.missing = FALSE)
  assert_character(unit, len = 1, any.missing = FALSE)

  param <- h_kable_param_default(
    list(...),
    caption = caption
  )
  if (stringr::str_length(unit) > 0) {
    tbl <- x@details %>% dplyr::mutate(Dose = paste0(Dose, " ", unit))
  } else {
    tbl <- x@details
  }
  param[["x"]] <- tbl
  rv <- paste0(
    h_get_recommendation_text(x, unit),
    h_get_table_intro(x),
    "\n\n",
    paste0(do.call(knitr::kable, param), collapse = "\n"),
    "\n\n"
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# Delete after running devtools::document()
registerS3method("knit_print", "RecommendationProbs", knit_print.RecommendationProbs)

# RecommendationThreePlusThree ----

#' Render a `RecommendationThreePlusThree` Object
#'
#' @description `r lifecycle::badge("experimental")`
#' @param x (`RecommendationThrePlusThree`)\cr The object to knit_print.
#' @param asis (`flag`)\cr Should the return value be wrapped in a call to `knitr::asis_output`?
#' @param unit (`character`)\cr The dose unit.
#' @param ... Not used at present
#'
#' @return The markdown representation of the object, as a character string
#' @seealso [`knit_print`] for more details.
#'
#' @export
#' @method knit_print RecommendationThreePlusThree
#' @rdname knit_print
knit_print.RecommendationThreePlusThree <- function(
    x,
    ...,
    asis = TRUE,
    unit = ""
) {
  assert_flag(asis)
  assert_character(unit, len = 1, any.missing = FALSE)

  if (x@stopHere) {
    rv <- paste0(
      "The estimated MTD is ",
      x@value,
      ifelse(stringr::str_length(unit) > 0, " ", ""),
      unit,
      " and the recommendation is that the trial should stop.\n\n"
    )
  } else {
    rv <- paste0(
      "The recommendeded dose for the next cohort is ",
      x@value,
      ifelse(stringr::str_length(unit) > 0, " ", ""),
      unit,
      " and the recommendation is that the trial should continue.\n\n"
    )
  }

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# Delete after running devtools::document()
registerS3method(
  "knit_print",
  "RecommendationThreePlusThree",
  knit_print.RecommendationThreePlusThree
)

#' Internal Helper Method to Describe a Dose Recommendation
#'
#' @param x (`RecommendationThrePlusThree`)\cr The `Recommendation` object.
#' @param ... Passed to class-specific implementations
#'
#' @returns a character scalar containing a representation of the current dose
#' recommendation
#' @keywords internal
h_get_recommendation_text <- function(x, ...) {
  UseMethod("h_get_recommendation_text")
}

#' Describe a dose recommendation from a `Recommendation` Object
#'
#' @param x (`Recommendation`) The `Recommendation` object
#' @param unit (`character`) a scalar character giving the units in which the dose
#' is expressed
#'
#' @returns a character scalar containing a representation of the current dose
#' recommendation
#' @keywords internal
#' @export
h_get_recommendation_text.Recommendation <- function(x, unit) {
  if (length(x@value) == 1) {
    paste0(
      "The recommended dose for the next cohort is ",
      x@value,
      ifelse(stringr::str_length(unit) > 0, " ", ""),
      unit,
      ".\n\n"
    )
  } else {
    paste0(
      "The recommended doses for the next cohort are ",
      x@value[1],
      ifelse(stringr::str_length(unit) > 0, " ", ""),
      unit,
      " for Drug 1 and ",
      x@value[2],
      ifelse(stringr::str_length(unit) > 0, " ", ""),
      unit,
      " for Drug 2.",
      "\n\n"
    )
  }
}

#' Describe a dose recommendation from a `RecommendationTD` Object
#'
#' @param x (`RecommendationTD`) The `RecommendationTD` object
#' @param unit (`character`) a scalar character giving the units in which the dose
#' is expressed
#'
#' @returns a character scalar containing a representation of the current dose
#' recommendation
#' @keywords internal
#' @export
h_get_recommendation_text.RecommendationTD <- function(x, unit) {
  paste0(
    "The recommended dose for the next cohort is ",
    x@value[1],
    ifelse(stringr::str_length(unit) > 0, " ", ""),
    unit,
    " and current estimate of the MTD is ",
    x@value[2],
    ifelse(stringr::str_length(unit) > 0, " ", ""),
    unit,
    ".\n\n"
  )
}

#' Describe a dose recommendation from a `RecommendationMaxGain` Object
#'
#' @param x (`RecommendationMaxGain`) The `RecommendationMaxGain` object
#' @param unit (`character`) a scalar character giving the units in which the dose
#' is expressed
#'
#' @returns a character scalar containing a representation of the current dose
#' recommendation
#' @keywords internal
#' @export
h_get_recommendation_text.RecommendationMaxGain <- function(x, unit) {
  paste0(
    "The recommended dose for the next cohort is ",
    x@value[1],
    ifelse(stringr::str_length(unit) > 0, " ", ""),
    unit,
    ", current estimate of the MTD is ",
    x@value[2],
    ifelse(stringr::str_length(unit) > 0, " ", ""),
    unit,
    " and the dose which, if used in the next cohort, maximises the information gain is ",
    x@value[3],
    ifelse(stringr::str_length(unit) > 0, " ", ""),
    unit,
    ".\n\n"
  )
}

#' Obtain the Introductory Text for a Details Table in a `RecommendationDetails` Object
#'
#' @param x (`RecommendationDetails`) The `RecommendationDetails` object
#' @param ... Passed to class-specific methods
#'
#' @returns a character scalar containing the introductory Markdown
#' @keywords internal
h_get_table_intro <- function(x, ...) {
  UseMethod("h_get_table_intro")
}

#' Obtain the Introductory Text for a Details Table in a `RecommendationDetails` Object
#'
#' @param x (`RecommendationDetails`) The `RecommendationDetails` object
#' @param ... Not used at present
#'
#' @returns a character scalar containing the introductory Markdown
#' @keywords internal
h_get_table_intro.RecommendationDetails <- function(x, ...) {
  "Default introduction text.\n\n"
}

#' Obtain the Introductory Text for a Details Table in a `RecommendationDetails` Object
#'
#' @param x (`RecommendationDetails`) The `RecommendationDetails` object
#' @param ... Not used at present
#'
#' @returns a character scalar containing the introductory Markdown
#' @keywords internal
h_get_table_intro.RecommendationDetails <- function(x, ...) {
  "Default introduction text.\n\n"
}

#' Obtain the Introductory Text for a Details Table in a `RecommendationDetails` Object
#'
#' @param x (`RecommendationDetails`) The `RecommendationDetails` object
#' @param ... Not used at present
#'
#' @returns a character scalar containing the introductory Markdown
#' @keywords internal
#' @export
h_get_table_intro.RecommendationDetails <- function(x, ...) {
  "Default introduction text.\n\n"
}

#' Obtain the Introductory Text for a Details Table in a `RecommendationAllocation` Object
#'
#' @param x (`RecommendationAllocation`) The `RecommendationAllocation` object
#' @param ... Not used at present
#'
#' @returns a character scalar containing the introductory Markdown
#' @keywords internal
#' @export
h_get_table_intro.RecommendationAllocation <- function(x, ...) {
  "The allocation probabilities at each dose in the dose grid are given in the table below.\n\n"
}

#' Obtain the Introductory Text for a Details Table in a `RecommendationNCRMLoss` Object
#'
#' @param x (`RecommendationNCRMLoss`) The `RecommendationNCRMLoss` object
#' @param ... Not used at present
#'
#' @returns a character scalar containing the introductory Markdown
#' @keywords internal
#' @export
h_get_table_intro.RecommendationNCRMLoss <- function(x, ...) {
  paste0(
    "For each dose in the dose grid, the table below gives:\n\n",
    "- The probability that the dose lies in each toxicity band\n\n",
    "- The mean probability that a participant treated at the dose reports a ",
    "toxicity (and its standard deviation)\n\n",
    "- The posterior loss associated with treating a participant at the dose\n\n",
    "\n\n"
  )
}

#' Obtain the Introductory Text for a Details Table in a `RecommendationTD` Object
#'
#' @param x (`RecommendationTD`) The `RecommendationTD` object
#' @param ... Not used at present
#'
#' @returns a character scalar containing the introductory Markdown
#' @keywords internal
#' @export
h_get_table_intro.RecommendationTD <- function(x, ...) {
  paste0(
    "The table below shows the dose currently expected to provide the target ",
    "toxicity rate both during the trial and at its end.  This latter estimate ",
    "is the current estimate of the MTD.  The 95% CI for the estimate of the MTD ",
    "is also shown, together with the ratio of its upper and lower bounds.",
    "\n\n"
  )
}

#' Obtain the Introductory Text for a Details Table in a `RecommendationMaxGain` Object
#'
#' @param x (`RecommendationMaxGain`) The `RecommendationMaxGain` object
#' @param ... Not used at present
#'
#' @returns a character scalar containing the introductory Markdown
#' @keywords internal
#' @export
h_get_table_intro.RecommendationMaxGain <- function(x, ...) {
  paste0(
    "The table below shows the dose currently expected to provide the target ",
    "toxicity rate both during the trial and at its end (which is the current ",
    "estimate of the MTD), together with the dose allocation for the next ",
    "cohort that will maximise the information gain.  For the latter two, the ",
    "95% CI for the estimate is also shown, together with the ratio of its ",
    "upper and lower bounds.",
    "\n\n"
  )
}

#' Obtain the Introductory Text for a Details Table in a `RecommendationProbsDualEndpoint` Object
#'
#' @param x (`RecommendationProbsDualEndpoint`) The `RecommendationProbsDualEndpoint` object
#' @param ... Not used at present
#'
#' @returns a character scalar containing the introductory Markdown
#' @keywords internal
#' @export
h_get_table_intro.RecommendationProbsDualEndpoint <- function(x, ...) {
  paste0(
    "The table below shows the current estimated probabilities of target efficacy ",
    "(based on the biomarker) and overdose (based on predicted incidence of ",
    "toxicities for each dose",
    "\n\n"
  )
}

#' Obtain the Introductory Text for a Details Table in a `RecommendationProbsDataCombot` Object
#'
#' @param x (`RecommendationProbsDataCombo`) The `RecommendationProbsDuataCombo` object
#' @param ... Not used at present
#'
#' @returns a character scalar containing the introductory Markdown
#' @keywords internal
#' @export
h_get_table_intro.RecommendationProbsDataCombo <- function(x, ...) {
  paste0(
    "The table below shows the current estimated probability of toxicity of ",
    "each grade for each dose combination, together with a flag indicating ",
    "whether the combination is eligible for adminsitration to the next cohort.",
    "\n\n"
  )
}

#' Obtain the Caption for a Details Table in a `RecommendationDetails` Object
#'
#' @param x (`RecommendationDetails`) The `RecommendationDetails` object
#' @param ... passed to class-specific methods
#'
#' @returns a character scalar containing the caption
#' @keywords internal
h_get_table_caption <- function(x, ...) {
  UseMethod("h_get_table_caption")
}

#' Obtain the Caption for a Details Table in a `RecommendationAllocation` Object
#'
#' @param x (`RecommendationAllocation`) The `RecommendationAllocation` object
#' @param ... Not used at present
#'
#' @returns a character scalar containing the caption
#' @keywords internal
#' @export
h_get_table_caption.RecommendationAllocation <- function(x, ...) {
  "Allocation Probabilities by Dose"
}

#' Obtain the Caption for a Details Table in a `RecommendationDetails` Object
#'
#' @param x (`RecommendationDetails`) The `RecommendationDetails` object
#' @param ... Not used at present
#'
#' @returns a character scalar containing the introductory Markdown
#' @keywords internal
#' @export
h_get_table_caption.RecommendationDetails <- function(x, ...) {
  "Details by Dose"
}

#' Obtain the Caption for a Details Table in a `RecommendationProbsDataCombo` Object
#'
#' @param x (`RecommendationProbsDataCombo`) The `RecommendationProbsDataCombo` object
#' @param ... Not used at present
#'
#' @returns a character scalar containing the introductory Markdown
#' @keywords internal
#' @export
h_get_table_caption.RecommendationProbsDataCombo <- function(x, ...) {
  "Eligibility and Probabilty of Toxicity by Dose Combination"
}

#' Obtain the Caption for a Details Table in a `RecommendationNCRMLoss` Object
#'
#' @param x (`RecommendationNCRMLoss`) The `RecommendationNCRMLoss` object
#' @param ... Not used at present
#'
#' @returns a character scalar containing the introductory Markdown
#' @keywords internal
#' @export
h_get_table_caption.RecommendationNCRMLoss <- function(x, ...) {
  "Probabilities, Toxicity Estimates and Posterior Losses by Dose"
}

#' Obtain the Caption for a Details Table in a `RecommendationNCRMLoss` Object
#'
#' @param x (`RecommendationTD`) The `RecommendationTD` object
#' @param ... Not used at present
#'
#' @returns a character scalar containing the introductory Markdown
#' @keywords internal
#' @export
h_get_table_caption.RecommendationTD <- function(x, ...) {
  "Summary by Estimation Time"
}

#' Obtain the Caption for a Details Table in a `RecommendationProbsDualEndpoint` Object
#'
#' @param x (`RecommendationProbsDualEndpoint`) The `RecommendationProbsDualEndpoint` object
#' @param ... Not used at present
#'
#' @returns a character scalar containing the introductory Markdown
#' @keywords internal
#' @export
h_get_table_caption.RecommendationDualEndpoint <- function(x, ...) {
  "Efficacy and Toxicity Probabilities by Dose"
}

# RecommendationDetails ----

#' Render a `RecommendationDetails` Object
#'
#' @description `r lifecycle::badge("experimental")`
#' @param x (`RecommendationDetails`)\cr The object to knit_print.
#' @param asis (`flag`)\cr Should the return value be wrapped in a call to `knitr::asis_output`?
#' @param caption (`character`)\cr The caption for the table.
#' @param unit (`character`)\cr The dose unit.
#' @param ... Not used at present
#'
#' @return The markdown representation of the object, as a character string
#' @seealso [`knit_print`] for more details.
#'
#' @export
#' @method knit_print RecommendationDetails
#' @rdname knit_print
knit_print.RecommendationDetails <- function(
    x,
    ...,
    asis = TRUE,
    unit = "",
    caption = h_get_table_caption(x)
) {
  assert_flag(asis)
  assert_character(caption, len = 1, any.missing = FALSE)
  assert_character(unit, len = 1, any.missing = FALSE)

  param <- h_kable_param_default(
    list(...),
    caption = caption
  )
  if (stringr::str_length(unit) > 0) {
    tbl <- x@details %>% dplyr::mutate(Dose = paste0(Dose, " ", unit))
  } else {
    tbl <- x@details
  }
  param[["x"]] <- tbl
  rv <- paste0(
    h_get_recommendation_text(x, unit),
    h_get_table_intro(x),
    paste0(do.call(knitr::kable, param), collapse = "\n"),
    "\n\n"
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# Delete after running devtools::document()
registerS3method(
  "knit_print",
  "RecommendationDetails",
  knit_print.RecommendationDetails
)

