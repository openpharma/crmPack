# Integration with knitr ----
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' We provide additional utility functions to allow human-friendly rendition of
#' crmPack objects in Markdown and Quarto files.  This file contains methods for
#' all design classes, not just those that are direct descendants of `Design`.
#'
#' @return a character string that represents the object in markdown.
#' @name knit_print
NULL

# RuleDesign                     Done
# DesignGrouped
# RuleDesignOrdinal
# Design                         Done
# TDsamplesDesign
# TDDesign
# DesignOrdinal
# DualDesign                     Done
# DualResponsesSamplesDesign
# DualResponsesDesign
# DADesign                       Done

# Helper class

#' `StartingDose`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`StartingDose`] is a simple wrapper class for the `startingDose` slot of all
#' design classes. It is used internally by `knit_print` methods
#'
#' @slot starting_dose (`numeric`)\cr the starting dose
#' @noRd
#' @keywords internal
.StartingDose <- setClass(
  Class = "StartingDose",
  slots = c(
    starting_dose = "numeric"
  ),
  prototype = prototype(
    starting_dose = 1
  ),
  contains = "CrmPackClass"
)

## constructor ----

#' @rdname StartingDose-class
#'
#' @param starting_dose (`positive_number`)\cr see slot definition.
#'
#' @noRd
#' @keywords internal
#'
StartingDose <- function(starting_dose) {
  new(
    "StartingDose",
    starting_dose = starting_dose
  )
}

#' @rdname StartingDose-class
#' @note Typically, end users will not use the `.DefaultStartingDose()` function.
#' @noRd
#' @keywords internal

.DefaultStartingDose <- function() {
  StartingDose(starting_dose = 5)
}

#' Internal Helper Functions for Validation of [`StartingDose`] Objects
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' These functions are only used internally to validate the format of an input
#' [`RuleDesign`] or inherited classes and therefore not exported.
#'
#' @name v_starting_dose
#' @param object (`StartingDose`)\cr object to validate.
#' @return A `character` vector with the validation failure messages,
#'   or `TRUE` in case validation passes.
NULL

#' @describeIn v_starting_dose validates that the [`StartingDose`] object
#'   contains valid `starting_dose`.
#'  @noRd
#' @keywords internal
v_starting_dose <- function(object) {
  v <- Validate()
  v$check(
    test_number(object@starting_dose, finite = TRUE, min = 0),
    "starting_dose must be a non-negative number"
  )
  v$result()
}



# Helper functions ----

#' @description A Helper Function to create Markdown Headers
#'
#' @param text (`character`) the header text
#' @param level (`positive_integer`) the level of the header.  Must be between 1 and 6.
#' @return the Markdown header string: a newline, `#` repeated `level` times,
#' a space, `text` followed by two newlines.
#' @keywords internal
#' @noRd
h_markdown_header <- function(text, level = 2L) {
  assert_character(text, any.missing = FALSE, len = 1L, min.chars = 2L)
  assert_integer(level, min = 1L, max = 6L, any.missing = FALSE, len = 1L)

  paste0(
    "\n",
    stringr::str_dup("#", level),
    " ",
    text,
    "\n\n"
  )
}

#' Modify a Set of Default Slot Labels With Custom Custom Labels
#'
#' x (`S4`)\cr the S4 object for which slot labels are required
#' default_labels (`character`)\cr a vector of slot labels whose names are a
#' superset of the slot names of `x`
#' user_labels (`character`)\cr a vector of slot labels whose names are a
#' superset of the slot names of `x`.  Can be `NA`, in which case no updates
#' are made
#' @returns `default_labels` updated according to `user_labels`
#' @noRd
#' @keywords internal
h_prepare_section_labels <- function(x, default_labels, user_labels = NA) {
  assert_true(isS4(x))
  assert_character(default_labels, any.missing = FALSE)

  if (!any(is.na(user_labels))) {
    assert_character(user_labels, any.missing = FALSE)
    assert_subset(names(user_labels), slotNames(x))

    for (nm in names(user_labels)) {
      default_labels[nm] <- user_labels[nm]
    }
  }
  default_labels
}

# Methods ----

# StartingDose ----

#' @description `r lifecycle::badge("experimental")`
#' @noRd
#' @keywords internal
#' @method knit_print StartingDose
knit_print.StartingDose <- function(x, ..., asis = TRUE) {
  args <- list(...)
  units <- ifelse("units" %in% names(args), paste0(" ", args[["units"]]), "")
  rv <- paste0(
    "The starting dose is ",
    paste0(x@starting_dose, units),
    ".\n\n"
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}


# RuleDesign ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.StoppingTargetProb
#' @param level (`positive_integer`) The level of the headings used to separate
#' slots.  Must be between 1 and 6.
#' @param title (`character`) The text of the heading of the section describing
#' the design
#' @param sections (`character`) a named vector of length at least 4 defining
#' the headings used to define the sections corresponding to the design's slots.
#' The element names must match the Design's slot names.
#' @rdname knit_print
#' @export
#' @method knit_print RuleDesign
knit_print.RuleDesign <- function(
    x,
    ...,
    level = 2L,
    title = "Design",
    sections = NA,
    asis = TRUE
) {
  assert_flag(asis)
  assert_integer(level, min = 1L, max = 6L, any.missing = FALSE, len = 1L)
  assert_character(title, any.missing = FALSE, len = 1L)

  args <- list(...)
  units <- ifelse("units" %in% names(args), paste0(" ", args[["units"]]), "")
  section_labels <- h_prepare_section_labels(
    x,
    c(
      "nextBest"     = "Dose recommendation",
      "cohort_size"  = "Cohort size",
      "data"         = "Observed data",
      "startingDose" = "Starting dose"
    ),
    sections
  )

  rv <- paste0(
    paste0(
      lapply(
        slotNames(x),
        function(nm) {
          tmp <- switch(
            nm,
            startingDose = knit_print(StartingDose(x@startingDose), asis = FALSE, ...),
            pl_cohort_size = ifelse (
              identical(slot(x, "pl_cohort_size"), CohortSizeConst(0)),
              "Placebo will not be administered in the trial.\n\n",
              knit_print(slot(x, "pl_cohort_size"), asis = FALSE, ...)
            ),
            knit_print(slot(x, nm), asis = FALSE, ...)
          )
          # if (nm == "startingDose") {
          #   tmp <- knit_print(StartingDose(x@startingDose), asis = FALSE, ...)
          # } else {
          #   tmp <- knit_print(slot(x, nm), asis = FALSE, ...)
          # }
          paste0(h_markdown_header(section_labels[nm], level + 1L), tmp)
        }
      ),
      collapse = "\n\n"
    ),
    "\n\n"
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# Design ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.RuleDesign
#' @rdname knit_print
#' @export
#' @method knit_print Design
knit_print.Design <- function(
    x,
    ...,
    level = 2L,
    title = "Design",
    sections = NA,
    asis = TRUE
) {
  section_labels <- h_prepare_section_labels(
    x,
    c(
      "increments"     = "Escalation rule",
      "stopping"       = "Stopping rule",
      "model"          = "Dose toxicity model",
      "pl_cohort_size" = "Use of placebo"
    ),
    sections
  )
  knit_print.RuleDesign(
    x,
    level = level,
    title = title,
    sections = section_labels,
    ...)
}

# DualDesign ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.RuleDesign
#' @rdname knit_print
#' @export
#' @method knit_print DualDesign
knit_print.DualDesign <- function(
    x,
    ...,
    level = 2L,
    title = "Design",
    sections = NA,
    asis = TRUE
) {
  knit_print.Design(
    x,
    level = level,
    title = title,
    sections = sections,
    ...)
}

# DADesign ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.RuleDesign
#' @rdname knit_print
#' @export
#' @method knit_print DADesign
knit_print.DADesign <- function(
    x,
    ...,
    level = 2L,
    title = "Design",
    sections = NA,
    asis = TRUE
) {
  section_labels <- h_prepare_section_labels(
    x,
    c(
      "data"          = "Observed data",
      "safetyWindow"  = "Safety window"
    ),
    sections
  )
  knit_print.Design(
    x,
    level = level,
    title = title,
    sections = section_labels,
    ...
  )
}

# TDDesign ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.RuleDesign
#' @rdname knit_print
#' @export
#' @method knit_print TDDesign
knit_print.TDDesign <- function(
    x,
    ...,
    level = 2L,
    title = "Design",
    sections = NA,
    asis = TRUE
) {
  knit_print.Design(
    x,
    level = level,
    title = title,
    sections = sections,
    ...
  )
}

# DualResponsesDesign ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.RuleDesign
#' @rdname knit_print
#' @export
#' @method knit_print DualResponsesDesign
knit_print.DualResponsesDesign <- function(
    x,
    ...,
    level = 2L,
    title = "Design",
    sections = NA,
    asis = TRUE
) {
  knit_print.Design(
    x,
    level = level,
    title = title,
    sections = sections,
    ...
  )
}
