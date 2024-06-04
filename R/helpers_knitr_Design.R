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

#' @describeIn v_starting_dose validates that the `StartingDose` object
#'   contains valid `starting_dose`.
#' @noRd
#' @keywords internal
v_starting_dose <- function(object) {
  v <- Validate()
  v$check(
    test_number(object@starting_dose, finite = TRUE, lower = 0),
    "starting_dose must be a non-negative, finite number"
  )
  v$result()
}

# Helper class

#' `StartingDose`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' [`StartingDose`] is a simple wrapper class for the `startingDose` slot of all
#' design classes. It is used internally by `knit_print` methods
#'
#' @slot starting_dose (`numeric`)\cr the starting dose
#' @rdname StartingDose-class
#' @keywords internal
.StartingDose <- setClass(
  Class = "StartingDose",
  slots = c(
    starting_dose = "numeric"
  ),
  prototype = prototype(
    starting_dose = 1
  ),
  validity = v_starting_dose,
  contains = "CrmPackClass"
)

## constructor ----

#' @rdname StartingDose-class
#' @param starting_dose (`positive_number`)\cr see slot definition.
StartingDose <- function(starting_dose) {
  new(
    "StartingDose",
    starting_dose = starting_dose
  )
}

#' @rdname StartingDose-class
#' @note Typically, end users will not use the `.DefaultStartingDose()` function.
.DefaultStartingDose <- function() {
  StartingDose(starting_dose = 5)
}

# Helper functions ----

h_knit_print_design <- function(
  x,
  ...,
  level = 2L,
  title = "Design",
  default_sections = NA,
  user_sections = NA,
  ignore_slots = c(),
  asis = TRUE
) {
  assert_flag(asis)
  # Because subsections use level + 1 and 6 is the lowest markdown header level
  assert_integer(level, lower = 1L, upper = 5L, any.missing = FALSE, len = 1L)
  assert_character(title, any.missing = FALSE, len = 1L)

  slots_to_process <- setdiff(slotNames(x), ignore_slots)

  args <- list(...)
  units <- ifelse("units" %in% names(args), paste0(" ", args[["units"]]), "")
  section_labels <- h_prepare_section_labels(
    x,
    default_sections,
    user_sections
  )
  assert_subset(slots_to_process, names(section_labels))

  rv <- paste0(
    h_markdown_header(title, level = level),
    paste0(
      lapply(
        slots_to_process,
        function(nm) {
          tmp <- switch(nm,
            starting_dose = knit_print(
              StartingDose(x@starting_dose),
              asis = FALSE,
              level = level + 1L,
              ...
            ),
            startingDose = knit_print(
              StartingDose(x@startingDose),
              asis = FALSE,
              level = level + 1L,
              ...
            ),
            pl_cohort_size = ifelse(
              identical(slot(x, "pl_cohort_size"), CohortSizeConst(0)),
              "Placebo will not be administered in the trial.\n\n",
              knit_print(
                slot(x, "pl_cohort_size"),
                asis = FALSE,
                level = level + 1L,
                ...
              )
            ), {
              knit_print(slot(x, nm), asis = FALSE, level = level + 1L, ...)
            }
          )
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
  assert_integer(level, lower = 1L, upper = 6L, any.missing = FALSE, len = 1L)

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
#' @rdname knit_print
#' @export
#' @method knit_print StartingDose
knit_print.StartingDose <- function(x, ..., asis = TRUE) {
  assert_flag(asis)

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
    asis = TRUE) {
  h_knit_print_design(
    x,
    ...,
    level = 2L,
    title = "Design",
    default_sections = c(
      "nextBest"     = "Dose recommendation",
      "cohort_size"  = "Cohort size",
      "data"         = "Observed data",
      "startingDose" = "Starting dose"
    ),
    user_sections = sections,
    asis = asis
  )
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
  h_knit_print_design(
    x,
    ...,
    level = 2L,
    title = "Design",
    default_sections = c(
      "nextBest" = "Dose recommendation",
      "cohort_size" = "Cohort size",
      "data" = "Observed data",
      "startingDose" = "Starting dose",
      "increments" = "Escalation rule",
      "stopping" = "Stopping rule",
      "model" = "Dose toxicity model",
      "pl_cohort_size" = "Use of placebo"
    ),
    user_sections = sections,
    asis = asis
  )
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
    asis = TRUE) {
  assert_flag(asis)
  assert_character(title, len = 1, any.missing = FALSE)
  assert_integer(level, len = 1, lower = 1, upper = 6)

  args <- list(...)
  bLabel <- ifelse(
    "biomarker_label" %in% names(args),
    args[["biomarker_label"]],
    "biomarker"
  )
  tLabel <- ifelse(
    "tox_label" %in% names(args),
    args[["tox_label"]],
    "toxicity"
  )

  if (is.na(sections)) {
    sections <- c("model" = paste0("Dose-", tLabel, " and dose-", bLabel, " models"))
  } else {
    if (!("model" %in% names(sections))) {
      sections["model"] <- paste0("Dose-", tLabel, " and dose-", bLabel, " models")
    }
  }

  knit_print.Design(
    x,
    level = level,
    title = title,
    sections = sections,
    asis = asis,
    ...
  )
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
    asis = TRUE) {
  h_knit_print_design(
    x,
    ...,
    level = 2L,
    title = "Design",
    default_sections = c(
      "nextBest" = "Dose recommendation",
      "cohort_size" = "Cohort size",
      "data" = "Observed data",
      "startingDose" = "Starting dose",
      "increments" = "Escalation rule",
      "stopping" = "Stopping rule",
      "model" = "Dose toxicity model",
      "pl_cohort_size" = "Use of placebo",
      "safetyWindow" = "Safety window"
    ),
    user_sections = sections,
    asis = asis
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
    asis = TRUE) {
  knit_print.Design(
    x,
    level = level,
    title = title,
    sections = sections,
    asis = asis,
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
    asis = TRUE) {
  knit_print.Design(
    x,
    level = level,
    title = title,
    sections = sections,
    asis = asis,
    ...
  )
}

# DesignOrdinal ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.RuleDesign
#' @rdname knit_print
#' @export
#' @method knit_print DesignOrdinal
knit_print.DesignOrdinal <- function(
    x,
    ...,
    level = 2L,
    title = "Design",
    sections = NA,
    asis = TRUE) {
  h_knit_print_design(
    x,
    ...,
    level = 2L,
    title = "Design",
    default_sections = c(
      "next_best" = "Dose recommendation",
      "cohort_size" = "Cohort size",
      "data" = "Observed data",
      "starting_dose" = "Starting dose",
      "increments" = "Escalation rule",
      "stopping" = "Stopping rule",
      "model" = "Dose toxicity model",
      "pl_cohort_size" = "Use of placebo"
    ),
    user_sections = sections,
    asis = asis
  )
}

# DesignGrouped ----

# Needs special handling because of the empty models and nested rules in the
# mono and combo slots and because of the many slots that are of built-in types
# rather than being of crmPack-specific types

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.RuleDesign
#' @rdname knit_print
#' @export
#' @method knit_print DesignGrouped
knit_print.DesignGrouped <- function(
    x,
    ...,
    level = 2L,
    title = "Design",
    sections = c(
      "model" = "Dose toxicity model",
      "mono" = "Monotherapy rules",
      "combo" = "Combination therapy rules",
      "other" = "Other details"
    ),
    asis = TRUE) {
  assert_flag(asis)
  assert_character(title, len = 1, any.missing = FALSE)
  assert_integer(level, len = 1, lower = 1, upper = 6)

  rv <- paste0(
    h_markdown_header(sections["model"], level = level),
    knit_print(x@model, asis = FALSE, ...),
    h_markdown_header(sections["mono"], level = level),
    h_knit_print_design(
      x@mono,
      asis = FALSE,
      level = level + 1L,
      ignore_slots = c("model"),
      default_sections = c(
        "nextBest" = "Dose recommendation",
        "cohort_size" = "Cohort size",
        "data" = "Observed monotherapy data",
        "startingDose" = "Starting dose",
        "increments" = "Escalation rule",
        "stopping" = "Stopping rule",
        "pl_cohort_size" = "Use of placebo"
      ),
      sections = sections[["mono"]],
      ...
    ),
    h_markdown_header(sections["combo"], level = level),
    h_knit_print_design(
      x@combo,
      asis = FALSE,
      level = level + 1L,
      ignore_slots = "model",
      default_sections = c(
        "nextBest"       = "Dose recommendation",
        "cohort_size"    = "Cohort size",
        "data"           = "Observed combination therapy data",
        "startingDose"   = "Starting dose",
        "increments"     = "Escalation rule",
        "stopping"       = "Stopping rule",
        "pl_cohort_size" = "Use of placebo"
      ),
      sections = sections[["combo"]],
      ...
    ),
    h_markdown_header(sections["other"], level = level),
    ifelse(
      x@first_cohort_mono_only,
      paste0(
        "No combination dosing may occur until the results of at least one ",
        "monotherapy cohort are available.\n\n"
      ),
      "Simultaneous combination and monotherapy dosing is permitted from the outset.\n\n"
    ),
    ifelse(
      x@same_dose_for_start,
      paste0(
        "When monotherapy and combination therapy are used in the same cohort ",
        "for the first time, the same dose must be used for both regimens.\n\n"
      ),
      paste0(
        "When monotherapy and combination therapy are used in the same cohort ",
        "for the first time, the use of a different dose in each regimen is permitted.\n\n"
      )
    ),
    ifelse(
      x@same_dose_for_all,
      paste0(
        "Whenever monotherapy and combination therapy are used in the same cohort, ",
        "the same dose must be used for both regimens.\n\n"
      ),
      paste0(
        "Whenever monotherapy and combination therapy are used in the same cohort, ",
        "the use of a different dose in each regimen is permitted.\n\n"
      )
    )
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# TDsamplesDesign ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.RuleDesign
#' @rdname knit_print
#' @export
#' @method knit_print TDsamplesDesign
knit_print.TDsamplesDesign <- function(
    x,
    ...,
    level = 2L,
    title = "Design",
    sections = NA,
    asis = TRUE) {
  h_knit_print_design(
    x,
    ...,
    level = level,
    title = title,
    default_sections = c(
      "nextBest"       = "Dose recommendation",
      "cohort_size"    = "Cohort size",
      "data"           = "Observed data",
      "startingDose"   = "Starting dose",
      "model"          = "Dose toxicity model",
      "stopping"       = "Stopping rule",
      "increments"     = "Escalation rule",
      "pl_cohort_size" = "Use of placebo"
    ),
    user_sections = sections,
    asis = asis
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
    asis = TRUE) {
  h_knit_print_design(
    x,
    ...,
    level = level,
    title = title,
    default_sections = c(
      "nextBest"       = "Dose recommendation",
      "cohort_size"    = "Cohort size",
      "data"           = "Observed data",
      "startingDose"   = "Starting dose",
      "increments"     = "Escalation rule",
      "stopping"       = "Stopping rule",
      "model"          = "Dose-toxicity model",
      "eff_model"      = "Dose-efficacy model",
      "pl_cohort_size" = "Use of placebo"
    ),
    ignore_sections = c("model", "eff_model"),
    sections = sections,
    asis = asis
  )
}

# DualResponsesSamplesDesign ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.RuleDesign
#' @rdname knit_print
#' @export
#' @method knit_print DualResponsesSamplesDesign
knit_print.DualResponsesSamplesDesign <- function(
    x,
    ...,
    level = 2L,
    title = "Design",
    sections = NA,
    asis = TRUE) {
  h_knit_print_design(
    x,
    ...,
    level = level,
    title = title,
    default_sections = c(
      "nextBest"       = "Dose recommendation",
      "cohort_size"    = "Cohort size",
      "data"           = "Observed data",
      "startingDose"   = "Starting dose",
      "increments"     = "Escalation rule",
      "stopping"       = "Stopping rule",
      "model"          = "Dose-toxicity model",
      "eff_model"      = "Dose-efficacy model",
      "pl_cohort_size" = "Use of placebo"
    ),
    ignore_sections = c("model", "eff_model"),
    sections = sections,
    asis = asis
  )
}

# RuleDesignOrdinal ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.RuleDesign
#' @rdname knit_print
#' @export
#' @method knit_print RuleDesignOrdinal
knit_print.RuleDesignOrdinal <- function(
    x,
    ...,
    level = 2L,
    title = "Design",
    sections = NA,
    asis = TRUE) {
  h_knit_print_design(
    x,
    ...,
    level = 2L,
    title = "Design",
    default_sections = c(
      "next_best" = "Dose recommendation",
      "cohort_size" = "Cohort size",
      "data" = "Observed data",
      "starting_dose" = "Starting dose"
    ),
    user_sections = sections,
    asis = asis
  )
}
