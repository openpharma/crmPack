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

# Helpers ---

#' Helper Function to Convert a `gap` Slot to Words
#'
#' @inheritParams knit_print.SafetyWindowConst
#' @param gap (`numeric`)\cr a vector of gaps
#' @return a Markdown representation of the `gap` parameter as a bullet list.
#' @noRd
#' @keywords internal
h_describe_safety_gap <- function(gap, ordinals, label, time_unit) {
  assert_character(ordinals, min.len = length(gap) - 1, any.missing = FALSE, unique = TRUE)

  if (length(gap) == 1) {
    paste0(
      "- The gap between consecutive enrolments should always be at least ",
      gap[1],
      " ",
      ifelse(gap[1] == 1, time_unit[1], time_unit[2]),
      ".\n\n"
    )
  } else {
    paste0(
      paste0(
        lapply(
          seq_along(1:(length(gap) - 1)),
          function(n) {
            paste0(
              "-  The gap between the enrolment of the ",
              ordinals[n],
              " and the ",
              ordinals[n + 1],
              " ",
              label[2],
              " in the cohort should be at least ",
              gap[n],
              " ",
              ifelse(gap[n] == 1, time_unit[1], time_unit[2])
            )
          }
        ),
        collapse = "\n\n"
      ),
      "\n",
      paste0(
        "- The gap between all subsequent ",
        label[2],
        " should be at least ",
        gap[length(gap)],
        " ",
        ifelse(gap[length(gap)] == 1, time_unit[1], time_unit[2]),
        "\n"
      ),
      sep = "\n"
    )
  }
}
# Methods ----

# SafetyWindow ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.StoppingTargetProb
#' @inheritParams knit_print.CohortSizeConst
#' @section Usage Notes:
#' `label` should be a character vector of length 1 or 2.  If of length 2, the first
#' element describes a count of 1 and the second describes all other counts.
#' If of length 1, the character `s` is appended to the value when the count is not 1.
#' @rdname knit_print
#' @export
#' @method knit_print SafetyWindow
knit_print.SafetyWindow <- function(
    x,
    ...,
    asis = TRUE,
    time_unit = "day",
    label = "participant") {
  assert_character(label, min.len = 1, max.len = 2, any.missing = FALSE)
  assert_character(time_unit, min.len = 1, max.len = 2, any.missing = FALSE)
  assert_flag(asis)

  if (length(label) == 1) {
    label[2] <- paste0(label[1], "s")
  }
  if (length(time_unit) == 1) {
    time_unit[2] <- paste0(time_unit[1], "s")
  }

  rv <- paste0(
    "To protect the welfare of individual ",
    label[2],
    ", the rate of enrolment within each cohort will be restricted.\n\n"
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# SafetyWindowConst ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.StoppingTargetProb
#' @inheritParams knit_print.CohortSizeConst
#' @param time_unit (`character`)\cr the word used to describe units of time.
#' See Usage Notes below.
#' @param ordinals (`character`)\cr a character vector whose nth defines the
#' word used as the written representation of the nth ordinal number.
#' @section Usage Notes:
#' `label` and `time_unit` are, collectively, labels.
#'
#' A label should be a character vector of length 1 or 2.  If of length 2, the first
#' element describes a count of 1 and the second describes all other counts.
#' If of length 1, the character `s` is appended to the value when the count is not 1.
#' @rdname knit_print
#' @export
#' @method knit_print SafetyWindowConst
knit_print.SafetyWindowConst <- function(
    x,
    ...,
    asis = TRUE,
    label = "participant",
    ordinals = c(
      "first", "second", "third", "fourth", "fifth", "sixth",
      "seventh", "eighth", "ninth", "tenth"
    ),
    time_unit = "day") {
  assert_character(label, min.len = 1, max.len = 2, any.missing = FALSE)
  assert_character(time_unit, min.len = 1, max.len = 2, any.missing = FALSE)
  assert_character(ordinals, min.len = length(x@gap) - 1, any.missing = FALSE, unique = TRUE)
  assert_flag(asis)

  if (length(label) == 1) {
    label[2] <- paste0(label[1], "s")
  }
  if (length(time_unit) == 1) {
    time_unit[2] <- paste0(time_unit[1], "s")
  }

  rv <- paste0(
    knit_print.SafetyWindow(x, asis = FALSE, label = label, ...),
    "For all cohorts:\n\n",
    h_describe_safety_gap(x@gap, ordinals, label, time_unit),
    "Before the next cohort can open, all ",
    label[2],
    " in the current cohort must have been followed up for at least ",
    x@follow,
    " ",
    ifelse(x@follow == 1, time_unit[1], time_unit[2]),
    " and at least one ",
    label[1],
    " must have been followed up for at least ",
    x@follow_min,
    " ",
    ifelse(x@follow_min == 1, time_unit[1], time_unit[2]),
    ".\n\n"
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# SafetyWindowSize ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.SafetyWindowConst
#' @inherit SafetyWindowConst sections
#' @param level (`count`)\cr the markdown level at which the headings for cohort size
#' will be printed.  An integer between 1 and 6
#' @rdname knit_print
#' @export
#' @method knit_print SafetyWindowSize
knit_print.SafetyWindowSize <- function(
    x,
    ...,
    asis = TRUE,
    # We could use package english here and avoid the need for `ordinals`, but
    # is an extra dependency for very limited benefit
    ordinals = c(
      "first", "second", "third", "fourth", "fifth", "sixth", "seventh",
      "eighth", "ninth", "tenth"
    ),
    label = "participant",
    time_unit = "day",
    level = 2L) {
  assert_character(label, min.len = 1, max.len = 2, any.missing = FALSE)
  assert_character(time_unit, min.len = 1, max.len = 2, any.missing = FALSE)
  assert_flag(asis)
  assert_integer(level, lower = 1, upper = 6, any.missing = FALSE)

  if (length(label) == 1) {
    label[2] <- paste0(label[1], "s")
  }
  if (length(time_unit) == 1) {
    time_unit[2] <- paste0(time_unit[1], "s")
  }

  rv <- paste0(
    knit_print.SafetyWindow(x, asis = FALSE, label = label, ...),
    paste0(
      lapply(
        seq_along(x@size),
        function(i) {
          paste0(
            dplyr::case_when(
              i == 1 ~ paste0(
                stringr::str_dup("#", level),
                " For cohort sizes of less than ",
                x@size[2]
              ),
              i == length(x@size) ~ paste0(
                stringr::str_dup("#", level),
                " For cohort sizes of ",
                x@size[i],
                " or more"
              ),
              TRUE ~ paste0(
                stringr::str_dup("#", level),
                " For cohort sizes greater than or equal to ",
                x@size[i],
                " and strictly less than ",
                x@size[i + 1]
              )
            ),
            "\n\n",
            h_describe_safety_gap(x@gap[[i]], ordinals, label, time_unit)
          )
        }
      ),
      collapse = "\n"
    )
  )

  rv <- paste0(
    rv,
    "For all cohorts, before the next cohort can open, all ",
    label[2],
    " in the current cohort must have been followed up for at least ",
    x@follow,
    " ",
    ifelse(x@follow == 1, time_unit[1], time_unit[2]),
    " and at least one ",
    label[1],
    " must have been followed up for at least ",
    x@follow_min,
    " ",
    ifelse(x@follow_min == 1, time_unit[1], time_unit[2]),
    ".\n\n"
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}
