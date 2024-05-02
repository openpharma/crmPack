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
    label = "participant"
) {
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
#' @param time_unit (`character`)\cr The word used to describe units of time.
#' See Usage Notes below.
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
    time_unit = "day"
) {
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
    knit_print.SafetyWindow(x, asis = FALSE, label = label, ...),
    "The minimum gap between the enrolment of all ",
    label[2],
    " in the current cohort must be at least ",
    x@gap,
    ifelse(x@gap == 1, time_unit[1], time_unit[2]),
    ".  Before the next cohort can open, all ",
    label[2],
    " must have been followed up for at least ",
    x@follow,
    ifelse(x@follow == 1, time_unit[1], time_unit[2]),
    " and at least one ",
    label[1],
    " must have been followed up for at least ",
    x@follow_min,
    ifelse(x@follow_min == 1, time_unit[1], time_unit[2]),
    ".\n\n"
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# SafetyWindowSize ----

# #' @description `r lifecycle::badge("experimental")`
# #' @inheritParams knit_print.StoppingTargetProb
# #' @inheritParams knit_print.CohortSizeConst
# #' @param time_unit (`character`)\cr The word used to describe units of time.
# #' See Usage Notes below.
# #' @section Usage Notes:
# #' `label` and `time_unit` are collectively labels.
# #'
# #' A label should be a character vector of length 1 or 2.  If of length 2, the first
# #' element describes a count of 1 and the second describes all other counts.
# #' If of length 1, the character `s` is appended to the value when the count is not 1.
# #' @rdname knit_print
# #' @export
# #' @method knit_print SafetyWindowConst
# knit_print.SafetyWindowConst <- function(
#     x,
#     ...,
#     asis = TRUE,
#     # We could use package english here and avoid the need for `ordinals`, but
#     # is an extra dependency for very limited benefit
#     ordinals = c(
#       "first", "second", "third", "fourth", "fifth", "sixth", "seventh",
#       "eighth", "ninth", "tenth"
#     ),
#     label = "participant",
#     time_unit = "day"
# ) {
#   assert_character(label, min.len = 1, max.len = 2, any.missing = FALSE)
#   assert_character(time_unit, min.len = 1, max.len = 2, any.missing = FALSE)
#   assert_true(length(ordinals) >= length(x))
#   assert_flag(asis)
#
#   if (length(label) == 1) {
#     label[2] <- paste0(label[1], "s")
#   }
#   if (length(time_unit) == 1) {
#     time_unit[2] <- paste0(time_unit[1], "s")
#   }
#
#   rv <- paste0(
#     knit_print.Safetywindow(x, asis = FALSE, label = label, ...),
#   )
#
#   if (asis) {
#     rv <- knitr::asis_output(rv)
#   }
#   rv
#
# }
