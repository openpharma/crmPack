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

# RuleDesign
# DesignGrouped
# RuleDesignOrdinal
# Design
# TDsamplesDesign
# TDDesign
# DesignOrdinal
# DualDesign"
# DualResponsesSamplesDesign
# DualResponsesDesign
# DADesign


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

# RuleDesign ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.StoppingTargetProb
#' @param level (`positive_integer`) The level of the headings used to separate
#' slots.  Must be between 1 and 6.
#' @rdname knit_print
#' @export
#' @method knit_print RuleDesign
knit_print.RuleDesign <- function(
    x,
    ...,
    level = 2L,
    asis = TRUE) {
  assert_flag(asis)
  assert_integer(level, min = 1L, max = 6L, any.missing = FALSE, len = 1L)

  rv <- paste0(
    h_markdown_header("Design", level),
    "The elements of the design are as follows:\n",
    h_markdown_header("NextBest rule", level + 1L),
    knit_print(x@nextBest, asis = FALSE, ...),
    h_markdown_header("Cohort size", level + 1L),
    knit_print(x@cohort_size, asis = FALSE, ...),
    h_markdown_header("Observed data", level + 1L),
    knit_print(x@data, asis = FALSE, ...),
    h_markdown_header("Starting dose", level + 1L),
    "The starting dose is ",
    x@startingDose,
    "."
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}
