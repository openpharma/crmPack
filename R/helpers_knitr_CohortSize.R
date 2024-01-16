# Integration with knitr ----

#' Implement S3 Method `knit_print` for all `crmPack` Classes
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' We provide additional utility functions to allow human-friendly rendition of
#' crmPack objects in Markdown and Quarto files
#'
#' @return a character string that represents the object in markdown.
#' @name knit_print
NULL

# CohortSize ----

# [1]  "CohortSizeMax"     "CohortSizeMin"
# [7] "CohortSizeOrdinal"

#' Render a CohortSizeConst Object
#'
#' @description `r lifecycle::badge("experimental")`
#' @param obj (`CohortSize`)\cr The object to knit_print.
#' @param asis (`flag`)\cr Should the return value be wrapped in a call to `asis_output`?
#' @param label (`character`)\cr The word used to label the participants.  See Usage Notes below.
#' @param ... Not used at present
#'
#' @section Usage Notes:
#' `label` describes the trial's participants.
#'
#' It should be a character vector of length 1 or 2.  If of length 2, the first
#' element describes a `cohort_size` of 1 and the second describes all other
#' `cohort_size`s.  If of length 1, the character `s` is appended to the value
#' when `cohort_size` is not 1.
#' @return The markdown representation of the object, as a character string
#' @seealso [`knit_print`] for more details.
#'
#' @export
#' @rdname knit_print
knit_print.CohortSizeConst <- function(obj, asis = TRUE, label = c("participant", "participants"), ...) {
  assert_character(label, min.len = 1, max.len = 2, any.missing = FALSE)
  assert_flag(asis)

  if (length(label) == 1) {
    label[2] <- paste0(label[1], "s")
  }
  paste0("A constant size of ", obj@size, " ", label[obj@size == 1 ? 1 : 2])
}

#' Render a CohortSizeRange Object
#'
#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.CohortSizeConst
#' @inheritDotParam knit_print.CohortSizeConst
#' @inherit knit_print.CohortSizeConst return
#'
#' @export
#' @rdname knit_print
knit_print.CohortSizeRange <- function(object, asis = TRUE, ...) {
  assert_flag(asis)

  dot_params <- list(...)
  if (!("col.names" %in% names(dot.params))) {
    dot_params[["col.names"]] <- c("Lower", "Upper", "Cohort size")
  }
  if (!("caption" %in% names(dot.params))) {
    dot_params[["caption"]] <- "Defined by the dose to be used in the next cohort"
  }
  rv <-  object %>%
    tidy() %>%
    do.call(knitr::kable, dot_params) %>%
    kableExtra::add_header_above(c("Dose" = 2, " " = 1))
  if (asis) {
    rv <- asis_output(rv)
  }
  rv
}

#' Render a CohortSizeDLT Object
#'
#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.CohortSizeConst
#' @inheritDotParam knit_print.CohortSizeConst
#' @inherit knit_print.CohortSizeConst return
#' @param ... (`numeric`)\cr Passed to `knitr::kable`.
#'
#' @section Usage Notes:
#' The by default, the columns are labelled `Lower`, `Upper` and `Cohort size`.  The table's caption is
#' `Defined by the number of DLTs so far observed`.  These values can be overridden by passing
#' `col.names` and `caption` in the function call.
#'
#' @export
#' @rdname knit_print
knit_print.CohortSizeDLT <- function(object, asis = TRUE, ...) {
  assert_flag(asis)

  dot_params <- list(...)
  if (!("col.names" %in% names(dot.params))) {
    dot_params[["col.names"]] <- c("Lower", "Upper", "Cohort size")
  }
  if (!("caption" %in% names(dot.params))) {
    dot_params[["caption"]] <- "Defined by the number of DLTs so far observed"
  }
  rv <- object %>%
    tidy() %>%
    do.call(knitr::kable, dot_params) %>%
    kableExtra::add_header_above(c("No of DLTs" = 2, " " = 1))
  if (asis) {
    rv <- asis_output(rv)
  }
  rv
}

#' Render a CohortSizeParts Object
#'
#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.CohortSizeConst
#' @inherit knit_print.CohortSizeConst return
#' @inheritSection knit_print.CohortSizeConst {Usage Notes}
#' @inheritDotParams knit_print.CohortSizeConst
#'
#' @export
#' @rdname knit_print
knit_print.CohortSizeParts <- function(obj, label = "participant", asis = TRUE, ...) {
  assert_character(label, min.len = 1, max.len = 2, any.missing = FALSE)
  assert_flag(asis)
  if (length(label) == 1) {
    label[2] <- paste0(label[1], "s")
  }
  rv <- paste0(
    "A size of ",
    obj@cohort_sizes[1],
    " ",
    label[ifelse(obj@cohort_sizes[1] == 1, 1, 2)],
    " in the first part and ",
    obj@cohort_sizes[2],
    " ",
    label[ifelse(obj@cohort_sizes[2] == 1, 1, 2)],
    " in the second."
  )
  if (asis) {
    rv <- asis_output(rv)
  }
  rv
}
