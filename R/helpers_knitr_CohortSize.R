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

# [1]  "CohortSizeParts"   "CohortSizeMax"     "CohortSizeMin"
# [7] "CohortSizeOrdinal"

#' Render a CohortSizeConst Object
#'
#' @description `r lifecycle::badge("experimental")`
#' @param obj (`numeric`)\cr The CohortSizeDLT object.
#' @param asis (`flag`)\cr Should the return value be wrapped in a call to `asis_output`? (Not used at present)
#' @param label (`character`)\cr The word used to label the participants.  See Usage Notes below.
#' @param ... (`numeric`)\cr Not used at present
#'
#' @section Usage Notes:
#' The default value of `label` is `NULL` and results in a label of `"participant"` if the cohort size is `1` and
#' `"participants"` otherwise.  In other cases, the end user is responsible for ensuring consistency of number
#' between the cohort size and its label
#' @return The markdown representation of the object, as a character string
#' @seealso [`knit_print`] for more details.
#'
#' @export
#' @rdname knit_print
knit_print.CohortSizeConst <- function(obj, asis = TRUE, label = c("participant", "participants"), ...) {
  assert_character(label, min.len = 1, max.len = 2, any.missing = FALSE)
  assert_flag(asis)

  paste0("A constant size of ", obj@size, " ", label)
}

#' Render a CohortSizeRange Object
#'
#' @description `r lifecycle::badge("experimental")`
#' @param obj (`numeric`)\cr The CohortSizeDLT object.
#' @param asis (`flag`)\cr Should the return value be wrapped in a call to `asis_output`?
#' @param ... (`numeric`)\cr Passed to `knitr::kable()`
#'
#' @section Usage Notes:
#' The by default, the columns are labelled `Lower`, `Upper` and `Cohort size`.  The table's caption is
#' `Defined by the dose to be used in the next cohort`.  These values can be overridden by passing
#' `col.names` and `caption` in the function call.
#' @return The markdown representation of the object, as a character string
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
#' @param obj (`numeric`)\cr The CohortSizeDLT object.
#' @param asis (`flag`)\cr Should the return value be wrapped in a call to `asis_output`?
#' @param ... (`numeric`)\cr Passed to `knitr::kable`.
#'
#' @section Usage Notes:
#' The by default, the columns are labelled `Lower`, `Upper` and `Cohort size`.  The table's caption is
#' `Defined by the number of DLTs so far observed`.  These values can be overridden by passing
#' `col.names` and `caption` in the function call.
#'
#' @return The markdown representation of the object, as a character string
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
#' @param obj (`numeric`)\cr The CohortSizeParts object.
#' @param asis (`flag`)\cr Should the return value be wrapped in a call to `asis_output`?
#' @param ... (`numeric`)\cr Passed to `knitr::kable`.
#'
#' @return The markdown representation of the object, as a character string
#'
#' @export
#' @rdname knit_print
knit_print.CohortSizeParts <- function(object, asis = TRUE, ...) {
  assert_flag(asis)

  rv <- paste0("A constant size of ", obj@size, " ", label)
  if (asis) {
    rv <- asis_output(rv)
  }
  rv
}
