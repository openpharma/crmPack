#' @importFrom knitr knit_print
NULL

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

# CohortSize ----

#' Render a CohortSizeConst Object
#'
#' @description `r lifecycle::badge("experimental")`
#' @param x (`CohortSize`)\cr The object to knit_print.
#' @param asis (`flag`)\cr Should the return value be wrapped in a call to `knitr::asis_output`?
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
knit_print.CohortSizeConst <- function(x, ..., asis = TRUE, label = c("participant", "participants")) {
  assert_character(label, min.len = 1, max.len = 2, any.missing = FALSE)
  assert_flag(asis)

  if (length(label) == 1) {
    label[2] <- paste0(label[1], "s")
  }
  rv <- paste0("A constant size of ", x@size, " ", label[ifelse(x@size == 1, 1, 2)], ".")
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

#' Render a CohortSizeRange Object
#'
#' @description `r lifecycle::badge("experimental")`
#' @inherit knit_print.CohortSizeConst return
#'
#' @export
#' @rdname knit_print
knit_print.CohortSizeRange <- function(x, ..., asis = TRUE) {
  assert_flag(asis)

  param <- list(...)
  if (!("col.names" %in% names(param))) {
    param[["col.names"]] <- c("Lower", "Upper", "Cohort size")
  }
  if (!("caption" %in% names(param))) {
    param[["caption"]] <- "Defined by the dose to be used in the next cohort"
  }
  x <- tidy(x)
  param[["x"]] <- x
  rv <- kableExtra::add_header_above(
    do.call(knitr::kable, param),
    c("Dose" = 2, " " = 1)
  )
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

#' Render a CohortSizeDLT Object
#'
#' @description `r lifecycle::badge("experimental")`
#' @inherit knit_print.CohortSizeConst return
#' @param ... Passed to `knitr::kable`.
#'
#' @section Usage Notes:
#' The by default, the columns are labelled `Lower`, `Upper` and `Cohort size`.  The table's caption is
#' `Defined by the number of DLTs so far observed`.  These values can be overridden by passing
#' `col.names` and `caption` in the function call.
#'
#' @export
#' @rdname knit_print
knit_print.CohortSizeDLT <- function(x, ..., asis = TRUE) {
  assert_flag(asis)

  param <- list(...)
  if (!("col.names" %in% names(param))) {
    param[["col.names"]] <- c("Lower", "Upper", "Cohort size")
  }
  if (!("caption" %in% names(param))) {
    param[["caption"]] <- "Defined by the number of DLTs so far observed"
  }
  param[["x"]] <- tidy(x)
  rv <- kableExtra::add_header_above(
    do.call(knitr::kable, param),
    c("No of DLTs" = 2, " " = 1)
  )
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

#' Render a CohortSizeParts Object
#'
#' @description `r lifecycle::badge("experimental")`
#' @inherit knit_print.CohortSizeConst return
#' @inheritSection knit_print.CohortSizeConst Usage Notes
#'
#' @export
#' @rdname knit_print
knit_print.CohortSizeParts <- function(x, ..., asis = TRUE, label = c("participant", "participants")) {
  assert_character(label, min.len = 1, max.len = 2, any.missing = FALSE)
  assert_flag(asis)

  if (length(label) == 1) {
    label[2] <- paste0(label[1], "s")
  }
  rv <- paste0(
    "A size of ",
    x@cohort_sizes[1],
    " ",
    label[ifelse(x@cohort_sizes[1] == 1, 1, 2)],
    " in the first part and ",
    x@cohort_sizes[2],
    " ",
    label[ifelse(x@cohort_sizes[2] == 1, 1, 2)],
    " in the second."
  )
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

#' Render a CohortSizeMax Object
#'
#' @description `r lifecycle::badge("experimental")`
#' @inherit knit_print.CohortSizeConst return
#' @param ... passed through to the `knit_print` methods of the constituent
#' rules
#'
#' @export
#' @rdname knit_print
knit_print.CohortSizeMax <- function(x, ..., asis = TRUE) {
  assert_flag(asis)
  rv <- paste0(
    "The maximum of the cohort sizes defined in the following rules:",
    paste0(
      lapply(
        x@cohort_sizes,
        function(x, ...) {
          knit_print(x, asis = asis, ...)
        }
      ),
      collapse = "\n"
    ),
    paste = "\n"
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

#' Render a CohortSizeMin Object
#'
#' @description `r lifecycle::badge("experimental")`
#' @inherit knit_print.CohortSizeConst return
#' @param ... passed through to the `knit_print` methods of the constituent
#' rules
#'
#' @export
#' @rdname knit_print
knit_print.CohortSizeMin <- function(x, ..., asis = TRUE) {
  assert_flag(asis)
  rv <- paste0(
    "The minimum of the cohort sizes defined in the following rules:",
    paste0(
      lapply(
        x@cohort_sizes,
        function(x, ...) {
          knit_print(x, asis = asis, ...)
        }
      ),
      collapse = "\n"
    ),
    paste = "\n"
  )
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

#' Render a CohortSizeOrdinal Object
#'
#' @description `r lifecycle::badge("experimental")`
#' @inherit knit_print.CohortSizeConst return
#' @param ... passed through to the `knit_print` method of the standard rule
#'
#' @export
#' @rdname knit_print
knit_print.CohortSizeOrdinal <- function(x, ..., asis = TRUE) {
  assert_flag(asis)

  rv <- paste0(
    "Based on a toxicity grade of ",
    x@grade,
    ": ",
    paste0(knit_print(x@rule, asis = asis, ...), collapse = "\n"),
    paste = "\n"
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}
