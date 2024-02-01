#' @importFrom knitr knit_print
NULL

# Integration with knitr ----
#'
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
  rv <- paste0("A constant size of ", obj@size, " ", label[ifelse(obj@size == 1, 1, 2)])
  if (asis) {
    rv <- asis_output(rv)
  }
  rv
}
registerS3method("knit_print", "CohortSizeConst", knit_print.CohortSizeConst)

#' Render a CohortSizeRange Object
#'
#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.CohortSizeConst
#' @inheritDotParams knit_print.CohortSizeConst
#' @inherit knit_print.CohortSizeConst return
#'
#' @export
#' @rdname knit_print
knit_print.CohortSizeRange <- function(object, asis = TRUE, ...) {
  assert_flag(asis)

  param <- list(...)
  if (!("col.names" %in% names(param))) {
    param[["col.names"]] <- c("Lower", "Upper", "Cohort size")
  }
  if (!("caption" %in% names(param))) {
    param[["caption"]] <- "Defined by the dose to be used in the next cohort"
  }
  x <- object %>% tidy()
  param[["x"]] <- x
  rv <- do.call(knitr::kable, param) %>%
    kableExtra::add_header_above(c("Dose" = 2, " " = 1))
  if (asis) {
    rv <- asis_output(rv)
  }
  rv
}
registerS3method("knit_print", "CohortSizeRange", knit_print.CohortSizeRange)

#' Render a CohortSizeDLT Object
#'
#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.CohortSizeConst
#' @inheritDotParams knit_print.CohortSizeConst
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
knit_print.CohortSizeDLT <- function(obj, asis = TRUE, ...) {
  assert_flag(asis)

  param <- list(...)
  if (!("col.names" %in% names(param))) {
    param[["col.names"]] <- c("Lower", "Upper", "Cohort size")
  }
  if (!("caption" %in% names(param))) {
    param[["caption"]] <- "Defined by the number of DLTs so far observed"
  }
  param[["x"]] <- obj %>% tidy()
  rv <- do.call(knitr::kable, param) %>%
    kableExtra::add_header_above(c("No of DLTs" = 2, " " = 1))
  if (asis) {
    rv <- asis_output(rv)
  }
  rv
}
registerS3method("knit_print", "CohortSizeDLT", knit_print.CohortSizeDLT)

#' Render a CohortSizeParts Object
#'
#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.CohortSizeConst
#' @inherit knit_print.CohortSizeConst return
#' @inheritSection knit_print.CohortSizeConst Usage Notes
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
registerS3method("knit_print", "CohortSizeParts", knit_print.CohortSizeParts)

#' Render a CohortSizeMax Object
#'
#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.CohortSizeConst
#' @inherit knit_print.CohortSizeConst return
#' @param ... passed through to the `knit_print` methods of the constituent
#' rules
#'
#' @export
#' @rdname knit_print
knit_print.CohortSizeMax <- function(obj, asis = TRUE, ...) {
  assert_flag(asis)
  rv <- paste0(
    "The maximum of the cohort sizes defined in the following rules:",
    paste0(
      lapply(
        obj@cohort_sizes,
        function(x, ...) {
          knit_print(x, asis = asis, ...)
        }
      ),
      collapse = "\n"
    ),
    paste = "\n"
  )

  if (asis) {
    rv <- asis_output(rv)
  }
  rv
}
registerS3method("knit_print", "CohortSizeMax", knit_print.CohortSizeMax)

#' Render a CohortSizeMin Object
#'
#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.CohortSizeConst
#' @inherit knit_print.CohortSizeConst return
#' @param ... passed through to the `knit_print` methods of the constituent
#' rules
#'
#' @export
#' @rdname knit_print
knit_print.CohortSizeMin <- function(obj, asis = TRUE, ...) {
  assert_flag(asis)
  rv <- paste0(
    "The minimum of the cohort sizes defined in the following rules:",
    paste0(
      lapply(
        obj@cohort_sizes,
        function(x, ...) {
          knit_print(x, asis = asis, ...)
        }
      ),
      collapse = "\n"
    ),
    paste = "\n"
  )
  if (asis) {
    rv <- asis_output(rv)
  }
  rv
}
registerS3method("knit_print", "CohortSizeMin", knit_print.CohortSizeMin)

#' Render a CohortSizeOrdinal Object
#'
#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.CohortSizeConst
#' @inherit knit_print.CohortSizeConst return
#' @param ... passed through to the `knit_print` method of the standard rule
#'
#' @export
#' @rdname knit_print
knit_print.CohortSizeOrdinal <- function(obj, asis = TRUE, ...) {
  asis_output(
    paste0(
      "Based on a toxicity garde of ",
      obj@grade,
      ": ",
      paste0(knit_print(obj@rule, asis = asis, ...), collapse = "\n"),
      paste = "\n"
    )
  )
}
registerS3method("knit_print", "CohortSizeOrdinal", knit_print.CohortSizeOrdinal)
