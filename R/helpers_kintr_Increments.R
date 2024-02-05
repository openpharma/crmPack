#' @importFrom knitr knit_print
NULL


# Increments ----

#' Render a IncrementsRelative Object
#'
#' @description `r lifecycle::badge("experimental")`
#' @inherit knit_print.CohortSizeConst return
#' @param ... passed to `knitr::kable`
#' @section Usage Notes:
#' The default value of `col.names` is `c("Min", "Max", "Increment")` and that
#' of `caption` is `"Defined by highest dose administered so far"`.  These
#' values can be overridden by passing `col.names` and `caption` in the function
#' call.
#' @export
#' @rdname knit_print
knit_print.IncrementsRelative <- function(x, ..., asis = TRUE) {
  assert_flag(asis)

  param <- list(...)
  if (!("col.names" %in% names(param))) {
    param[["col.names"]] <- c("Min", "Max", "Increment")
  }
  if (!("caption" %in% names(param))) {
    param[["caption"]] <- "Defined by highest dose administered so far"
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

#' Render an IncrementsRelativeDLT object
#'
#' @description `r lifecycle::badge("experimental")`
#' @inherit knit_print.CohortSizeConst return
#' @param ... passed to `knitr::kable`
#' @section Usage Notes:
#' The default value of `col.names` is `c("Min", "Max", "Increment")` and that
#' of `caption` is `"Defined by number of DLTs reported so far"`. These values
#' can be overridden by passing `col.names` and `caption` in the function call.
#' @export
#' @rdname knit_print
knit_print.IncrementsRelativeDLT <- function(x, ..., asis = TRUE) {
  assert_flag(asis)

  param <- list(...)
  if (!("col.names" %in% names(param))) {
    param[["col.names"]] <- c("Min", "Max", "Increment")
  }
  if (!("caption" %in% names(param))) {
    param[["caption"]] <- "Defined by number of DLTs reported so far"
  }
  x <- tidy(x)
  param[["x"]] <- x
  rv <- kableExtra::add_header_above(
    do.call(knitr::kable, param),
    c("No DLTs" = 2, " " = 1)
  )
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}







#' IncrementsDoseLevels
#' IncrementsHSRBeta
#' IncrementsMin
#' IncrementsOrdinal
#' IncrementsRelativeParts
#' IncrementsRelativeDLTCurrent
