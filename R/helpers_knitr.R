#' Set Default Values for kable Parameters
#'
#' Sets default values for `col.names` and `caption` if not already provided
#' by the user in the parameter list.
#'
#' @param param (`list`)\cr The list of `...` parameters passed to `knit_print`
#' @param col_names (`character`)\cr Default column names for the table
#' @param caption (`character`)\cr Default caption for the table
#' @return The updated parameter list with defaults applied
#' @keywords internal
h_kable_param_default <- function(param, col_names = NULL, caption = NULL) {
  assert_list(param)
  if (!is.null(col_names)) {
    assert_character(col_names, any.missing = FALSE)
    if (!("col.names" %in% names(param))) {
      param[["col.names"]] <- col_names
    }
  }
  if (!is.null(caption)) {
    assert_character(caption, len = 1, any.missing = FALSE)
    if (!("caption" %in% names(param))) {
      param[["caption"]] <- caption
    }
  }
  param
}

#' Check That Labels Are Valid and Useful
#'
#' A vector of labels is valid and useful if it is of length 2, of type character
#' and its values are distinct.
#'
#' If `x` is a scalar, a second element is added, whose value is the value of the
#' scalar with "s" appended.  If `x` is `"toxicity"`, the plural is handled appropriately.
#'
#' @param x (`character`)\cr The vector to be checked
#' @keywords internal
#' @return a character vector of length 2 whose values are distinct
h_prepare_labels <- function(x) {
  assert_character(
    x,
    min.len = 1,
    max.len = 2,
    any.missing = FALSE,
    unique = TRUE
  )

  if (length(x) == 1) {
    if (x == "toxicity") {
      x <- c("toxicity", "toxicities")
    } else {
      x[2] <- paste0(x[1], "s")
    }
  }
  x
}

#' Append Units to a Numeric Dose
#'
#' @param units (`character`)\cr the units to be displayed
#' @keywords internal
#' @return if `units` is `NA`, then `NA`.  Otherwise, `units`, ensuring that exactly
#' one space precedes the first non-whitespace character
h_prepare_units <- function(units = NA) {
  assert_character(units, len = 1)

  ifelse(
    is.na(units),
    "",
    paste0(" ", stringr::str_trim(units, "left"))
  )
}
