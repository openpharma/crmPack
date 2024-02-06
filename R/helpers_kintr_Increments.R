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
registerS3method("knit_print", "IncrementsRelative", knit_print.IncrementsRelative)

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
  d <- tibble::tibble(
    intervals = x@intervals,
  ) %>%
  h_range_to_minmax(.data$intervals) %>%
  dplyr::filter(max > 0) %>%
  tibble::add_column(increments = x@increments)

  param[["x"]] <- d
  rv <- kableExtra::add_header_above(
    do.call(knitr::kable, param),
    c("No DLTs" = 2, " " = 1)
  )
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}
registerS3method("knit_print", "IncrementsRelativeDLT", knit_print.IncrementsRelativeDLT)

#' Render an IncrementsDoseLevels object
#'
#' @description `r lifecycle::badge("experimental")`
#' @inherit knit_print.CohortSizeConst return
#' @export
#' @rdname knit_print
knit_print.IncrementsDoseLevels <- function(object, ..., asis = TRUE) {
  assert_flag(asis)

  rv <- paste0(
    "The maximum increment between cohorts is ",
    object@levels,
    ifelse(object@levels == 1, " level", " levels"),
    " relative to the ",
    ifelse(object@basis_level == "last", "dose used in the previous cohort.", "highest dose used so far."),
    "\n"
  )
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}
registerS3method("knit_print", "IncrementsDoseLevels", knit_print.IncrementsDoseLevels)

#' Render an IncrementsHSRBeta object
#'
#' @description `r lifecycle::badge("experimental")`
#' @inherit knit_print.CohortSizeConst return
#' @export
#' @rdname knit_print
knit_print.IncrementsHSRBeta <- function(x, ..., asis = TRUE) {
  assert_flag(asis)

  rv <- paste0(
    "The maximum increment is defined by a hard safety rule, independent of the CRM model, ",
    " and based on a beta(", x@a, ", ", x@b, ") ",
    "prior with a target toxicity rate of ",
    x@target,
    " and a probability threshold of ",
    x@prob,
    ".\n"
  )
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}
registerS3method("knit_print", "IncrementsHSRBeta", knit_print.IncrementsHSRBeta)

#' Render an IncrementsMin object
#'
#' @description `r lifecycle::badge("experimental")`
#' @inherit knit_print.CohortSizeConst return
#' @param ... passed through to the `knit_print` methods of the constituent
#' rules
#'
#' @export
#' @rdname knit_print
knit_print.IncrementsMin <- function(x, ..., asis = TRUE) {
  assert_flag(asis)

  rv <- paste0(
    "The minimum of the increments defined in the following rules:",
    paste0(
      lapply(
        x@increments_list,
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
registerS3method("knit_print", "IncrementsMin", knit_print.IncrementsMin)

#' Render an IncrementsOrdinal object
#' @inherit knit_print.CohortSizeConst return
#' @param ... passed through to the `knit_print` method of the standard rule
#'
#' @export
#' @rdname knit_print
knit_print.IncrementsOrdinal <- function(x, ..., asis = TRUE) {
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
registerS3method("knit_print", "IncrementsOrdinal", knit_print.IncrementsOrdinal)

#' Render an IncrementsRelativeParts object
#'
#' @inherit knit_print.CohortSizeConst return
#' @param labels (`character`)\cr The word used to describe toxicities.  See
#' Usage Notes below.
#' @section Usage Notes:
#' `labels` defines how toxicities are described.
#'
#' It should be a character vector of length 1 or 2.  If of length 2, the first
#' element describes a single toxicity and the second describes all other
#' toxicity counts.  If of length 1, the character `s` is appended to the value
#' describing a single toxicity.
#'
#' @export
#' @rdname knit_print
knit_print.IncrementsRelativeParts <- function(x, ..., asis = TRUE, labels = c("toxicity", "toxicities")) {
  assert_flag(asis)
  assert_character(labels, min.len = 1, max.len = 2, any.missing = FALSE)

  if (length(labels) == 1) {
    labels[2] <- paste0(labels[1], "s")
  }
  rv <- paste0(
    "The maximum increment in Part 1 is defined by the `part1Ladder` slot of ",
    "the associated `DataParts` object.\n\n",
    "If no ", labels[2], " are reported in Part 1, the starting dose for Part 2 ",
    "will be ",
    ifelse(
      x@clean_start == 0,
      " the highest dose used in Part 1.\n\n",
      paste0(
        x@clean_start,
        ifelse(abs(x@clean_start) == 1, " dose ", " doses "),
        ifelse(x@clean_start < 0, "below ", "above "),
        "the highest dose used in Part 1.\n\n"
      )
    ),
    "If one or more ", labels[2], " are reported in Part 1, the starting dose for Part 2 ",
    "will be ",
    ifelse(
      x@dlt_start == 0,
      "the highest dose used in Part 1.\n\n",
      paste0(
        x@dlt_start,
        ifelse(abs(x@dlt_start) == 1, " dose ", " doses "),
        ifelse(x@dlt_start < 0, "below ", "above "),
        "the highest dose used in Part 1.\n\n",
      )
    ) ,
    "Once Part 2 has started, the maximum increment in dose levels will be based ",
    "on the number of ", labels[2], " reported so far, as described in the ",
    "following table:"
  )
  d <- tibble::tibble(
    intervals = x@intervals,
    increments = x@increments
  ) %>%
    h_range_to_minmax(.data$intervals) %>%
    dplyr::filter(max > 0) %>%
    tibble::add_column(increments = x@increments)

  param <- list(...)
  if (!("col.names" %in% names(param))) {
    param[["col.names"]] <- c("Lower", "Upper", "Increment")
  }
  if (!("caption" %in% names(param))) {
    param[["caption"]] <- paste0("Defined by the number of ", labels[2], " reported so far")
  }
  param[["x"]] <- d
  header <- c(2, 1)
  names(header) <- c(stringr::str_to_title(labels[2]), " ")
  d_tab <- kableExtra::add_header_above(
    do.call(knitr::kable, param),
    header
  )
  rv <- paste(rv, d_tab)
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}
registerS3method("knit_print", "IncrementsRelativeParts", knit_print.IncrementsRelativeParts)

#' Render an IncrementsRelativeDLTCurrent object
#'
#' @description `r lifecycle::badge("experimental")`
#' @inherit knit_print.CohortSizeConst return
#' @param labels (`character`)\cr The word used to describe toxicities.  See
#' Usage Notes below.
#' @param ... passed to `knitr::kable`
#' @section Usage Notes:
#' The default value of `col.names` is `c("Min", "Max", "Increment")` and that
#' of `caption` is `"Defined by number of DLTs in the current cohort"`. These values
#' can be overridden by passing `col.names` and `caption` in the function call.
#'
#' `labels` defines how toxicities are described.
#'
#' It should be a character vector of length 1 or 2.  If of length 2, the first
#' element describes a single toxicity and the second describes all other
#' toxicity counts.  If of length 1, the character `s` is appended to the value
#' describing a single toxicity.
#'
#' @export
#' @rdname knit_print
knit_print.IncrementsRelativeDLTCurrent <- function(x, ..., asis = TRUE, labels = c("DLT", "DLTs")) {
  assert_flag(asis)
  assert_character(labels, min.len = 1, max.len = 2, any.missing = FALSE)

  if (length(labels) == 1) {
    labels[2] <- paste0(labels[1], "s")
  }

  param <- list(...)
  if (!("col.names" %in% names(param))) {
    param[["col.names"]] <- c("Min", "Max", "Increment")
  }
  if (!("caption" %in% names(param))) {
    param[["caption"]] <- "Defined by number of DLTs reported in the current cohort"
  }
  x <- tidy(x) %>%
    h_range_to_minmax(intervals) %>%
    dplyr::filter(max > 0) %>%
    add_column(increments = x@increments)
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
registerS3method("knit_print", "IncrementsRelativeDLTCurrent", knit_print.IncrementsRelativeDLTCurrent)

