#' Convert Object's Attributes to a Tibble
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A helper function that interrogates an object to see if it has attributes
#' and, if so, converts them to a (list of) tibbles.  Columns are named after
#' attributes.  If attributes have different lengths but recycling is possible,
#' a single `tibble` is returned.  Otherwise, a `list` of tibbles is returned.
#'
#' @param x (`CrmPackObject`)\cr object whose attributes will be interrogated.
#' @param .ignore (`character`)\cr names of attrributes to be ignored.
#'
#' @return A [`tibble`] or `list` of [`tibble`]s containg the values of the
#' object's attributes.
#'
#' @keywords internal
#' @noRd
h_handle_attributes <- function(x, .ignore = c("names", "class", "description", "row.names")) {
  a <- attributes(x)
  valid_names <- setdiff(names(a), .ignore)
  lapply(
    valid_names,
    function(n) {
      z <- attr(x, n)
      rv <- NULL
      # Some Design classes have attributes that are functions or CrmPackClass objects
      if (!is.function(z)) {
        if (length(z) == 1) {
          if (is(z, "CrmPackClass")) {
            z <- z |> tidy()
          }
          rv <- tibble::tibble(X = z)
        } else {
          if (length(z) == 0) {
            rv <- tibble::tibble(X = NA)
          } else {
            if (is(z, "CrmPackClass")) {
              rv <- z |> tidy()
            } else {
              rv <- tibble::tibble(X = list(z))
            }
          }
        }
        names(rv) <- n
      }
      rv
    }
  ) |>
    dplyr::bind_cols()
}

#' Tidy a Single Slot of a CrmPackObject
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A helper function that converts a single slot of a `CrmPackObject` to a tibble.
#' If the slots value is a `list`, each element of the list is tidied individually.
#'
#' @param obj (`CrmPackObject`)\cr object to be converted.
#' @param slot_name (`character`)\cr name of the slot to be tidied.
#' @param col (`character`)\cr The name of the corresponding column in the tidied
#' tibble.  Defaults to `slot_name`.
#' @param attributes (`flag`)\cr shoud the object's attributes, if any, be added
#'  to the output tibble
#'
#' @return A [`tibble`]
#'
#' @keywords internal
#' @noRd
h_tidy_slot <- function(obj, slot_name, col = NULL, attributes = FALSE) {
  if (is.list(slot(obj, slot_name))) {
    return(
      lapply(
        slot(obj, slot_name),
        function(x) {
          if (is.data.frame(x)) {
            return(x)
          } else if (is.list(x) && stringr::str_detect(class(x)[1], stringr::fixed("tbl_"))) {
            # Already tidied to a list
            return(x)
          } else if (is.numeric(x) | is.character(x)) {
            # tidy.numeric & tidy.character are deprecated
            return(tibble::tibble(!!{{ slot_name }} := x))
          } else {
            return(x |> tidy())
          }
        }
      )
    )
  }
  if (is(slot(obj, slot_name), "CrmPackClass")) {
    rv <- slot(obj, slot_name) |>
      tidy()
  } else {
    if (is.null(col)) {
      col <- slot_name
    }
    rv <- tibble::tibble({{ col }} := slot(obj, slot_name))
  }
  if (attributes) {
    a <- h_handle_attributes(slot(obj, slot_name))
    if (nrow(a) > 0) {
      rv <- rv |> dplyr::bind_cols(a)
    }
  }
  rv
}

#' Tidy All Slots of a CrmPackObject
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A helper function that converts all the slots of a `CrmPackObject` to a
#' (list of) tibble(s).
#'
#' @param obj (`CrmPackObject`)\cr object to be tidied.
#' @param ... passed to h_tidy_slot
#'
#' @return A (list of) [`tibble`](s)
#'
#' @keywords internal
#' @noRd
h_tidy_all_slots <- function(obj, ...) {
  slot_names <- slotNames(obj)
  rv <- list()
  for (nm in slot_names) {
    if (!is.function(slot(obj, nm))) {
      rv[[nm]] <- h_tidy_slot(obj, nm, ...)
    }
  }
  # Column bind of all list elements have the same number of rows
  if (length(rv) > 1 && length(unique(sapply(rv, nrow))) == 1) {
    rv <- rv |> dplyr::bind_cols() # nolint
  }
  rv
}

#' Amend the Class of a Tibble to Indicate that it Contains a Tidied `CrmPackObject`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A helper function that prepends `tbl_<cls>`, where `<cls>` is the first
#' element of the class attribute of the original `CrmPackObject` to the class
#' attribute of a tibble
#'
#' @param d (`tibble`)\cr the tibble containing the tidied version of `obj`.
#' @param obj (`CrmPackObject`)\cr object to be converted.
#'
#' @return `d`, with an amended class attribute
#'
#' @keywords internal
#' @noRd
h_tidy_class <- function(d, obj) {
  cls <- class(obj)
  class(d) <- c(paste0("tbl_", cls[1]), class(d))
  d
}

#' Convert a `CrmPackObject`'s "Interval list" to a Min-Max
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' `CrmPackClass` objects that define a set of intervals (such as `CohortSizeRange`)
#' typically contain a left-open vector that dfines the intervals.  For example,
#' `my_size <- CohortSizeRange(intervals = c(0, 20), cohort_size = c(1, 3))` defines
#' two dose ranges: [0, 20) and [20, Inf).  This is convenient for coding, but
#' awkward for reporting.  This helper function converts this single-column
#' representation to a two-column representation that explicitly defines the
#' lower and upper ends of each interval.  Using the example above, the converted
#' tibble would look like this:
#'
#' | cohort_size | min  | max  |
#' | ----------: | ---: | ---: |
#' | 1           | -Inf | 20   |
#' | 3           |  20  | Inf  |
#'
#' @param x (`tibble`)\cr the tibble to be converted.
#' @param col (`tidy-eval`)\cr column containing the intervals.
#' @param min_col (`character`)\cr name of the column containing the lower end
#' of the interval in the returned value.
#' @param max_col (`character`)\cr name of the column containing the upper end
#' of the interval in the returned value.
#' @param range_min (`numeric`)\cr value of the lower end of the first interval.
#' @param range_max (`numeric`)\cr value of the upper end of the last interval.
#'
#' @return A `tibble` in min-max format, with one row more than the input tibble.
#'
#' @keywords internal
#' @noRd
h_range_to_minmax <- function(
    x,
    col,
    min_col = "min",
    max_col = "max",
    range_min = -Inf,
    range_max = Inf) {
  vals <- x %>% dplyr::pull({{ col }})
  tibble(
    {{ min_col }} := c(range_min, vals),
    {{ max_col }} := c(vals, range_max)
  )
}
