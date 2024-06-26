#' Format a `doseGrid` for Printing
#'
#' @param grid (`numeric`)\cr the dose grid
#' @param units (`character`)\cr The units in which the values in `doseGrid` are
#' @param fmt (`character`)\cr The format used to display values in `doseGrid`.
#' If `NA`, grid values are not pre-formatted
#' @param ... not used at present
#' measured.  Appended to each value in `doseGrid` when `knit_print`ed.  The
#' default, `NA`, omits the units.
#' @return A character string containing the formatted dose grid.  If the grid
#' is `c(1, 2, 3)` and `units` is `"mg"`, the returned value is `"1 mg, 2 mg and 3 mg"`.
#' @keywords internal
h_get_formatted_dosegrid <- function(grid, units = NA, fmt = NA, ...) {
  assert_numeric(grid, lower = 0, min.len = 2, unique = TRUE, finite = TRUE, sorted = TRUE, any.missing = FALSE)
  assert_character(units, len = 1)

  n <- length(grid)
  units <- h_prepare_units(units)
  formattedGrid <- if (is.na(fmt)) {
    as.character(grid)
  } else {
    sprintf(fmt, grid)
  }
  paste0(
    paste(
      lapply(
        formattedGrid[1:(n - 1)],
        paste0,
        sep = units
      ),
      collapse = ", "
    ),
    " and ",
    formattedGrid[n],
    paste0(units, ".\n\n")
  )
}

#' Set Column Headers in Custom `knit_print` Methods
#'
#' This is a helper method used `knit_print` for `crmPack` classes.
#'
#' @param x (`ANY`)\cr object that will be printed
#' @param param (`list`)\cr A list of the `...` parameters passed to `knit_print`
#' @param summarise (`flag`)\cr Is the object to be summarised (default) or listed?
#' @return A character vector of column names.
#' @noRd
h_knit_print_set_headers <- function(x, param, summarise, ...) {
  UseMethod("h_knit_print_set_headers")
}

#' @description `r lifecycle::badge("experimental")`
#' @rdname knit_print_set_headers
#' @noRd
h_knit_print_set_headers.GeneralData <- function(x, param, summarise, ...) {
  if (!("col.names" %in% names(param))) {
    if (summarise == "none") {
      param[["col.names"]] <- c("ID", "Cohort", "Dose", "DLT?")
    } else if (summarise == "dose") {
      param[["col.names"]] <- c("Dose", "Evaluable", "With Toxicities")
    } else {
      param[["col.names"]] <- c("Cohort", "Evaluable", "With Toxicities")
    }
  }
  param
}

#' @description `r lifecycle::badge("experimental")`
#' @rdname knit_print_set_headers
#' @noRd
h_knit_print_set_headers.DataDA <- function(x, param, summarise, ...) {
  if (!("col.names" %in% names(param))) {
    if (summarise == "none") {
      param[["col.names"]] <- c("ID", "Cohort", "Dose", "Tox", "U", "T0", "TMax")
    } else if (summarise == "dose") {
      param[["col.names"]] <- c("Dose", "Evaluable", "With Toxicities")
    } else {
      param[["col.names"]] <- c("Cohort", "Evaluable", "With Toxicities")
    }
  }
  param
}

#' @description `r lifecycle::badge("experimental")`
#' @rdname knit_print_set_headers
#' @noRd
h_knit_print_set_headers.DataGrouped <- function(x, param, summarise, ...) {
  if (!("col.names" %in% names(param))) {
    if (summarise == "none") {
      param[["col.names"]] <- c("ID", "Cohort", "Dose", "Group", "Tox")
    } else if (summarise == "dose") {
      param[["col.names"]] <- c("Dose", "Group", "Evaluable", "With Toxicities")
    } else {
      param[["col.names"]] <- c("Cohort", "Group", "Evaluable", "With Toxicities")
    }
  }
  param
}

#' @description `r lifecycle::badge("experimental")`
#' @rdname knit_print_set_headers
#' @noRd
h_knit_print_set_headers.DataParts <- function(x, param, summarise, ...) {
  if (!("col.names" %in% names(param))) {
    if (summarise == "none") {
      param[["col.names"]] <- c("ID", "Part", "Cohort", "Dose", "Tox")
    } else if (summarise == "dose") {
      param[["col.names"]] <- c("Dose", "Evaluable", "With Toxicities")
    } else {
      param[["col.names"]] <- c("Cohort", "Evaluable", "With Toxicities")
    }
  }
  param
}

#' @description `r lifecycle::badge("experimental")`
#' @noRd
h_knit_print_set_headers.DataOrdinal <- function(x, param, summarise, ...) {
  if (!("col.names" %in% names(param))) {
    if (summarise == "none") {
      param[["col.names"]] <- c("ID", "Cohort", "Dose", paste0("Cat", 0:(length(x@yCategories) - 1)))
    } else if (summarise == "dose") {
      param[["col.names"]] <- c("Dose", "Evaluable", names(x@yCategories))
    } else {
      param[["col.names"]] <- c("Cohort", "Evaluable", names(x@yCategories))
    }
  }
  param
}

#' @description `r lifecycle::badge("experimental")`
#' @rdname knit_print_set_headers
#' @noRd
h_knit_print_set_headers.DataDual <- function(x, param, summarise, ...) {
  if (!("col.names" %in% names(param))) {
    if (summarise == "none") {
      param[["col.names"]] <- c("ID", "Cohort", "Dose", "Tox", "W")
    } else if (summarise == "dose") {
      param[["col.names"]] <- c("Dose", "Evaluable", "With Toxicities")
    } else {
      param[["col.names"]] <- c("Cohort", "Evaluable", "With Toxicities")
    }
  }
  param
}

#' Select Columns to Print in Custom `knit_print` Methods
#'
#' This is a helper method used `knit_print` for `crmPack` classes.
#'
#' @param x (`ANY`)\cr object that will be printed
#' @param ... Not used at present.
#' @return A tidied version of `x`, containing only the selected columns.
#' @noRd
h_knit_print_select_columns <- function(x, ...) {
  UseMethod("h_knit_print_select_columns")
}

#' @description `r lifecycle::badge("experimental")`
#' @rdname knit_print_select_columns
#' @noRd
h_knit_print_select_columns.GeneralData <- function(x, ...) {
  x %>%
    tidy() %>%
    dplyr::select("ID", "Cohort", "Dose", "Tox")
}

#' @description `r lifecycle::badge("experimental")`
#' @rdname knit_print_select_columns
#' @noRd
h_knit_print_select_columns.Data <- function(x, ...) {
  x %>%
    tidy() %>%
    dplyr::select("ID", "Cohort", "Dose", "Tox")
}

#' @description `r lifecycle::badge("experimental")`
#' @rdname knit_print_select_columns
#' @noRd
h_knit_print_select_columns.DataParts <- function(x, ...) {
  x %>%
    tidy() %>%
    dplyr::select("ID", "Part", "Cohort", "Dose", "Tox")
}

#' @description `r lifecycle::badge("experimental")`
#' @rdname knit_print_select_columns
#' @noRd
h_knit_print_select_columns.DataOrdinal <- function(x, ...) {
  x %>%
    tidy() %>%
    dplyr::select("ID", "Cohort", "Dose", tidyselect::starts_with("Cat"))
}

#' @description `r lifecycle::badge("experimental")`
#' @rdname knit_print_select_columns
#' @noRd
h_knit_print_select_columns.DataDA <- function(x, param, summarise, ...) {
  x %>%
    tidy() %>%
    dplyr::select("ID", "Cohort", "Dose", "Tox", "U", "T0", "TMax")
}

#' @description `r lifecycle::badge("experimental")`
#' @rdname knit_print_select_columns
#' @noRd
h_knit_print_select_columns.DataGrouped <- function(x, param, summarise, ...) {
  x %>%
    tidy() %>%
    dplyr::select("ID", "Cohort", "Dose", "Group", "Tox")
}

#' @description `r lifecycle::badge("experimental")`
#' @rdname knit_print_select_columns
#' @noRd
h_knit_print_select_columns.DataDual <- function(x, param, summarise, ...) {
  x %>%
    tidy() %>%
    dplyr::select("ID", "Cohort", "Dose", "Tox", "W")
}

#' Summarise a `Data` Object by Dose or Cohort for Display in Custom `knit_print` Methods
#'
#' This is a helper method used `knit_print` for `crmPack` classes.
#'
#' @param x (`ANY`)\cr object that will be printed
#' @param full_grid (`flag`)\cr Should the full grid be included or only those
#' doses with at least one evaluable participant?
#' @param ... Not used at present.
#' @return A tibble containing the summarised data
#' @noRd
h_knit_print_summarise <- function(x, summarise, full_grid, ...) {
  UseMethod("h_knit_print_summarise")
}

#' @description `r lifecycle::badge("experimental")`
#' @rdname knit_print_summarise
#' @noRd
h_knit_print_summarise.GeneralData <- function(x, summarise, full_grid, ...) {
  xTidy <- x %>% tidy()
  xTidy <- xTidy %>%
    dplyr::group_by(.data[[stringr::str_to_title(summarise)]]) %>%
    dplyr::summarise(
      N = dplyr::n(),
      ToxCount = sum(Tox)
    )
  if (full_grid && summarise == "dose") {
    xTidy <- xTidy %>%
      tidyr::complete(
        Dose = x@doseGrid,
        fill = list(ToxCount = 0, N = 0)
      )
  }
  xTidy
}

#' @description `r lifecycle::badge("experimental")`
#' @rdname knit_print_summarise
#' @noRd
h_knit_print_summarise.DataOrdinal <- function(x, summarise, full_grid, ...) {
  xTidy <- x %>% tidy()
  xTidy <- xTidy %>%
    dplyr::group_by(.data$Dose) %>%
    dplyr::summarise(
      N = dplyr::n(),
      dplyr::across(tidyselect::starts_with("Cat"), sum)
    )
  if (full_grid && summarise == "dose") {
    replace_list <- as.list(
      c(
        "N",
        names(xTidy)[which(stringr::str_detect(names(xTidy), "Cat\\d+"))]
      )
    )
    # Create a list whose names are the columns in which we need to replace NAs
    # and whose values are 0
    names(replace_list) <- sapply(replace_list, \(x) x)
    replace_list <- lapply(replace_list, \(x) 0)
    # Expand the tibble and do the replacement
    xTidy <- tidyr::expand_grid(Dose = x@doseGrid) %>%
      dplyr::left_join(xTidy, by = "Dose") %>%
      tidyr::replace_na(replace_list)
  }
  xTidy
}

#' @description `r lifecycle::badge("experimental")`
#' @rdname knit_print_summarise
#' @noRd
h_knit_print_summarise.DataGrouped <- function(x, summarise, full_grid, ...) {
  xTidy <- x %>% tidy()
  xTidy <- xTidy %>%
    dplyr::group_by(.data[[stringr::str_to_title(summarise)]], .data$Group) %>%
    dplyr::summarise(
      N = dplyr::n(),
      ToxCount = sum(Tox)
    )
  if (full_grid && summarise == "dose") {
    xTidy <- tidyr::expand_grid(
      Dose = x@doseGrid,
      Group = c("mono", "combo")
    ) %>%
      dplyr::left_join(xTidy, by = c("Dose", "Group")) %>%
      tidyr::replace_na(list(N = 0, ToxCount = 0))
  }
  xTidy
}

#' Print a `GeneralData` Object in a Markdown or Quarto Chunk
#'
#' @param label (`character`)\cr How to describe the participants in the trial.
#' See Usage Notes below.
#' @param full_grid (`flag`)\cr Should the full dose grid appear in the output table
#' or simply those doses for whom at least one evaluable participant is available?
#' Ignored unless `summarise == "dose"`.
#' @param summarise (`character`)\cr How to summarise the observed data.  The default,
#' `"none"`, lists observed data at the participant level.  `"dose"` presents
#' participant counts by dose and `"cohort"` by cohort.
#' @param summarize (`character`)\cr Synonym for `summarise`
#' @param format_func (`function`)\cr The function used to format the participant table.
#' The default applies no formatting.  Obvious alternatives include `kableExtra::kable_styling`.
#' @param ... passed to [knitr::kable()]
#' @section Usage Notes:
#' `label` describes the trial's participants.
#'
#' It should be a character vector of length 1 or 2.  If of length 2, the first
#' element describes a single participant and the second describes all other
#' situations.  If of length 1, the character `s` is appended to the value
#' when the number of participants is not 1.
#' The default values of `col.names` and `caption` vary depending on the summary
#' requested. The default values can be overridden by passing `col.names` and
#' `caption` in the function call.
#'
#' @inheritParams h_get_formatted_dosegrid
#' @export
#' @method knit_print GeneralData
#' @rdname knit_print
knit_print.GeneralData <- function(
    x, ..., asis = TRUE,
    label = c("participant", "participants"),
    full_grid = FALSE,
    summarise = c("none", "dose", "cohort"),
    summarize = summarise,
    units = NA,
    format_func = function(x) x) {
  # Validate
  assert_flag(asis)
  assert_flag(full_grid)
  assert_function(format_func)
  summarise <- match.arg(summarise)
  summarize <- match.arg(summarize)

  if (is.na(summarise) || is.null(summarise)) {
    summarise <- summarize
  }
  assert_choice(summarise, c("none", "dose", "cohort"))
  # Initialise
  label <- h_prepare_labels(label)
  param <- list(...)

  # Execute
  param <- h_knit_print_set_headers(x, param, summarise, ...)
  if (summarise == "none") {
    if (!("caption" %in% names(param))) {
      param[["caption"]] <- paste("Evaluable", label[2], "to-date")
    }
    xTidy <- h_knit_print_select_columns(x)
  } else {
    xTidy <- h_knit_print_summarise(x, summarise, full_grid)
    if (!("caption" %in% names(param))) {
      param[["caption"]] <- paste0("Summarised by ", summarise)
    }
  }
  param[["x"]] <- xTidy
  rv <- if (length(x@x) > 0) {
    paste((do.call(knitr::kable, param)) %>% format_func(), collapse = "\n")
  } else {
    paste("No", label[2], "are yet evaluable.\n\n")
  }
  rv <- paste0(
    rv,
    paste0(
      "\n\nThe dose grid is ",
      h_get_formatted_dosegrid(
        grid = x@doseGrid,
        units = units,
        ...
      ),
      ""
    ),
    "\n\n",
    collpase = "\n"
  )
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

#' @export
#' @method knit_print DataParts
#' @rdname knit_print
knit_print.DataParts <- function(
    x, ..., asis = TRUE,
    label = c("participant", "participants"),
    full_grid = FALSE,
    summarise = c("none", "dose", "cohort"),
    summarize = summarise,
    units = NA,
    format_func = function(x) x) {
  rv <- NextMethod()
  rv <- paste0(
    rv,
    paste0(
      "\n\nThe part 1 ladder is ",
      h_get_formatted_dosegrid(x@part1Ladder, units)
    ),
    paste0("\n\nThe next part is Part ", x@nextPart, ".\n\n")
  )
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}
