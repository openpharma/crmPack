# Data
# DataOrdinal
# DataDual
# DataParts
# DataMixture
# DataDA
# DataGrouped

#' Format a `doseGrid` for Printing
#'
#' @param grid (`double`)\cr the dose grid
#' @param units (`character`)\cr The units in which the values in `doseGrid` are
#' measured.  Appended to each value in `doseGrid` when `knit_print`ed.  The
#' default, `NA`, omits the units.
#' @return A character string containing the formatted dose grid.  If the grid
#' is `c(1, 2, 3)` and `units` is `"mg"`, the returned value is `"1 mg, 2 mg and 3 mg"`.
#' @internal
h_get_formatted_dosegrid <- function(grid, units = NA) {
  assert_numeric(grid, lower = 0, min.len = 2, unique = TRUE, finite = TRUE, sorted = TRUE, any.missing = FALSE)
  assert_character(units, len = 1)

  n <- length(grid)
  if (is.na(units)) {
    units <- ""
  } else {
    units <- paste0(" ", units)
  }
  paste0(
    paste(
      lapply(grid[1:(n - 1)], paste0, sep = units),
      collapse = ", "
    ),
    " and ",
    grid[n],
    paste0(units, ".")
)
}

#' Print a `Data` Object in a Markdown or Quarto Chunk
#'
#' @param labels (`character`)\cr How to describe the participants in the trial.
#' See Usage Notes below.
#' @param full_grid (`flag`)\cr Should the full dose grid appear in the output table
#' or simply those doses for whom at least one evaluable participant is available?
#' Ignored unless `summarise == "dose"`.
#' @param summarise (`character`)\cr How to summarise the observed data.  The default,
#' `"none"`, lists observed data at the particpant level.  `"dose"` presents
#' participant counts by dose and `"cohort"` by cohort.
#' @param summarize (`character`)\cr Synonym for `summarise`
#' @param format_func (`function`)\cr The function used to format the participant table.
#' The default applies no formatting.  Obvious alternatives include `kableExtra::kable_styling`.
#' @param ... passed to [knitr::kable()]
#' @section Usage Notes:
#' `labels` describes the trial's participants.
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
#' @rdname knit_print
knit_print.GeneralData <- function(
    x, ..., asis = TRUE,
    labels = c("participant", "participants"),
    full_grid = FALSE,
    summarise = c("none", "dose", "cohort"),
    summarize = summarise,
    units = NA,
    format_func = function(x) x
  ) {
  # Validate
  assert_character(labels, max.len = 2, any.missing = FALSE)
  assert_flag(asis)
  assert_flag(full_grid)
  assert_function(format_func)
  summarise <- match.arg(summarise)
  summarize <- match.arg(summarize)

  if (is.na(summarise) | is.null(summarise)) {
    summarise <- summarize
  }
  assert_choice(summarise, c("none", "dose", "cohort"))
  # Initialise
  if (length(labels) == 1) {
    labels[2] <- paste0(labels[1], "s")
  }
  param <- list(...)

  # Execute
  xTidy <- tidy(x)
  if (summarise == "none") {
    if (!("col.names" %in% names(param))) {
      param[["col.names"]] <- c("ID", "Cohort", "Dose", "DLT?")
    }
    if (!("caption" %in% names(param))) {
      param[["caption"]] <- paste("Evaluable", labels[2], "to-date")
    }
    xTidy <- xTidy %>%
      dplyr::select(ID,	Cohort,	Dose, Tox)
  } else {
    xTidy <- xTidy %>%
      dplyr::group_by(.data[[stringr::str_to_title(summarise)]]) %>%
      dplyr::summarise(
        N = dplyr::n(),
        ToxCount = sum(Tox)
      )
    if (full_grid & summarise == "dose") {
      xTidy <- xTidy %>%
        tidyr::complete(
          Dose = x@doseGrid,
          fill = list(ToxCount = 0, N = 0)
        )
    }
    if (!("col.names" %in% names(param))) {
      if (summarise == "dose") {
        param[["col.names"]] <- c("Dose", "Evaluable", "With Toxicities")
      } else {
        param[["col.names"]] <- c("Cohort", "Evaluable", "With Toxicities")
      }
    }
    if (!("caption" %in% names(param))) {
      param[["caption"]] <- paste0("Summarised by ", summarise)
    }

  }
  param[["x"]] <- xTidy
  rv <- ifelse(
    length(x@x) > 0,
    (do.call(knitr::kable, param)) %>% format_func(),
    paste("No", labels[2], "are yet evaluable.\n\n")
  )
  rv <- paste0(
    rv,
    paste0(
      "The dose grid is ",
      h_get_formatted_dosegrid(
        grid = x@doseGrid,
        units = "mg/kg"
      ),
      ""
    ),
    collpase = "<br>"
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}
registerS3method("knit_print", "GeneralData", knit_print.GeneralData)
