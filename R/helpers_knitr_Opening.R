# OpeningMinDose ----

#' @description `r lifecycle::badge("experimental")`
#' @param asis (`flag`)\cr Not used at present
#' @rdname knit_print
#' @export
#' @method knit_print OpeningMinDose
knit_print.OpeningMinDose <- function(
  x,
  ...,
  asis = TRUE
) {
  assert_flag(asis)

  rv <- paste0(
    "If the backfill cohort's dose is ",
    x@min_dose,
    " or higher.\n\n"
  )
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# OpeningMinCohorts ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.OpeningMinDose
#' @rdname knit_print
#' @export
#' @method knit_print OpeningMinCohorts
knit_print.OpeningMinCohorts <- function(
  x,
  ...,
  asis = TRUE
) {
  assert_flag(asis)

  rv <- paste0(
    "If ",
    x@min_cohorts,
    " or more cohorts have been treated in total.\n\n"
  )
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# OpeningNone ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.OpeningMinDose
#' @rdname knit_print
#' @export
#' @method knit_print OpeningNone
knit_print.OpeningNone <- function(
  x,
  ...,
  asis = TRUE
) {
  assert_flag(asis)

  rv <- paste0(
    "No backfill cohorts at all will be opened.\n\n"
  )
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# OpeningMinResponses ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.OpeningMinDose
#' @rdname knit_print
#' @export
#' @method knit_print OpeningMinResponses
knit_print.OpeningMinResponses <- function(
  x,
  ...,
  asis = TRUE
) {
  assert_flag(asis)

  doses_text <- ifelse(
    x@include_lower_doses,
    "at this dose or lower",
    "at this dose"
  )

  rv <- paste0(
    "If ",
    x@min_responses,
    " or more responses have been observed ",
    doses_text,
    ".\n\n"
  )
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# OpeningList ----

#' @description `r lifecycle::badge("experimental")`
#' @param preamble (`character`)\cr the text that introduces the list of rules.
#' @param indent (`integer`)\cr the indent level of the current opening rule
#'   list. Spaces with length `indent * 4` will be prepended to the beginning of
#'   the rendered opening rule list.
#' @inheritParams knit_print.OpeningMinDose
#' @rdname knit_print
#' @export
#' @method knit_print OpeningList
knit_print.OpeningList <- function(
  x,
  ...,
  preamble,
  indent = 0L,
  asis = TRUE
) {
  assert_flag(asis)
  assert_integer(indent, lower = 0)

  if (missing(preamble)) {
    case_string <- switch(
      as.character(length(x@open_list)),
      `1` = "rule applies",
      "rules apply"
    )
    preamble <- paste0(
      "The following opening ",
      case_string,
      ":\n"
    )
  } else {
    assert_string(preamble)
  }

  rules_list <- paste0(
    lapply(
      x@open_list,
      function(z, indent) {
        paste0(
          strrep(" ", indent * 4),
          "-  ",
          knit_print(z, asis = FALSE, indent = indent + 1L, ...)
        )
      },
      indent = indent
    ),
    collapse = "\n"
  )
  rv <- paste0(
    preamble,
    "\n",
    rules_list,
    "\n\n"
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# OpeningAll ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.OpeningList
#' @rdname knit_print
#' @export
#' @method knit_print OpeningAll
knit_print.OpeningAll <- function(
  x,
  ...,
  preamble,
  asis = TRUE
) {
  if (missing(preamble)) {
    case_string <- switch(
      as.character(length(x@open_list)),
      `1` = c("this ", "rule is "),
      `2` = c("both of the ", "rules are "),
      c("all of the ", "rules are ") # this works as default case
    )
    preamble <- paste0(
      "If ",
      case_string[1],
      "following ",
      case_string[2],
      "satisfied:\n"
    )
  }
  knit_print.OpeningList(x, ..., preamble = preamble, asis = asis)
}

# OpeningAny ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.OpeningList
#' @rdname knit_print
#' @export
#' @method knit_print OpeningAny
knit_print.OpeningAny <- function(
  x,
  ...,
  preamble,
  asis = TRUE
) {
  if (missing(preamble)) {
    case_string <- switch(
      as.character(length(x@open_list)),
      `1` = c("this ", "rule is "),
      `2` = c("either of the ", "rules are "),
      c("any of the ", "rules are ") # this works as default case
    )
    preamble <- paste0(
      "If ",
      case_string[1],
      "following ",
      case_string[2],
      "satisfied:\n"
    )
  }
  knit_print.OpeningList(x, ..., preamble = preamble, asis = asis)
}
