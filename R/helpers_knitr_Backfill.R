# Backfill ----

#' @description `r lifecycle::badge("experimental")`
#' @param asis (`flag`)\cr Not used at present
#' @rdname knit_print
#' @export
#' @method knit_print Backfill
knit_print.Backfill <- function(
  x,
  ...,
  asis = TRUE
) {
  assert_flag(asis)

  if (is(x@opening, "OpeningNone")) {
    rv <- "No backfill cohorts at all will be opened.\n\n"
  } else {
    # Render the cohort size
    cohort_size_text <- paste0(
      "**Cohort size**: ",
      knit_print(x@cohort_size, asis = FALSE, ...),
      "\n"
    )

    # Render the opening rule
    opening_text <- paste0(
      "**Opening rule**: ",
      knit_print(x@opening, asis = FALSE, ...),
      "\n"
    )

    # Render the recruitment rule
    recruitment_text <- paste0(
      "**Recruitment**: ",
      knit_print(x@recruitment, asis = FALSE, ...),
      "\n"
    )

    # Render total size and priority
    max_size_text <- paste0(
      "**Total number of backfill patients**: ",
      ifelse(x@max_size >= 1e6, "Unlimited", x@max_size),
      " backfill patients.\n\n"
    )

    priority_text <- paste0(
      "**Priority of higher vs. lower dose backfill cohorts**: ",
      x@priority,
      " dose.\n\n"
    )

    rv <- paste0(
      cohort_size_text,
      opening_text,
      recruitment_text,
      max_size_text,
      priority_text
    )
  }

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}
