# RecruitmentUnlimited ----

#' @description `r lifecycle::badge("experimental")`
#' @param asis (`flag`)\cr Not used at present
#' @rdname knit_print
#' @export
#' @method knit_print RecruitmentUnlimited
knit_print.RecruitmentUnlimited <- function(
  x,
  ...,
  asis = TRUE
) {
  assert_flag(asis)

  rv <- paste0(
    "Unlimited recruitment of backfill patients is allowed.\n\n"
  )
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# RecruitmentRatio ----

#' @description `r lifecycle::badge("experimental")`
#' @param asis (`flag`)\cr Not used at present
#' @rdname knit_print
#' @export
#' @method knit_print RecruitmentRatio
knit_print.RecruitmentRatio <- function(
  x,
  ...,
  asis = TRUE
) {
  assert_flag(asis)

  rv <- paste0(
    "Backfill patients are recruited at a ratio of ",
    x@ratio,
    " per patient in the main trial cohort.\n\n"
  )
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}
