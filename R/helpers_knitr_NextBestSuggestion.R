# Integration with knitr ----
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' We provide additional utility functions to allow human-friendly rendition of
#' crmPack objects in Markdown and Quarto files
#'
#' @return a character string that represents the object in markdown.
#' @name knit_print
NULL

# NextBestSuggestion ----

#' Render a `NextBestSuggestion` Object
#'
#' @description `r lifecycle::badge("experimental")`
#' @param x (`NextBestSuggestion`)\cr The object to knit_print.
#' @param asis (`flag`)\cr Should the return value be wrapped in a call to `knitr::asis_output`?
#' @param unit (`character`)\cr The dose unit.
#' @param ... Not used at present
#'
#' @return The markdown representation of the object, as a character string
#' @seealso [`knit_print`] for more details.
#'
#' @export
#' @method knit_print NextBestSuggestion
#' @rdname knit_print
knit_print.NextBestSuggestion <- function(
    x,
    ...,
    asis = TRUE,
    unit = ""
) {
  assert_flag(asis)

  rv <- paste0(
    "The suggested dose for the next cohort is ",
    x@value,
    ifelse(stringr::str_length(unit) > 0, " ", ""),
    unit,
    ".\n\n"
  )
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# Delete after running devtools::document()
registerS3method("knit_print", "NextBestSuggestion", knit_print.NextBestSuggestion)


# NextBestSuggestionProbs ----

#' Render a `NextBestSuggestionProbs` Object
#'
#' @description `r lifecycle::badge("experimental")`
#' @param x (`NextBestSuggestionProbs`)\cr The object to knit_print.
#' @param asis (`flag`)\cr Should the return value be wrapped in a call to `knitr::asis_output`?
#' @param caption (`character`)\cr The caption for the probs table.
#' @param unit (`character`)\cr The dose unit.
#' @param ... Not used at present
#'
#' @return The markdown representation of the object, as a character string
#' @seealso [`knit_print`] for more details.
#'
#' @export
#' @method knit_print NextBestSuggestionProbs
#' @rdname knit_print
knit_print.NextBestSuggestionProbs <- function(
    x,
    ...,
    asis = TRUE,
    unit = "",
    caption = "Probabilities by dose"
) {
  assert_flag(asis)
  assert_character(caption, len = 1, any.missing = FALSE)
  assert_character(unit, len = 1, any.missing = FALSE)

  param <- h_kable_param_default(
    list(...),
    # col_names = c("Dose", "P(overdose)", "Suggested?"),
    caption = caption
  )
  if (stringr::str_length(unit) > 0) {
    tbl <- x@probs %>% dplyr::mutate(Dose = paste0(Dose, " ", unit))
  } else {
    tbl <- x@probs
  }
  param[["x"]] <- tbl
  # label <- h_prepare_labels(label)
  rv <- paste0(
    "The suggested dose for the next cohort is ",
    x@value,
    ifelse(stringr::str_length(unit) > 0, " ", ""),
    unit,
    ".\n\nThe overdose probabilities at each dose in the dose grid are given in the table below.\n\n",
    paste0(do.call(knitr::kable, param), collapse = "\n"),
    "\n\n"
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# Delete after running devtools::document()
registerS3method("knit_print", "NextBestSuggestionProbs", knit_print.NextBestSuggestionProbs)
