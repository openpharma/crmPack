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

# Recommendation ----

#' Render a `Recommendation` Object
#'
#' @description `r lifecycle::badge("experimental")`
#' @param x (`Recommendation`)\cr The object to knit_print.
#' @param asis (`flag`)\cr Should the return value be wrapped in a call to `knitr::asis_output`?
#' @param unit (`character`)\cr The dose unit.
#' @param ... Not used at present
#'
#' @return The markdown representation of the object, as a character string
#' @seealso [`knit_print`] for more details.
#'
#' @export
#' @method knit_print Recommendation
#' @rdname knit_print
knit_print.Recommendation <- function(
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
registerS3method("knit_print", "Recommendation", knit_print.Recommendation)

# RecommendationProbs ----

#' Render a `RecommendationProbs` Object
#'
#' @description `r lifecycle::badge("experimental")`
#' @param x (`RecommendationProbs`)\cr The object to knit_print.
#' @param asis (`flag`)\cr Should the return value be wrapped in a call to `knitr::asis_output`?
#' @param caption (`character`)\cr The caption for the probs table.
#' @param unit (`character`)\cr The dose unit.
#' @param ... Not used at present
#'
#' @return The markdown representation of the object, as a character string
#' @seealso [`knit_print`] for more details.
#'
#' @export
#' @method knit_print RecommendationProbs
#' @rdname knit_print
knit_print.RecommendationProbs <- function(
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
    # col_names = c("Dose", "P(overdose)", "Recommended?"),
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
    "The recommended dose for the next cohort is ",
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
registerS3method("knit_print", "RecommendationProbs", knit_print.RecommendationProbs)

# RecommendationThreePlusThree ----

#' Render a `RecommendationThreePlusThree` Object
#'
#' @description `r lifecycle::badge("experimental")`
#' @param x (`RecommendationThrePlusThree`)\cr The object to knit_print.
#' @param asis (`flag`)\cr Should the return value be wrapped in a call to `knitr::asis_output`?
#' @param unit (`character`)\cr The dose unit.
#' @param ... Not used at present
#'
#' @return The markdown representation of the object, as a character string
#' @seealso [`knit_print`] for more details.
#'
#' @export
#' @method knit_print RecommendationProbs
#' @rdname knit_print
knit_print.RecommendationThreePlusThree <- function(
    x,
    ...,
    asis = TRUE,
    unit = ""
) {
  assert_flag(asis)
  assert_character(unit, len = 1, any.missing = FALSE)

  if (x@stopHere) {
    rv <- paste0(
      "The estimated MTD is ",
      x@value,
      ifelse(stringr::str_length(unit) > 0, " ", ""),
      unit,
      " and the recommendation is that the trial should stop.\n\n"
    )
  } else {
    rv <- paste0(
      "The recommendeded dose for the next cohort is ",
      x@value,
      ifelse(stringr::str_length(unit) > 0, " ", ""),
      unit,
      " and the recommendation is that the trial should continue.\n\n"
    )
  }

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# Delete after running devtools::document()
registerS3method(
  "knit_print",
  "RecommendationThreePlusThree",
  knit_print.RecommendationThreePlusThree
)
