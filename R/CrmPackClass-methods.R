# tidy ----

## generic ----

#' Tidying `CrmPackClass` objects
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' In the spirit of the `broom` package, provide a method to convert a
#' `CrmPackClass` object to a (list of) tibble9s).
#'
#' @param x (`CrmPackClass`)\cr the object to be tidied.
#' @param ... \cr Used by class-specific methods
#'
#' @return A (list of) tibble(s) represnting the object in tidy form.
#'
#' @export
#'
setGeneric(
  name = "tidy",
  def = function(x, ...) {
    standardGeneric("tidy")
  }
)

## CrmPackClass ----

#' Tidy a `CrmPackClass` Object
#'
#' Following the pronciples of the `broom` package, convert a `CrmPackClass`
#' object to a (list of) tibbles.  This is a basic, default representation
#'
#' @param x (`CrmPackClass`)\cr The object to be tidied
#' @param ... \cr Not used at present
#' @rdname tidy
#' @aliases tidy-CrmPackClass
#' @keywords methods
#' @example examples/CrmPackClass-method-tidy.R
#'
#' @export
setMethod(
  f = "tidy",
  signature = signature(x = "CrmPackClass"),
  definition = function(x, ...) {
    rv <- h_tidy_all_slots(x) |> h_tidy_class(x)
    if (length(rv) == 1) {
      rv[[names(rv)[1]]]
    } else {
      rv
    }
  }
)
