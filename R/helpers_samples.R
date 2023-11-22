#' Convert a Samples Object from an ordinal Model to the Equivalent Samples Object
#' from a Binary Model
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A simple helper function that converts a [`Samples`] object from the fit of an
#' ordinal CRM model to that which would have been obtained from fitting a binary
#' CRM model for toxicities of a specified grade to the same observed data.
#'
#' @param obj (`DataOrdinal`)\cr the `DataOrdinal` object to covert
#' @param grade (`integer`)\cr the toxicity grade for which the equivalent data
#' is required.
#' @return A [`Samples`] object.
#'
#' @export
h_convert_ordinal_samples <- function(obj, grade) {
  # Validate
  assert_integer(grade, len = 1, lower = 1)
  assert_class(obj, "Samples")
  assert_subset(c(paste0("alpha[", 1:grade, "]"), "beta"), names(obj@data))
  # Execute
  d <- list("alpha0" = obj@data[[paste0("alpha[", grade, "]")]], "alpha1" = obj@data$beta)
  Samples(data = d, options = obj@options)
}
