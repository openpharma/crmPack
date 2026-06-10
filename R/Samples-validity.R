#' @include McmcOptions-methods.R
NULL

# v_samples ----

#' Internal Helper Functions for Validation of [`Samples`] Objects
#'
#' @description These functions are only used internally to validate the format
#'   of an input [`Samples`] or inherited classes and therefore not exported.
#'
#' @name v_samples_objects
#' @param object (`Samples`)\cr object to validate.
#' @return A `character` vector with the validation failure messages, or `TRUE`
#'   in case validation passes.
NULL

#' @describeIn v_samples_objects validates that the [`Samples`] object contains
#'   valid `data` slot.
v_samples <- function(object) {
  v <- Validate()
  v$check(
    all(sapply(object@data, NROW) == size(object@options)),
    "Every element in data must be of the same length (no. of rows) as the sample size was"
  )
  v$check(
    all(sapply(object@data, test_numeric, finite = TRUE, any.missing = FALSE)),
    "Every element in data must be a finite object of type integer or double"
  )
  v$result()
}

#' @describeIn v_samples_objects validates that the [`HierarchicalSamples`]
#'   object contains valid per-arm sample mappings.
v_hierarchical_samples <- function(object) {
  v <- Validate()
  arm_samples <- object@arm_samples

  v$check(
    test_list(arm_samples, any.missing = FALSE),
    "arm_samples must be a list without missings"
  )
  v$check(
    test_names(names(arm_samples), type = "unique"),
    "arm_samples must be a named list with unique arm names"
  )

  if (isTRUE(test_list(arm_samples, any.missing = FALSE))) {
    for (arm_name in names(arm_samples)) {
      mapping <- arm_samples[[arm_name]]
      v$check(
        test_character(mapping, min.len = 1L, any.missing = FALSE, unique = TRUE),
        paste0(
          "arm_samples entry '", arm_name,
          "' must be a unique character vector"
        )
      )
      v$check(
        test_names(names(mapping), type = "unique"),
        paste0(
          "arm_samples entry '", arm_name,
          "' must have unique parameter names"
        )
      )

      if (isTRUE(test_character(mapping, any.missing = FALSE))) {
        v$check(
          all(unname(mapping) %in% names(object@data)),
          paste0(
            "arm_samples entry '", arm_name,
            "' must refer to sample names present in data"
          )
        )
      }
    }
  }

  v$result()
}
