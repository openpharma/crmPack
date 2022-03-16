#' @include helpers.R
NULL

#' Appending a Dummy Number for Selected Slots in Data
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A helper function that appends a dummy value to a given slots in [`Data`]
#' class object, if and only if the total number of observations (as indicated
#' by `object@nObs`) equals to `1`. Otherwise, the `object` is not changed.
#'
#' @note The main motivation behind this function is related to the `JAGS`.
#'   If there is only one observation, the data is not passed correctly to
#'   `JAGS`, i.e. e.g. `x` and `y` are treated like scalars in the data file.
#'   Therefore it is necessary to add dummy values to the vectors in this case
#'   As we don't change the number of observations (`nObs`), this addition of
#'   zeros doesn't affect the results of `JAGS` computations.
#'
#' @param object (`Data`)\cr object into which dummy values will be added.
#' @param where (`character`)\cr names of slots in `object` to which a `dummy`
#'   number will be appended.
#' @param dummy (`number`)\cr a dummy number that will be appended to selected
#'   slots in `object`. Default to `0`.
#'
#' @return A [`Data`] object with slots updated with dummy number.
#'
#' @export
#' @example examples/helpers-jags_add_dummy.R
#'
h_jags_add_dummy <- function(object, where, dummy = 0) {
  assert_class(object, "Data")
  assert_character(where)
  assert_subset(where, slotNames(object))
  assert_number(dummy)

  if (object@nObs == 1L) {
    for (i in where) {
      # Add dummy value and preserve the class.
      slot(object, i) <- as(c(slot(object, i), dummy), class(slot(object, i)))
    }
  }
  object
}

#' Joining `JAGS` Models
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This helper function joins two JAGS models in the way that the body of the
#' second model is appended to the body of the first model (in this order).
#' After that, the first, body-extended model is returned. The arguments of
#' `model1`, `model2` model functions (if any) are not combined in any way.
#'
#' @note `model1` and `model2` functions must have a multi-expression
#'   body, i.e. braced expression(s). Environments or any attributes of the
#'   function bodies are not preserved in any way after joining.
#'
#' @param model1 (`function`)\cr the first model to join.
#' @param model2 (`function`)\cr the second model to join.
#'
#' @return joined models.
#'
#' @export
#'
h_jags_join_models <- function(model1, model2) {
  assert_function(model1)
  assert_function(model2)
  assert_class(body(model1), "{")
  assert_class(body(model2), "{")

  body2 <- as.list(body(model2))
  if (length(body2) >= 2) {
    body1 <- as.list(body(model1))
    body(model1) <- as.call(c(body1, body2[-1]))
  }
  model1
}

#' Setting Initial Values for `JAGS` Model Parameters
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A simple helper function that prepares an object for `inits` argument of
#'   [rjags::jags.model()], which is invoked by [mcmc()] method. The `inits`
#'   argument specifies initial values for model parameters.
#'
#' @param model (`GeneralModel`)\cr an input model.
#' @param data (`GeneralData`)\cr an input data.
#'
#' @return A `list` of starting values for parameters required to be initialized
#'   in the MCMC `JAGS `sampler.
#'
#' @export
#' @example examples/helpers-jags_get_model_inits.R
#'
h_jags_get_model_inits <- function(model, data) {
  assert_class(model, "GeneralModel")
  assert_class(data, "GeneralData")

  inits <- do.call(model@init, h_slots(data, formalArgs(model@init)))
  assert_list(inits)
  inits[sapply(inits, length) > 0L]
}

#' Getting Data for `JAGS`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A simple helper function that prepares an object for `data` argument of
#'   [rjags::jags.model()], which is invoked by [mcmc()] method.
#'
#' @param model (`GeneralModel`)\cr an input model.
#' @param data (`GeneralData`)\cr an input data.
#' @param from_prior (`flag`)\cr sample from the prior only? In this case
#'   data will not be appended to the output.
#'
#' @export
#' @example examples/helpers-jags_get_data.R
#'
h_jags_get_data <- function(model, data, from_prior) {
  assert_class(model, "GeneralModel")
  assert_class(data, "GeneralData")
  assert_flag(from_prior)

  modelspecs <- do.call(
    model@modelspecs,
    h_slots(data, formalArgs(model@modelspecs))
  )
  assert_list(modelspecs)

  data_model <- if (from_prior) {
    # Remove elements named "ref_dose" to avoid JAGS error of unused variables.
    modelspecs <- modelspecs[setdiff(names(modelspecs), "ref_dose")]
    NULL
  } else {
    # Add dummy to ensure that e.g. `x` and `y` in `data` won't be treated as
    # scalars by `JAGS` if `data@nObs == 0`, which leads to failures.
    add_where <- setdiff(
      model@datanames,
      c("nObs", "nGrid", "nObsshare", "yshare", "xshare", "Tmax")
    )
    h_slots(h_jags_add_dummy(data, where = add_where), model@datanames)
  }
  c(data_model, modelspecs)
}

#' Writing JAGS Model to a File
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This function converts a R function with JAGS model into a text and then
#' writes it into a given file. During the "model into text" conversion, the
#' format of numbers of which absolute value is less than `0.001` or greater
#' than `10000` is changed. These numbers will be converted into scientific
#' format with specified number of significant digits using [formatC()]
#' function.
#'
#' @note JAGS syntax allows truncation specification like `dnorm(...) I(...)`,
#'   which is illegal in R. To overcome this incompatibility, use dummy operator
#'   `\%_\%` before `I(...)`, i.e. `dnorm(...) \%_\% I(...)` in the model's
#'   code. This dummy operator `\%_\%` will be removed just before saving the
#'   JAGS code into a file.
#'   Due to technical issues related to conversion of numbers to scientific
#'   format, it is required that the body of a model function does not contain
#'   `TEMP_NUM_PREF_` or `_TEMP_NUM_SUF` character constants in its body.
#'
#' @param model (`function`)\cr function containing the JAGS model.
#' @param file (`string` or `NULL`)\cr the name of the file (including the
#'   optional path) where the model will be saved. If `NULL`, the file will be
#'   created in a `R_crmPack` folder placed under temporary directory indicated
#'   by [tempdir()] function.
#' @param digits (`count`)\cr a desired number of significant digits for
#'   for numbers used in JAGS input, see [formatC()].
#' @return The name of the file where the model was saved.
#'
#' @export
#' @example examples/helpers-jags_write_model.R
#'
h_jags_write_model <- function(model, file = NULL, digits = 5) {
  assert_function(model)
  assert_count(digits)

  if (!is.null(file)) {
    assert_path_for_output(file)
  } else {
    dir <- file.path(tempdir(), "R_crmPack")
    # Don't warn, as the temp dir often exists (which is OK).
    dir.create(dir, showWarnings = FALSE)
    file <- tempfile(
      pattern = "jags_model_fun",
      tmpdir = dir,
      fileext = ".txt"
    )
  }

  # Replace scientific notation.
  model_sci_replaced <- h_rapply(
    x = body(model),
    fun = h_format_number,
    classes = c("integer", "numeric"),
    digits = digits,
    prefix = "TEMP_NUM_PREF_",
    suffix = "_TEMP_NUM_SUF"
  )
  # Transform `model` body into character vector.
  model_text <- deparse(model_sci_replaced, control = NULL)
  model_text <- gsub("\"TEMP_NUM_PREF_|_TEMP_NUM_SUF\"", "", model_text)
  model_text <- gsub("%_% ", "", model_text)
  model_text <- c("model", model_text)

  log_trace("Writting JAGS model function into: %s", file)
  writeLines(model_text, con = file)
  file
}

#' Extracting Samples from `JAGS` `mcarray` Object
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A simple helper function that extracts a sample from
#'   [`rjags::mcarray.object`] S3 class object. The [`rjags::mcarray.object`]
#'   object is used by the [rjags::jags.samples()] function to represent MCMC
#'   output from a `JAGS` model.
#'
#' @param x an [`rjags::mcarray.object`] object.
#'
#' @export
#'
h_jags_extract_samples <- function(x) {
  assert_class(x, "mcarray")

  x <- x[, , 1L]
  # In case that there are multiple parameters in a node.
  if (is.matrix(x)) {
    x <- t(x)
  }
  x
}
