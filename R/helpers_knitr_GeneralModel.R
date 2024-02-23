#' Render the Parameters of a Normal Distribution
#'
#' @description `r lifecycle::badge("experimental")`
#' @keywords internal
h_knit_print_render_model_params <- function(x, use_values = TRUE, fmt = "%5.2f", ...) {
  assert_class(x, "ModelParamsNormal")
  assert_true(length(x@mean) == 2)

  muAlpha <- ifelse(
    use_values,
    sprintf(fmt, x@mean[1]),
    "\\mu~\\alpha~"
  )
  muBeta <- ifelse(
    use_values,
    sprintf(fmt, x@mean[2]),
    "\\mu;~\\beta~"
  )
  sigma11 <- ifelse(
    use_values,
    sprintf(fmt, x@cov[1, 1]),
    "\\sigma_\\alpha\\sigma_\\alpha"
  )
  sigma22 <- ifelse(
    use_values,
    sprintf(fmt, x@cov[2, 2]),
    "\\sigma_\\beta\\sigma_\\beta"
  )
  sigma12 <- ifelse(
    use_values,
    sprintf(fmt, x@cov[1, 2]),
    "\\rho\\sigma_\\alpha\\sigma_\\beta"
  )
  sigma21 <- ifelse(
    use_values,
    sprintf(fmt, x@cov[2, 1]),
    "\\rho\\sigma_\\beta\\sigma_\\alpha"
  )
  paste0(
    "N \\left(\\begin{bmatrix}", muAlpha, " \\\\ ", muBeta, "\\end{bmatrix} , ",
    "\\begin{bmatrix*}[S] ", sigma11, " & ", sigma12, " \\\\ ", sigma21, " & ", sigma22,
    "\\end{bmatrix*} \\right)"
  )
}

knit_print.ModelParamsNormal <- function(x, ..., asis = TRUE, use_values = TRUE, fmt = "%5.2f") {
  result <- paste0(
    "The prior for &theta; is given by\\n",
    "$$ \\theta = \\begin{bmatrix*}[S] \\alpha \\\\ log(\\beta) \\end{bmatrix*}",
    " \\sim ",
    h_knit_print_render_model_params(x),
    " $$"
  )
  if (asis) {
    knitr::asis_output(result)
  }
  result
}

# registerS3method(
#   "knit_print",
#   "ModelParamsNormal",
#   knit_print.ModelParamsNormal
# )

#' @keywords internal
knit_print.GeneralModel <- function(x, ..., asis = TRUE, use_values = TRUE, fmt = "%5.2f", units = NA) {
  # Validate
  assert_flag(asis)
  assert_flag(use_values)
  assert_format(fmt)
  # Execute
  rv <- paste0(
    h_knit_print_render_model(x, use_values = use_values, fmt = fmt),
    knit_print(x@params, ..., asis = asis, use_values = use_values, fmt = fmt),
    "\\n\\n",
    h_knit_print_render_ref_dose(x, use_values = use_values, fmt = fmt, unit = unit)
  )
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

registerS3method(
  "knit_print",
  "GeneralModel",
  knit_print.GeneralModel
)

# ModelLogNormal
# LogisticLogNormalSub           Done
# LogisticKadane                 Done
# LogisticNormalFixedMixture     Done
# DualEndpoint                   Paceholder
# OneParLogNormalPrior
# OneParExpPrior
# LogisticNormal                 Done
# LogisticLogNormal              Done
# ProbitLogNormal                Done
# ProbitLogNormalRel
# LogisticLogNormalGrouped
# LogisticKadaneBetaGamma
# DualEndpointRW                 Placeholder
# DualEndpointBeta               Placeholder
# DualEndpointEmax               Placeholder
# LogisticLogNormalMixture
# DALogisticLogNormal
# TITELogisticLogNormal
# FractionalCRM
# LogisticLogNormalOrdinal

#' Obtain a Text Representation of the Reference Dose
#'
#' This is a helper method used `knit_print` for `crmPack` classes.
#'
#' @param x (`GeneralModel`)\cr the model object that will be printed
#' @param ...\cr Not used at present
#' @return A character string containing a LaTeX rendition of the model.
#' @noRd
h_knit_print_render_ref_dose <- function(x, ...) {
  UseMethod("h_knit_print_render_ref_dose")
}

#' @keywords internal
h_knit_print_render_ref_dose.GeneralModel <- function(x, ..., use_values = TRUE, fmt = "%5.2f", units = NA) {
  # Validate
  assert_character(units, len = 1)
  # Initialise
  if (is.na(units)) {
    units <- ""
  } else {
    units <- paste0(" ", units)
  }
  # Execute
  ref_dose <- ifelse(
    use_values,
    paste0(
      " The reference dose will be ",
      stringr::str_trim(sprintf(fmt, x@ref_dose)),
      units,
      ".\n\n"
    )
  )
  ref_dose
}

registerS3method(
  "h_knit_print_render_ref_dose",
  "GeneralModel",
  h_knit_print_render_ref_dose.GeneralModel
)

#' @keywords internal
h_knit_print_render_ref_dose.LogisticKadane <- function(x, ...) {
  # The LogisticKadane class has no reference dose slot
  ""
}

registerS3method(
  "h_knit_print_render_ref_dose",
  "LogisticKadane",
  h_knit_print_render_ref_dose.LogisticKadane
)

#' Render a Model Function in a `knit_print` Method
#'
#' This is a helper method used `knit_print` for `crmPack` classes.
#'
#' @param x (`GeneralModel`)\cr the model object that will be printed
#' @param ...\cr Not used at present
#' @return A character string containing a LaTeX rendition of the model.
#' @noRd
h_knit_print_render_model <- function(x, ...) {
  UseMethod("h_knit_print_render_model")
}

#' @description `r lifecycle::badge("experimental")`
#' @noRd
h_knit_print_render_model.LogisticLogNormal <- function(x, ...) {
  z <- "e^{\\alpha + \\beta \\cdot log(d/d_{ref})}"
  paste0(
    "A logistic log normal model will describe the relationship between dose and toxicity: ",
    "$$ p(Tox | d) = f(X = 1 | \\theta, d) = \\frac{", z, "}{1 + ", z, "} $$\\n ",
    "where d~ref~ denotes a reference dose.\n\n"
  )
}

registerS3method(
  "h_knit_print_render_model",
  "LogisticLogNormal",
  h_knit_print_render_model.LogisticLogNormal
)

#' @description `r lifecycle::badge("experimental")`
#' @noRd
h_knit_print_render_model.LogisticLogNormalSub <- function(x, ...) {
  z <- "e^{\\alpha + \\beta \\cdot (d \\, - \\, d_{ref})}"
  paste0(
    "A logistic log normal model with subtractive dose normalisation will ",
    "describe the relationship between dose and toxicity: \n\n",
    "$$ p(Tox | d) = f(X = 1 | \\theta, d) = \\frac{", z, "}{1 + ", z, "} $$\\n ",
    "where d~ref~ denotes a reference dose.\n\n"
  )
}

registerS3method(
  "h_knit_print_render_model",
  "LogisticLogNormalSub",
  h_knit_print_render_model.LogisticLogNormalSub
)

#' @description `r lifecycle::badge("experimental")`
#' @noRd
h_knit_print_render_model.LogisticNormal <- function(x, ...) {
  z <- "e^{\\alpha + \\beta \\cdot d/d_{ref}}"
  paste0(
    "A logistic log normal model will describe the relationship between dose and toxicity: ",
    "$$ p(Tox | d) = f(X = 1 | \\theta, d) = \\frac{", z, "}{1 + ", z, "} $$\\n ",
    "where d~ref~ denotes a reference dose.\n\n"
  )
}

registerS3method(
  "h_knit_print_render_model",
  "LogisticNormal",
  h_knit_print_render_model.LogisticNormal
)

#' @description `r lifecycle::badge("experimental")`
#' @noRd
h_knit_print_render_model.ProbitLogNormal <- function(x, ...) {
  paste0(
    "A probit log normal model will describe the relationship between dose and toxicity: ",
    "$$ \\Phi^{-1}(Tox | d) = f(X = 1 | \\theta, d) = \\alpha + \\beta \\cdot log(d/d_{ref}) $$\\n ",
    "where d~ref~ denotes a reference dose.\n\n"
  )
}

registerS3method(
  "h_knit_print_render_model",
  "ProbitLogNormal",
  h_knit_print_render_model.ProbitLogNormal
)

#' @description `r lifecycle::badge("experimental")`
#' @noRd
h_knit_print_render_model.ProbitLogNormalRel <- function(x, ...) {
  paste0(
    "A probit log normal model will describe the relationship between dose and toxicity: ",
    "$$ \\Phi^{-1}(Tox | d) = f(X = 1 | \\theta, d) = \\alpha + \\beta \\cdot d/d_{ref} $$\\n ",
    "where d~ref~ denotes a reference dose.\n\n"
  )
}

registerS3method(
  "h_knit_print_render_model",
  "ProbitLogNormalRel",
  h_knit_print_render_model.ProbitLogNormalRel
)

#' @description `r lifecycle::badge("experimental")`
#' @noRd
h_knit_print_render_model.LogisticNormalMixture <- function(x, ...) {
  z <- "e^{\\alpha + \\beta \\cdot log(d/d_{ref})}"
  paste0(
    "A mixture of two logistic log normal models will describe the relationship between dose and toxicity: ",
    "$$ p(Tox | d) = f(X = 1 | \\theta, d) = \\frac{", z, "}{1 + ", z, "} $$\\n ",
    "where d~ref~ denotes a reference dose.\n\n"
  )
}

registerS3method(
  "h_knit_print_render_model",
  "LogisticNormalMixture",
  h_knit_print_render_model.LogisticNormalMixture
)

#' @keywords internal
knit_print.LogisticNormalMixture <- function(x, ..., asis = TRUE, use_values = TRUE, fmt = "%5.2f", units = NA) {
  # Validate
  assert_flag(asis)
  assert_flag(use_values)
  assert_format(fmt)
  # Execute
  rv <- paste0(
    h_knit_print_render_model(x, use_values = use_values, fmt = fmt),
    "The prior for &theta; is given by\\n",
    "$$ \\theta = \\begin{bmatrix*}[S] \\alpha \\\\ log(\\beta) \\end{bmatrix*}",
    " \\sim ",
    "w \\cdot ",
    h_knit_print_render_model_params(x@comp1),
    " + (1 - w) \\cdot ",
    h_knit_print_render_model_params(x@comp2),
    " $$\\n\\n",
    " and the prior for w is given by \n\n",
    " $$ w \\sim Beta(", x@weightpar[1], ", ", x@weightpar[2], ") $$\\n\\n",
    h_knit_print_render_ref_dose(x, units = units, fmt = fmt, use_values = use_values, ...)
  )
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

registerS3method(
  "knit_print",
  "LogisticNormalMixture",
  knit_print.LogisticNormalMixture
)

#' @keywords internal
knit_print.LogisticNormalFixedMixture <- function(x, ..., asis = TRUE, use_values = TRUE, fmt = "%5.2f", units = NA) {
  # Validate
  assert_flag(asis)
  assert_flag(use_values)
  assert_format(fmt)
  # Execute
  rv <- paste0(
    h_knit_print_render_model(x, use_values = use_values, fmt = fmt),
    " The prior for &theta; is given by\\n\\n",
    "$$ \\theta = \\begin{bmatrix*}[S] \\alpha \\\\ log(\\beta) \\end{bmatrix*}",
    " \\sim \\sum_{i=1}^{", length(x@components),"}",
    "w_i \\cdot N \\left( \\mathbf{\\mu}_i ,  \\mathbf{\\Sigma}_i \\right)",
    " $$ \\n\\n",
    " with \\n\\n",
    "$$ \\sum_{i=1}^{", length(x@components),"} w_i = 1 $$ \\n\\n",
    " The individual components of the mixture are "
  )
  for (i in seq_along(x@components)) {
    comp <- x@components[[i]]
    rv <- paste0(
      rv,
      " \\n\\n $$ ", h_knit_print_render_model_params(comp, use_values, fmt, asis = FALSE), " \\text{ with weight ", x@weights[i], "} $$ ",
      ifelse(
        i < length(x@components),
        " and",
        " "
      )
    )
  }
  rv <- paste0(rv, " \\n\\n ", h_knit_print_render_ref_dose(x, units = units, fmt = fmt, use_values = use_values, ...))
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

registerS3method(
  "knit_print",
  "LogisticNormalFixedMixture",
  knit_print.LogisticNormalFixedMixture
)

#' @description `r lifecycle::badge("experimental")`
#' @noRd
h_knit_print_render_model.LogisticNormalFixedMixture <- function(x, ...) {
  z <- "e^{\\alpha + \\beta \\cdot log(d/d_{ref})}"
  paste0(
    "A mixture of ",
    length(x@components),
    " logistic log normal models with fixed weights will describe the relationship ",
    "between dose and toxicity: ",
    "$$ p(Tox | d) = f(X = 1 | \\theta, d) = \\frac{", z, "}{1 + ", z, "} $$\\n ",
    "where d~ref~ denotes a reference dose.\n\n"
  )
}

registerS3method(
  "h_knit_print_render_model",
  "LogisticNormalFixedMixture",
  h_knit_print_render_model.LogisticNormalFixedMixture
)

#' @description `r lifecycle::badge("experimental")`
#' @noRd
knit_print.LogisticKadane <- function(x, ..., asis = TRUE, use_values = TRUE, fmt = "%5.2f", units = NA) {
  # Validate
  assert_flag(asis)
  assert_flag(use_values)
  assert_format(fmt)
  # Initialise
  if (is.na(units)) {
    units <- ""
  } else {
    units <- paste0(" ", units)
  }
  #Execute
  rv <- paste0(
    "A logistic model using the parameterisation of Kadane (1980)  will ",
    "describe the relationship between dose and toxicity.\n\n ",
    ifelse(
      use_values,
      paste0(
        "Let the minimum (x~min~) and maximum (x~max~) doses be ",
        paste0(stringr::str_trim(sprintf(fmt, x@xmin)), units),
        " and ",
        paste0(stringr::str_trim(sprintf(fmt, x@xmax)), units),
        ".\n\n"
      ),
      "Let x~min~ and x~max~ denote, respectively, the minimum and maximum doses.\n\n  "
    ),
    "Further, let &theta; denote the target toxicity rate and &rho;~0~ = p(DLT | D = x~min~).\n\n",
    "Let &gamma; be the dose with target toxicity rate &theta;, so that p(DLT | D = &gamma;) = &theta;",
    ifelse(
      use_values,
      paste0(" = ", x@theta,".\n\n"),
      ".\n\n"
    ),
    "Using this parameterisation, standard logistic regression model has slope ",
    "$$ \\frac{\\gamma \\text{logit}(\\rho_0) - x_{min} \\text{logit}(\\theta)}{\\gamma - x_{min}} $$",
    " and intercept ",
    "$$ \\frac{\\text{logit}(\\theta) - logit(\\rho_0)}{\\gamma - x_{min}} $$",
    " The priors for &Gamma; and &Rho;~0~ are ",
    # ifelse(
    #   use_values,
    #   paste0("$$ \\Gamma \\sim U(", sprintf(fmt, x@xmin), ", ", sprintf(fmt, x@xmax), ") $$"),
    #   "$$ \\Gamma \\sim U(x_{min}, x_{max}) $$"
    # ),
    # " and, independently, ",
    # ifelse(
    #   use_values,
    #   paste0("$$ \\mathrm{P}_0 \\sim U(0, ", x@theta, ") $$"),
    #   "$$ \\mathrm{P}_0 \\sim U(0, \\theta) $$"
    # ),
    "\n\n Note that x~min~ and x~max~ need not be equal to the smallest and ",
    "largest values in the `doseGrid` slot of the corresponding `Data` object.\n\n"
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

registerS3method(
  "knit_print",
  "LogisticKadane",
  knit_print.LogisticKadane
)

#' @description `r lifecycle::badge("experimental")`
#' @noRd
knit_print.DualEndpoint <- function(x, ...) {
  "TO DO"
}

registerS3method(
  "knit_print",
  "DualEndpoint",
  knit_print
)
