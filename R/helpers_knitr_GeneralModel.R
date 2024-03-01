#' Render the Parameters of a Normal Distribution
#'
#' @description `r lifecycle::badge("experimental")`
#' @keywords internal
h_knit_print_render_model_params <- function(x, use_values = TRUE, fmt = "%5.2f", ...) {
  assert_class(x, "ModelParamsNormal")
  assert_true(length(x@mean) < 4)

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
  if (length(x@mean) == 2) {
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
    rv <- paste0(
      "N \\left(\\begin{bmatrix}", muAlpha, " \\\\ ", muBeta, "\\end{bmatrix} , ",
      "\\begin{bmatrix*}[S] ", sigma11, " & ", sigma12, " \\\\ ", sigma21, " & ", sigma22,
      "\\end{bmatrix*} \\right)"
    )
  } else {
    mu <- c()
    mu <- sapply(
      1:(length(x@mean)-1),
      function(i) {
        ifelse(
          use_values,
          sprintf(fmt, x@mean[i]),
          paste0("\\mu_{\\alpha_", i, "}")
        )
      }
    )
    sigma <- sapply(
      1:(length(x@mean)-1),
      function(i) {
        ifelse(
          use_values,
          sprintf(fmt, x@cov[i, i]),
          paste0("\\sigma_{\\alpha_", i, "}^2")
        )
      }
    )
    mu[length(mu) + 1] <- ifelse(
      use_values,
      sprintf(fmt, x@mean[length(x@mean)]),
      "\\mu_\\beta"
    )
    sigma[length(sigma) + 1] <- ifelse(
      use_values,
      sprintf(fmt, x@cov[length(x@mean), length(x@mean)]),
      "\\sigma_{\\beta}^2"
    )

    rv <- paste0(
      "N \\left(\\begin{bmatrix}",
      paste(mu, collapse = " \\\\ "),
      "\\end{bmatrix} , ",
      "\\begin{bmatrix*}[S] ",
      paste(
        sapply(
          1:length(x@mean),
          function(i) {
            paste(
              c(
                rep("0", i-1),
                sigma[i],
                rep("0", length(x@mean) - i)
              ),
              collapse = " & "
            )
          }
        ),
        collapse = " \\\\ "
      ),
      "\\end{bmatrix*} \\right)"
    )
  }
  rv
}

knit_print.ModelParamsNormal <- function(x, ..., asis = TRUE, use_values = TRUE, fmt = "%5.2f") {
  result <- paste0(
    "The prior for &theta; is given by\\n",
    "$$ \\theta = \\begin{bmatrix*}[S] ",
    paste(
      paste0(
        c(paste0("\\alpha_", 1:(length(x@mean) -1)), "log(\\beta)"),
        collapse = " \\\\ "
      )
    ),
    "\\end{bmatrix*}",
    " \\sim ",
    h_knit_print_render_model_params(x, use_values = use_values),
    " $$"
  )
  if (asis) {
    result <- knitr::asis_output(result)
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
# DualEndpoint                   Placeholder
# OneParLogNormalPrior
# OneParExpPrior
# LogisticNormal                 Done
# LogisticLogNormal              Done
# ProbitLogNormal                Done
# ProbitLogNormalRel
# LogisticLogNormalGrouped
# LogisticKadaneBetaGamma        Done
# DualEndpointRW                 Placeholder
# DualEndpointBeta               Placeholder
# DualEndpointEmax               Placeholder
# LogisticLogNormalMixture
# DALogisticLogNormal
# TITELogisticLogNormal
# FractionalCRM
# LogisticLogNormalOrdinal       Done

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
    ),
    ""
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
    ifelse(
      use_values,
      paste0("$$ \\Gamma \\sim U(", sprintf(fmt, x@xmin), ", ", sprintf(fmt, x@xmax), ") $$"),
      "$$ \\Gamma \\sim U(x_{min}, x_{max}) $$"
    ),
    " and, independently, ",
    ifelse(
      use_values,
      paste0("$$ \\mathrm{P}_0 \\sim U(0, ", x@theta, ") $$"),
      "$$ \\mathrm{P}_0 \\sim U(0, \\theta) $$"
    ),
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
knit_print.LogisticKadaneBetaGamma <- function(x, ..., asis = TRUE, use_values = TRUE, fmt = "%5.2f", units = NA) {
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
    "describe the relationship between dose and toxicity, using a Beta ",
    "distribution as the prior for &rho;~0~ and a Gamma distribution as the prior ",
    "for &gamma;.\n\n ",
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
    ifelse(
      use_values,
      paste0("$$ \\Gamma \\sim U(", sprintf(fmt, x@shape), ", ", sprintf(fmt, x@rate), ") $$"),
      "$$ \\Gamma \\sim Gamma( \\text{shape}, \\text{rate}) $$"
    ),
    " and, independently, ",
    ifelse(
      use_values,
      paste0("$$ \\mathrm{P}_0 \\sim Beta(", x@alpha, ", ", x@beta, ") $$"),
      "$$ \\mathrm{P}_0 \\sim Beta(\\alpha, \\beta) $$"
    ),
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
  "LogisticKadaneBetaGamma",
  knit_print.LogisticKadaneBetaGamma
)

#' @description `r lifecycle::badge("experimental")`
#' @param biomarker_name (`character`)\n A description of the biomarker
#' @noRd
knit_print.DualEndpoint <- function(x, ..., asis = TRUE, use_values = TRUE, fmt = "%5.2f", units = NA, biomarker_name= "a PD biomarker") {
  assert_flag(asis)
  # Validate
  assert_flag(asis)
  assert_flag(use_values)
  assert_format(fmt)
  assert_character(biomarker_name, len = 1, any.missing = FALSE)
  # Initialise
  if (is.na(units)) {
    units <- ""
  } else {
    units <- paste0(" ", units)
  }
  #Execute
  toxModel <- ProbitLogNormal(
    cov = x@betaZ_params@cov,
    mean = x@betaZ_params@mean,
    ref_dose = x@ref_dose
  )
  rv <- paste0(
    "The relationships between dose and toxicity and between dose and ",
    biomarker_name,
    " will be modelled simultaneously.\n\n",
    knit_print(toxModel, asis = asis, use_values = use_values, fmt = fmt, units = units),
    "\n\n"
  )
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

registerS3method(
  "knit_print",
  "DualEndpoint",
  knit_print.DualEndpoint
)

#' @description `r lifecycle::badge("experimental")`
#' @noRd
h_knit_print_render_model.ModelLogNormal <- function(x, ...) {
  "TO DO"
}

registerS3method(
  "h_knit_print_render_model",
  "ModelLogNormal",
  h_knit_print_render_model.ModelLogNormal
)

#' @description `r lifecycle::badge("experimental")`
#' @noRd
knit_print.OneParLogNormalPrior <- function(x, ..., asis = TRUE) {
  assert_flag(asis)
  rv <- "TODO"
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

registerS3method(
  "knit_print",
  "OneParLogNormalPrior",
  knit_print.OneParLogNormalPrior
)

#' @description `r lifecycle::badge("experimental")`
#' @noRd
knit_print.OneParExpPrior <- function(x, ..., asis = TRUE) {
  assert_flag(asis)
  rv <- "TODO"
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

registerS3method(
  "knit_print",
  "OneParExpPrior",
  knit_print.OneParExpPrior
)

#' @description `r lifecycle::badge("experimental")`
#' @noRd
knit_print.LogisticLogNormalGrouped <- function(x, ..., asis = TRUE) {
  assert_flag(asis)
  rv <- "TODO"
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

registerS3method(
  "knit_print",
  "LogisticLogNormalGrouped",
  knit_print.LogisticLogNormalGrouped
)

#' @description `r lifecycle::badge("experimental")`
#' @noRd
h_knit_print_render_model.LogisticLogNormalOrdinal <- function(x, ...) {
  z <- "e^{\\alpha_k + \\beta \\cdot log(d/d_{ref})}"
  paste0(
    "Let p~k~(d) be the probability that the response of a patient treated at ",
    "dose d is in category k *_or higher_*, k=0, ..., K; d=1, ..., D.\n\nThen ",
    "$$ p_k(d) = f(X \\ge k \\; | \\; \\theta, d) = \\begin{dcases} 1 & k = 0 \\\\ ",
    "\\frac{", z, "}{1 + ", z, "} & k=1, ..., K",
    "\\end{dcases} $$\n\n",
    "where d~ref~ denotes a reference dose.\n\nThe &alpha;s are constrained such that &alpha;~1~ > &alpha;~2~ > ... > &alpha;~K~.\n\n"
  )
}

registerS3method(
  "h_knit_print_render_model",
  "LogisticLogNormalOrdinal",
  h_knit_print_render_model.LogisticLogNormalOrdinal
)
