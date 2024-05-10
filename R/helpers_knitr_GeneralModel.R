# Generics ----

#' Obtain a Text Representation of the Reference Dose
#'
#' This is a helper method used `knit_print` for `crmPack` classes.
#'
#' @param x (`GeneralModel`)\cr the model object that will be printed
#' @param ... Not used at present
#' @return A character string containing a LaTeX rendition of the model.
#' @noRd
h_knit_print_render_ref_dose <- function(x, ...) {
  UseMethod("h_knit_print_render_ref_dose")
}

#' Render a Model Function in a `knit_print` Method
#'
#' This is a helper method used `knit_print` for `crmPack` classes.
#'
#' @param x (`GeneralModel`)\cr the model object that will be printed
#' @param ... Not used at present
#' @return A character string containing a LaTeX rendition of the model.
#' @noRd
h_knit_print_render_model <- function(x, ...) {
  UseMethod("h_knit_print_render_model")
}

#' Obtain a Text Representation of a Biomarker Model
#'
#' This is a helper method used `knit_print` for `DualEndpoint` classes.
#'
#' @param x (`DualEndpoint`)\cr the model object containing the biomarker model
#' @param use_values (`flag`)\cr print the values associated with hyperparameters,
#' or the symbols used to define the hyper-parameters.  That is, for example, mu or 1.
#' @param ... Not used at present
#' @return A character string containing a LaTeX rendition of the model.
#' @noRd
h_knit_print_render_biomarker_model <- function(x, use_values = TRUE, ...) {
  UseMethod("h_knit_print_render_biomarker_model")
}

# Methods ----

# DualEndpoint ----

#' @description `r lifecycle::badge("experimental")`
#' @inheritParams knit_print.StoppingTargetProb
#' @param biomarker_name (`character`)\cr A description of the biomarker
#' @rdname knit_print
#' @export
#' @method knit_print DualEndpoint
knit_print.DualEndpoint <- function(
    x,
    ...,
    asis = TRUE,
    use_values = TRUE,
    fmt = "%5.2f",
    units = NA,
    tox_label = "toxicity",
    biomarker_name = "PD biomarker") {
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
  # Execute
  toxModel <- ProbitLogNormal(
    cov = x@betaZ_params@cov,
    mean = x@betaZ_params@mean,
    ref_dose = x@ref_dose
  )
  rv <- paste0(
    "The relationships between dose and ",
    tox_label,
    " and between dose and ",
    biomarker_name,
    " will be modelled simultaneously.\n\n",
    knit_print(
      toxModel,
      asis = asis,
      tox_label = tox_label,
      use_values = use_values,
      fmt = fmt,
      units = units
    ),
    "\n\n",
    "The ",
    biomarker_name,
    " response `w` at dose `d` is modelled as ",
    "$$ w(d) \\sim N(f(d), \\sigma_w^2) $$ \n\nwhere ",
    h_knit_print_render_biomarker_model(x, use_values = use_values)
  )
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

#' @description `r lifecycle::badge("experimental")`
#' @noRd
h_knit_print_render_biomarker_model.DualEndpoint <- function(x, ..., use_values = TRUE) {
  "f(d) is a function of dose that is defined elsewhere."
}

# DualEndpointBeta ----

#' @description `r lifecycle::badge("experimental")`
#' @noRd
h_knit_print_render_biomarker_model.DualEndpointBeta <- function(x, ...) {
  paste0(
    "f(d) is a parametric rescaled beta density function such that\n\n",
    "$$ f(d) = ",
    "E_0 + (E_{max} - E_0) \\times Beta(\\delta_1, \\delta_2) \\times ",
    "\\left(\\frac{d}{d_{max}}\\right)^{\\delta_1}  \\times \\left(1 - ",
    "\\frac{d}{d_{max}}\\right)^{\\delta_2} $$\n\n",
    "where d~max~ is the maximum dose in the dose grid, &delta;~1~ and ",
    "&delta;~2~ are the parameters of the Beta function and ",
    "E~0~ and E~max~ are, respectively, the minimum and maximum levels of the ",
    "biomarker.  The mode can be written as \n\n",
    "$$ \\text{mode} = \\frac{\\delta_1}{\\delta_1 + \\delta_2} $$\n\n",
    " and this is the parameterisation used to define the model.\n\n",
    "In this case, \n\n",
    ifelse(
      length(x@E0) == 1,
      paste0("$$ E_0 = ", x@E0, " $$\n\n)"),
      paste0("$$ E_0  \\sim U(", x@E0[1], ", ", x@E0[2], ") $$\n\n")
    ),
    ifelse(
      length(x@Emax) == 1,
      paste0("$$ E_{max} = ", x@Emax, " $$\n\n)"),
      paste0("$$ E_{max}  \\sim U(", x@Emax[1], ", ", x@Emax[2], ") $$\n\n")
    ),
    ifelse(
      length(x@delta1) == 1,
      paste0("$$ \\delta_1 = ", x@delta1, " $$\n\n)"),
      paste0("$$ \\delta_1  \\sim U(", x@delta1[1], ", ", x@delta1[2], ") $$\n\n")
    ),
    ifelse(
      length(x@mode) == 1,
      paste0("$$ \\text{mode} = ", x@mode, " $$\n\n)"),
      paste0("$$ \\text{mode}  \\sim U(", x@mode[1], ", ", x@mode[2], ") $$\n\n")
    ),
    " and \n\n",
    ifelse(
      length(x@ref_dose_beta) == 1,
      paste0("$$ d_{max} = ", x@ref_dose_beta, " $$\n\n"),
      paste0("$$ d_{max}  \\sim U(", x@ref_dose_beta[1], ", ", x@ref_dose_beta[2], ") $$\n\n")
    )
  )
}

# DualEndpointEmax ----

#' @description `r lifecycle::badge("experimental")`
#' @noRd
h_knit_print_render_biomarker_model.DualEndpointEmax <- function(x, ...) {
  paste0(
    "f(d) is a parametric Emax density function such that\n\n",
    "$$ f(d) = ",
    "E_0 + \\frac{(E_{max} - E_0) \\times \\frac{d}{d^*}}{\\text{ED}_{50} + \\frac{d}{d^*}} $$\n\n",
    "where d* is the reference dose, E~0~ and E~max~ are, respectively, the ",
    "minimum and maximum levels of the biomarker and ED~50~ is the dose achieving ",
    "half the maximum effect, 0.5 &times; E~max~.\n\n",
    "In this case, \n\n",
    ifelse(
      length(x@E0) == 1,
      paste0("$$ E_0 = ", x@E0, " $$\n\n)"),
      paste0("$$ E_0  \\sim U(", x@E0[1], ", ", x@E0[2], ") $$\n\n")
    ),
    ifelse(
      length(x@Emax) == 1,
      paste0("$$ E_{max} = ", x@Emax, " $$\n\n)"),
      paste0("$$ E_{max}  \\sim U(", x@Emax[1], ", ", x@Emax[2], ") $$\n\n")
    ),
    ifelse(
      length(x@ED50) == 1,
      paste0("$$ \\text{ED}_{50} = ", x@ED50, " $$\n\n)"),
      paste0("$$ \\text{ED}_{50}  \\sim U(", x@ED50[1], ", ", x@ED50[2], ") $$\n\n")
    ),
    " and \n\n",
    ifelse(
      length(x@ref_dose_emax) == 1,
      paste0("$$ d^* = ", x@ref_dose_emax, " $$\n\n"),
      paste0("$$ d^* \\sim U(", x@ref_dose_emax[1], ", ", x@ref_dose_emax[2], ") $$\n\n")
    )
  )
}

# DualEndpointRW ----

#' @description `r lifecycle::badge("experimental")`
#' @noRd
h_knit_print_render_biomarker_model.DualEndpointRW <- function(x, ..., use_values = TRUE) {
  paste0(
    "f(d) is a ",
    ifelse(x@rw1, "first", "second"),
    " order random walk such that\n\n",
    "$$ f(d) = ",
    "\\beta_{W_i} - \\beta_{W_{i - ",
    ifelse(x@rw1, "1", "2"),
    "}}",
    "\\sim N(0, ",
    ifelse(x@rw1, "", "2 \\times "),
    ifelse(
      use_values & length(x@sigma2betaW) == 1,
      x@sigma2betaW,
      "\\sigma_{\\beta_W}^2"
    ),
    " \\times (d_i - d_{i - ",
    ifelse(x@rw1, "1", "2"),
    "})",
    ")",
    " $$\n\n",
    ifelse(
      length(x@sigma2betaW) == 1,
      ifelse(
        use_values,
        "",
        paste0(" and $\\sigma_{\\beta_W}^2$ is fixed at ", x@sigma2betaW)
      ),
      paste0(
        " and the prior for $\\sigma_{\\beta_W}^2$ is an inverse-gamma distribution with parameters ",
        "a = ",
        x@sigma2betaW["a"],
        " and b = ",
        x@sigma2betaW["b"]
      )
    )
  )
}

# ModelParamsNormal ----

#' Render a Normal Prior
#'
#' @param x (`ModelParamsNormal`)\cr the object to be rendered
#' @param use_values (`flag`)\cr print the values associated with hyperparameters,
#' or the symbols used to define the hyper-parameters.  That is, for example, mu or 1.
#' @param fmt (`character`)\cr the `sprintf` format string used to render
#' numerical values.  Ignored if `use_values` is `FALSE`.
#' @param params (`character`)\cr The names of the model parameters.  See Usage
#' Notes below.
#' @param preamble (`character`)\cr The text used to introduce the LaTeX representation
#' of the model
#' @param asis (`flag`)\cr wrap the return value in a call to `knitr::asis_output`?
#' @param theta (`character`)\cr the LaTeX representation of the theta vector
#' @param ... Not used at present
#' @section Usage Notes:
#' `params` must be a character vector of length equal to that of `x@mean` (and
#' `x@cov`).  Its values represent the parameters of the model as entries in the
#' vector `theta`, on the left-hand side of "~" in the definition of the prior.
#' If named, names should be valid LaTeX, escaped as usual for R character variables.
#' For example, `"\\alpha"` or `"\\beta_0"`.  If unnamed, names are constructed by
#' pre-pending an escaped backslash to each value provided.
#' @return A character string containing a LaTeX rendition of the object.
#' @description `r lifecycle::badge("experimental")`
#' @export
#' @rdname knit_print
#' @method knit_print ModelParamsNormal
knit_print.ModelParamsNormal <- function(
    x,
    use_values = TRUE,
    fmt = "%5.2f",
    params = c("alpha", "beta"),
    preamble = "The prior for &theta; is given by\\n",
    asis = TRUE,
    theta = "\\theta",
    ...) {
  # Validate
  assert_class(x, "ModelParamsNormal")
  assert_format(fmt)
  assert_character(preamble, len = 1)
  assert_true(length(x@mean) == length(params))
  assert_flag(asis)
  # Initialise
  n <- length(params)
  if (is.null(names(params))) {
    names(params) <- paste0("\\", params)
  }
  # Execute
  # Construct LaTeX representation of mean vector
  mu <- sapply(
    1:n,
    function(i) {
      ifelse(
        use_values,
        sprintf(fmt, x@mean[i]),
        paste0("\\mu_{\\", params[i], "}")
      )
    }
  )
  # Construct LaTeX representation of covariance matrix
  cov <- sapply(
    1:n,
    function(i) {
      sapply(
        1:n,
        function(j) {
          ifelse(
            use_values,
            sprintf(fmt, x@cov[i, j]),
            ifelse(
              i == j,
              paste0("\\sigma_{\\", params[i], "}^2"),
              paste0("\\rho\\sigma_{\\", params[i], "}\\sigma_{\\", params[j], "}")
            )
          )
        }
      )
    }
  )
  # Construct LaTeX representation of prior
  rv <- paste0(
    preamble,
    "$$ \\boldsymbol",
    theta,
    " = \\begin{bmatrix}",
    paste0(names(params), collapse = " \\\\ "),
    "\\end{bmatrix}",
    "\\sim N \\left(\\begin{bmatrix}",
    paste0(mu, collapse = " \\\\ "),
    "\\end{bmatrix} , ",
    "\\begin{bmatrix} ",
    paste0(
      sapply(
        1:n,
        function(j) {
          stringr::str_trim(paste0(cov[, j], collapse = " & "))
        }
      ),
      collapse = " \\\\ "
    ),
    "\\end{bmatrix}",
    " \\right)",
    " $$\n\n"
  )
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# GeneralModel ----

#' @export
#' @rdname knit_print
#' @method knit_print GeneralModel
knit_print.GeneralModel <- function(
    x,
    ...,
    params = c("alpha", "beta"),
    asis = TRUE,
    use_values = TRUE,
    fmt = "%5.2f",
    units = NA) {
  # Validate
  assert_flag(asis)
  assert_flag(use_values)
  assert_format(fmt)
  # Execute
  rv <- paste0(
    h_knit_print_render_model(x, use_values = use_values, fmt = fmt),
    knit_print(x@params, ..., asis = asis, use_values = use_values, fmt = fmt, params = params),
    "\\n\\n",
    h_knit_print_render_ref_dose(x, use_values = use_values, fmt = fmt, unit = unit)
  )
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
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

# LogisticKadane ----

#' @keywords internal
h_knit_print_render_ref_dose.LogisticKadane <- function(x, ...) {
  # The LogisticKadane class has no reference dose slot
  ""
}

#' @description `r lifecycle::badge("experimental")`
#' @rdname knit_print
#' @export
#' @method knit_print LogisticKadane
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
  # Execute
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
      paste0(" = ", x@theta, ".\n\n"),
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

# LogisticKadaneBetaGamma ----

#' @description `r lifecycle::badge("experimental")`
#' @rdname knit_print
#' @export
#' @method knit_print LogisticKadaneBetaGamma
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
  # Execute
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
      paste0(" = ", x@theta, ".\n\n"),
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

# LogisticLogNormal ----

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

#' @description `r lifecycle::badge("experimental")`
#' @rdname knit_print
#' @export
#' @method knit_print LogisticLogNormal
knit_print.LogisticLogNormal <- function(
    x,
    ...,
    use_values = TRUE,
    fmt = "%5.2f",
    params = c(
      "\\alpha" = "alpha",
      "log(\\beta)" = "beta"
    ),
    preamble = "The prior for &theta; is given by\\n",
    asis = TRUE) {
  assert_flag(asis)
  # Can't use NextMethod() on a S4 class
  knit_print.GeneralModel(
    x,
    ...,
    use_values = use_values,
    fmt = fmt,
    params = params,
    preamble = preamble,
    asis = asis
  )
}

# LogisticLogNormalMixture ----

#' @description `r lifecycle::badge("experimental")`
#' @noRd
h_knit_print_render_model.LogisticLogNormalMixture <- function(x, use_values = TRUE, ...) {
  z1 <- "e^{\\alpha_1 + \\beta_1 \\cdot log(d/d^*)}"
  z2 <- "e^{\\alpha_2 + \\beta_2 \\cdot log(d/d^*)}"
  pi_text <- ifelse(
    use_values,
    x@share_weight,
    "\\pi"
  )
  paste0(
    "A mixture of two logistic log normal models will describe the relationship between dose and toxicity: ",
    "$$ p(Tox | d) = f(X = 1 | \\theta, d) = ",
    pi_text,
    " \\times \\frac{", z1, "}{1 + ", z1, "} + (1 - ",
    pi_text,
    ") \\times \\frac{", z2, "}{1 + ", z2, "} $$",
    ifelse(
      use_values,
      "where d* denotes a reference dose.\n\n",
      "where d* denotes a reference dose and &pi; is a fixed value between 0 and 1.\n\n"
    )
  )
}

#' @export
#' @rdname knit_print
#' @method knit_print LogisticLogNormalMixture
knit_print.LogisticLogNormalMixture <- function(x, ..., asis = TRUE, use_values = TRUE, fmt = "%5.2f", units = NA) {
  # Validate
  assert_flag(asis)
  assert_flag(use_values)
  assert_format(fmt)
  # Execute
  rv <- paste0(
    h_knit_print_render_model(x, use_values = use_values, fmt = fmt),
    knit_print(
      x@params,
      ...,
      asis = asis,
      use_values = use_values,
      fmt = fmt,
      preamble = "The priors for both &theta;~1~ and &theta;~2~ are given by\\n"
    ),
    "\\n\\n",
    h_knit_print_render_ref_dose(x, use_values = use_values, fmt = fmt, unit = unit)
  )
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# LogisticLogNormalSub ----

#' @description `r lifecycle::badge("experimental")`
#' @noRd
h_knit_print_render_model.LogisticLogNormalSub <- function(x, ...) {
  z <- "e^{\\alpha + \\beta \\cdot (d \\, - \\, d^*)}"
  paste0(
    "A logistic log normal model with subtractive dose normalisation will ",
    "describe the relationship between dose and toxicity: \n\n",
    "$$ p(Tox | d) = f(X = 1 | \\theta, d) = \\frac{", z, "}{1 + ", z, "} $$\\n ",
    "where d* denotes a reference dose.\n\n"
  )
}

#' @description `r lifecycle::badge("experimental")`
#' @rdname knit_print
#' @export
#' @method knit_print LogisticLogNormalSub
knit_print.LogisticLogNormalSub <- function(
    x,
    ...,
    use_values = TRUE,
    fmt = "%5.2f",
    params = c(
      "\\alpha" = "alpha",
      "log(\\beta)" = "beta"
    ),
    preamble = "The prior for &theta; is given by\\n",
    asis = TRUE) {
  NextMethod(params = params)
}

# LogisticNormal ----

#' @description `r lifecycle::badge("experimental")`
#' @noRd
h_knit_print_render_model.LogisticNormal <- function(x, ...) {
  z <- "e^{\\alpha + \\beta \\cdot d/d^*}"
  paste0(
    "A logistic log normal model will describe the relationship between dose and toxicity: ",
    "$$ p(Tox | d) = f(X = 1 | \\theta, d) = \\frac{", z, "}{1 + ", z, "} $$\\n ",
    "where d* denotes a reference dose.\n\n"
  )
}

# ProbitLogNormal ----

#' @description `r lifecycle::badge("experimental")`
#' @noRd
h_knit_print_render_model.ProbitLogNormal <- function(x, ..., tox_label = "toxicity") {
  paste0(
    "A probit log normal model will describe the relationship between dose and ",
    tox_label,
    ": ",
    "$$ \\Phi^{-1}(Tox | d) = f(X = 1 | \\theta, d) = \\alpha + \\beta \\cdot log(d/d^*) $$\\n ",
    "where d* denotes a reference dose.\n\n"
  )
}

# ProbitLogNormalRel ----

#' @description `r lifecycle::badge("experimental")`
#' @noRd
h_knit_print_render_model.ProbitLogNormalRel <- function(
    x,
    ...,
    tox_label = "toxicity",
    asis = TRUE
) {
  assert_flag(asis)
  paste0(
    "A probit log normal model will describe the relationship between dose and ",
    tox_label,
    ": ",
    "$$ \\Phi^{-1}(Tox | d) = f(X = 1 | \\theta, d) = \\alpha + \\beta \\cdot d/d^* $$\\n ",
    "where d* denotes a reference dose.\n\n"
  )
}

# LogisticNormalMixture ----

#' @description `r lifecycle::badge("experimental")`
#' @noRd
h_knit_print_render_model.LogisticNormalMixture <- function(x, ...) {
  z <- "e^{\\alpha + \\beta \\cdot log(d/d^*)}"
  paste0(
    "A mixture of two logistic log normal models will describe the relationship between dose and toxicity: ",
    "$$ p(Tox | d) = f(X = 1 | \\theta, d) = \\frac{", z, "}{1 + ", z, "} $$\\n ",
    "where d* denotes a reference dose.\n\n"
  )
}

#' @export
#' @rdname knit_print
#' @method knit_print LogisticNormalMixture
knit_print.LogisticNormalMixture <- function(x, ..., asis = TRUE, use_values = TRUE, fmt = "%5.2f", units = NA) {
  # Validate
  assert_flag(asis)
  assert_flag(use_values)
  assert_format(fmt)
  # Execute
  rv <- paste0(
    h_knit_print_render_model(x, use_values = use_values, fmt = fmt),
    "The prior for &theta; is given by\\n",
    "$$ \\theta = \\begin{bmatrix} \\alpha \\\\ log(\\beta) \\end{bmatrix}",
    " \\sim ",
    "w \\cdot ",
    knit_print(
      x@comp1,
      params = c("\\alpha" = "alpha", "\\beta" = "beta")
    ),
    " + (1 - w) \\cdot ",
    knit_print(
      x@comp2,
      params = c("\\alpha" = "alpha", "\\beta" = "beta")
    ),
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

# LogisticNormalFixedMixture ----

#' @export
#' @rdname knit_print
#' @method knit_print LogisticNormalFixedMixture
knit_print.LogisticNormalFixedMixture <- function(x, ..., asis = TRUE, use_values = TRUE, fmt = "%5.2f", units = NA) {
  # Validate
  assert_flag(asis)
  assert_flag(use_values)
  assert_format(fmt)
  # Execute
  beta <- ifelse(x@log_normal, "log(\\beta)", "\\beta")
  rv <- paste0(
    h_knit_print_render_model(x, use_values = use_values, fmt = fmt),
    " The prior for &theta; is given by\\n\\n",
    "$$ \\theta = \\begin{bmatrix} \\alpha \\\\ ", beta, " \\end{bmatrix}",
    " \\sim \\sum_{i=1}^{", length(x@components), "}",
    "w_i \\cdot N \\left( \\mathbf{\\mu}_i ,  \\mathbf{\\Sigma}_i \\right)",
    " $$ \\n\\n",
    " with \\n\\n",
    "$$ \\sum_{i=1}^{", length(x@components), "} w_i = 1 $$ \\n\\n",
    " The individual components of the mixture are "
  )
  if (x@log_normal) {
    params <- c("\\alpha" = "alpha", "log(\\beta)" = "beta")
  } else {
    params <- c("\\alpha" = "alpha", "\\beta" = "beta")
  }
  for (i in seq_along(x@components)) {
    comp <- x@components[[i]]
    rv <- paste0(
      rv,
      knit_print(
        comp,
        params = params,
        preamble = " ",
        use_values = use_values,
        fmt = fmt,
        theta = paste0("\\theta_", i)
      ),
      " with weight ", x@weights[i],
      ifelse(
        i < length(x@components),
        " and",
        " "
      )
    )
  }
  rv <- paste0(
    rv,
    " \\n\\n ",
    h_knit_print_render_ref_dose(x, units = units, fmt = fmt, use_values = use_values, ...)
  )
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

#' @description `r lifecycle::badge("experimental")`
#' @noRd
h_knit_print_render_model.LogisticNormalFixedMixture <- function(x, ...) {
  z <- "e^{\\alpha + \\beta \\cdot log(d/d^*)}"
  paste0(
    "A mixture of ",
    length(x@components),
    " logistic log normal models with fixed weights will describe the relationship ",
    "between dose and toxicity: ",
    "$$ p(Tox | d) = f(X = 1 | \\theta, d) = \\frac{", z, "}{1 + ", z, "} $$\\n ",
    "where d* denotes a reference dose.\n\n"
  )
}

# ModelLogNormal ----

#' @description `r lifecycle::badge("experimental")`
#' @noRd
h_knit_print_render_model.ModelLogNormal <- function(x, ...) {
  "The model used to characterise the dose toxicity relationship is defined in  subclasses.\n\n"
}

# OneParLogNormalPrior ----

#' @description `r lifecycle::badge("experimental")`
#' @rdname knit_print
#' @export
#' @method knit_print OneParLogNormalPrior
knit_print.OneParLogNormalPrior <- function(x, ..., asis = TRUE, use_values = TRUE, fmt = "%5.2f") {
  assert_flag(asis)

  s2text <- ifelse(
    use_values,
    stringr::str_trim(sprintf(fmt, x@sigma2)),
    "\\sigma^2"
  )
  rv <- paste0(
    "The relationship between dose and toxicity will be modelled using a version ",
    "of the one parameter CRM of O'Quigley et al (1990) with an exponential prior on the ",
    "power parameter for the skeleton prior probabilities, with",
    ifelse(
      use_values,
      paste0("$$ \\Theta \\sim Exp(", s2text, ") $$"),
      "$$ \\Theta \\sim Exp(\\lambda) $$"
    ),
    "and skeleton probabilities as in the table below.\n\n"
  )
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# OneParExpPrior ----

#' @description `r lifecycle::badge("experimental")`
#' @rdname knit_print
#' @export
#' @method knit_print OneParExpPrior
knit_print.OneParExpPrior <- function(x, ..., asis = TRUE) {
  assert_flag(asis)
  rv <- "TODO\n\n"
  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

# LogisticLogNormalGrouped ----

#' @description `r lifecycle::badge("experimental")`
#' @rdname knit_print
#' @export
#' @method knit_print LogisticLogNormalGrouped
knit_print.LogisticLogNormalGrouped <- function(
    x,
    ...,
    use_values = TRUE,
    fmt = "%5.2f",
    params = c(
      "\\alpha" = "alpha",
      "\\beta" = "beta",
      "log(\\delta_0)" = "delta_0",
      "log(\\delta_1)" = "delta_1"
    ),
    preamble = "The prior for &theta; is given by\\n",
    asis = TRUE) {

  NextMethod(params = params)
}

#' @description `r lifecycle::badge("experimental")`
#' @noRd
h_knit_print_render_model.LogisticLogNormalGrouped <- function(x, ...) {
  z <- "e^{(\\alpha + I_c \\times \\delta_0) + (\\beta  + I_c \\times \\delta_1) \\cdot log(d/d^*)}"
  paste0(
    "A logistic log normal model will describe the relationship between dose and toxicity: ",
    "$$ p(Tox | d) = f(X = 1 | \\theta, d) = \\frac{", z, "}{1 + ", z, "} $$\\n ",
    "where d* denotes a reference dose and I~c~ is a binary indicator which ",
    "is 1 for the combo arm and 0 for the mono arm.\n\n"
  )
}

# LogisticLogNormalOrdinal ----

#' @description `r lifecycle::badge("experimental")`
#' @noRd
h_knit_print_render_model.LogisticLogNormalOrdinal <- function(x, ...) {
  z <- "e^{\\alpha_k + \\beta \\cdot log(d/d^*)}"
  paste0(
    "Let p~k~(d) be the probability that the response of a patient treated at ",
    "dose d is in category k *_or higher_*, k=0, ..., K; d=1, ..., D.\n\nThen ",
    "$$ p_k(d) = f(X \\ge k \\; | \\; \\theta, d) = \\begin{cases} 1 & k = 0 \\\\ ",
    "\\frac{", z, "}{1 + ", z, "} & k=1, ..., K",
    "\\end{cases} $$\n\n",
    "where d* denotes a reference dose.\n\nThe &alpha;s are constrained ",
    "such that &alpha;~1~ > &alpha;~2~ > ... > &alpha;~K~.\n\n"
  )
}

# LogisticLogNormalOrdinal ----

#' @description `r lifecycle::badge("experimental")`
#' @rdname knit_print
#' @export
#' @method knit_print LogisticLogNormalOrdinal
knit_print.LogisticLogNormalOrdinal <- function(
    x,
    ...,
    use_values = TRUE,
    fmt = "%5.2f",
    params = NA,
    preamble = "The prior for &theta; is given by\\n",
    asis = TRUE) {
  assert_flag(asis)
  if (is.na(params)) {
    params <- c(
      paste0("alpha_", 1:(length(x@params@mean) - 1)),
      "beta"
    )
    names(params) <- paste0("\\", params)
  }
  NextMethod(params = params)
}

# LogisticIndepBeta ----

#' @description `r lifecycle::badge("experimental")`
#' @rdname knit_print
#' @export
#' @method knit_print LogisticIndepBeta
knit_print.LogisticIndepBeta <- function(
    x,
    ...,
    use_values = TRUE,
    fmt = "%5.2f",
    params = NA,
    tox_label = "DLAEs",
    preamble = "The prior for &theta; is given by\\n",
    asis = TRUE) {
  assert_flag(asis)

  y <- tidy(x)
  z <- "e^{\\phi_1 + \\phi_2 \\cdot log(d)}"
  posterior <- ModelParamsNormal(mean = c(x@phi1, x@phi2), cov = x@Pcov)
  # knit_print.ModelParamsNormal expects no row or column names
  rownames(posterior@cov) <- NULL
  colnames(posterior@cov) <- NULL

  rv <- paste0(
    "A logistic log normal model will describe the relationship between dose and toxicity: ",
    "$$ p(Tox | d) = f(X = 1 | \\theta, d) = \\frac{", z, "}{1 + ", z, "} $$\\n ",

    "The prior is expressed in terms of pseudo data and, consequently, the number ",
    " of cases and of ",
    tox_label,
    " need not be whole numbers.\n\nThe pseudo data are ",
    "defined in the following table:\n\n",
    paste0(
      do.call(
        function(x) {
          kableExtra::kable_styling(
            knitr::kable(x, col.names = c("Dose", "N", tox_label)),
            bootstrap_options = c("striped", "hover", "condensed")
          )
        },
        list(x = y$pseudoData)
      ),
      collapse = "\n"
    ),
    ifelse(
      nrow(y$data) == 0,
      "\n\nNo observed data has yet been recorded.\n",
      paste(
        "\n\nThe observed data are given in the following table:\n\n",
        paste((do.call(knitr::kable, list(x = y$data))), collapse = "\n")
      )
    ),
    knit_print(
      posterior,
      preamble = paste0(
        "\n\nTogether, the pseudo and observed data give rise to ",
        "the following posterior for the model parameters:\n\n"
      ),
      params = c("\\phi_1" = "phi1", "\\phi_2" = "phi2"),
      theta = "\\phi",
      asis = FALSE,
      ...
    ),
    "\n\n"
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}


# Effloglog ----

#' @description `r lifecycle::badge("experimental")`
#' @rdname knit_print
#' @export
#' @method knit_print Effloglog
knit_print.Effloglog <- function(
    x,
    ...,
    use_values = TRUE,
    fmt = "%5.2f",
    params = NA,
    tox_label = "DLAEs",
    eff_label = "efficacy",
    label = "participant",
    preamble = "The prior for &theta; is given by\\n",
    asis = TRUE
) {
  assert_flag(asis)
  assert_character(eff_label, len = 1, any.missing = FALSE)

  y <- tidy(x)
  # knit_print.ModelParamsNormal expects no row or column names
  posterior <- ModelParamsNormal(mean = c(x@theta1, x@theta2), cov = x@Q)
  rownames(posterior@cov) <- NULL
  colnames(posterior@cov) <- NULL

  rv <- paste0(
    "A linear log-log model with a pseudo data prior will describe the ",
    "relationship between dose and ",
    eff_label,
    ".  The model is given by\n ",
    "$$ y_i = \\theta_1 + \\theta_2 \\cdot \\log(\\log(d_i + k)) + \\epsilon_i $$\\n ",
    "where k is a constant (equal to ",
    x@const,
    "), y~i~ is the ",
    eff_label,
    " response for ",
    label[1],
    " i, treated at dose d~i~ and &epsilon;~i~ is an error term.  ",
    "The &epsilon;s are iid N(0, &nu;^-1^).\n\n  ",
    "The ",
    ifelse(
      length(x@nu) == 1,
      paste0(
        ifelse(nrow(y$data) == 0, "prior", "posterior"),
        " value of &nu; is ",
        x@nu,
        "."
      ),
      paste0(
        ifelse(nrow(y$data) == 0, "prior", "posterior"),
        " distribution of &nu; is currently &Gamma;(",
        sprintf(fmt, x@nu[1]),
        ", ",
        sprintf(fmt, x@nu[2]),
        ")."
      )
    ),
    "\n\nThe joint distribution of ",
    "&theta;~1~ and &theta;~2~ is given by\n\n",
    "$$ \\boldsymbol\\theta = \\begin{bmatrix}\\theta_1 \\\\ \\theta_2\\end{bmatrix} ",
    "\\sim N\\left(\\mu, \\nu \\boldsymbol{Q}^\\intercal \\right) $$ \nwhere ",
    "$\\boldsymbol{Q} = \\boldsymbol{X_0}^\\intercal\\boldsymbol{X_0} + ",
    "\\boldsymbol{X}^\\intercal\\boldsymbol{X}$ and **X~0~** is a design matrix ",
    "based on the dose levels in the pseudo data and **X** is a design matrix ",
    "based on the dose levels of ",
    label[2],
    " no-",
    tox_label,
    " ",
    eff_label,
    " responses in the observed data, if any.\n\n",
    ifelse(
      nrow(y$data) == 0,
      "\n\nNo observed data has yet been recorded.\n",
      paste(
        "\n\nThe data observed to date are given in the following table:\n\n",
        paste(
          (do.call(
            function(z) {
              z %>%
                dplyr::select(-c(NObs, NGrid, DoseGrid, XLevel)) %>%
                knitr::kable() %>%
                kableExtra::kable_styling(
                  bootstrap_options = c("striped", "hover", "condensed")
                )
            },
            list(z = y$data)
          )),
          collapse = "\n"
        )
      )
    ),
    knit_print(
      posterior,
      preamble = paste0(
        "\n\nTogether, the pseudo and observed data give rise to ",
        "the following posterior for the model parameters:\n\n"
      ),
      params = c("\\theta_1" = "theta1", "\\theta_2" = "theta2"),
      asis = FALSE,
      ...
    ),
    "\n\n"
  )

  if (asis) {
    rv <- knitr::asis_output(rv)
  }
  rv
}

