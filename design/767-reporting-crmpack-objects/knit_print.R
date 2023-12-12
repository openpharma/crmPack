knit_print.Data <-  function(object, ...) {
  ifelse(
    length(object@x > 0),
    tibble(
      Dose = object@x,
      DLT = object@y
    ) |>
    group_by(Dose) |>
    summarise(
      N = n(),
      DLT = sum(DLT),
      .groups = "drop"
    ) |>
    complete(
      Dose = object@doseGrid,
      fill = list(N = 0, DLT = 0)
    ) |>
    kable(),
    "No participants are yet evaluable."
  )
}

knit_print.IncrementsDoseLevels <- function(object, ...) {
  paste0(
    "The maximum increment between cohorts is ",
    object@levels,
    ifelse (object@levels == 1, " level", " levels"),
    " relative to the ",
    ifelse(object@basis_level == "last", "dose used in the previous cohort.", "highest dose used so far.")
  )
}


knit_print.StoppingMinPatients <- function(object, ...) {
  asis_output(
    paste0(
      object@report_label,
      ": When ",
      object@nPatients,
      " evaluable participants have enrolled."
    )
  )
}

knit_print.StoppingTargetProb <- function(object, ...) {
  asis_output(
    paste0(
      object@report_label,
      ": When the probability that the target dose is in the range ",
      object@target[1],
      " to ",
      object@target[2],
      " is at least ",
      object@prob,
      "."
    )
  )
}

knit_print.StoppingPatientsNearDose <- function(object, ...) {
  asis_output(
    paste0(
      object@report_label,
      ": When the number of participants ",
      ifelse(
        object@percentage == 0,
        "at the target dose ",
        paste0("within ", object@percentage, "% of the target dose ")
      ),
      "is ",
      object@nPatients,
      " or more."
    )
  )
}

knit_print.StoppingAll <- function(object, indent = 0L, ...) {
  items <- lapply(
    object@stop_list,
    function(x, indent) {
      paste0(
        rep("  ", indent),
        # "- ",
        knit_print(x, ...)
      )
    },
    indent = indent + 1
  )

  asis_output(
    paste0(
      rep("  ", indent),
      "- ",
      object@report_label,
      ": When all of the following conditions are satisfied:\n\n",
      paste0(
        items,
        collapse = "\n",
       "")
    )
  )
}

knit_print.StoppingAny <-  function(object, indent = 0L, ...) {
  items <- lapply(
    object@stop_list,
    function(x, indent) {
      paste0(
        rep("  ", indent),
        # "- ",
        knit_print(x, ...)
      )
    },
    indent = indent + 1
  )

  asis_output(
    paste0(
      rep("  ", indent),
      "- ",
      object@report_label,
      ": When any of the following conditions are satisfied:\n\n",
      paste0(
        items,
        collapse = "\n"
      )
    )
  )
}

knit_print.ModelParamsNormal <- function(object, use_values = TRUE, fmt = "%5.2f", ...) {
  assert_true(length(object@mean) == 2)

  muAlpha <- ifelse(
    use_values,
    sprintf(fmt, object@mean[1]),
    "\\mu~\\alpha~"
  )
  muBeta <- ifelse(
    use_values,
    sprintf(fmt, object@mean[2]),
    "\\mu;~\\beta~"
  )
  sigma11 <- ifelse(
    use_values,
    sprintf(fmt, object@cov[1, 1]),
    "\\sigma_\\alpha\\sigma_\\alpha"
  )
  sigma22 <- ifelse(
    use_values,
    sprintf(fmt, object@cov[2, 2]),
    "\\sigma_\\beta\\sigma_\\beta"
  )
  sigma12 <- ifelse(
    use_values,
    sprintf(fmt, object@cov[1, 2]),
    "\\rho\\sigma_\\alpha\\sigma_\\beta"
  )
  sigma21 <- ifelse(
    use_values,
    sprintf(fmt, object@cov[2, 1]),
    "\\rho\\sigma_\\beta\\sigma_\\alpha"
  )

  z <- "e^{\\alpha + \\beta \\cdot log(d/d_{ref})}"
  result <- paste0(
    "$$ p(Tox | d) = f(X = 1 | \\theta, d) = \\frac{", z, "}{1 + ", z, "} $$\\n",
    "where the prior for &theta; is given by\\n",
    "$$ \\theta = \\begin{bmatrix*}[r] \\alpha \\\\ log(\\beta) \\end{bmatrix*}",
    " \\sim N \\left(\\begin{bmatrix}", muAlpha, " \\\\ ", muBeta, "\\end{bmatrix} , ",
    "\\begin{bmatrix} ", sigma11, " && ", sigma12, " \\\\ ", sigma21, " && ", sigma22,
    "\\end{bmatrix} \\right)",
    " $$"
  )

  asis_output(result)
}

knit_print.GeneralModel <- function(object, ...) {
  result <- paste0(
    knit_print(object@params, ...),
    "\\n\\nThe reference dose will be ",
    object@ref_dose, ".\\n\\n"
  )
  asis_output(result)
}

knit_print.NextBestNCRM <- function(object,  ...) {
  asis_output(
    object |>
      tidy() |>
      kable(
        full.width = FALSE,
        col.names = c("Toxicity range", "Lower", "Upper", "Max pct increment"),
        caption = "Neuenchwander's nCRM rule"
      ) |>
      kableExtra::add_header_above(c(" " = 1, "Probability range" = 2, " " = 1))
  )
}

knit_print.CohortSizeRange <- function(object, ...) {
  asis_output(
    object |>
      tidy() |>
      kable(
        col.names = c("Lower", "Upper", "Cohort size"),
        caption = "Defined by the dose to be used in the next cohort"
      ) |>
      add_header_above(c("Dose" = 2, " " = 1))
  )
}

knit_print.CohortSizeDLT <- function(object, ...) {
  asis_output(
    object |>
      tidy() |>
      kable(
        col.names = c("Lower", "Upper", "Cohort size"),
        caption = "Defined by the number of DLTs so far observed"
      ) |>
      add_header_above(c("No of DLTs" = 2, " " = 1))
  )
}

knit_print.CohortSizeMax <- function(object, ...) {
  asis_output(
    paste0(
      "The maximum of the cohort sizes defined in the following tables:",
      paste0(
        lapply(
          object@cohort_sizes,
          function(x, ...) {
            knit_print(x, ...)
          }
        ),
        collapse = "\n"
      ),
      paste = "\n"
    )
  )
}

knit_print.Design <- function(object, level = 1, caption = "Trial design", ...) {
  asis_output(
    paste(
      paste0(str_dup("#", level), " ", caption),
      paste0(str_dup("#", level + 1), " Observed data"),
      knit_print(object@data, ...),
      paste0(str_dup("#", level + 1), " Model"),
      knit_print(object@model , ...),
      paste0(str_dup("#", level + 1), " Maximum permitted increment"),
      knit_print(object@increments , ...),
      paste0(str_dup("#", level + 1), " Recommended dose"),
      knit_print(object@nextBest , ...),
      paste0(str_dup("#", level + 1), " Cohort size"),
      knit_print(object@cohort_size , ...),
      paste0(str_dup("#", level + 1), " Stopping rules"),
      knit_print(object@stopping , ...),
      paste0(str_dup("#", level + 1), " Starting dose"),
      paste0("The dose used in the first cohort will be ", object@startingDose, "."),
      sep = "\n\n"
    )
  )
}
