# Validate-class ----

#' `Validate`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The [`Validate`] class is a Reference Class
#' to help programming validation for new S4 classes.
#'
#' @details Starting from an empty \code{msg} vector, with each check
#'   that is returning `FALSE` the vector gets a new element - the string
#'   explaining the failure of the validation.
#'
#' @name Validate
#' @field msg (`character`)\cr the cumulative messages.
#'
Validate <- setRefClass(
  Class = "Validate",
  fields = list(msg = "character"),
  methods = list(
    check = function(test, string = "") {
      "Check whether the \\code{test} is \\code{TRUE}; if so, return \\code{NULL}.
      Otherwise, add the \\code{string} message into the cumulative messages vector \\code{msg}."
      assert_flag(test)
      assert_string(string)
      if (test) {
        NULL
      } else {
        msg <<- c(msg, string)
      }
    },
    result = function() {
      "Return either cumulative messages vector \\code{msg}
      (which contains the error messages from all the checks),
      or \\code{NULL}, if \\code{msg} is empty (i.e. all the checks were successful)."
      if (length(msg) > 0) {
        msg
      } else {
        TRUE
      }
    }
  )
)

#' Joining JAGS Models
#'
#' @description `r lifecycle::badge("stable")`
#'
#' This helper function joins two JAGS models in the way that the body of the
#'   second model is appended to the body of the first model (in this order).
#'   After that, the first, body-extended model is returned.
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
h_join_models <- function(model1, model2) {
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

# nolint start

##' Helper function for value matching with tolerance
##'
##' This is a modified version of \code{match} that supports tolerance.
##'
##' @param x the values to be matched
##' @param table the values to be matched against
##' @return A vector of the same length as \code{x} or
##'  empty vector if \code{table} is empty.
##'
##' @export
##' @keywords programming
##' @example examples/matching-tolerance.R
matchTolerance <- function(x, table) {
  if (length(table) == 0) {
    return(integer())
  }

  as.integer(sapply(x, function(.x) {
    which(sapply(table, function(.table) {
      isTRUE(all.equal(.x, .table,
        tolerance = 1e-10,
        check.names = FALSE,
        check.attributes = FALSE
      ))
    }))[1]
  }))
}

##' @describeIn matchTolerance Helper function for checking inclusion in a table with tolerance
##' @export
`%~%` <- function(x, table) {
  !is.na(matchTolerance(x = x, table = table))
}

##' Check overlap of two character vectors
##'
##' @param a first character vector
##' @param b second character vector
##' @return returns TRUE if there is no overlap between the two character
##' vectors, otherwise FALSE
##'
##' @keywords internal
noOverlap <- function(a, b) {
  identical(
    intersect(a, b),
    character(0)
  )
}

##' Checking for scalar
##'
##' @param x the input
##' @return Returns \code{TRUE} if \code{x} is a length one vector
##' (i.e., a scalar)
##'
##' @keywords internal
is.scalar <- function(x) {
  return(identical(length(x), 1L))
}

##' Predicate checking for a boolean option
##'
##' @param x the object being checked
##' @return Returns \code{TRUE} if \code{x} is a length one logical vector (i.e., a
##' scalar)
##'
##' @keywords internal
is.bool <- function(x) {
  return(is.scalar(x) &&
    is.logical(x))
}


##' checks for whole numbers (integers)
##'
##' @param x the numeric vector
##' @param tol the tolerance
##' @return TRUE or FALSE for each element of x
##'
##' @keywords internal
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}


##' Safe conversion to integer vector
##'
##' @param x the numeric vector
##' @return the integer vector
##'
##' @keywords internal
safeInteger <- function(x) {
  testres <- is.wholenumber(x)
  if (!all(testres)) {
    notInt <- which(!testres)
    stop(paste(
      "elements",
      paste(notInt, sep = ", "),
      "of vector are not integers!"
    ))
  }
  as.integer(x)
}

##' Predicate checking for a probability
##'
##' @param x the object being checked
##' @param bounds whether to include the bounds 0 and 1 (default)
##' @return Returns \code{TRUE} if \code{x} is a probability
##'
##' @keywords internal
is.probability <- function(x,
                           bounds = TRUE) {
  return(is.scalar(x) &&
    if (bounds) {
      0 <= x && 1 >= x
    } else {
      0 < x && 1 > x
    })
}

##' Predicate checking for a numeric range
##'
##' @param x the object being checked
##' @return Returns \code{TRUE} if \code{x} is a numeric range
##'
##' @keywords internal
is.range <- function(x) {
  return(identical(length(x), 2L) &&
    x[1] < x[2])
}

##' Predicate checking for a probability range
##'
##' @param x the object being checked
##' @param bounds whether to include the bounds 0 and 1 (default)
##' @return Returns \code{TRUE} if \code{x} is a probability range
##'
##' @keywords internal
is.probRange <- function(x,
                         bounds = TRUE) {
  return(is.range(x) &&
    all(sapply(x, is.probability, bounds = bounds)))
}


##' Shorthand for logit function
##'
##' @param x the function argument
##' @return the logit(x)
##'
##' @export
##' @keywords programming
logit <- function(x) {
  qlogis(x)
}

##' Shorthand for probit function
##'
##' @param x the function argument
##' @return the probit(x)
##'
##' @export
##' @keywords programming
probit <- function(x) {
  qnorm(x)
}

##' Open the example pdf for crmPack
##'
##' Calling this helper function should open the example.pdf document,
##' residing in the doc subfolder of the package installation directory.
##'
##' @return nothing
##' @export
##' @keywords documentation
##' @author Daniel Sabanes Bove \email{sabanesd@@roche.com}
crmPackExample <- function() {
  crmPath <- system.file(package = "crmPack")
  printVignette(list(PDF = "example.pdf", Dir = crmPath))
  ## instead of utils:::print.vignette
}

##' Open the browser with help pages for crmPack
##'
##' This convenience function opens your browser with the help pages for
##' crmPack.
##'
##' @return nothing
##' @export
##' @importFrom utils help
##' @keywords documentation
##' @author Daniel Sabanes Bove \email{sabanesd@@roche.com}
crmPackHelp <- function() {
  utils::help(package = "crmPack", help_type = "html")
}


## this is the new version, working on the gtable objects:
##' Plots gtable objects
##'
##' @method plot gtable
##' @param x the gtable object
##' @param \dots additional parameters for \code{\link[grid]{grid.draw}}
##'
##' @importFrom grid grid.draw
##' @export
plot.gtable <- function(x, ...) {
  grid::grid.draw(x, ...)
}

##' @export
print.gtable <- function(x, ...) {
  plot.gtable(x, ...)
}


#' Multiple plot function
#'
#' ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects).
#' If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#' then plot 1 will go in the upper left, 2 will go in the upper right, and
#' 3 will go all the way across the bottom.
#'
#' @param \dots Objects to be passed
#' @param plotlist a list of additional objects
#' @param rows Number of rows in layout
#' @param layout A matrix specifying the layout. If present, \code{rows}
#' is ignored.
#'
#' @return Used for the side effect of plotting
#' @importFrom grid grid.newpage pushViewport viewport
#' @export
multiplot <- function(..., plotlist = NULL, rows = 1, layout = NULL) {
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots <- length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, rows * ceiling(numPlots / rows)),
      nrow = rows, ncol = ceiling(numPlots / rows),
      byrow = TRUE
    )
  }

  if (numPlots == 1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(
      nrow(layout),
      ncol(layout)
    )))

    # Make each plot, in the correct location
    for (i in seq_len(numPlots))
    {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = grid::viewport(
        layout.pos.row = matchidx$row,
        layout.pos.col = matchidx$col
      ))
    }
  }
}

##' Taken from utils package (print.vignette)
##'
##' @importFrom tools file_ext
##' @importFrom utils browseURL
##' @keywords internal
printVignette <- function(x, ...) {
  if (nzchar(out <- x$PDF)) {
    ext <- tools::file_ext(out)
    out <- file.path(x$Dir, "doc", out)
    if (tolower(ext) == "pdf") {
      pdfviewer <- getOption("pdfviewer")
      if (identical(pdfviewer, "false")) {
      } else if (.Platform$OS.type == "windows" && identical(
        pdfviewer,
        file.path(R.home("bin"), "open.exe")
      )) {
        shell.exec(out)
      } else {
        system2(pdfviewer, shQuote(out), wait = FALSE)
      }
    } else {
      browseURL(out)
    }
  } else {
    warning(gettextf("vignette %s has no PDF/HTML", sQuote(x$Topic)),
      call. = FALSE, domain = NA
    )
  }
  invisible(x)
}

##' Compute the density of Inverse gamma distribution
##' @param x vector of quantiles
##' @param a the shape parameter of the inverse gamma distribution
##' @param b the scale parameter of the inverse gamma distribution
##' @param log logical; if TRUE, probabilities p are given as log(p)
##' @param normalize logical; if TRUE, the output will be normalized
##'
##' @keywords internal
dinvGamma <- function(x,
                      a,
                      b,
                      log = FALSE,
                      normalize = TRUE) {
  ret <- -(a + 1) * log(x) - b / x
  if (normalize) {
    ret <- ret + a * log(b) - lgamma(a)
  }
  if (log) {
    return(ret)
  } else {
    return(exp(ret))
  }
}

##' Compute the distribution function of Inverse gamma distribution
##'
##' @param q vector of quantiles
##' @param a the shape parameter of the inverse gamma distribution
##' @param b the scale parameter of the inverse gamma distribution
##' @param lower.tail logical; if TRUE (default), probabilities are `P(X  > x)`,
##'   otherwise, `P(X <= x)`.
##' @param logical; FALSE (default) if TRUE, probabilities/densities p
##'   are returned as `log(p)`
##'
##' @keywords internal
pinvGamma <- function(q,
                      a,
                      b,
                      lower.tail = TRUE,
                      log.p = FALSE) {
  pgamma(
    q = 1 / q,
    shape = a,
    rate = b,
    lower.tail = !lower.tail,
    log.p = log.p
  )
}

##' Compute the quantile function of Inverse gamma distribution
##' @param p vector of probabilities
##' @param a the shape parameter of the inverse gamma distribution
##' @param b the scale parameter of the inverse gamma distribution
##' @param lower.tail logical; if TRUE (default), probabilities are `P(X  > x)`,
##'   otherwise, `P(X <= x)`.
##' @param logical; FALSE (default) if TRUE, probabilities/densities p are
##'   returned as `log(p)`
##'
##' @keywords internal
qinvGamma <- function(p,
                      a,
                      b,
                      lower.tail = TRUE,
                      log.p = FALSE) {
  1 / qgamma(
    p = p,
    shape = a,
    rate = b,
    lower.tail = !lower.tail,
    log.p = log.p
  )
}
##' The random generation of the Inverse gamma distribution
##' @param n the number of observations
##' @param a the shape parameter of the inverse gamma distribution
##' @param b the scale parameter of the inverse gamma distribution
##'
##' @keywords internal
rinvGamma <- function(n,
                      a,
                      b) {
  1 / rgamma(n,
    shape = a,
    rate = b
  )
}


#' Convenience function to make barplots of percentages
#'
#' @param x vector of samples
#' @param description xlab string
#' @param xaxisround rounding for xaxis labels (default: 0, i.e. integers will
#' be used)
#'
#' @return the ggplot2 object
#'
#' @keywords internal
#' @importFrom ggplot2 ggplot geom_histogram aes xlab ylab xlim
#' @example examples/myBarplot.R
myBarplot <- function(x, description, xaxisround = 0) {
  tabx <- table(x) / length(x)
  dat <- data.frame(x = as.numeric(names(tabx)), perc = as.numeric(tabx) * 100)
  ggplot() +
    geom_bar(aes(x = x, y = perc),
      data = dat,
      stat = "identity",
      position = "identity",
      width = ifelse(nrow(dat) > 1, min(diff(dat$x)) / 2, 1)
    ) +
    xlab(description) +
    ylab("Percent") +
    scale_x_continuous(
      breaks =
        round(dat$x, xaxisround)
    )
}

# nolint end

#' Comparison with Numerical Tolerance and Without Name Comparison
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This helper function ensures a default tolerance level equal to `1e-10`,
#' and ignores names and other attributes.
#' In contrast to [all.equal()], it always returns a logical type object.
#'
#' @param target (`numeric`)\cr target values.
#' @param current (`numeric`)\cr current values.
#' @param tolerance (`number`) relative differences smaller than this are not reported.
#' @return `TRUE` when `target` and `current` do not differ
#'   up to desired tolerance and without looking at names or other attributes,
#'   `FALSE` otherwise.
#'
h_all_equivalent <- function(target,
                             current,
                             tolerance = 1e-10) {
  assert_numeric(target)
  assert_numeric(current)
  assert_number(tolerance)

  tmp <- all.equal(
    target = target,
    current = current,
    tolerance = tolerance,
    check.names = FALSE,
    check.attributes = FALSE
  )
  isTRUE(tmp)
}

#' Preparing Data for Plotting
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This helper function prepares a `data.frame` object based on `Data` class object.
#' The resulting data frame is used by the plot function for `Data` class objects.
#'
#' @param data (`Data`)\cr object from which data is extracted and converted
#'   into a data frame.
#' @param blind (`flag`)\cr should data be blinded?
#'   If `TRUE`, then for each cohort, all DLTs are assigned to the first subjects
#'   in the cohort. In addition, the placebo (if any) is set to the active dose
#'   level for that cohort.
#' @param ... further arguments passed to `data.frame` constructor.
#'   It can be e.g. an extra `column_name = value` pair based on a slot
#'   from `x` (which in this case might be a subclass of `Data`)
#'   which does not appear in `Data`.
#' @return A [`data.frame`] object with values to plot.
#'
h_plot_data_df <- function(data, blind = FALSE, ...) {
  df <- data.frame(
    patient = seq_along(data@x),
    ID = paste(" ", data@ID),
    cohort = data@cohort,
    dose = data@x,
    toxicity = ifelse(data@y == 1, "Yes", "No"),
    ...
  )

  if (blind) {
    # This is to blind the data.
    # For each cohort, all DLTs are assigned to the first subjects in the cohort.
    # In addition, the placebo (if any) is set to the active dose level for that cohort.
    # Notice: dapply reorders records of df according to the lexicographic order of cohort.
    df <- dapply(df, f = ~cohort, FUN = function(coh) {
      coh$toxicity <- sort(coh$toxicity, decreasing = TRUE)
      coh$dose <- max(coh$dose)
      coh
    })
  } else if (data@placebo) {
    # Placebo will be plotted at y = 0 level.
    df$dose[df$dose == data@doseGrid[1]] <- 0
  }

  df
}

#' Preparing Cohort Lines for Data Plot
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This helper function prepares a `ggplot` geom with reference lines
#' separating different cohorts on the plot of `Data` class object.
#' Lines are either vertical or horizontal of green color and longdash type.
#'
#' @details The geom object is returned if and only if `placebo` is equal to `TRUE`
#'   and there are more than one unique values in `cohort`. Otherwise, this function
#'   returns `NULL` object.
#'
#' @param cohort (`integer`)\cr the cohort indices.
#' @param placebo (`flag`)\cr is placebo included in the doses?
#'   If it so, this function returns `NULL` object as in this case all doses
#'   in a given cohort are equal and there is no need to separate them.
#' @param vertical (`flag`)\cr should the line be vertical? Otherwise it is horizontal.
#'
h_plot_data_cohort_lines <- function(cohort,
                                     placebo,
                                     vertical = TRUE) {
  assert_integer(cohort)
  assert_flag(placebo)
  assert_flag(vertical)

  # If feasible, add vertical or horizontal green lines separating sub-sequent cohorts.
  if (placebo & length(unique(cohort)) > 1) {
    intercept <- head(cumsum(table(cohort)), n = -1) + 0.5
    if (vertical) {
      geom_vline(xintercept = intercept, colour = "green", linetype = "longdash")
    } else {
      geom_hline(yintercept = intercept, colour = "green", linetype = "longdash")
    }
  } else {
    NULL
  }
}

#' Checking Formals of a Function
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This helper function checks whether a given function `fun` has required or
#' allowed arguments. The argument check is based only on the names of the
#' arguments. No any further logic is verified here.
#'
#' @param fun (`function`)\cr a function name whose argument names will be
#'   checked.
#' @param mandatory (`character` or `NULL`)\cr the names of the arguments which
#'   must be present in `fun`. If `mandatory` is specified as `NULL` (default)
#'   this requirement is ignored.
#' @param allowed (`character` or `NULL`)\cr the names of the arguments which
#'   are allowed in `fun`. Names that do not belong to `allowed` are simply not
#'   allowed. The `allowed` parameter is independent from the `mandatory`, in a
#'   sense that if `mandatory` is specified as a `character` vector, it does not
#'   have to be repeated in `allowed`. If `allowed` is specified as `NULL`
#'   (default), then it means that there must be no any arguments in `fun`
#'   (except these ones which are specified in `mandatory`).
#'
h_check_fun_formals <- function(fun, mandatory = NULL, allowed = NULL) {
  assert_function(fun)
  assert_character(mandatory, null.ok = TRUE)
  assert_character(allowed, null.ok = TRUE)

  arg_names <- names(formals(fun))
  mandatory_check <- all(mandatory %in% arg_names)
  allowed_check <- all(arg_names %in% c(mandatory, allowed))

  mandatory_check && allowed_check
}

#' Testing Matrix for Positive Definiteness
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This helper function checks whether a given numeric matrix `x`
#' is positive-definite.
#'
#' @details The positive definiteness test implemented in this function
#'   is based on the following characterization valid for real matrices:
#'   `A symmetric matrix is positive-definite if and only if all of its
#'   eigenvalues are positive.` In this function an eigenvalue is considered
#'   as positive if and only if it is greater than the `tolerance`.
#'
#' @param x (`matrix`)\cr a matrix to be checked.
#' @param tolerance (`number`)\cr a given tolerance number used to check whether
#'   an eigenvalue is positive or not. An eigenvalue is considered
#'   as positive if and only if it is greater than the `tolerance`.
#'
#' @return `TRUE` if a given matrix is a positive-definite, `FALSE` otherwise.
#'
h_is_positive_definite <- function(x, tolerance = 1e-08) {
  assert_matrix(x, any.missing = FALSE)
  assert_numeric(x)
  assert_number(tolerance)

  # If x is not a square matrix or it is not a symmetric matrix.
  if ((nrow(x) != ncol(x)) || (sum(x == t(x)) != (nrow(x)^2))) {
    return(FALSE)
  }
  eigenvalues <- eigen(x, only.values = TRUE)$values
  all(eigenvalues > tolerance)
}

#' Getting the Slots from a S4 Object
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This helper function extracts requested slots from the S4 class object.
#' It is a simple wrapper of [`methods::slot`] function.
#'
#' @param object (`S4`)\cr an object from a formally defined S4 class.
#' @param names (`character`)\cr a vector with names of slots to be fetched.
#'   This function assumes that for every element in `names`, there exists a
#'   slot of the same name in the `object`.
#'
#' @return `list` with the slots extracted from `object` according to
#'   `names`.
#'
h_get_slots <- function(object, names) {
  assert_true(isS4(object))
  assert_character(names, min.len = 1, any.missing = FALSE)
  assert_true(all(names %in% slotNames(object)))

  sapply(names, function(n) slot(object, n), simplify = FALSE, USE.NAMES = TRUE)
}
