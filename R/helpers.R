# Validate-class ----

#' `Validate`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' The [`Validate`] class is a Reference Class
#' to help programming validation for new S4 classes.
#'
#' @details Starting from an empty `msg` vector, with each check
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

# positive_number-class ----

#' `positive_number`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' The [`positive_number`] class is a class to store not `NULL`, non `NA`,
#' finite and strictly positive numerical value. It is mainly used to store
#' reference dose value in model classes.
#'
#' @name positive_number
#'
positive_number <- setClass(
  Class = "positive_number",
  contains = "numeric",
  validity = function(object) {
    v <- Validate()
    v$check(
      test_number(object, finite = TRUE) && object > 0,
      "Object's value must be strictly positive"
    )
    v$result()
  }
)

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
match_within_tolerance <- function(x, table) {
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
  if (bounds) {
    return(test_numeric(x, lower = 0, upper = 1, any.missing = FALSE))
  } else {
    return(test_numeric(x, lower = 0, upper = 1, any.missing = FALSE) && x != 0 && x != 1)
  }
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

# nolint end

#' Combining S4 Class Validation Results
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A simple helper function that combines two outputs from calls to `result()`
#'   function which is placed in a slot of [Validate()] reference class.
#'
#' @param v1 (`logical` or `character`)\cr an output from `result()` function from
#'   [Validate()] reference class, to be combined with `v2`.
#' @param v2 (`logical` or `character`)\cr an output from `result()` function from
#'   [Validate()] reference class, to be combined with `v1`.
#'
#' @export
#' @examples
#' h_validate_combine_results(TRUE, "some_message")
h_validate_combine_results <- function(v1, v2) {
  assert_true(test_true(v1) || test_character(v1, any.missing = FALSE, min.len = 1L))
  assert_true(test_true(v2) || test_character(v2, any.missing = FALSE, min.len = 1L))

  isTRUEv2 <- isTRUE(v2)
  if (isTRUE(v1)) {
    if (isTRUEv2) {
      TRUE
    } else {
      v2
    }
  } else {
    if (isTRUEv2) {
      v1
    } else {
      c(v1, v2)
    }
  }
}

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
#' @param tolerance (`number`) relative differences smaller than this are not
#'   reported.
#' @return `TRUE` when `target` and `current` do not differ
#'   up to desired tolerance and without looking at names or other attributes,
#'   `FALSE` otherwise.
#'
#' @export
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
#' This helper function prepares a `data.frame` object based on `Data` class
#' object. The resulting data frame is used by the plot function for `Data`
#' class objects.
#'
#' @param data (`Data`)\cr object from which data is extracted and converted
#'   into a data frame.
#' @param blind (`flag`)\cr should data be blinded?
#'   If `TRUE`, then for each cohort, all DLTs are assigned to the first
#'   subjects in the cohort. In addition, the placebo (if any) is set to the
#'   active dose level for that cohort.
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
    # In addition, the placebo (if any) is set to the active dose level for that
    # cohort.
    # Notice: dapply reorders records of df according to the lexicographic order
    # of cohort.
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
#' @details The geom object is returned if and only if `placebo` is equal to
#'   `TRUE` and there are more than one unique values in `cohort`. Otherwise,
#'   this function returns `NULL` object.
#'
#' @param cohort (`integer`)\cr the cohort indices.
#' @param placebo (`flag`)\cr is placebo included in the doses?
#'   If it so, this function returns `NULL` object as in this case all doses
#'   in a given cohort are equal and there is no need to separate them.
#' @param vertical (`flag`)\cr should the line be vertical? Otherwise it is
#'   horizontal.
#'
h_plot_data_cohort_lines <- function(cohort,
                                     placebo,
                                     vertical = TRUE) {
  assert_integer(cohort)
  assert_flag(placebo)
  assert_flag(vertical)

  # If feasible, add vertical or horizontal green lines separating sub-sequent
  # cohorts.
  if (placebo && length(unique(cohort)) > 1) {
    intercept <- head(cumsum(table(cohort)), n = -1) + 0.5
    if (vertical) {
      geom_vline(xintercept = intercept, colour = "green", linetype = "longdash") # nolintr
    } else {
      geom_hline(yintercept = intercept, colour = "green", linetype = "longdash") # nolintr
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
#' @export
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

#' Getting the Slots from a S4 Object
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This helper function extracts requested slots from the S4 class object.
#' It is a simple wrapper of [methods::slot()] function.
#'
#' @param object (`S4`)\cr an object from a formally defined S4 class.
#' @param names (`character`)\cr a vector with names of slots to be fetched.
#'   This function assumes that for every element in `names`, there exists a
#'   slot of the same name in the `object`.
#' @param simplify (`flag`)\cr should an output be simplified? This has an
#'   effect if and only if a single slot is about to be extracted, i.e.
#'   `names` is just a single string.
#'
#' @return `list` with the slots extracted from `object` according to `names`,
#'   or single slot if simplification is required and possible.
#'
#' @export
#'
h_slots <- function(object, names, simplify = FALSE) {
  assert_true(isS4(object))
  assert_character(names, any.missing = FALSE, null.ok = TRUE)
  assert_true(all(names %in% slotNames(object)))

  if (is.null(names) || length(names) == 0L) {
    return(list())
  }

  slots_list <- sapply(names, function(n) slot(object, n), simplify = FALSE, USE.NAMES = TRUE)

  if (simplify && length(names) == 1) {
    slots_list[[1]]
  } else {
    slots_list
  }
}

#' Conditional Formatting Using C-style Formats
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This helper function conditionally formats a number with [formatC()]
#' function using `"E"` format and specific number of digits as given by the
#' user. A number is formatted if and only if its absolute value is less than
#' `0.001` or greater than `10000`. Otherwise, the number is not formatted.
#' Additionally, custom prefix or suffix can be appended to character string
#' with formatted number, so that the changes are marked.
#'
#' @note This function was primarily designed as a helper for
#'   [h_jags_write_model()] function.
#'
#' @param x (`number`)\cr a number to be formatted.
#' @param digits (`function`)\cr the desired number of significant digits.
#' @param prefix (`string`)\cr a prefix to be added in front of the formatted
#'   number.
#' @param suffix (`string`)\cr a suffix to be appended after the formatted
#'   number.
#'
#' @return Either formatted `x` as `string` or unchanged `x` if the
#'   formatting condition is not met.
#'
#' @export
#' @examples
#' h_format_number(50000)
#' h_format_number(50000, prefix = "P", suffix = "S")
h_format_number <- function(x,
                            digits = 5,
                            prefix = "",
                            suffix = "") {
  assert_number(x)
  assert_int(digits)
  assert_string(prefix)
  assert_string(suffix)

  if ((abs(x) < 1e-3) || (abs(x) > 1e+4)) {
    paste0(prefix, formatC(x, digits = digits, format = "E"), suffix)
  } else {
    x
  }
}

#' Recursively Apply a Function to a List
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This helper function recursively iterates through a "list-like" object and it
#' checks whether an element is of a given class. If it so, then it replaces
#' that element by the result of an execution of a given function. Otherwise,
#' and if the element is of length greater than 1 (i.e. not a scalar), it
#' replaces that element by the result of `h_rapply()`, recursively called for
#' that element. In the remaining case, that is, the element is not of a given
#' class and is a scalar, then that element remains unchanged.
#'
#' @note This helper function is conceptually similar the same as [rapply()]
#'   function. However, it differs from [rapply()] in two major ways. First, the
#'   `h_rapply()` is not limited to objects of type `list` or `expression` only.
#'   It can be any "list-like" object of any type for which subsetting operator
#'   [`[[`][Extract] is defined. This can be, for example, an object of type
#'   `language`, often obtained from the [body()] function. The second
#'   difference is that the flexibility of [rapply()] on how the result is
#'   structured is not available with `h_rapply()` for the user. That is, with
#'   `h_rapply()` each element of `x`, which has a class included in `classes`,
#'   is replaced by the result of applying `fun` to the element. This behavior
#'   corresponds to [rapply()] when invoked with fixed `how = replace`.
#'   This function was primarily designed as a helper for [h_jags_write_model()]
#'   function.
#'
#' @param x (`any`)\cr "list-like" object for which subsetting operator
#'   [`[[`][Extract] is defined.
#' @param fun (`function`)\cr a function of one "principal" argument, passing
#'   further arguments via `...`.
#' @param classes (`character`)\cr class names.
#' @param ... further arguments passed to function `fun`.
#'
#' @return "list-like" object of similar structure as `x`.
#'
#' @export
#' @example examples/helpers-rapply.R
#'
h_rapply <- function(x, fun, classes, ...) {
  assert_function(fun)
  assert_character(classes, min.len = 1L)
  # To assert that `x` is subsettable.
  # If it works, fine, we don't see the result, otherwise it gives the error.
  force(x[[1]])

  for (i in seq_along(x)) {
    if (class(x[[i]]) %in% classes) {
      x[[i]] <- do.call(fun, c(list(x[[i]]), ...))
    } else if (length(x[[i]]) > 1L) {
      x[[i]] <- h_rapply(x[[i]], fun, classes, ...)
    }
  }
  x
}

#' Getting `NULL` for `NA`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A simple helper function that replaces `NA` object by `NULL` object.
#'
#' @param x (`any`)\cr atomic object of length `1`. For the definition of
#'   "atomic", see [is.atomic()].
#'
#' @return `NULL` if `x` is `NA`, otherwise, `x`.
#'
#' @export
#' @examples
#' h_null_if_na(NA)
h_null_if_na <- function(x) {
  assert_atomic(x, len = 1L)

  if (is.na(x)) {
    NULL
  } else {
    x
  }
}

#' Getting the default value for an empty object
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A simple helper function that sets a default value for an empty object,
#' that is an object for which [length()] function returns `0L`.
#'
#' @param x (`any`) \cr an object to handle. It can be any object for which
#'   [length()] function is defined.
#' @param default (`any`) \cr a default value for `x` object.
#'
#' @export
#' @examples
#' h_default_if_empty(character(0), default = "default label")
#' h_default_if_empty("custom label", default = "default label")
#' h_default_if_empty(NA, default = "default label")
h_default_if_empty <- function(x, default) {
  if (length(x) == 0L) {
    default
  } else {
    x
  }
}
#' Testing Matrix for Positive Definiteness
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This helper function checks whether a given numerical matrix `x` is a
#' positive-definite square matrix of a given size, without any missing
#' values. This function is used to test if a given matrix is a covariance
#' matrix, since every symmetric positive semi-definite matrix is a covariance
#' matrix.
#'
#' @details The positive definiteness test implemented in this function
#'   is based on the following characterization valid for real matrices:
#'   `A symmetric matrix is positive-definite if and only if all of its
#'   eigenvalues are positive.` In this function an eigenvalue is considered
#'   as positive if and only if it is greater than the `tol`.
#'
#' @param x (`matrix`)\cr a matrix to be checked.
#' @param size (`integer`)\cr a size of the square matrix `x` to be checked
#'   against for.
#' @param tol (`number`)\cr a given tolerance number used to check whether
#'   an eigenvalue is positive or not. An eigenvalue is considered
#'   as positive if and only if it is greater than the `tol`.
#'
#' @return `TRUE` if a given matrix is a positive-definite, `FALSE` otherwise.
#'
#' @export
#'
h_is_positive_definite <- function(x, size = 2, tol = 1e-08) {
  assert_number(tol)

  is_matrix <- test_matrix(
    x,
    mode = "numeric", nrows = size, ncols = size, any.missing = FALSE
  )

  if (is_matrix) {
    is_symmetric <- all.equal(x, t(x), tolerance = tol)
    if (isTRUE(is_symmetric)) {
      ev <- eigen(x, only.values = TRUE)$values
      all(ev > tol)
    } else {
      FALSE
    }
  } else {
    FALSE
  }
}

#' Check that an argument is a named vector of type numeric
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A simple helper function that tests whether an object is a named numerical
#' vector.
#'
#' @note This function is based on [`checkmate::test_numeric()`] and
#'   [`checkmate::test_names()`] functions.
#'
#' @param x (`any`)\cr object to check.
#' @inheritParams checkmate::test_names
#' @inheritParams checkmate::test_numeric
#' @param ... further parameters passed to [`checkmate::test_numeric()`].
#'
#' @return `TRUE` if `x` is a named vector of type numeric, otherwise `FALSE`.
#'
#' @export
#' @examples
#' h_test_named_numeric(1:2, permutation.of = c("a", "b"))
#' h_test_named_numeric(c(a = 1, b = 2), permutation.of = c("a", "b"))
#' h_test_named_numeric(c(a = 1, b = 2), permutation.of = c("b", "a"))
h_test_named_numeric <- function(x,
                                 subset.of = NULL, # nolintr
                                 must.include = NULL, # nolintr
                                 permutation.of = NULL, # nolintr
                                 identical.to = NULL, # nolintr
                                 disjunct.from = NULL, # nolintr
                                 lower = 0 + .Machine$double.xmin,
                                 finite = TRUE,
                                 any.missing = FALSE, # nolintr
                                 len = 2,
                                 ...) {
  is_valid_num <- test_numeric(
    x,
    lower = lower,
    finite = finite,
    any.missing = any.missing,
    len = len,
    ...,
    names = "named"
  )
  are_names_valid <- test_names(
    names(x),
    subset.of = subset.of,
    must.include = must.include,
    permutation.of = permutation.of,
    identical.to = identical.to,
    disjunct.from = disjunct.from,
  )
  is_valid_num && are_names_valid
}

#' Check which elements are in a given range
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A simple helper function that tests whether elements of a given vector or
#' matrix are within specified interval.
#'
#' @param x (`numeric`)\cr vector or matrix with elements to test.
#' @param range (`numeric`)\cr an interval, i.e. sorted two-elements vector.
#' @param bounds_closed (`logical`)\cr should bounds in the `range` be treated
#'   as closed? This can be a scalar or vector of length two. If it is a scalar,
#'   then its value applies to lower bound `range[1]` and upper bound
#'   `range[2]`. If this is a vector with two flags, the first flag corresponds
#'   to the lower bound only, and the second to the upper bound only.
#'
#' @return A logical vector or matrix of length equal to the length of `x`, that
#'   for every element of `x`, indicates whether a given element of `x` is in
#'   the `range`.
#'
#' @export
#' @examples
#' x <- 1:4
#' h_in_range(x, range = c(1, 3))
#' h_in_range(x, range = c(1, 3), bounds_closed = FALSE)
#' h_in_range(x, range = c(1, 3), bounds_closed = c(FALSE, TRUE))
#' mat <- matrix(c(2, 5, 3, 10, 4, 9, 1, 8, 7), nrow = 3)
#' h_in_range(mat, range = c(1, 5))
h_in_range <- function(x, range = c(0, 1), bounds_closed = TRUE) {
  assert_numeric(x)
  assert_numeric(range, any.missing = FALSE, len = 2, sorted = TRUE)
  assert_logical(bounds_closed, min.len = 1, max.len = 2, any.missing = FALSE)

  above_lwr <- if (bounds_closed[1]) {
    x >= range[1]
  } else {
    x > range[1]
  }

  below_upr <- if (tail(bounds_closed, 1)) {
    x <= range[2]
  } else {
    x < range[2]
  }

  above_lwr & below_upr
}

#' Find Interval Numbers or Indices and Return Custom Number For 0.
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A simple wrapper of [`findInterval()`] function that invokes
#' [`findInterval()`], takes its output and replaces all the
#' elements with \eqn{0} value to a custom number as specified in `replacement`
#' argument.
#'
#' @inheritDotParams base::findInterval
#' @param replacement (`number`)\cr a custom number to be used as a replacement
#'   for \eqn{0}. Default to `-Inf`.
#'
#' @export
#' @examples
#' h_find_interval(1, c(2, 4, 6))
#' h_find_interval(3, c(2, 4, 6))
#' h_find_interval(1, c(2, 4, 6), replacement = -1)
h_find_interval <- function(..., replacement = -Inf) {
  assert_number(replacement)

  x <- findInterval(...)
  ifelse(x == 0, yes = replacement, no = x)
}

#' unpack stopping rules and return list
#'
#' @description
#'
#' recursively unpack nested stopping rules logical value and label given
#'
#' @param stopit_tree object from simulate method

h_unpack_stopit <- function(stopit_tree) {
  label <- attr(stopit_tree, "report_label")
  value <- stopit_tree[1]
  names(value) <- label
  value
  if (is.null(attr(stopit_tree, "individual"))) {
    return(value)
  } else {
    return(unlist(c(value, lapply(attr(stopit_tree, "individual"), h_unpack_stopit))))
  }
}

#' calculate percentage of true stopping rules for report label output
#'
#' @description
#'
#' calculates true column means and converts output into percentages
#' before combining the output with the report label; output is passed
#' to [`show()`] and output with cat to console
#'
#' @param stop_report object from summary method

h_calc_report_label_percentage <- function(stop_report) {
  stop_pct <- colMeans(stop_report) * 100
  stop_pct_to_print <- stop_pct[!is.na(names(stop_pct))]
  return(stop_pct_to_print)
}

#' Group Together Mono and Combo Data
#'
#' This is only used in the simulation method for `DesignGrouped` to combine
#' the separately generated data sets from mono and combo arms and to fit the
#' combined logistic regression model.
#' Hence the ID and cohort information is not relevant and will be
#' arbitrarily assigned to avoid problems with the [`DataGrouped`] validation.
#'
#' @param mono_data (`Data`)\cr mono data.
#' @param combo_data (`Data`)\cr combo data.
#'
#' @return A [`DataGrouped`] object containing both `mono_data` and `combo_data`,
#'   but with arbitrary ID and cohort slots.
#'
#' @keywords internal
h_group_data <- function(mono_data, combo_data) {
  assert_class(mono_data, "Data")
  assert_class(combo_data, "Data")

  df <- data.frame(
    x = c(mono_data@x, combo_data@x),
    y = c(mono_data@y, combo_data@y),
    group = rep(c("mono", "combo"), c(length(mono_data@x), length(combo_data@x)))
  )
  df <- df[order(df$x), ]

  DataGrouped(
    x = df$x,
    y = df$y,
    ID = seq_along(df$x),
    cohort = as.integer(factor(df$x)),
    doseGrid = sort(unique(c(mono_data@doseGrid, combo_data@doseGrid))),
    group = df$group
  )
}
