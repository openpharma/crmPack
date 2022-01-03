#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: crmPack
##
## Time-stamp: <[writeModel.R] by DSB Mon 05/01/2015 17:38>
##
## Description:
## This is the write.model functionality from R2WinBUGS. We only need this,
## therefore we do not want to require the whole package, which in turn would
## require WinBUGS installation.
##
## History:
## 08/12/2014   file creation
#####################################################################################

##' Creating a WinBUGS model file
##'
##' Convert R function to a \pkg{WinBUGS} model file. BUGS models follow
##' closely S syntax. It is therefore possible to write most BUGS models as R
##' functions.
##' As a difference, BUGS syntax allows truncation specification like this:
##' \code{dnorm(...) I(...)}  but this is illegal in R. To overcome this
##' incompatibility, use dummy operator \code{\%_\%} before \code{I(...)}:
##' \code{dnorm(...) \%_\% I(...)}. The dummy operator \code{\%_\%} will be
##' removed before the BUGS code is saved.
##' In S-PLUS, a warning is generated when the model function is defined if the
##' last statement in the model is an assignment. To avoid this warning, add the
##' line \code{invisible()} to the end of the model definition. This line will be
##' removed before the BUGS code is saved.
##'

#' @param model R function containing the JAGS model
#'  in the BUGS ##' model language, for minor differences see Section Details.
##' @param con passed to \code{\link{writeLines}} which actually writes the
##' model file
##' @param digits number of significant digits used for \pkg{WinBUGS}
##' input, see \code{\link{formatC}}
##'
##' @export
##' @author original idea by Jouni Kerman, modified by Uwe Ligges,
##'  DSB removed S-Plus part, WW modified the implementation.
writeModel <- function(model, con = "model.bug", digits = 5) { # nolintr
  if (is.R()) {
    model_text <- c("model", h_replace_scientific(model, digits = digits))
  } else {
    model_text <- paste("model", as.character(model))
  }
  model_text <- gsub("%_%", "", model_text)
  writeLines(model_text, con = con)
}

h_replace_scientific <- function(model, digits = 5) {
  assert_function(model)
  assert_int(digits)

  bmodel <- h_bapply(
    body(model),
    h_format_number,
    classes = c("integer", "numeric"),
    digits
  )
  bmodel_text <- deparse(bmodel, control = NULL)
  gsub("\"TEMP_NUM_PREF_|_TEMP_NUM_SUF\"", "", bmodel_text)
}

# Body apply
h_bapply <- function(b, f, classes, ...) {
  assert_function(f)
  assert_character(classes, min.len = 1L)

  for (i in seq_along(b)) {
    if (class(b[[i]]) %in% classes) {
      b[[i]] <- do.call(f, c(list(b[[i]]), ...))
    } else if (length(b[[i]]) > 1L) {
      b[[i]] <- h_bapply(b[[i]], f, classes, ...)
    }
  }
  b
}

h_format_number <- function(x,
                            digits = 5,
                            pref = "TEMP_NUM_PREF_",
                            suf = "_TEMP_NUM_SUF") {
  assert_number(x)
  assert_int(digits)

  if ((abs(x) < 1e-3) || (abs(x) > 1e+4)) {
    paste0(pref, formatC(x, digits = digits, format = "E"), suf)
  } else {
    x
  }
}
