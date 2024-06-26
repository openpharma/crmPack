# nolint start

#####################################################################################
## Author: Daniel Sabanes Bove [sabanesd *a*t* roche *.* com]
## Project: Object-oriented implementation of CRM designs
##
## Time-stamp: <[crmPack-package.R] by DSB Mon 11/05/2015 17:47>
##
## Description:
## Package description.
##
## History:
## 29/01/2014   file creation
#####################################################################################

#' Object-oriented implementation of CRM designs
#'
#' @name crmPack-package
#' @aliases crmPack
#' @docType package
#' @title Object-oriented implementation of CRM designs
#' @import checkmate
#' @import ggplot2
#' @import methods
#' @import tibble
#' @importFrom grid grid.draw
#' @importFrom gridExtra arrangeGrob
#' @importFrom graphics plot hist legend lines matlines matplot
#' @importFrom stats binomial coef cov2cor gaussian glm lm median model.matrix
#'   optim pgamma plogis pnorm qgamma qlogis qnorm quantile rbinom rgamma
#'   approxfun rnorm runif uniroot var vcov step mad pbeta dbeta dgamma
#'   setNames
#' @importFrom utils data head tail capture.output
#' @importFrom lifecycle badge
#' @importFrom rjags jags.model jags.samples
#' @importFrom futile.logger flog.threshold flog.logger flog.trace TRACE FATAL
#' @importFrom knitr knit_print
#' @importFrom kableExtra kbl add_header_above column_spec collapse_rows
#'   kable_styling add_footnote kable
#'
#' @keywords package
#' @references Sabanes Bove D, Yeung WY, Palermo G, Jaki T (2019).
#' "Model-Based Dose Escalation Designs in R with crmPack."
#' Journal of Statistical Software, 89(10), 1-22.
#' doi:10.18637/jss.v089.i10 (URL: http://doi.org/10.18637/jss.v089.i10).
{}

##' @keywords internal
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Type crmPackHelp() to open help browser\n",
    "Type crmPackExample() to open example\n"
  )
}

## need to declare global variable / function
## names in order to avoid R CMD check notes:
globalVariables(c(
  "log.betaZ",
  "precW",
  "pow",
  "nObs",
  "betaZ",
  "x",
  "betaW",
  "xLevel",
  "precW",
  "z",
  "nGrid",
  "doseGrid",
  "betaWintercept",
  "delta",
  "deltaStart",
  "delta2",
  "Effsamples",
  "logit<-",
  "rho0",
  "alpha0",
  "delta0",
  "alpha1",
  "delta1",
  "inverse",
  "priorCov",
  "theta",
  "comp0",
  "w",
  "DLTs",
  "y",
  "group",
  "annotate",
  "probSamples",
  "prec",
  "nu",
  "samples",
  "Type",
  "patient",
  "toxicity",
  "ID",
  "biomarker",
  "traj",
  "Statistic",
  "perc",
  "..density..",
  "middle",
  "lower",
  "upper",
  "middleBiomarker",
  "lowerBiomarker",
  "upperBiomarker",
  "nObsshare",
  "xshare",
  "yshare",
  "thisProb.PL",
  "thisMeanEff.PL",
  "thisSize.PL",
  "probit<-",
  "refDose",
  "Tmax",
  "u",
  "eps",
  "h",
  "lambda",
  "cadj",
  "A",
  "lambda_p",
  "cond",
  "t0",
  "tend",
  "t0_case",
  "tend_case",
  "yhat",
  "ref_dose",
  "comp",
  "X",
  "skel_probs",
  "is_combo",
  "results",
  "k",
  "value",
  "Parameter",
  "intervals",
  "Group",
  "Tox",
  "MaxOverdoseProb",
  "DoseGrid",
  "NGrid",
  "NObs",
  "XLevel"
))

# nolint end
