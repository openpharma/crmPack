\docType{methods}
\name{stopTrial,StoppingMinCohorts,ANY,ANY,ANY,Data-method}
\alias{stopTrial,StoppingMinCohorts,ANY,ANY,ANY,Data-method}
\title{Stop based on minimum number of cohorts}
\usage{
\S4method{stopTrial}{StoppingMinCohorts,ANY,ANY,ANY,Data}(stopping, dose,
  samples, model, data, ...)
}
\arguments{
  \item{stopping}{The rule, an object of class
  \code{\linkS4class{Stopping}}}

  \item{dose}{the recommended next best dose}

  \item{samples}{the \code{\linkS4class{Samples}} object}

  \item{model}{The model input, an object of class
  \code{\linkS4class{Model}}}

  \item{data}{The data input, an object of class
  \code{\linkS4class{Data}}}

  \item{...}{additional arguments}
}
\description{
Stop based on minimum number of cohorts
}

