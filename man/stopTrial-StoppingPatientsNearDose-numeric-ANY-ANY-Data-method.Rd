\docType{methods}
\name{stopTrial,StoppingPatientsNearDose,numeric,ANY,ANY,Data-method}
\alias{stopTrial,StoppingPatientsNearDose,numeric,ANY,ANY,Data-method}
\title{Stop based on number of patients near to next best dose}
\usage{
\S4method{stopTrial}{StoppingPatientsNearDose,numeric,ANY,ANY,Data}(stopping,
  dose, samples, model, data, ...)
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
Stop based on number of patients near to next best dose
}

