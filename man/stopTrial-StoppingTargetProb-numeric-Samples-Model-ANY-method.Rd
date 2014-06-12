\docType{methods}
\name{stopTrial,StoppingTargetProb,numeric,Samples,Model,ANY-method}
\alias{stopTrial,StoppingTargetProb,numeric,Samples,Model,ANY-method}
\title{Stop based on probability of target tox interval}
\usage{
\S4method{stopTrial}{StoppingTargetProb,numeric,Samples,Model,ANY}(stopping,
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
Stop based on probability of target tox interval
}

