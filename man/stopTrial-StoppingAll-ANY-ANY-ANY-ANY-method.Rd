\docType{methods}
\name{stopTrial,StoppingAll,ANY,ANY,ANY,ANY-method}
\alias{stopTrial,StoppingAll,ANY,ANY,ANY,ANY-method}
\title{Stop based on fulfillment of all multiple stopping rules}
\usage{
\S4method{stopTrial}{StoppingAll,ANY,ANY,ANY,ANY}(stopping, dose, samples,
  model, data, ...)
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
Stop based on fulfillment of all multiple stopping rules
}

