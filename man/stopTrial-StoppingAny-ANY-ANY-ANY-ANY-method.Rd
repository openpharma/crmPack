\docType{methods}
\name{stopTrial,StoppingAny,ANY,ANY,ANY,ANY-method}
\alias{stopTrial,StoppingAny,ANY,ANY,ANY,ANY-method}
\title{Stop based on fulfillment of any stopping rule}
\usage{
\S4method{stopTrial}{StoppingAny,ANY,ANY,ANY,ANY}(stopping, dose, samples,
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
Stop based on fulfillment of any stopping rule
}

