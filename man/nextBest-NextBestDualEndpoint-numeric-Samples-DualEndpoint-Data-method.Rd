\docType{methods}
\name{nextBest,NextBestDualEndpoint,numeric,Samples,DualEndpoint,Data-method}
\alias{nextBest,NextBestDualEndpoint,numeric,Samples,DualEndpoint,Data-method}
\title{Find the next best dose based on the dual endpoint model}
\usage{

  \S4method{nextBest}{NextBestDualEndpoint,numeric,Samples,DualEndpoint,Data}(nextBest,
  doselimit, samples, model, data, ...)
}
\arguments{
  \item{nextBest}{The rule, an object of class
  \code{\linkS4class{NextBest}}}

  \item{doselimit}{The maximum allowed next dose}

  \item{samples}{the \code{\linkS4class{Samples}} object}

  \item{model}{The model input, an object of class
  \code{\linkS4class{Model}}}

  \item{data}{The data input, an object of class
  \code{\linkS4class{Data}}}

  \item{...}{possible additional arguments without method
  dispatch}
}
\description{
Find the next best dose based on the dual endpoint model
}

