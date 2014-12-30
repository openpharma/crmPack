\docType{methods}
\name{nextBest,NextBestThreePlusThree,missing,missing,missing,Data-method}
\alias{nextBest,NextBestThreePlusThree,missing,missing,missing,Data-method}
\title{Find the next best dose based on the 3+3 method}
\usage{

  \S4method{nextBest}{NextBestThreePlusThree,missing,missing,missing,Data}(nextBest,
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
Find the next best dose based on the 3+3 method
}

