\docType{methods}
\name{nextBest,NextBestNCRM,numeric,Samples,Model,DataParts-method}
\alias{nextBest,NextBestNCRM,numeric,Samples,Model,DataParts-method}
\title{Find the next best dose based on the NCRM method when two parts
trial is used}
\usage{
\S4method{nextBest}{NextBestNCRM,numeric,Samples,Model,DataParts}(nextBest,
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
Find the next best dose based on the NCRM method when two
parts trial is used
}

