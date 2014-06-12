\docType{methods}
\name{stopTrial,StoppingTargetBiomarker,numeric,Samples,DualEndpoint,ANY-method}
\alias{stopTrial,StoppingTargetBiomarker,numeric,Samples,DualEndpoint,ANY-method}
\title{Stop based on probability of targeting biomarker}
\usage{

  \S4method{stopTrial}{StoppingTargetBiomarker,numeric,Samples,DualEndpoint,ANY}(stopping,
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
Stop based on probability of targeting biomarker
}

