\docType{methods}
\name{initialize,LogisticLogNormalSub-method}
\alias{initialize,LogisticLogNormalSub-method}
\title{Initialization method for the "LogisticLogNormalSub" class}
\usage{
\S4method{initialize}{LogisticLogNormalSub}(.Object, mean, cov, refDose, ...)
}
\arguments{
  \item{.Object}{the
  \code{\linkS4class{LogisticLogNormalSub}} we want to
  initialize}

  \item{mean}{the prior mean vector}

  \item{cov}{the prior covariance matrix}

  \item{refDose}{the reference dose}

  \item{...}{data to include in the new object.  Named
  arguments correspond to slots in the class definition.
  Unnamed arguments must be objects from classes that this
  class extends.}
}
\description{
Initialization method for the "LogisticLogNormalSub" class
}
\keyword{methods}

