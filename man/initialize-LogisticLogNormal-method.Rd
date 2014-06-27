\docType{methods}
\name{initialize,LogisticLogNormal-method}
\alias{initialize,LogisticLogNormal-method}
\title{Initialization method for the "LogisticLogNormal" class}
\usage{
\S4method{initialize}{LogisticLogNormal}(.Object, mean, cov, refDose, ...)
}
\arguments{
  \item{.Object}{the \code{\linkS4class{LogisticLogNormal}}
  we want to initialize}

  \item{mean}{the prior mean vector}

  \item{cov}{the prior covariance matrix}

  \item{refDose}{the reference dose}

  \item{...}{data to include in the new object.  Named
  arguments correspond to slots in the class definition.
  Unnamed arguments must be objects from classes that this
  class extends.}
}
\description{
Initialization method for the "LogisticLogNormal" class
}
\keyword{methods}

