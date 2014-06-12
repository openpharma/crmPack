\docType{methods}
\name{initialize,LogisticNormal-method}
\alias{initialize,LogisticNormal-method}
\title{Initialization method for the "LogisticNormal" class}
\usage{
\S4method{initialize}{LogisticNormal}(.Object, mean, cov, refDose, ...)
}
\arguments{
  \item{.Object}{the \code{\linkS4class{LogisticNormal}} we
  want to initialize}

  \item{mean}{the prior mean vector}

  \item{cov}{the prior covariance matrix}

  \item{refDose}{the reference dose}

  \item{...}{data to include in the new object.  Named
  arguments correspond to slots in the class definition.
  Unnamed arguments must be objects from classes that this
  class extends.}
}
\description{
Initialization method for the "LogisticNormal" class
}
\keyword{methods}

