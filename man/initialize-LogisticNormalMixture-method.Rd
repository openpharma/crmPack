\docType{methods}
\name{initialize,LogisticNormalMixture-method}
\alias{initialize,LogisticNormalMixture-method}
\title{Initialization method for the "LogisticNormalMixture" class}
\usage{
\S4method{initialize}{LogisticNormalMixture}(.Object, comp1, comp2, weightpar,
  refDose, ...)
}
\arguments{
  \item{.Object}{the
  \code{\linkS4class{LogisticNormalMixture}} we want to
  initialize}

  \item{comp1}{the specifications of the first component: a
  list with \code{mean} and \code{cov} for the first
  bivariate normal prior}

  \item{comp2}{the specifications of the second component}

  \item{weightpar}{the beta parameters for the weight of
  the first component}

  \item{refDose}{the reference dose}

  \item{...}{data to include in the new object.  Named
  arguments correspond to slots in the class definition.
  Unnamed arguments must be objects from classes that this
  class extends.}
}
\description{
Initialization method for the "LogisticNormalMixture" class
}
\keyword{methods}

