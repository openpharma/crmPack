\docType{methods}
\name{initialize,LogisticNormalFixedMixture-method}
\alias{initialize,LogisticNormalFixedMixture-method}
\title{Initialization method for the "LogisticNormalFixedMixture" class}
\usage{
\S4method{initialize}{LogisticNormalFixedMixture}(.Object, components, weights,
  refDose, logNormal = FALSE, ...)
}
\arguments{
  \item{.Object}{the
  \code{\linkS4class{LogisticNormalFixedMixture}} we want
  to initialize}

  \item{components}{the specifications of the mixture
  components: a list with one list of \code{mean} and
  \code{cov} for each bivariate (log) normal prior}

  \item{weights}{the weights of the components, these must
  be positive and will be normalized to sum to 1}

  \item{refDose}{the reference dose}

  \item{logNormal}{should a log normal prior be specified,
  such that the mean vectors and covariance matrices are
  valid for the intercept and log slope? (not default)}

  \item{...}{data to include in the new object.  Named
  arguments correspond to slots in the class definition.
  Unnamed arguments must be objects from classes that this
  class extends.}
}
\description{
Initialization method for the "LogisticNormalFixedMixture"
class
}
\keyword{methods}

