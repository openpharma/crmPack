\docType{methods}
\name{initialize,LogisticKadane-method}
\alias{initialize,LogisticKadane-method}
\title{Initialization method for the "LogisticKadane" class}
\usage{
\S4method{initialize}{LogisticKadane}(.Object, theta, xmin, xmax, ...)
}
\arguments{
  \item{.Object}{the \code{\linkS4class{LogisticKadane}} we
  want to initialize}

  \item{theta}{the target toxicity probability}

  \item{xmin}{the minimum of the dose range}

  \item{xmax}{the maximum of the dose range}

  \item{...}{data to include in the new object.  Named
  arguments correspond to slots in the class definition.
  Unnamed arguments must be objects from classes that this
  class extends.}
}
\description{
Initialization method for the "LogisticKadane" class
}
\keyword{methods}

