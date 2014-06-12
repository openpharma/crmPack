\docType{methods}
\name{initialize,McmcOptions-method}
\alias{initialize,McmcOptions-method}
\title{Initialization method for the "McmcOptions" class}
\usage{
\S4method{initialize}{McmcOptions}(.Object, burnin = 10000L, step = 2L,
  samples = 10000L, ...)
}
\arguments{
  \item{.Object}{the \code{\linkS4class{McmcOptions}} we
  want to initialize}

  \item{burnin}{number of burn-in iterations which are not
  saved (default: \code{10,000})}

  \item{step}{only every step-th iteration is saved after
  the burn-in (default: \code{2})}

  \item{samples}{number of resulting samples (by default
  \code{10,000} will result)}

  \item{...}{data to include in the new object.  Named
  arguments correspond to slots in the class definition.
  Unnamed arguments must be objects from classes that this
  class extends.}
}
\description{
Initialization method for the "McmcOptions" class
}
\keyword{methods}

