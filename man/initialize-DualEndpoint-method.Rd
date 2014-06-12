\docType{methods}
\name{initialize,DualEndpoint-method}
\alias{initialize,DualEndpoint-method}
\title{Initialization method for the "DualEndpoint" class}
\usage{
\S4method{initialize}{DualEndpoint}(.Object, mu, Sigma, sigma2betaW, sigma2W,
  rho, smooth = c("RW1", "RW2"), ...)
}
\arguments{
  \item{.Object}{the \code{\linkS4class{DualEndpoint}} we
  want to initialize}

  \item{mu}{see \code{\linkS4class{DualEndpoint}}}

  \item{Sigma}{see \code{\linkS4class{DualEndpoint}}}

  \item{sigma2betaW}{see \code{\linkS4class{DualEndpoint}}}

  \item{sigma2W}{see \code{\linkS4class{DualEndpoint}}}

  \item{rho}{see \code{\linkS4class{DualEndpoint}}}

  \item{smooth}{either \dQuote{RW1} (default) or
  \dQuote{RW2}, for specifying the random walk prior on the
  biomarker level.}

  \item{...}{data to include in the new object.  Named
  arguments correspond to slots in the class definition.
  Unnamed arguments must be objects from classes that this
  class extends.}
}
\description{
Initialization method for the "DualEndpoint" class
}
\keyword{methods}

