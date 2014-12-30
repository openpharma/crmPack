\docType{methods}
\name{initialize,DualEndpoint2-method}
\alias{initialize,DualEndpoint2-method}
\title{Initialization method for the "DualEndpoint2" class (JAGS version)}
\usage{
\S4method{initialize}{DualEndpoint2}(.Object, mu, Sigma, sigma2betaW, sigma2W,
  rho, smooth = c("RW1", "RW2"), ...)
}
\arguments{
  \item{.Object}{the \code{\linkS4class{DualEndpoint2}} we
  want to initialize}

  \item{mu}{see \code{\linkS4class{DualEndpoint2}}}

  \item{Sigma}{see \code{\linkS4class{DualEndpoint2}}}

  \item{sigma2betaW}{see
  \code{\linkS4class{DualEndpoint2}}}

  \item{sigma2W}{see \code{\linkS4class{DualEndpoint2}}}

  \item{rho}{see \code{\linkS4class{DualEndpoint2}}}

  \item{smooth}{either \dQuote{RW1} (default) or
  \dQuote{RW2}, for specifying the random walk prior on the
  biomarker level.}

  \item{...}{data to include in the new object.  Named
  arguments correspond to slots in the class definition.
  Unnamed arguments must be objects from classes that this
  class extends.}
}
\description{
Initialization method for the "DualEndpoint2" class (JAGS
version)
}
\keyword{methods}

