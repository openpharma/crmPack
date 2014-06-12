\name{mcmc}
\alias{mcmc}
\title{Obtain posterior samples for all model parameters}
\usage{
mcmc(data, model, options, ...)
}
\arguments{
  \item{data}{The data input, an object of class
  \code{\linkS4class{Data}}}

  \item{model}{The model input, an object of class
  \code{\linkS4class{Model}}}

  \item{options}{MCMC options, an object of class
  \code{\linkS4class{McmcOptions}}}

  \item{\dots}{unused}
}
\value{
The posterior samples, an object of class
\code{\linkS4class{Samples}}.
}
\description{
Obtain posterior samples for all model parameters
}
\details{
This is the function to actually run the MCMC machinery to
produce posterior samples from all model parameters and
required derived values. It is a generic function, so that
customized versions may be conveniently defined for
specific subclasses of Data, Model, and McmcOptions input.
}
\keyword{methods}
\keyword{regression}

