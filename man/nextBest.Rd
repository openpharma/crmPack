\name{nextBest}
\alias{nextBest}
\title{Find the next best dose}
\usage{
nextBest(nextBest, doselimit, samples, model, data, ...)
}
\arguments{
  \item{nextBest}{The rule, an object of class
  \code{\linkS4class{NextBest}}}

  \item{doselimit}{The maximum allowed next dose}

  \item{samples}{the \code{\linkS4class{Samples}} object}

  \item{model}{The model input, an object of class
  \code{\linkS4class{Model}}}

  \item{data}{The data input, an object of class
  \code{\linkS4class{Data}}}

  \item{\dots}{possible additional arguments without method
  dispatch}
}
\value{
a list with the next best dose (element \code{value}) on
the grid defined in \code{data}, and a plot depicting this
recommendation (element \code{plot})
}
\description{
Compute the recommended next best dose.
}
\details{
This function outputs the next best dose recommendation
based on the corresponding rule \code{nextBest}, the
posterior \code{samples} from the \code{model} and the
underlying \code{data}.
}
\keyword{methods}

