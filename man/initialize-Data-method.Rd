\docType{methods}
\name{initialize,Data-method}
\alias{initialize,Data-method}
\title{Initialization method for the "Data" class}
\usage{
\S4method{initialize}{Data}(.Object, x = numeric(), y = integer(),
  ID = integer(), cohort = integer(), doseGrid = numeric(), ...)
}
\arguments{
  \item{.Object}{the \code{\linkS4class{Data}} we want to
  initialize}

  \item{x}{the doses for the patients}

  \item{y}{the vector of toxicity events (0 or 1 integers)}

  \item{ID}{unique patient IDs (integer vector)}

  \item{cohort}{the cohort indices (sorted values from 0,
  1, 2, ...)}

  \item{doseGrid}{the vector of all possible doses}

  \item{...}{data to include in the new object.  Named
  arguments correspond to slots in the class definition.
  Unnamed arguments must be objects from classes that this
  class extends.}
}
\description{
This is the method for initializing a "Data" class object.
}
\details{
Note that \code{ID} and \code{cohort} can be missing, then
a warning will be issued and the variables will be filled
with default IDs and best guesses, respectively.
}
\keyword{methods}

