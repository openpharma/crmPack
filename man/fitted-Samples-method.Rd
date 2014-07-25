\docType{methods}
\name{fitted,Samples-method}
\alias{fitted,Samples-method}
\title{Fit method for the Samples class}
\usage{
\S4method{fitted}{Samples}(object, model, data, points = data@doseGrid,
  quantiles = c(0.025, 0.975), middle = mean, ...)
}
\arguments{
  \item{object}{the \code{\linkS4class{Samples}} object}

  \item{model}{the \code{\linkS4class{Model}} object}

  \item{data}{the \code{\linkS4class{Data}} object}

  \item{points}{at which dose levels is the fit requested?
  default is the dose grid}

  \item{quantiles}{the quantiles to be calculated (default:
  0.025 and 0.975)}

  \item{middle}{the function for computing the middle
  point. Default: \code{\link{mean}}}

  \item{...}{other arguments.}
}
\value{
data frame with dose, middle, lower and upper quantiles
}
\description{
Fit method for the Samples class
}
\keyword{methods}

