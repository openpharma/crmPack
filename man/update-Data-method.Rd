\docType{methods}
\name{update,Data-method}
\alias{update,Data-method}
\title{Update method for the "Data" class}
\usage{
\S4method{update}{Data}(object, x, y, ID, ...)
}
\arguments{
  \item{object}{the old \code{\linkS4class{Data}} object}

  \item{x}{the dose level (one level only!)}

  \item{y}{the DLT vector (0/1 vector), for all patients in
  this cohort}

  \item{ID}{the patient IDs}

  \item{...}{Additional arguments to the call, or arguments
  with changed values. Use \code{name = NULL} to remove the
  argument \code{name}.}
}
\value{
the new \code{\linkS4class{Data}} object
}
\description{
Add new data to the \code{\linkS4class{Data}} object
}
\keyword{methods}

