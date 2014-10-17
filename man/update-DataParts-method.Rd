\docType{methods}
\name{update,DataParts-method}
\alias{update,DataParts-method}
\title{Update method for the "DataParts" class}
\usage{
\S4method{update}{DataParts}(object, x, y, ID = (if (length(object@ID))
  max(object@ID) else 0L) + seq_along(y), ...)
}
\arguments{
  \item{object}{the old \code{\linkS4class{DataParts}}
  object}

  \item{x}{the dose level (one level only!)}

  \item{y}{the DLT vector (0/1 vector), for all patients in
  this cohort}

  \item{ID}{the patient IDs}

  \item{...}{Additional arguments to the call, or arguments
  with changed values. Use \code{name = NULL} to remove the
  argument \code{name}.}
}
\value{
the new \code{\linkS4class{DataParts}} object
}
\description{
Add new data to the \code{\linkS4class{DataParts}} object
}
\keyword{methods}

