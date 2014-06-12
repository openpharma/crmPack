\docType{methods}
\name{size,CohortSizeDLT,ANY,Data-method}
\alias{size,CohortSizeDLT,ANY,Data-method}
\title{Determine the cohort size based on the number of DLTs so far}
\usage{
\S4method{size}{CohortSizeDLT,ANY,Data}(cohortSize, dose, data, ...)
}
\arguments{
  \item{cohortSize}{The rule, an object of class
  \code{\linkS4class{CohortSize}}}

  \item{dose}{the next dose}

  \item{data}{The data input, an object of class
  \code{\linkS4class{Data}}}

  \item{...}{additional arguments}
}
\description{
Determine the cohort size based on the number of DLTs so
far
}

