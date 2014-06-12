\docType{methods}
\name{size,CohortSizeRange,ANY,Data-method}
\alias{size,CohortSizeRange,ANY,Data-method}
\title{Determine the cohort size based on the range into which the next
dose falls into}
\usage{
\S4method{size}{CohortSizeRange,ANY,Data}(cohortSize, dose, data, ...)
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
Determine the cohort size based on the range into which the
next dose falls into
}

