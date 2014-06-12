\docType{methods}
\name{size,CohortSizeMin,ANY,Data-method}
\alias{size,CohortSizeMin,ANY,Data-method}
\title{Size based on minimum of multiple cohort size rules}
\usage{
\S4method{size}{CohortSizeMin,ANY,Data}(cohortSize, dose, data, ...)
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
Size based on minimum of multiple cohort size rules
}

