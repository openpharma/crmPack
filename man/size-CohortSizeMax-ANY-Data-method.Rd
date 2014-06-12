\docType{methods}
\name{size,CohortSizeMax,ANY,Data-method}
\alias{size,CohortSizeMax,ANY,Data-method}
\title{Size based on maximum of multiple cohort size rules}
\usage{
\S4method{size}{CohortSizeMax,ANY,Data}(cohortSize, dose, data, ...)
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
Size based on maximum of multiple cohort size rules
}

