\name{maxDose}
\alias{maxDose}
\title{Determine the maximum possible next dose}
\usage{
maxDose(increments, data, ...)
}
\arguments{
  \item{increments}{The rule, an object of class
  \code{\linkS4class{Increments}}}

  \item{data}{The data input, an object of class
  \code{\linkS4class{Data}}}

  \item{\dots}{further arguments}
}
\value{
the maximum possible next dose
}
\description{
Determine the upper limit of the next dose based on the
increments rule.
}
\details{
This function outputs the maximum possible next dose, based
on the corresponding rule \code{increments} and the
\code{data}.
}
\keyword{methods}

