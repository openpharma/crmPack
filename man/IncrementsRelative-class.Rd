\docType{class}
\name{IncrementsRelative-class}
\alias{IncrementsRelative-class}
\title{Increments control based on relative differences in intervals}
\description{
Note that \code{intervals} is to be read as follows. If for
example, we want to specify three intervals: First 0 to
less than 50, second at least 50 up to less than 100 mg,
and third at least 100 mg, then we specify \code{intervals}
to be \code{c(0, 50, 100, Inf)}. That means, the right
bound of the intervals are exclusive to the interval.
}
\section{Slots}{

\describe{
\item{\code{intervals}}{a vector with the bounds of the relevant intervals of length
\code{n}.}

\item{\code{increments}}{a vector of length \code{n-1} with the maximum allowable
relative increments in the \code{intervals}}
}}
\keyword{classes}

