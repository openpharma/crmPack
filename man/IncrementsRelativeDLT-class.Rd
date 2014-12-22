\docType{class}
\name{IncrementsRelativeDLT-class}
\alias{IncrementsRelativeDLT-class}
\title{Increments control based on relative differences in terms of DLTs}
\description{
Note that \code{DLTintervals} is to be read as follows. If
for example, we want to specify three intervals: First 0
DLTs, second 1 or 2 DLTs, and third at least 3 DLTs, then
we specify \code{DLTintervals} to be \code{c(0, 1, 3,
Inf)}. That means, the right bound of the intervals are
exclusive to the interval.
}
\section{Slots}{

\describe{
\item{\code{DLTintervals}}{a vector with the bounds of the relevant DLT intervals of
length \code{n}}

\item{\code{increments}}{a vector of length \code{n-1} with the maximum allowable
relative increments in the \code{DLTintervals}}
}}
\keyword{classes}

