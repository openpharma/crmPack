\docType{class}
\name{DataParts-class}
\alias{DataParts-class}
\title{Class for the data with two study parts}
\description{
This is a subclass of \code{\linkS4class{Data}}, so
contains all slots from \code{\linkS4class{Data}}, and in
addition information on the two study parts.
}
\section{Slots}{

\describe{
\item{\code{part}}{integer vector; which part does each of the patients belong to?}

\item{\code{nextPart}}{integer; what is the part for the next cohort?}

\item{\code{part1Ladder}}{sorted numeric vector; what is the escalation ladder for
part 1? This shall be a subset of the \code{doseGrid}.}
}}
\keyword{classes}

