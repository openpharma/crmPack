\docType{class}
\name{RuleDesign-class}
\alias{RuleDesign-class}
\title{Class for rule-based designs}
\description{
The difference to \code{\linkS4class{Design}} class is that
model, stopping and increments slots are missing.
}
\section{Slots}{

\describe{
\item{\code{nextBest}}{how to find the next best dose, an object of class
\code{\linkS4class{NextBest}}}

\item{\code{cohortSize}}{rules for the cohort sizes,
an object of class \code{\linkS4class{CohortSize}}}

\item{\code{data}}{what is the dose grid, any previous data, etc., contained
in an object of class \code{\linkS4class{Data}}}

\item{\code{startingDose}}{what is the starting dose? Must lie on the grid in \code{data}}
}}
\keyword{classes}

