\docType{class}
\name{Design-class}
\alias{Design-class}
\title{Class for the CRM design}
\description{
Class for the CRM design
}
\section{Slots}{

\describe{
\item{\code{model}}{the model to be used, an object of class
\code{\linkS4class{Model}}}

\item{\code{nextBest}}{how to find the next best dose, an object of class
\code{\linkS4class{NextBest}}}

\item{\code{stopping}}{stopping rule(s) for the trial, an object of class
\code{\linkS4class{Stopping}}}

\item{\code{increments}}{how to control increments between dose levels,
an object of class \code{\linkS4class{Increments}}}

\item{\code{cohortSize}}{rules for the cohort sizes,
an object of class \code{\linkS4class{CohortSize}}}

\item{\code{data}}{what is the dose grid, any previous data, etc., contained
in an object of class \code{\linkS4class{Data}}}

\item{\code{startingDose}}{what is the starting dose? Must lie on the grid in \code{data}}
}}
\keyword{classes}

