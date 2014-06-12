\docType{class}
\name{StoppingAll-class}
\alias{StoppingAll-class}
\title{Stop based on fullfillment of all multiple stopping rules}
\description{
This class can be used to combine multiple stopping rules
with an AND operator.
}
\details{
\code{stopList} contains all stopping rules, which are
again objects of class \code{\linkS4class{Stopping}}. All
stopping rules must be fulfilled in order that the result
of this rule is to stop.
}
\section{Slots}{

\describe{
\item{\code{stopList}}{list of stopping rules
of the stopping rules into a single result}
}}
\keyword{classes}

