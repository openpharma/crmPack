\docType{class}
\name{StoppingAny-class}
\alias{StoppingAny-class}
\title{Stop based on fullfillment of any stopping rule}
\description{
This class can be used to combine multiple stopping rules
with an OR operator.
}
\details{
\code{stopList} contains all stopping rules, which are
again objects of class \code{\linkS4class{Stopping}}. Any
of these rules must be fulfilled in order that the result
of this rule is to stop.
}
\section{Slots}{

\describe{
\item{\code{stopList}}{list of stopping rules
of the stopping rules into a single result}
}}
\keyword{classes}

