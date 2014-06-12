\docType{class}
\name{StoppingList-class}
\alias{StoppingList-class}
\title{Stop based on multiple stopping rules}
\description{
This class can be used to combine multiple stopping rules.
}
\details{
\code{stopList} contains all stopping rules, which are
again objects of class \code{\linkS4class{Stopping}}, and
the \code{summary} is a function taking a logical vector of
the size of \code{stopList} and returning a single logical
value. For example, if the function \code{all} is given as
\code{summary} function, then this means that all stopping
rules must be fulfilled in order that the result of this
rule is to stop.
}
\section{Slots}{

\describe{
\item{\code{stopList}}{list of stopping rules}

\item{\code{summary}}{the summary function to combine the results
of the stopping rules into a single result}
}}
\keyword{classes}

