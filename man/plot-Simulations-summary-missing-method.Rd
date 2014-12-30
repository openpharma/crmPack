\docType{methods}
\name{plot,Simulations-summary,missing-method}
\alias{plot,Simulations-summary,missing-method}
\title{Plot summaries of the model-based design simulations}
\usage{
\S4method{plot}{Simulations-summary,missing}(x, y, type = c("nObs",
  "doseSelected", "propDLTs", "nAboveTarget", "meanFit"), ...)
}
\arguments{
  \item{x}{the \code{\linkS4class{Simulations-summary}}
  object we want to plot from}

  \item{type}{the types of plots you want to obtain.}

  \item{y}{the y coordinates of points in the plot,
  \emph{optional} if \code{x} is an appropriate structure.}

  \item{...}{Arguments to be passed to methods, such as
  \link{graphical parameters} (see \code{\link{par}}).
  Many methods will accept the following arguments:
  \describe{ \item{\code{type}}{what type of plot should be
  drawn.  Possible types are \itemize{ \item \code{"p"} for
  \bold{p}oints, \item \code{"l"} for \bold{l}ines, \item
  \code{"b"} for \bold{b}oth, \item \code{"c"} for the
  lines part alone of \code{"b"}, \item \code{"o"} for both
  \sQuote{\bold{o}verplotted}, \item \code{"h"} for
  \sQuote{\bold{h}istogram} like (or \sQuote{high-density})
  vertical lines, \item \code{"s"} for stair \bold{s}teps,
  \item \code{"S"} for other \bold{s}teps, see
  \sQuote{Details} below, \item \code{"n"} for no plotting.
  } All other \code{type}s give a warning or an error;
  using, e.g., \code{type = "punkte"} being equivalent to
  \code{type = "p"} for S compatibility.  Note that some
  methods, e.g.\ifelse{{latex}{\out{~}}{
  }}{}\code{\link{plot.factor}}, do not accept this.  }
  \item{\code{main}}{an overall title for the plot: see
  \code{\link{title}}.} \item{\code{sub}}{a sub title for
  the plot: see \code{\link{title}}.} \item{\code{xlab}}{a
  title for the x axis: see \code{\link{title}}.}
  \item{\code{ylab}}{a title for the y axis: see
  \code{\link{title}}.} \item{\code{asp}}{the \eqn{y/x}
  aspect ratio, see \code{\link{plot.window}}.} } }
}
\value{
A single \code{\link[ggplot2]{ggplot2}} object if a single
plot is asked for, otherwise a
\code{\link{gridExtra}{gTree}} object.
}
\description{
Graphical display of the simulation summary
}
\details{
This plot method can be applied to
\code{\linkS4class{Simulations-summary}} objects in order
to summarize them graphically. Possible \code{type} of
plots at the moment are those listed in
\code{\link{plot,GeneralSimulations-summary-method}} plus:
\describe{ \item{meanFit}{Plot showing the average fitted
dose-toxicity curve across the trials, together with 95\%
credible intervals, and comparison with the assumed truth
(as specified by the \code{truth} argument to
\code{\link{summary,Simulations-method}})} } You can
specify any subset of these in the \code{type} argument.
}
\keyword{methods}

