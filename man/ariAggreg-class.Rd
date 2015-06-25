\name{ariAggreg-class}
%\docType(class)
\alias{ariAggreg}
\alias{ariAggreg-class}
\alias{ariAggreg, ANY-method}
\title{Class \code{ariAggreg}}

\description{
An object that stores the aggregated group priorities as well as consistency ratio of the individual judgements.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("ariAggreg", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{AIP}:}{A vector of aggregated group priorities.}
    \item{\code{ICR}:}{A vector of individals judgements consistency ratio.}
    \item{\code{IP}:}{A matrix of individual and aggregated group AHP priorities.}
  }
}

\author{
Daryanaz Dargahi <daryanazdargahi@gmail.com>
}
