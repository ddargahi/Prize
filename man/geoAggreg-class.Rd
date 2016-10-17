\name{geoAggreg-class}
%\docType(class)
\alias{geoAggreg}
\alias{geoAggreg-class}
\alias{geoAggreg, ANY-method}
\title{Class \code{geoAggreg}}

\description{
An object that stores the aggregated group judgements as well as consistency ratio of the individuals and group judgements.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("geoAggreg", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{AIJ}:}{A matrix of aggregated group judgements (a pairwise comparison matrix).}
    \item{\code{GCR}:}{The consistency ratio of the aggregated group judgement.}
    \item{\code{CI}:}{A vector of the individuals consistency index.}
    \item{\code{ICR}:}{A vector of individals judgements consistency ratio.}
    \item{\code{IP}:}{A matrix of individual and aggregated group AHP priorities.}
  }
}

\author{
Daryanaz Dargahi <daryanazdargahi@gmail.com>
}

