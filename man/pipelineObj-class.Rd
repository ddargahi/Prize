\name{pipelineObj-class}
%\docType(class)
\alias{pipelineObj}
\alias{pipelineObj-class}
\alias{pipelineObj, ANY-method}
\title{Class \code{pipelineObj}}

\description{
An object that stores the problem hierarchy as well as AHP priorities.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("pipelineObj", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{ahp_plot}:}{A matrix of problem hierarchy.}
    \item{\code{weight_plot}:}{A list of matrices representing AHP priorities at the criteria and subcriteria level.}
    \item{\code{rainbow_plot}:}{A list of matrices representing final AHP ranking of alternatives at the criteria and subcriteria level.}
    \item{\code{ahp_weights}:}{A list of matrices representing AHP priorities for all decision elements.}
    \item{\code{simulation}:}{A numeric value of the simulation size.}
    \item{\code{saaty_inconsistency}:}{A list of Saaty inconsistency ratios.}
  }
} 

\author{
Daryanaz Dargahi <daryanazdargahi@gmail.com>
}
