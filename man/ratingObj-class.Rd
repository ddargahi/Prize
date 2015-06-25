\name{ratingObj-class}
%\docType(class)
\alias{ratingObj}
\alias{ratingObj-class}
\alias{ratingObj, ANY-method}
\title{Class \code{ratingObj}}

\description{
An object that stores the idealised priorities of alternatives (The rating AHP model).
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("ratingObj", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{weight}:}{A matrix of rating categoris and their AHP and idealised prioritise.}
    \item{\code{saaty_inconsistency}:}{The consistency ratio of the categories pairwise comparison matrix.}
    \item{\code{RM}:}{A matrix of alternatives with their scale category and idealised prioritise.}
  }
}

\author{
Daryanaz Dargahi <daryanazdargahi@gmail.com>
}
