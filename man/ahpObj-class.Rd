\name{ahpObj-class}
%\docType(class)
\alias{ahpObj}
\alias{ahpObj-class}
\alias{ahpObj, ANY-method}
\title{Class \code{ahpObj}}

\description{
An object that stores AHP priorities and consistency ratio.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("ahpObj", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{weight}:}{A vector of AHP priorities.}
    \item{\code{saaty_inconsistency}:}{The consistency ratio.}
  }
}

\author{
Daryanaz Dargahi <daryanazdargahi@gmail.com>
}

