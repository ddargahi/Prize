\name{ahp_matrix}
\alias{ahp_matrix}
\alias{ahpmatrix}
\alias{ahp_matrix,ANY-method}
\title{AHP matrix slot}

\description{
ahp_matrix is a slot of ahmatrixObj class. It consists of a square pairwise comparison matrix.
}
\examples{
mat <- matrix(nrow = 3, ncol = 3, data = NA)
mat[1,2] <- 5
mat[1,3] <- 2
mat[2,3] <- 7
res <- ahmatrix(mat)
ahp_matrix(res)
}
\author{
Daryanaz Dargahi <daryanazdargahi@gmail.com>
}


