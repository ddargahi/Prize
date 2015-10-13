\name{RM}
\alias{RM}
\alias{Rm}
\alias{rm}
\alias{RM,ANY-method}
\title{Rating matrix (RM) slot}

\description{
RM (Rating matrix) is a slot of ratingObj class. It consists of a matrix of alternatives with their scale category and idealised prioritise.
}
\examples{
mat <- matrix(nrow = 4, ncol = 4, data = NA)

# Category PCM matrix
rownames(mat) <- c('excellent','good','fair','poor')
colnames(mat) <- c('excellent','good','fair','poor')
mat[1,] <- c(1,2,4,6)
mat[2,] <- c(NA,1,2,4)
mat[3,] <- c(NA,NA,1,2)
mat[4,] <- c(NA,NA,NA,1)

# Alternative matrix
alt <- matrix(nrow = 5, ncol = 2, data = NA)
alt[,1] <- c("Andy", "Emily", "Nina", "Alex", "Jack")
alt[,2] <- c("good", "poor", "good", "fair", "excellent")

result <- rating(mat, alt, simulation = 500)
RM(result)
}
\author{
Daryanaz Dargahi <daryanazdargahi@gmail.com>
}


