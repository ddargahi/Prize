\name{weight}
\alias{weight}
\alias{weight,ANY-method}
\title{Weight slot}

\description{
weight is a slot of ahpObj and ratingObj classes. It consists of computed AHP weights from a pairwise comparison matrix.
}
\examples{
# ahp()
mat <- matrix(c(1,2,5, 1/2,1,3, 1/5,1/3,1), nrow = 3, ncol = 3, byrow = TRUE)
res <- ahp(mat, simulation = 500)
weight(res)

# rating()
mat <- matrix(nrow = 4, ncol = 4, data = NA)

## Category PCM matrix
rownames(mat) <- c('excellent','good','fair','poor')
colnames(mat) <- c('excellent','good','fair','poor')
mat[1,] <- c(1,2,4,6)
mat[2,] <- c(NA,1,2,4)
mat[3,] <- c(NA,NA,1,2)
mat[4,] <- c(NA,NA,NA,1)
## Alternative matrix
alt <- matrix(nrow = 5, ncol = 2, data = NA)
alt[,1] <- c("Andy", "Emily", "Nina", "Alex", "Jack")
alt[,2] <- c("good", "poor", "good", "fair", "excellent")

result <- rating(mat, alt, simulation = 500)
weight(result)
}


\author{
Daryanaz Dargahi <daryanazdargahi@gmail.com>
}



