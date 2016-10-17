\name{CI}
\alias{CI}
\alias{Ci}
\alias{ci}
\alias{CI,ANY-method}
\title{Consistency index (CI) slot}

\description{
CI (Consistency index) is a slot of geoAggreg class. It consists of a vector of the individuals consistency index.
}
\examples{
mat <- matrix(nrow = 4, ncol = 1, data = NA)
mat[,1] <- c(system.file('extdata','ind1.tsv',package = 'Prize'),
            system.file('extdata','ind2.tsv',package = 'Prize'),
            system.file('extdata','ind3.tsv',package = 'Prize'),
            system.file('extdata','ind4.tsv',package = 'Prize'))
rownames(mat) <- c('ind1','ind2','ind3', 'ind4')
colnames(mat) <- c('individual_judgement')

# non-weighted aggregation
res <- gaggregate(srcfile = mat, method = 'geometric', simulation = 500)
CI(res)
}
\author{
Daryanaz Dargahi <daryanazdargahi@gmail.com>
}


