\name{GCR}
\alias{GCR}
\alias{Gcr}
\alias{gcr}
\alias{GCR,ANY-method}
\title{Group consistency ratio (GCR) slot}

\description{
GCR (Group consistency ratio) is a slot of geoAggreg class. It consists of the consistency ratio of the aggregated group judgement.
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
GCR(res)
}
\author{
Daryanaz Dargahi <daryanazdargahi@gmail.com>
}


