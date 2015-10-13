\name{AIJ}
\alias{AIJ}
\alias{aij}
\alias{Aij}
\alias{AIJ,ANY-method}
\title{Aggregated individual judgements (AIJ) slot}

\description{
AIJ (Aggregated individual judgements) is a slot of geoAggreg class. It consists of a matrix of aggregated group judgements (a pairwise comparison matrix).
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
AIJ(res)
}
\author{
Daryanaz Dargahi <daryanazdargahi@gmail.com>
}

