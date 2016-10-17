\name{AIP}
\alias{AIP}
\alias{aip}
\alias{Aip}
\alias{AIP,ANY-method}
\title{Aggregated individual priorities (AIP) slot}

\description{
AIP (Aggregated individual priorities) is a slot of ariAggreg class. It consists of a vector of aggregated group priorities.
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
res <- gaggregate(srcfile = mat, method = 'arithmetic', simulation = 500)
AIP(res)
}
\author{
Daryanaz Dargahi <daryanazdargahi@gmail.com>
}


