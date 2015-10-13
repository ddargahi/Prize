\name{ICR}
\alias{ICR}
\alias{Icr}
\alias{icr}
\alias{ICR,ANY-method}
\title{Individual consistency ratio (ICR) slot}

\description{
Individual consistency ratio (ICR) is a slot of geoAggreg and ariAggreg classes. It consists of a vector of individals judgements consistency ratio.
}
\examples{
mat <- matrix(nrow = 4, ncol = 1, data = NA)
mat[,1] <- c(system.file('extdata','ind1.tsv',package = 'Prize'),
            system.file('extdata','ind2.tsv',package = 'Prize'),
            system.file('extdata','ind3.tsv',package = 'Prize'),
            system.file('extdata','ind4.tsv',package = 'Prize'))
rownames(mat) <- c('ind1','ind2','ind3', 'ind4')
colnames(mat) <- c('individual_judgement')

# Aggregation of individual judgements
res <- gaggregate(srcfile = mat, method = 'geometric', simulation = 500)
ICR(res)

# Aggregation of individual priorities
res <- gaggregate(srcfile = mat, method = 'arithmetic', simulation = 500)
ICR(res)
}
\author{
Daryanaz Dargahi <daryanazdargahi@gmail.com>
}


