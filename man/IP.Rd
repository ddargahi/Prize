\name{IP}
\alias{IP}
\alias{Ip}
\alias{ip}
\alias{IP,ANY-method}
\title{Individual priority (IP) slot}

\description{
IP (Individual priority) is a slot of geoAggreg and ariAggreg classes. It consists of a matrix of individual and aggregated group AHP priorities.
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
IP(res)

# Aggregation of individual priorities
res <- gaggregate(srcfile = mat, method = 'arithmetic', simulation = 500)
IP(res)
}
\author{
Daryanaz Dargahi <daryanazdargahi@gmail.com>
}


