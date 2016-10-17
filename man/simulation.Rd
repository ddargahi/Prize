\name{simulation}
\alias{simulation}
\alias{simulation,ANY-method}
\title{Simulation slot}

\description{
simulation is a slot of pipelineObj class. It consists of a numeric value of the simulation size (used in order to estimate Saaty's consistency ratio).
}
\examples{
mat <- matrix(nrow = 7, ncol = 3, data = NA)
mat[,1] <- c('0', '1','2','3','4','4.1','4.2')
mat[,2] <- c('Prioritization_of_DE_genes','Tumor_expression','Normal_expression',
             'Frequency', 'Epitopes', 'Number_of_epitopes', 'Size_of_epitopes')
mat[,3] <- c(system.file('extdata','aggreg.judgement.tsv',package = 'Prize'),
             system.file('extdata','tumor.PCM.tsv',package = 'Prize'),
             system.file('extdata','normal.PCM.tsv',package = 'Prize'),
             system.file('extdata','freq.PCM.tsv',package = 'Prize'),
             system.file('extdata','epitope.PCM.tsv',package = 'Prize'),
             system.file('extdata','epitopeNum.PCM.tsv',package = 'Prize'),
             system.file('extdata','epitopeLength.PCM.tsv',package = 'Prize'))

result <- pipeline(mat, model = 'relative', simulation = 500)
simulation(result)
}
\author{
Daryanaz Dargahi <daryanazdargahi@gmail.com>
}



