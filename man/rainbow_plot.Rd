\name{rainbow_plot}
\alias{rainbow_plot}
\alias{rainbow_plot,ANY-method}
\title{Rainbow plot slot}

\description{
rainbow_plot is a slot of pipelineObj class. It consists of a list of matrices representing final AHP ranking of alternatives at the criteria and subcriteria level.
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
rainbow_plot(result)
}
\author{
Daryanaz Dargahi <daryanazdargahi@gmail.com>
}



