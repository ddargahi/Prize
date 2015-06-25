\name{saaty_inconsistency}
\alias{saaty_inconsistency}
\alias{saatyinconsistency}
\alias{saaty_inconsistency,ANY-method}
\title{Saaty inconsistency slot}

\description{
saaty_inconsistency is a slot of ahpObj, pipelineObj, and ratingObj classes. It consists of Saarty's consistency ratio of a pairwise comparison matrix.
}
\examples{
# ahp()
mat <- matrix(c(1,2,5, 1/2,1,3, 1/5,1/3,1), nrow = 3, ncol = 3, byrow = TRUE)
res <- ahp(mat, simulation = 500)
saaty_inconsistency(res)

# pipeline()
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
saaty_inconsistency(result)

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
saaty_inconsistency(result)
}
\author{
Daryanaz Dargahi <daryanazdargahi@gmail.com>
}




