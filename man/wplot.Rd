% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/wplot.R
\name{wplot}
\alias{wplot}
\title{Wplot}
\usage{
wplot(srcfile, color = "rainbow", fontsize = 15, xcex = 10, ycex = 10,
  pcex = 5, digit = 2, xlab = NULL, ylab = NULL, type = "bar",
  main = NULL)
}
\arguments{
\item{srcfile}{a matrix, where the first column includes criteria/subcriteria ID and the second includes the ahp weights.}

\item{color}{the color palette to fill bars. Either provide a vector of n colors, where n is the number of criteria, or choose from the following palettes 'rainbow, heat, terrain, topo, cm'. The default value is rainbow.}

\item{fontsize}{the font size of the plot title, and x and y axis labels. The default value is 15.}

\item{xcex,ycex}{the font size of the x and y axis, respectively. The default values is 10.}

\item{pcex}{the font size of the labels inside pie chart}

\item{digit}{the number of digits after decimal point to be shown on the x axis.}

\item{xlab,ylab}{the label of the x and y axis, respectively.}

\item{type}{wplot offers two plot types; bar and pie plots. Default value is bar.}

\item{main}{the plot title}
}
\value{
An object created by 'ggplot'.
}
\description{
Plotting the criteria/subcriteria ahp weights in a bar/pie plot.
}
\examples{
mat <- matrix(nrow = 4, ncol = 2, data = NA)
mat[,1] <- c('Tumor_expression','Normal_expression','Frequency','Epitope')
mat[,2] <- c(0.470, 0.341, 0.116, 0.073)

wplot(mat, xlab = 'Weight', ylab = 'Criteria', type = 'bar')
wplot(mat, type = 'pie')
}
\author{
Daryanaz Dargahi
}

