#' dplot
#' 
#' @description Computing and plotting the distance between individuals and group judgement. Distances are computed using classical multidimensional scaling (MDS) approach.
#' @param srcfile a numeric matrix of individual and group priorities.  
#' @param fontsize the font size of the plot title, and x and y axis labels. The default value is 15.
#' @param xcex,ycex the font size of the x and y axis, respectively. The default values is 10.
#' @param lcex the font size of point labels in dplot
#' @param hjust,vjust the horizontal and vertical justification of point labels, respectively.
#' @param main the plot title
#' @param xlab,ylab the label of the x and y axis, respectively. 
#' @author Daryanaz Dargahi
#' @return An object created by 'ggplot'.
#' @references J.C. Gower. Some distance properties of latent root and vector methods used in multivariate analysis. Biometrika, 53(3/4):pp. 325-338, 1966.
#' @examples
#' 
#' mat <- matrix(nrow = 5, ncol = 4, data = NA)
#' rownames(mat) <- c('Ind1','Ind2','Ind3', 'Ind4' ,'Group judgement')
#' colnames(mat) <- c('Tumor_expression','Normal_expression','Frequency','Epitopes')
#' mat[1,] <- c(0.4915181, 0.3058879, 0.12487821, 0.07771583) 
#' mat[2,] <- c(0.3060687, 0.4949012, 0.12868606, 0.07034399)
#' mat[3,] <- c(0.4627138, 0.3271881, 0.13574662, 0.07435149)
#' mat[4,] <- c(0.6208484, 0.2414021, 0.07368481, 0.06406465)
#' mat[5,] <- c(0.4697298, 0.3406738, 0.11600194, 0.07359445)
#' 
#' dplot(mat, xlab = 'Coordinate 1', ylab = 'Coordinate 2', main = 'Distance plot')
#' 
#' @export

dplot <- function(srcfile, fontsize = 15, xcex = 10, ycex = 10, lcex = 5, hjust=0.5, vjust=1, xlab = 'Coordinate 1', ylab = 'Coordinate 2', main = NULL){
  if (class(srcfile) %in% c('matrix', 'data.frame')){
    file <- as.matrix(srcfile)
  }else{
    stop('\"srcfile\" must be a numeric matrix.')
  }    
  
  d <- dist(file) # euclidean distances between the rows
  fited <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
  
  mat <- as.data.frame(fited$points)
  colnames(mat) <- c('X','Y')  
  mat <- cbind(mat, rownames(mat))
  colnames(mat)[dim(mat)[2]] <- 'lab'
  
  ggplot(data = mat, aes_string(x = 'X',y = 'Y')) + 
    geom_point() + 
    geom_text(aes_string(label='lab'), size = lcex, hjust = hjust, vjust = vjust) + 
    theme(text = element_text(size=fontsize)) + 
    theme(axis.text.x = element_text(size = xcex)) + 
    theme(axis.text.y = element_text(size = ycex)) +
    ggtitle(main) +
    xlab(xlab) +
    ylab(ylab)  
}
  
  