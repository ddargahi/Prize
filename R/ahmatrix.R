#' ahmatrix
#' 
#' @description Converting a triangular matrix into a square pairwise comparison matrix (PCM) where the diagonal values are equal to 1 and a[i,j] = 1/a[j,i].
#' 
#' @param x a numeric triangular matrix, where empty elements are specified with NA.
#' @return An S4 object including a PCM.
#' @author Daryanaz Dargahi
#' @examples
#' 
#' mat <- matrix(nrow = 3, ncol = 3, data = NA)
#' mat[1,2] <- 5
#' mat[1,3] <- 2
#' mat[2,3] <- 7
#' 
#' res <- ahmatrix(mat)
#' @export

ahmatrix <- function(x){
  srcfile <- x
  # reading input
  if (class(srcfile) %in% c('matrix', 'data.frame')){
    x <- as.matrix(srcfile)
  } else {
    if(file.exists(srcfile)){
      x <- read.delim(srcfile, sep = '\t', header = TRUE, row.names = 1)
    }else {
      stop('\"srcfile\" is missing.')
    }    
  }
  
  x <- as.matrix(x)
  # change the checks to this all(is.na(tmp[lower.tri(tmp)]))
  if(length(which(x==0,arr.ind = TRUE)) > 0){
    stop('Zero is not allowed in the matrix, infinite values are being created in the square matrix.')
  }
  if(all(is.na(x[upper.tri(x)]))){ 
    lower <- TRUE
    upper <- FALSE
    x[upper.tri(x,diag = TRUE)] = 0
  } else if(all(is.na(x[lower.tri(x)]))){ 
    upper <- TRUE
    lower <- FALSE
    x[lower.tri(x,diag = TRUE)] = 0
  } else {
    stop('\"srcfile\" is not a triangular matrix.')
  }
  
  if(length(which(x == Inf , arr.ind = TRUE)) > 0 || length (which(is.na(x) == TRUE , arr.ind = TRUE)) > 0){
    stop('infinite or missing values in the triangular matrix')
  }
  
  if(upper == TRUE){
    x[lower.tri(x)] <- 0
    tmp <- t(1/(x))
    tmp[upper.tri(tmp)] <- 0 
    tmp[!is.finite(tmp)] <- 0
    x <- x + tmp
    diag(x) <- 1
    x <- as.matrix(x)
  }
  if(lower == TRUE){
    x[upper.tri(x)] <- 0
    tmp <- t(1/(x))
    tmp[lower.tri(tmp)] <- 0 
    tmp[!is.finite(tmp)] <- 0
    x <- x + tmp
    diag(x) <- 1
    x <- as.matrix(x)    
  }
  return(new("ahmatrixObj", ahp_matrix = x))
}