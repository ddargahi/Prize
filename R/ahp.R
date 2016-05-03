#' ahp
#' 
#' @description Computing AHP weights as well as Satty's inconsistency.
#' @param x a pairwise comparison matrix (PCM) with diagonal values equal 1 and a[i,j] = 1/a[j,i].
#' @param simulation the simulation size in order to compute Satty's inconsistency. The default value is 500.
#' @return An S4 object including a numeric vector of AHP weights and Satty's inconsistency.
#' @author Daryanaz Dargahi
#' @references T.L. Saaty. A scaling method for priorities in hierarchical structures. Journal of Mathematical Psychology, 15(3):234-281, 1977.
#' @references T.L. Saaty. The Analytic Hierarchy Process, Planning, Piority Setting, Re- source Allocation. McGraw-Hill, New york, 1980.
#' @examples 
#' 
#' mat <- matrix(c(1,2,5, 1/2,1,3, 1/5,1/3,1), nrow = 3, ncol = 3, byrow = TRUE)
#' res <- ahp(mat, simulation = 500)
#' @export

ahp <- function(x, simulation = 500){
  # reading input
  srcfile <- x
  if (class(srcfile) %in% c('matrix', 'data.frame')){
    x <- srcfile
  } else {
    if(file.exists(srcfile)){
      x <- read.delim(srcfile, sep = '\t', header = TRUE, row.names = 1)
    }else {
      stop('\"srcfile\" is missing.')
    }    
  }
  # inconsistency
  if (simulation == 0) {
    inconsistency <- FALSE
  } else {
    inconsistency <- TRUE
  }
  
  x <- as.matrix(x)
  
  #if((x[1,2] %in% c("","NA", NA)) || (x[2,1] %in% c("","NA", NA))){
  if(all(is.na(x[upper.tri(x)])) || all(is.na(x[lower.tri(x)]))){
    triangular <- TRUE
  } else {
    triangular <- FALSE
  }  
  
  if(triangular == FALSE){
    if(nrow(x) != ncol(x)){ # not a square matrix
      stop('The input is not a square matrix.')
    }
    if(!all(diag(x) == 1)){ # diag values must be 1.
      stop('All diagonal values must be 1.')
    }
    if(length(which(x == Inf , arr.ind = TRUE)) > 0 || length (which(is.na(x) == TRUE , arr.ind = TRUE)) > 0){
      stop('infinite or missing values in \'srcfile\'')
    }
    # check for if a[i,j] = y then a[j,i] = 1/y
    tmp1 <- x
    tmp1[lower.tri(tmp1,diag = TRUE)] <- NA
    tmp1 <- ahmatrix(tmp1)
    tmp1 <- as.matrix(tmp1@ahp_matrix)
    if((!identical(round(tmp1,2),round(x,2))) || length(which(x==0,arr.ind = TRUE)) > 0){
      stop('Please check if a[i,j] = y then a[j,i] = 1/y.')
    }
  }
  if (triangular == TRUE){
    out <- ahmatrix(x)
    x <- as.matrix(out@ahp_matrix)
    # the output matrix is square, satisfy ahp matrix criteria, no inf/NA value
  }
  
  # calculating AHP weights
  res <- log10(x[1:dim(x)[1],])
  weight <- apply(res,1,sum)
  weight <- weight/ncol(x)
  weight <- 10^weight
  tmp_sum <- sum(weight)
  weight <- weight/tmp_sum
  #obj <- list(ahpweight = weight)
  consistency_ratio <- numeric()
  
  # if satty_inconsistency true then calculate consistency ratio
  if(inconsistency == TRUE){
    lambda_max <- max(Re(eigen(x)$value))
    consistency_index <- (lambda_max - dim(x)[1])/(dim(x)[1] - 1) 
    random_index <- rep(0,simulation)
    for (i in 1:simulation){
      tmp1 <- sample(1:9, dim(x)[1] * dim(x)[1], replace = TRUE)
      tmp2 <- sample(0:1, dim(x)[1] * dim(x)[1], replace = TRUE)
      tmp1[which(tmp2 == 0)] <-  1/tmp1[which(tmp2 == 0)]
      mat <- matrix(nrow = dim(x)[1], ncol = dim(x)[1], data = tmp1)
      mat[lower.tri(mat)] <- NA
      tmp <- ahmatrix(mat)
      mat <- as.matrix(tmp@ahp_matrix)
      lambda_max_tmp <- max(Re(eigen(mat)$value))
      random_index[i] <- (lambda_max_tmp - dim(mat)[1])/(dim(mat)[1] - 1) 
    }
    consistency_ratio <- consistency_index/mean(random_index)  
    #obj <- list(weighting = weight, Saaty_inconsistency = consistency_ratio)
  }
  return(new("ahpObj", weight = weight, saaty_inconsistency = consistency_ratio)) # simulation = simulation))
}
