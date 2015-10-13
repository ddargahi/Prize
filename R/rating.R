#' rating
#' 
#' @description Estimates idealised priorities of alternatives (the rating AHP model).  
#' @param scale a pairwise comparison matrix (PCM) of rating categories.
#' @param alternative a N by 2 character matrix, where N is the number of alternatives. The matrix includes alternatives on column #1 and the rating category they belong to on column #2.
#' @param NA_category a character string or vector which specifies categories with the value of zero. Since zero is not achievable by PCM matrix.
#' @param simulation simulation size for computation of Saaty's inconsistency
#' @author Daryanaz Dargahi
#' @references  T.L. Saaty. Rank from comparisons and from ratings in the analytic hierarchy/network processes. European Journal of Operational Research, 168(2):557-570, January 2006.
#' @references T.L. Saaty. The Analytic Hierarchy Process, Planning, Piority Setting, Resource Allocation. McGraw-Hill, New york, 1980.
#' @return An S4 object including the raw and normalized ahp priorities, Satty's inconsistency, and rating matrix.
#' @examples
#' 
#' mat <- matrix(nrow = 4, ncol = 4, data = NA)
#' 
#' # The category PCM matrix
#' rownames(mat) <- c('excellent','good','fair','poor')
#' colnames(mat) <- c('excellent','good','fair','poor')
#' mat[1,] <- c(1,2,4,6)
#' mat[2,] <- c(NA,1,2,4)
#' mat[3,] <- c(NA,NA,1,2)
#' mat[4,] <- c(NA,NA,NA,1)
#' 
#' # The alternative matrix
#' alt <- matrix(nrow = 5, ncol = 2, data = NA)
#' alt[,1] <- c("Andy", "Emily", "Nina", "Alex", "Jack")
#' alt[,2] <- c("good", "poor", "good", "fair", "excellent")
#' 
#' result <- rating(mat, alt, simulation = 500)
#' 
#' # Specifying a category with value of zero
#' alt <- rbind(alt, c('shannon', 'Not_available'))
#' 
#' result <- rating(mat, alt, NA_category = 'Not_available', simulation = 500)
#' @export

rating <- function(scale, alternative, NA_category = NULL, simulation = 500){
  # reading the srcfile
  if (class(scale) %in% c('matrix', 'data.frame')){
    scale <- as.matrix(scale)
  }else {
      stop('\"scale\" must be a square matrix.')
  }    

  if (length(NA_category) > 0) {
    if((class(NA_category) %in% 'character') || all(is.na(NA_category))){
      # do nothing if it is a character or NA
      if(NA_category %in% c("NA")){
        NA_category <- NA 
      }
    } else {
      stop('NA_category is either a character string or a character vector.')
    }
  }
  
  if(length(NA_category) > 0){ 
    category <- append(rownames(scale), NA_category)
  } else {
    category <- rownames(scale)
  }
  
  if (class(alternative) %in% c('matrix', 'data.frame')){
    alternative <- as.matrix(alternative)
    if (all(unique(alternative[,2]) %in% category)){
      # do nothing
    } else {
      name <- paste(as.character(category), collapse= ', ')
      stop(paste('please assign each alternative one of the following categories:', name, sep = ' '))
    }
  }else {
    stop('\"alternative\" must be a n by 2 character matrix, where n is the number of alternatives.')
  } 
  
  tmp <- ahp(scale,simulation = simulation)
  weight <- list(scale = as.matrix(tmp@weight))  
  weight[[1]] <- cbind(weight[[1]] , (weight[[1]] / max(weight[[1]][,1])))  
  colnames(weight[[1]]) <- c('priorities', 'idealised_priorities')
  inconsis <- tmp@saaty_inconsistency
  
  if(length(NA_category) > 0){ 
    nas = length(NA_category)
    for(i in seq_along(NA_category)){ # 1:length(NA_category)
      weight[[1]] <- rbind(weight[[1]], c(0, 0))
      rownames(weight[[1]])[dim(weight[[1]])[1]] <- NA_category[i]
    }
  }
  
  alternative <- cbind(alternative,0)
  nalt = length(alternative[,1])
  for(i in seq_len(nalt)){ # 1:length(alternative[,1])
    index <- which(rownames(weight[[1]]) %in% as.character(alternative[i,2])) ## %in%
    score <- weight[[1]][index,2]
    alternative[i,3] <- score  
  }
  colnames(alternative) <- c('alternative','scale_category','idealised_priorities')
  rownames(alternative) <- alternative[,1]
  return(new("ratingObj", weight = weight$scale, saaty_inconsistency = inconsis, RM = alternative[,c(2,3)]))
}

