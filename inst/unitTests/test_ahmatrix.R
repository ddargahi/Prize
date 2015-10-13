test_ahmatrix <- function(){
  library(Prize)
  library(RUnit)
  library(diagram) # plotting
  library(ggplot2) # plotting
  library(stringr) 
  library(reshape2)
  library(base)

  ahp_matrix <- matrix(c(1.0,5.00,2, 0.2,1.00,4, 0.5,0.25,1), nrow = 3, ncol = 3, byrow = TRUE)
  ahmat <- matrix(c(NA,5,2, NA,NA,4, NA,NA,NA), nrow = 3, ncol = 3, byrow = TRUE)
  ahpmat <- ahmatrix(ahmat)@ahp_matrix
  checkEqualsNumeric(ahp_matrix,ahpmat)
}

