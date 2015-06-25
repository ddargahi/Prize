test_ahp <- function(){
  library(Prize)
  library(RUnit)
  library(diagram) # plotting
  library(ggplot2) # plotting
  library(stringr) 
  library(reshape2)
  library(base)
  
  ahp_res <- c(0.6013351, 0.2591074, 0.1395575)
  ahmat <- matrix(c(NA,5,2, NA,NA,4, NA,NA,NA), nrow = 3, ncol = 3, byrow = TRUE)
  ahpRes <- ahp(ahmat)@weight
  checkEqualsNumeric(round(ahp(ahmat)@weight, digits=5), round(ahp_res,digits = 5))
}

