test_rating <- function(){
  library(Prize)
  library(RUnit)
  library(diagram) # plotting
  library(ggplot2) # plotting
  library(stringr) 
  library(reshape2)
  library(base)
  
  rating_res <- matrix(c("good","0.537284965911771", "poor","0.144337567297406",
                         "good","0.537284965911771", "fair","0.268642482955885",
                         "excellent","1"), nrow = 5, ncol = 2, byrow = TRUE, 
                       dimnames = list(c("Andy","Emily","Nina","Alex","Jack"), 
                                       c("scale_category","idealised_priorities")))
  rate <- matrix(nrow = 4, ncol = 4, data = NA)
  rownames(rate) = c('excellent','good','fair','poor')
  colnames(rate) = c('excellent','good','fair','poor')
  rate[1,] = c(1,2,4,6)
  rate[2,] = c(NA,1,2,4)
  rate[3,] = c(NA,NA,1,2)
  rate[4,] = c(NA,NA,NA,1)
  alt = matrix(nrow = 5, ncol = 2, data = NA)
  alt[,1] = c("Andy", "Emily", "Nina", "Alex", "Jack")
  alt[,2] = c("good", "poor", "good", "fair", "excellent")
  ratingRes <- rating(rate, alt, simulation = 500)@RM
  checkEquals(rating_res,ratingRes)
}

