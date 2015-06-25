setMethod("show", signature(object = 'ahpObj'), function(object){
  cat("Object of class 'ahp'","\n")
  cat("This object has the following slots:\n")
  cat(paste("(1) weight: AHP weights of ", length(object@weight), " elements. \n", sep = ""))
  cat(paste("(2) saaty_inconsistency: Saaty's inconsistency with the value of ", round(object@saaty_inconsistency,digits=5), ".\n ", sep = ""))
})

#setMethod("summary", signature(object='ahp_out'), function(object){
#  cat(paste("An AHP object including the weight of ",length(object@weight), " elements and Saaty inconsistency value of ", round(object@saaty_inconsistency,digits=5), ".\n", sep = ''))
#})

setMethod("show", signature(object = 'ahmatrixObj'), function(object){
  cat("Object of class 'ahmatrix'","\n")
  cat("This object has the following slot:\n")
  cat(paste("(1) ahp_matrix: a ", dim(object@ahp_matrix)[1], " by ", dim(object@ahp_matrix)[2] ," square ahp matrix.\n", sep = ''))
})

setMethod("show", signature(object = 'pipelineObj'), function(object){
  cat("Object of class 'pipeline',","\n")
  cat("This object has the following slots:\n")
  cat("(1) ahp_plot: a matrix including the problem hierarchy and ahp weights. Visualize with ahplot().\n")
  cat("(2) weight_plot: a list including two matrices of ahp weights at criteria (criteria_wplot) and subcriteria (subcriteria_wplot) levels. Each matrix can be visualized with wplot(). \n")
  cat(paste("(3) rainbow_plot: a list including two matrices of ", dim(object@rainbow_plot$criteria_rainbowplot)[1] , " alternatives with their calculated ahp score at criteria (criteria_rainbowplot) and subcriteria (subcriteria_rainbowplot) levels. Visualize with rainbowplot().\n", sep = ''))
  cat(paste("(4) ahp_weights: a list of ",length(object@ahp_weights), " matrices including raw ahp weights.\n", sep = ''))
  cat(paste("(5) simulation: simulation size is ", object@simulation, ".\n" , sep = ''))
  cat("(6) saaty_inconsistency: a list of Saaty's inconsistencies.\n")
})

setMethod("show", signature(object = 'geoAggreg'), function(object){
  cat("Object of class 'aggregation'","\n")
  cat("This object has the following slots:\n")
  cat(paste("(1) AIJ: a ", dim(object@AIJ)[1], " by ", dim(object@AIJ)[2], " square matrix build from aggregation of ", length(object@ICR), " individual judgements.\n", sep = ""))
  cat(paste("(2) GCR: Saaty's inconsistency of the aggregated matrix equal to ", round(object@GCR, digits = 5) ,". \n", sep = ""))
  cat(paste("(3) CI: consisyency index for ",length(object@ICR)," individual judgements. \n",sep = ''))
  cat(paste("(4) ICR: Saaty's inconsistency value of ", length(object@ICR) ," individual judgements. \n", sep = ""))
  cat(paste("(5) IP: AHP priority score matrix of ",length(object@ICR)," individual(s) and thier aggregated group judgement. \n",sep = ''))
})

setMethod("show", signature(object = 'ariAggreg'), function(object){
  cat("Object of class 'aggregation'","\n")
  cat("This object has the following slots:\n")
  cat(paste("(1) AIP: a numeric vector of priorities with length of ", length(object@AIP),", build from aggregation of individual judgements.\n",sep = ""))
  cat(paste("(2) ICR: Saaty's inconsistency value of ", length(object@ICR) ," individual judgements. \n", sep = ""))
  cat(paste("(3) IP: AHP priority score matrix of ",length(object@ICR)," individual(s) and thier aggregated group judgements. \n",sep = ''))
})

setMethod("show", signature(object = 'ratingObj'), function(object){
  cat("Object of class 'rating'","\n")
  cat("This object has the following slots:\n")
  cat(paste("(1) weight: a ",dim(object@weight)[1]," by ", dim(object@weight)[2], " matrix of scale categories, including raw and idealised priorities. \n",sep = ''))
  cat(paste("(2) saaty_inconsistency: Saaty's inconsistency for a ", dim(object@weight)[1], " by ", dim(object@weight)[1], " scale matrix. \n",sep = ''))
  cat(paste("(3) RM (rating matrix): a matrix including rating values of " , dim(object@rating_mat)[1]," alternatives. \n",sep = ""))
})



