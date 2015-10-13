test_gaggregate <- function(){
  library(Prize)
  library(RUnit)
  library(diagram) # plotting
  library(ggplot2) # plotting
  library(stringr) 
  library(reshape2)
  library(base)
  
  AIJ_res <- matrix(c(1.0000000, 1.7431670, 3.3548322, 4.918313, 
                      0.5736685, 1.0000000, 3.6510408, 5.141337, 
                      0.2980775, 0.2738945, 1.0000000, 1.802501, 
                      0.2033217, 0.1945019, 0.5547847, 1.000000), nrow = 4, ncol = 4, byrow = TRUE)
  AIP_res <- c(0.45240240, 0.35504904, 0.11942840, 0.07312017 )
  DM <- matrix(nrow = 4, ncol = 2, data = NA)
  DM[,1] <- c(system.file('extdata','ind1.tsv',package = 'Prize'), 
              system.file('extdata','ind2.tsv',package = 'Prize'), 
              system.file('extdata','ind3.tsv',package = 'Prize'),
              system.file('extdata','ind4.tsv',package = 'Prize'))
  rownames(DM) <- c('ind1','ind2','ind3', 'ind4')
  colnames(DM) <- c('individual_judgement', 'individual_weight') 
  DM[,2] <- c(0.45,0.30,0.10,0.15)
  AIJ <- gaggregate(srcfile = DM, method = 'geometric', simulation = 500)@AIJ
  AIP <- gaggregate(srcfile = DM, method = 'arithmetic', simulation = 500)@AIP
  checkEqualsNumeric(round(AIJ_res, digits = 5),round(AIJ,digits = 5))
  checkEqualsNumeric(round(AIP_res, digits = 5),round(AIP,digits = 5))
}

