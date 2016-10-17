test_pipeline <- function(){
  library(Prize)
  library(RUnit)
  library(diagram) # plotting
  library(ggplot2) # plotting
  library(stringr) 
  library(reshape2)
  library(base)
  
  rainbow_res <- matrix(c(0.061335201, 0.01248896, 0.016786400, 0.007461660, 0.09807222, 
                          0.061335201, 0.01248896, 0.016786400, 0.007461660, 0.09807222,
                          0.017114410, 0.04853918, 0.002480381, 0.007461660, 0.07559563,
                          0.061335201, 0.04853918, 0.016786400, 0.007461660, 0.13412244,
                          0.061335201, 0.04853918, 0.016786400, 0.007461660, 0.13412244,
                          0.061335201, 0.01248896, 0.006816627, 0.006950588, 0.08759138,
                          0.061335201, 0.01248896, 0.016786400, 0.007461660, 0.09807222,
                          0.017114410, 0.08407232, 0.003506147, 0.007461660, 0.11215454,
                          0.061335201, 0.01248896, 0.016786400, 0.007461660, 0.09807222,
                          0.006154539, 0.04853918, 0.002480381, 0.006950588, 0.06412469),
                        nrow = 10, ncol = 5, byrow = TRUE)
  ahplot_res <- matrix(c("0",   "Prioritization_of_DE_genes", NA,
                         "1",   "Tumor_expression",        "0.469729767043589", 
                         "2",   "Normal_expression",       "0.340673842031607",
                         "3",   "Frequency",               "0.116001935957881",
                         "4",   "Epitopes",                "0.0735944549669227",
                         "4.1", "Number_of_epitopes",      "0.0091993044560472",
                         "4.2", "Size_of_epitopes",        "0.0643951505108755"), 
                       nrow = 7, ncol = 3, byrow = TRUE)
  pipe <- matrix(nrow = 7, ncol = 3, data = NA)
  pipe[,1] <- c('0', '1','2','3','4','4.1','4.2')
  pipe[,2] <- c('Prioritization_of_DE_genes','Tumor_expression','Normal_expression',
                'Frequency', 'Epitopes', 'Number_of_epitopes', 'Size_of_epitopes')
  pipe[,3] <- c(system.file('extdata','aggreg.judgement.tsv',package = 'Prize'), 
                system.file('extdata','tumor.PCM.tsv',package = 'Prize'), 
                system.file('extdata','normal.PCM.tsv',package = 'Prize'), 
                system.file('extdata','freq.PCM.tsv',package = 'Prize'), 
                system.file('extdata','epitope.PCM.tsv',package = 'Prize'), 
                system.file('extdata','epitopeNum.PCM.tsv',package = 'Prize'), 
                system.file('extdata','epitopeLength.PCM.tsv',package = 'Prize'))
  
  pipeRes <- pipeline(pipe, model = 'relative', simulation = 500) # ahp_plot and rainbow_plot$criteria_rainbowplot
  
  checkEqualsNumeric(round(rainbow_res, digits = 5),round(pipeRes@rainbow_plot$criteria_rainbowplot,digits = 5))
  checkEqualsNumeric(pipeRes@ahp_plot, ahplot_res)
}

