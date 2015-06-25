#' rainbowplot
#' 
#' @description Plotting prioritized alternatives in a color-coded barplot, where a color is assigned to each criteria/subcriteria. 
#' @param srcfile a numeric matrix with alternatives on the rows and criteria/subcriteria on the columns. alternative's AHP weights for each criteria, and the sum of each row on the last column (see example below).
#' @param range specifies which alternatives to plot. Either provide a range (e.g. 1:5) or a vector of numbers (e.g. c(2,5,12,20)). if range is NULL then all alternatives will be plotted. The default value is NULL.
#' @param fontsize the font size of the plot title, x and y axis labels, and legend. The default value is 10.
#' @param xcex the font size of the labels on the bars. The default values is 4.
#' @param ycex the font size of the y axis. The default values is 8.
#' @param color the color palette to fill bars. Either provide a vector of n colors, where n is the number of criteria/subcriteria, or choose from the following palettes 'rainbow, heat, terrain, topo, cm'. The default value is rainbow.
#' @param xlab,ylab the label of the x and y axis, respectively.
#' @param digit the number of digits after decimal point to be shown on the bars. The default value is 3.
#' @param dist the distance between the bar and its lable. The default value is 0.02.
#' @param main the plot title
#' @return An object created by 'ggplot'.
#' @author Daryanaz Dargahi
#' @examples
#' 
#' mat <- matrix(c(0.007,0.289,0.033,0.118,0.447, 0.015,0.155,0.015,0.088,0.275, 0.048,0.078,0.007,0.044,0.177, 0.039,0.042,0.003,0.017,0.101), 
#'                 nrow = 4, ncol = 5, byrow = TRUE, dimnames = list(c('CA9','MUC16','CD70','MUC1'), c('Tumor_expression','Normal_expression','Style','Frequency','Epitopes')))
#' rainbowplot(mat, range = NULL, xlab = 'Total priority score' , ylab = 'Alternative', dist = 0.04)
#' @export

rainbowplot <- function(srcfile, range = NULL, fontsize = 10, xcex = 4, ycex = 8, color = 'rainbow', xlab = 'Total priority score' , ylab = 'Alternative', digit = 3 , dist = 0.02, main = NULL){
  
  if (class(srcfile) %in% c('matrix', 'data.frame')){
    file <- data.frame(srcfile)
  }else{
    if(file.exists(srcfile)){
      file <- read.delim(srcfile, sep = '\t', header = TRUE, row.names = 1)
    }else {
      stop('\"srcfile\" is missing.')
    }    
  }
  
  # if the last column is not the total priority (sum of values on a row for an alternative)
  temp <- sum(file[,dim(file)[2]])
  if(temp < 0.999){
    file <- cbind(file, apply(file,1,sum))
    colnames(file)[dim(file)[2]] <- 'total_priorities'
  }
  colnames(file)[dim(file)[2]] <- 'total_priorities'
  
  # sort alternatives according to their priority value
  file <- file[order(file$total_priorities, decreasing=TRUE), ]
  
  if(length(range) == 0){ 
    # meaning range is NULL, therefore do nothing
  } else {
    if(all(is.na(range))){  
      # do nothing - plot all 
    } else {
      if (all(range <= dim(file)[1])){
        file <- file[range,]
      } else {
        stop('The specified range is greater than the number of alternatives.')
      }
    }
  }
  
  N <- dim(file)[2] - 1 # number of criteria and subcriteria
  file[,dim(file)[2]] <- rownames(file) # file <- cbind(file, rownames(file))
  colnames(file)[dim(file)[2]] <- 'Candid'
  DF <- melt(file, id.var="Candid")
  colnames(DF) <- c('Candid','Criteria','Value')
  
  # colors
  if(length(color) == 1 && color %in% c('rainbow','heat','terrain','topo','cm')){
    if(color == 'rainbow'){
      colors <- rainbow(N)
    }
    if(color == 'heat'){
      colors <- heat.colors(N)
    }
    if(color == 'terrain'){
      colors <- terrain.colors(N)
    }
    if(color == 'topo'){
      colors <- topo.colors(N)
    }  
    if(color == 'cm'){
      colors <- cm.colors(N)
    }
  } else {
    if(length(color) != N){
      stop (paste(N, ' colors is required.', sep = '')) 
    }else {
      colors <- color
    }
  }
  
  # plotting
  DF$Candid <- as.character(DF$Candid)
  DF$Candid <- factor(DF$Candid, levels = unique(as.character(DF$Candid)) ,ordered=TRUE)
  
  # computing the labels and their positions
  DF2 <- DF[order(DF$Candid), ]
  totals <- as.vector(by(DF2$Value, DF2$Candid, sum))
  totals <- round(totals,digits = digit)
  pos <- rep(totals + dist, each=N) 
  labels <- unlist(lapply(as.character(totals), function(x) c(rep("", (N-1)), x))) 
  DF2 <- data.frame(DF2, Pos=pos, Labels=labels)
  
  # plotting
  ggplot(DF2, aes_string(x = 'Candid', y = 'Value', fill = 'Criteria')) + 
    geom_bar(position = 'stack', stat = "identity") +
    geom_text(aes_string(y='Pos', label='Labels'), size = xcex, vjust=0) +
    coord_flip() + theme(text = element_text(size=fontsize)) + 
    theme(axis.text.x = element_blank(), axis.line.x=element_blank(), axis.ticks.x=element_blank()) + 
    theme(axis.text.y = element_text(size = ycex)) +
    scale_fill_manual(values=colors) + 
    xlab(ylab) +
    ylab(xlab) +
    ggtitle(main) 
  
  
  #ggplot(DF, aes_string(x = 'Candid', y = 'Value', fill = 'Criteria')) + geom_bar(stat = "identity") + 
  #  coord_flip() + theme(text = element_text(size=fontsize)) + 
  #  theme(axis.text.x = element_text(size = xcex)) + 
  #  theme(axis.text.y = element_text(size = ycex)) +
  #  scale_fill_manual(values=colors) + 
  #  xlab(ylab) +
  #  ylab(xlab) +
  #  ggtitle(main)
}
