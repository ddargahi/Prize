#' crplot
#' 
#' @description Plotting the consistency ratio (CR) of individual judgements. According to Satty, a pairwise comparison matrix is considered to be consistent if CR is equal or less than 0.1. Therefore, CRs equal to or less than 0.1 are colored in green (Pass) and CRs greater than 0.1 are colored in red (Failed).
#' @param srcfile a numeric vector of individual CR 
#' @param fontsize the font size of plot title, x and y axis labels. The default value is 15.
#' @param xcex,ycex the font size of x and y axis, respectively. The default values is 10.
#' @param angle the angle of the labels on x axis
#' @param main the plot title
#' @param xlab,ylab the label to be shown on the x and y axis, respectively. 
#' @author Daryanaz Dargahi
#' @return An object created by 'ggplot'.
#' @examples
#' 
#' data <- c(0.1132, 0.0142, 0.0324, 0.10075, 0.0883)
#' names(data) <- c('individual_1','individual_2','individual_3','individual_4','individual_5')
#' crplot(data, fontsize = 15, xcex = 10, ycex = 10, xlab = 'ID', ylab = 'ICR', main = 'Individuals consistency ratio')
#' @export

crplot <- function(srcfile, fontsize = 15, xcex = 10, ycex = 10, angle = 90, xlab = 'ID', ylab = 'ICR', main = NULL){
  
  if (class(srcfile) %in% c('numeric')){
    file <- data.frame(srcfile)
  }else{
      stop('\"srcfile\" must be a numeric vector.')
  }    

  file[,2] <- rownames(file)
  file[,3] <- 'Pass'
  index <- which(file[,1] > 0.1)
  if(length(index) > 0){
    file[index,3] <- 'Failed'
  }
  
  colnames(file) <- c('ICR', 'ID', 'Condition')
  if(all(unique(file$Condition) %in% 'Pass')){
    colors <- 'green4'
  }
  if(all(unique(file$Condition) %in% 'Failed')){
    colors <- 'red3'
  }
  if(length(unique(file$Condition)) > 1 && all(unique(file$Condition) %in% c('Pass','Failed'))){
    colors <- c('red3', 'green4')
  }
  
  file$ID <- as.character(file$ID)
  file$ID <- factor(file$ID, levels = unique(as.character(file$ID)) ,ordered=TRUE)
  
  ggplot(data=file, aes_string(x='ID', y='ICR', fill = 'Condition')) + 
    geom_bar(stat="identity") + 
    scale_fill_manual(values=colors) +
    theme(text = element_text(size=fontsize)) + 
    theme(axis.text.x = element_text(size = xcex , angle = angle)) + 
    theme(axis.text.y = element_text(size = ycex)) +
    ggtitle(main) +
    geom_hline(aes(yintercept=0.10), colour="black", linetype="dashed") +
    xlab(xlab) +
    ylab(ylab)
}



