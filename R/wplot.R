#' Wplot
#' 
#' @description Plotting the criteria/subcriteria ahp weights in a bar/pie plot.
#' @param srcfile a matrix, where the first column includes criteria/subcriteria ID and the second includes the ahp weights.
#' @param color the color palette to fill bars. Either provide a vector of n colors, where n is the number of criteria, or choose from the following palettes 'rainbow, heat, terrain, topo, cm'. The default value is rainbow. 
#' @param fontsize the font size of the plot title, and x and y axis labels. The default value is 15.
#' @param xcex,ycex the font size of the x and y axis, respectively. The default values is 10.
#' @param pcex the font size of the labels inside pie chart
#' @param digit the number of digits after decimal point to be shown on the x axis.
#' @param xlab,ylab the label of the x and y axis, respectively.
#' @param main the plot title
#' @param type wplot offers two plot types; bar and pie plots. Default value is bar.
#' @author Daryanaz Dargahi
#' @return An object created by 'ggplot'.
#' @examples
#' 
#' mat <- matrix(nrow = 4, ncol = 2, data = NA)
#' mat[,1] <- c('Tumor_expression','Normal_expression','Frequency','Epitope')
#' mat[,2] <- c(0.470, 0.341, 0.116, 0.073)
#' 
#' wplot(mat, xlab = 'Weight', ylab = 'Criteria', type = 'bar')
#' wplot(mat, type = 'pie')
#' @export

wplot <- function(srcfile, color = 'rainbow', fontsize = 15, xcex = 10, ycex = 10, pcex = 5, digit = 2, xlab = NULL, ylab = NULL, type = 'bar' , main = NULL){
    
  if(class(srcfile) %in% c('matrix', 'data.frame')){
    file <- data.frame(srcfile)
  }else{
    stop('\"srcfile\" must be a matrix.')   
  }
  
  if(dim(file)[2] == 1){
    file <- cbind(rownames(file), file)
  }
  
  if(length(which(table(file[,1]) > 1)) > 0){
    # there are repeat names
    file[,1] <- paste(seq(1,length(file[,1])), file[,1], sep = '. ')
  }
  
  N <- dim(file)[1]
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
  
  colnames(file) <- c('Criteria','Weight')
  file <- file[order(file$Weight, decreasing = TRUE), ]
  file <- as.matrix(file)
  file[,2] <- round(as.numeric(file[,2]),digits=digit)
  
  file <- cbind(file, (as.numeric(file[,2]) / sum(as.numeric(file[,2]))))
  colnames(file)[dim(file)[2]] <- 'fraction'
  file <- cbind(file, cumsum(as.numeric(file[,3])))
  colnames(file)[dim(file)[2]] <- 'ymax'
  file <- cbind(file, c(0, head(as.numeric(file[,4]), n = -1)))
  colnames(file)[dim(file)[2]] <- 'ymin'
  file <- cbind(file, paste(round((as.numeric(file[,3]) * 100), digits = digit), '%', sep = ''))
  colnames(file)[dim(file)[2]] <- 'type'
  file <- cbind(file, ((as.numeric(file[,4])+as.numeric(file[,5]))/2))
  colnames(file)[dim(file)[2]] <- 'coord'
  
  file <- data.frame(file)
  file[,2] <- as.numeric(as.character(file[,2]))
  file[,3] <- as.numeric(as.character(file[,3]))
  file[,4] <- as.numeric(as.character(file[,4]))
  file[,5] <- as.numeric(as.character(file[,5]))  
  file$Criteria <- as.character(file$Criteria)
  file$Criteria <- factor(file$Criteria, levels = unique(as.character(file$Criteria)) ,ordered=TRUE)
  
  if (type == 'bar'){
    
    ggplot(data=file, aes_string(y='Weight', x='Criteria', fill = 'Criteria')) + 
      geom_bar(stat="identity") +
      guides(fill=FALSE) + coord_flip() + 
      theme(text = element_text(size=fontsize)) + 
      theme(axis.text.x = element_text(size = xcex)) + 
      theme(axis.text.y = element_text(size = ycex)) +
      scale_fill_manual(values=colors) +
      xlab(ylab) +
      ylab(xlab) +
      ggtitle(main)
    
  } else if(type == 'pie'){
    ymax = NULL
    ymin = NULL
      ggplot(data = file, aes_string(fill = 'Criteria', ymax = 'ymax', ymin = 'ymin', xmax = 4, xmin = 3)) +
        geom_rect() +
        coord_polar(theta = "y") +
        theme(panel.grid=element_blank()) +
        theme(axis.text=element_blank()) +
        theme(axis.ticks=element_blank()) +
        scale_fill_manual(values=colors) + 
        theme(text = element_text(size=fontsize)) + 
        theme(axis.text.x = element_text(size = xcex)) + 
        theme(axis.text.y = element_text(size = ycex)) +
        geom_text(aes(x = 3.5, y = (ymax+ymin)/2, label = type), size = pcex) + # (ymax+ymin)/2
        xlab(ylab) +
        ylab(xlab) +
        ggtitle(main)
        
  } else {
    stop ('Plot type is not recognized. Please select one of the following types; bar, pie.')
  }  
}
  





