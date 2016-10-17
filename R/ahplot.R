#' ahplot
#' 
#' @description Plotting the problem hierarchy, showing the relationships between goal, criteria, and subcriteria.
#' @param srcfile a character matrix, where the first column specifies the hierarchy order. The second and third columns include the ID of decisison elements and ahp weights (optional), respectively (See the example below).
#' @param fontsize the font size of characters.
#' @param cradx,crady the horizontal and vertical radius of the criteria box, respectively.
#' @param sradx,srady the horizontal and vertical radius of the subcriteria box, respectively.
#' @param cirx,ciry the horizontal and vertical radius of the goal    
#' @param main a character string as the plot title
#' @param gcol,ccol,scol the filling color of the goal, criteria, and subcriteria boxes, respectively.
#' @param lcol the line color surrounding the goal, criteria, and subcriteria boxes.
#' @param dist the distance between the weights and the tree edges.
#' @param digit the number of digits after decimal point to be shown on the arrows.
#' @return An object created by 'diagram'.
#' @author Daryanaz Dargahi
#' @examples
#' 
#' mat <- matrix(nrow = 7, ncol = 2, data = NA)
#' mat[,1] <- c('0', '1','2','3','4','4.1','4.2')
#' mat[,2] <- c('Prioritization_of_DE_genes','Tumor_expression','Normal_expression', 
#' 'Frequency', 'Epitopes', 'Number_of_epitopes', 'Size_of_epitopes')
#' 
#' # plotting a problem hierarchy
#' ahplot(mat, fontsize = 0.7, cradx = 0.11 ,sradx = 0.12, cirx= 0.18, ciry = 0.07)
#' 
#' # plotting a problem hierarchy with AHP weights shown on the edges of the graph
#' mat <- cbind(mat, c(1, 0.470, 0.341, 0.117, 0.073, 0.009, 0.064)) 
#' ahplot(mat, fontsize = 0.7, cradx = 0.11 ,sradx = 0.12, cirx= 0.18, ciry = 0.07)
#' @export

ahplot <- function(srcfile, fontsize = 0.5, cradx = 0.07, crady = 0.05, sradx = 0.05 , srady = 0.05, cirx = 0.1, ciry = 0.05, gcol = 'green', ccol = 'yellow', scol = 'orange', lcol = 'black', dist = 0.05, digit = 3, main = NULL){
  # reading the input and preprocessing
  if (class(srcfile) %in% c('matrix', 'data.frame' )){ #, 'list')){
    if(dim(srcfile)[2] == 2){ # no weights
      colnames(srcfile) <- c('Level','ID')
      weight <- NULL
      file <- srcfile
    } 
    if(dim(srcfile)[2] == 3){ # weights provided
      colnames(srcfile) <- c('Level','ID', 'Weight')
      weight <- srcfile[,c(2,3)]
      rownames(weight) <- srcfile[,2]
      weight <- weight[-1,] # removing goal
      file <- srcfile[,c(1,2)]
    }
  }else{
    if(file.exists(srcfile)){
      srcfile <- read.delim(srcfile, sep = '\t', header = TRUE, row.names = 1)
      if(dim(srcfile)[2] == 2){ # no weights
        colnames(srcfile) <- c('Level','ID')
        weight <- NULL
        file <- srcfile
      } 
      if(dim(srcfile)[2] == 3){ # weights provided
        colnames(srcfile) <- c('Level','ID', 'Weight')
        weight <- srcfile[,c(2,3)]
        rownames(weight) <- srcfile[,2]
        weight <- weight[-1,] # removing goal
        file <- srcfile[,c(1,2)]    
      }
    }else {
      stop('\"srcfile\" is missing.')
    }    
  }
  
  file <- as.matrix(file)
  file <- file[order(file[,1]),]
  
  if (unlist(strsplit(as.character(file[1,1]), '[.]'))[1] != 0){
    stop ('level 0 (the goal) is missing.')
  }
  goal <- as.character(file[1,2])
  ffile <- file
  file <- file[-1,]
  file <- as.matrix(file)
  levels <- strsplit(as.vector(file[,1]), split = "[.]")
  for (i in seq_along(levels)){ # 1:length(levels)
    if(levels[[i]][length(levels[[i]])] == 0){
      levels[[i]] <- levels[[i]][-length(levels[[i]])]
    }
  }
  
  ## finding the level of each element in tree
  dep <- 0
  sub <- numeric()
  tmp <- rep(0,length(levels))
  names(tmp) <- 2:(length(levels)+1)
  for(i in seq_along(levels)){ # 1:length(levels)
    sub <- append(sub,length(levels[[i]]))    
  }
  
  ## finding the number of childs for each node
  for(i in seq_along(sub)){ # 1:length(sub)
    if(sub[i] == sub[i+1] && i < length(sub)){ 
      #Do nothing
    }
    if(((sub[i+1]-sub[i]) == 1) && i < length(sub)){ # next node is a child 
      k <- i +1
      while(sub[k] - sub[i] > 0 && k <= length(sub)){  
        if(sub[k] - sub[i] == 1){
          tmp[i] <- tmp[i] + 1
        }
        k <- k +1
      }
    }
    if( i == length(sub)){
      tmp[i] <- 0
    }
  }
  
  index <- append(1, table(sub))
  struc <- rbind(sub,tmp)
  struc <- cbind(c(0,index[2]), struc)
  rownames(struc) <- c('level','child')
  struc <- t(struc)  
  rownames(struc) <- ffile[,2]
  struc <- struc[order(struc[,1], decreasing = FALSE),]
  dep <- length(index) # depth of tree including the goal
  
  ## making the edges of the graph (from ... to ...)
  openplotmat(main = main)
  elpos <- coordinates (index)
  fromto <- matrix(ncol = 2, nrow = length(sub), data = 0) # arrows from box a to box b
  fromto[,2] <- as.matrix(2:(dim(fromto)[1] + 1))
  k <- 1
  arrow = dim(fromto)[1]
  for(i in seq_len(arrow)){
    if(struc[i,2] > 0){
      fromto[k:(k+struc[i,2]-1),1] <- i
      k <- k + struc[i,2] 
    }
  }

  nr <- nrow(fromto)
  arrpos <- matrix(ncol = 2, nrow = nr)
  
  # plotting the edges
  for (i in seq_len(nr)){
    arrpos[i, ] <- straightarrow (to = elpos[fromto[i, 2], ], from = elpos[fromto[i, 1], ], 
                                  lwd = 1, arr.pos = 0.5, arr.length = 0.4)
  }
  # plotting the goal
  textellipse(elpos[1,], radx = cirx, rady = ciry, lab = goal, box.col = gcol, lcol = lcol, shadow.col = "white", shadow.size = 0.001, cex = fontsize)

  # plotting criteria
  clabels <- rownames(struc)[which(struc[,1] == 1)]
  N <- length(clabels)
  for(i in seq(from = 2, to = (N +1))){ # 2:(length(clabels)+1)
    textrect(elpos[i,], cradx, crady,lab = clabels[i-1], box.col = ccol, lcol = lcol, shadow.col = "white", shadow.size = 0.001, cex = fontsize)
  }

  # plotting sub-criteria
  flag <- length(which(names(index) == '2'))
  if(flag > 0){ # if there is any subcriteria(s)
    for(i in seq(from = 3, to = dep)){ # 3:dep # dep = 1 -> goal, dep = 2 -> criteria and now dep = 3 (sub)
      slabels <- rownames(struc)[which(struc[,1] == (i-1))]
      # subcriteria ( if there is any)
      if(length(slabels) > 0){
        for(i in seq(from = (N+1+1), to = (N+1+length(slabels)))){ # (N+1+1):(N+1+length(slabels))
          textrect(elpos[i,], sradx, srady,lab = slabels[i-(N+1)], box.col = scol, lcol = lcol ,shadow.col = "white", shadow.size = 0.001, cex = fontsize)
        }
      }
      N <- N + length(slabels) 
    }
  }
  # writing the weights on the edges of the tree
  if(!is.null(weight)){
    ord <- rownames(struc)
    ord <- ord[-1] # removing level goal
    weight <- weight[ord,]
    weight <- as.matrix(weight)
    num = dim(weight)[1]
    for(i in seq_len(num)){ # 1:length(weight[,1])
      text(arrpos[i, 1] - dist, arrpos[i, 2], round(as.numeric(weight[i,2]),digit), cex = fontsize)
    }
  }  
}

