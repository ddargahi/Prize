#' pipeline
#' 
#' @description a pipeline for prioritization estimation using analytic hierarchy process (AHP), which supports both relative and rating AHP models.
#' @param srcfile a character matrix, where the first column specifies the hierarchy order, the second column includes elements IDs, and the third column includes the path to the PCM/priority matrices (See the example below).
#' @param model the AHP computation model. Choose from relative and rating models. If using the relative model pairwise comparison matrices must be provided for the evaluation of alternatives. However, if using the raing model, rating matrices must be provided for the evaluation of alternatives.  
#' @param simulation simulation size for computation of Saaty's inconsistency
#' @author Daryanaz Dargahi
#' @return An S4 object including the ahp wight and consistancy measures, and data structures to visualize with ahplot(), rainbowplot(), and wplot().
#' @examples
#' 
#' mat <- matrix(nrow = 7, ncol = 3, data = NA)
#' mat[,1] <- c('0', '1','2','3','4','4.1','4.2')
#' mat[,2] <- c('Prioritization_of_DE_genes','Tumor_expression','Normal_expression',
#'              'Frequency', 'Epitopes', 'Number_of_epitopes', 'Size_of_epitopes')
#' mat[,3] <- c(system.file('extdata','aggreg.judgement.tsv',package = 'Prize'), 
#'              system.file('extdata','tumor.PCM.tsv',package = 'Prize'), 
#'              system.file('extdata','normal.PCM.tsv',package = 'Prize'), 
#'              system.file('extdata','freq.PCM.tsv',package = 'Prize'), 
#'              system.file('extdata','epitope.PCM.tsv',package = 'Prize'), 
#'              system.file('extdata','epitopeNum.PCM.tsv',package = 'Prize'), 
#'              system.file('extdata','epitopeLength.PCM.tsv',package = 'Prize'))
#' 
#' result <- pipeline(mat, model = 'relative', simulation = 500)
#' @export

pipeline <- function(srcfile, model, simulation = 500){
  
  if(model %in% c('relative','rating')){} else {
    stop('please select one of the following models: relative or rating.')
  }
  
  # reading the srcfile    
  if (class(srcfile) %in% c('matrix', 'data.frame')){
    file <- as.matrix(srcfile)
  } else {
    stop('\"srcfile\" must be a matrix.')
  }    
  
  # sorting the file according to the levels on column 1
  file <- file[order(file[,1]),]
    
  # finding the level of each node
  levels <- strsplit(as.vector(file[,1]), split = '[.]')
  levels <- levels[-1] # removing the goal
  for (i in seq_along(levels)){ # 1:length(levels)
    if(levels[[i]][length(levels[[i]])] == 0){
      levels[[i]] <- levels[[i]][-length(levels[[i]])]
    }
  }
  subs <- numeric()
  for(i in seq_along(levels)){
    subs <- append(subs,length(levels[[i]]))
  }
  lev <- table(subs)
  subs <- append(0,subs) 
  
  # check if there are criteria with the same ID on level 1 (since it is not allowed)
  tmp <- table(file[(which(subs == 1)),2])
  if(length(which(tmp > 1)) > 0){  
    stop('There are criteria with the same ID.')
  }
  
  # check if any subcriteria ID has been repeated (on all levels > 1)
  Rtmp <- table(file[(which(subs > 1)),2])
  Repeat <- FALSE
  if(length(which(Rtmp > 1)) > 0){
    # there is repeat in subcriteria ids!
    Repeat <- TRUE
  }
  
  # marking leaf nodes and renaming decision elements if Repeat is TRUE
  file_tmp <- file
  lfile <- file
  lfile[,3] <- 'NA'
  for(i in seq(from = 1 , to = dim(file)[1])){ # 1:length(file[,1])
    # rename subcriteria, since some names are repeated
    if(Repeat == TRUE){
      if(subs[i] > 1){ # only for subcriteria
        tmp <- unlist(strsplit(as.character(file[i,1]), split = '[.]'))
        if(length(tmp) > 1){
          tmp <- tmp[-length(tmp)]
        }
        file[i,2] <- paste(file[i,2], tmp, sep = '_')
      }
    }
    # mark leaf nodes  
    if(subs[i] >= subs[i+1] && i < length(file[,1])){ # this is a leaf node
      lfile[i,3] <- 'leaf'
    } else {
      if(length(file[,1]) == i){# end of file
        lfile[i,3] <- 'leaf'
      }
    }
  }
  lfile[,2] <- file[,2]
  m <- max(subs)
  
  # plotfile and plotweight store the criteria/subcriteria and their calculated AHP weights, respectively.
  ncriteria <- lev[which(names(lev) == 1)] 
  file[,2] <- str_replace_all(string=as.character(file[,2]), pattern=" ", replacement="_")
  names(subs) <- file[,2]
  plotfile <- file[,c(1,2)]   
  plotweight <- file[,c(2,2)]
  plotweight[,2] <- 5
  plotweight <- plotweight[-1,] # removing the goal  
  inconsis <- NULL
  fnames <- TRUE
  
  # reading the criteria, subcriteria, and alternative files
  cfile <- lapply(as.character(file[,3]), function(x) read.delim(x, header = TRUE, row.names = 1))
  index = which(lfile[,3] == 'leaf')
  lf = cfile[index]
  if(!all(sapply(lf[-1], function(x) all(rownames(x) == rownames(lf[[1]]))))){
    stop('Alternatives files have differing rownames.')
  }
  
  # calculate AHP weight (using ahp function) and save the matrices in cweight
  for(i in seq(from = 1 , to = dim(file)[1])){ # 1:length(file[,1])
    if(i == 1 ){ # goal
      goal <- as.character(file[i,2])
      # cfile is always a PCM
      tahp <- ahp(cfile[[i]],simulation = simulation)
      cweight <- list(criteria = as.matrix(tahp@weight)) # criteria weight
      if(simulation > 0){
        inconsis <- list(incons = tahp@saaty_inconsistency)
        names(inconsis)[length(inconsis)] <- str_replace_all(string=as.character(file[i,2]), pattern=" ", replacement="_")
      }
      rownames(cweight[[i]]) <- str_replace_all(string=as.character(rownames(cweight[[i]])), pattern=" ", replacement="_")
      # saving criteria weights in plotweight 
      for(j in seq(from = 1, to = dim(as.matrix(cweight$criteria))[1])){ # 1:length(as.matrix(cweight$criteria)[,1])
        index <- which(plotweight[,1] == rownames(as.matrix(cweight$criteria))[j])
        if(length(index) > 0){
          plotweight[index,2] <- as.numeric(as.matrix(cweight$criteria)[j,1])
        }
        if(length(index) == 0){
          stop('The name of criteria/subcriteria is not consistent in all files.')
        }
      }
    }
  
    if(i > 1){ # calculate local wight for criteria and subcriteria levels
      criteriaID <- as.character(file[i,2])
      # assign number to subcriteria if there is a repeat of IDs
      if(Repeat == TRUE){
        if((lfile[i,3] == 'leaf') && (fnames == TRUE)){ # the first leaf visited
          altnames <- rownames(cfile[[i]])
          fnames <- FALSE
        }
        if((subs[i] >= 1) && (lfile[i,3] != 'leaf')){
          ltmp <- unlist(strsplit(as.character(file[i,1]), split = '[.]'))
          if(ltmp[length(ltmp)] == 0 && length(ltmp) == 2){  
            ltmp <- ltmp[-length(ltmp)]
          }
          ltmp <- paste(ltmp, collapse = '.')
          rownames(cfile[[i]]) <- paste(rownames(cfile[[i]]), ltmp, sep = '_')
        }
      }
      # cfile could be either a PCM or a rating matrix
      if(dim(cfile[[i]])[1] == dim(cfile[[i]])[2]){ 
        # cfile is a PCM, a square numeric matrix
        tahp <- ahp(cfile[[i]],simulation = simulation)
        tmp <- list(sub = as.matrix(tahp@weight))
        cweight <- append(cweight,tmp)
        if(simulation > 0){
          inconsis <- append(inconsis, list(incons = tahp@saaty_inconsistency))
          names(inconsis)[length(inconsis)] <- str_replace_all(string=as.character(file[i,2]), pattern=" ", replacement="_")
        }
      } else if (dim(cfile[[i]])[1] != dim(cfile[[i]])[2] && dim(cfile[[i]])[2] == 2) { # if input is a scale or priority vector
          tmp <- as.matrix(cfile[[i]][,dim(cfile[[i]])[2]]) #2 dim(cfile)[2]
          rownames(tmp) <- rownames(cfile[[i]])
          colnames(tmp) <- colnames(cfile[[i]])[dim(cfile[[i]])[2]] #[2]
          tmp <- list(sub = as.matrix(tmp))
          cweight <- append(cweight,tmp)  
      } else {
        stop ('Judgements should either be a pairwise comparison or rating matrix.')
      }
      rownames(cweight[[i]]) <- str_replace_all(string=as.character(rownames(cweight[[i]])), pattern=" ", replacement="_")                 
    }
    message(paste(as.character(file[i,1]), as.character(file[i,2]) , 'is processed.', sep = ' '))
    names(cweight)[i] = str_replace_all(string=as.character(file[i,2]), pattern=" ", replacement="_") #sprintf("W%i",1:length(cweight))
  }
      
  # computing global scores
  plotfile <- cbind(plotfile, (rbind(c(NA, NA),plotweight))[,2])
  mat <- numeric()
  mRow <- 1
  for(i in seq(from = 1, to = dim(plotfile)[1])){ # start from 2 to skip the weights calculated in the goal level
    if(subs[i] == 0){ # this is the goal
      # do  nothing
    } else {
      k <- i
      flag <- TRUE
      while((k > 1) && (subs[k] != 0) && (flag == TRUE)){ # sub[k] != 1 : to jump out after seeing the first criteria (since inside k = k-1 is happenning)
        k <- k - 1
        if((subs[i] - subs[k] == 1) && (flag == TRUE)){ # meaning k is the direct parent
          flag <- FALSE
          index <- which(rownames(cweight[[k]]) == names(subs)[i])
          if(dim(cweight[[k]])[2] == 1){
            num <- cweight[[k]][index,1]
          }
          if(dim(cweight[[k]])[2] == 2){
            num <- cweight[[k]][index,2]
          }
          cweight[[i]] = cbind(cweight[[i]] , (cweight[[i]] * as.numeric(num)))
          colnames(cweight[[i]]) = c('local', 'global')

          # updating the weights
          if(length(which(plotfile[,2] %in% rownames(cweight[[i]]))) > 0){ # (subs[i] + 1 == subs[i+1]) # or check if there is any child... meaning it is not an end node (if the level of the next one is current+1)
            for(l in seq(from = 1, to = dim(cweight[[i]])[1])){ # 1:length(cweight[[i]][,1])
              iindex <- which(plotfile[,2] %in% rownames(cweight[[i]])[l])
              if(length(iindex) > 0){
                plotfile[iindex,3] <- cweight[[i]][l,2]
              }
            }
          } else { # making the matrix of raw scores for plot
            # this is an end node (leaf)
            # making mat, the sub criteria matrix
            mat <- cbind(mat, as.matrix(cweight[[i]][,2]))
            colnames(mat)[mRow] <- as.character(names(subs)[i])
            mRow <- mRow + 1
          } 
        } # end of while        
      }
    } 
  } # end of global score computation
    
  # computing total AHP scores for alternatives. mat stores scores breaked down by subcriteria
  mat <- as.matrix(mat)
  mat <- cbind(mat, apply(mat,1,sum))
  colnames(mat)[dim(mat)[2]] <- 'total_priorities'
   
  # sort 
  #mat = mat[order(mat[,dim(mat)[2]], decreasing=T), ]
  
  # computing total AHP scores for alternatives. Cmat stores scores breaked down by criteria
  index <- which(subs == 1) # level 1s are criteria.
  Cname <- names(index)
  index <- append(index, length(subs))
  Cmat <- numeric()
  for(i in seq_along(index)){
    if(i < length(index)){
      #names(subs)[index[i]:(index[i+1]-1)]
      if(i == (length(index) - 1)){
        ind <- which(colnames(mat) %in% names(subs)[index[i]:(index[i+1])])
      } else {
        ind <- which(colnames(mat) %in% names(subs)[index[i]:(index[i+1]-1)])
      }
      if(length(ind) == 1){
        Cmat <- cbind(Cmat, mat[,ind])
      } 
      if(length(ind) > 1) {
        Cmat <- cbind(Cmat, apply(mat[,ind],1,sum))
      }
    }
  }
  colnames(Cmat) <- Cname
  Cmat <- as.matrix(Cmat)
  Cmat <- cbind(Cmat, apply(Cmat,1,sum))
  colnames(Cmat)[dim(Cmat)[2]] <- 'total_priorities'
    
  # ahplot
  colnames(plotfile) <- c('level','ID','Weight')
  ahp_plot <- plotfile 
  
  #rainbowplot
  if(model == 'relative'){
    tmp <- list(sub = as.matrix(Cmat))
    cweight <- append(cweight,tmp)
    names(cweight)[length(cweight)] <- 'Criteria_prioritization_matrix' 
    tmp <- list(sub = as.matrix(mat))
    cweight <- append(cweight,tmp)
    names(cweight)[length(cweight)] <- 'Subcriteria_prioritization_matrix' 
    
    criteria_rainbowplot <- as.matrix(Cmat) 
    sub_rainbowplot <- as.matrix(mat) 
  } 
  if(model == 'rating'){    
    colnames(Cmat)[dim(Cmat)[2]] <- 'total_priorities'
    temp <- sum(Cmat[,dim(Cmat)[2]]) 
    Cmat <- cbind (Cmat, Cmat[,dim(Cmat)[2]] / temp)
    colnames(Cmat)[dim(Cmat)[2]] <- 'normal_priorities'
    Nmax <-  max(Cmat[,dim(Cmat)[2]-1])
    Cmat <- cbind(Cmat, Cmat[,dim(Cmat)[2]-1]/Nmax)
    colnames(Cmat)[dim(Cmat)[2]] <- 'ideal_priorities'
    tmp <- list(sub = as.matrix(Cmat))
    cweight <- append(cweight,tmp)
    names(cweight)[length(cweight)] <- 'Criteria_prioritization_matrix' 
    
    colnames(mat)[dim(mat)[2]] <- 'total_priorities'
    temp <- sum(mat[,dim(mat)[2]]) 
    mat <- cbind (mat, mat[,dim(mat)[2]] / temp)
    colnames(mat)[dim(mat)[2]] <- 'normal_priorities'
    Nmax <-  max(mat[,dim(mat)[2]-1])
    mat <- cbind(mat, mat[,dim(mat)[2]-1]/Nmax)
    colnames(mat)[dim(mat)[2]] <- 'ideal_priorities'
    tmp <- list(sub = as.matrix(mat))
    cweight <- append(cweight,tmp)
    names(cweight)[length(cweight)] <- 'Subcriteria_prioritization_matrix' 
        
    criteria_rainbowplot <- as.matrix(Cmat[,c(1:(dim(Cmat)[2]-2))]) #as.matrix(Cmat)
    sub_rainbowplot <- as.matrix(mat[,c(1:(dim(mat)[2]-2))]) #as.matrix(mat)
    tmp <- sum(criteria_rainbowplot[,dim(criteria_rainbowplot)[2]])
    criteria_rainbowplot[,dim(criteria_rainbowplot)[2]] <- criteria_rainbowplot[,dim(criteria_rainbowplot)[2]] / tmp
    tmp <- sum(sub_rainbowplot[,dim(sub_rainbowplot)[2]])
    sub_rainbowplot[,dim(sub_rainbowplot)[2]] <- sub_rainbowplot[,dim(sub_rainbowplot)[2]] / tmp
  }
  
  # wplot
  index <- which(ahp_plot[,2] %in% colnames(sub_rainbowplot))
  sub_wplot <- ahp_plot[index,c(2,3)]
  colnames(sub_wplot) <- c('ID','Weight')
  criteria_wplot <- as.matrix(plotfile[which(subs == 1),c(2,3)])
  colnames(criteria_wplot) <- c('ID','Weight')
  
  # rename the criteria/subcriteria to their original name (changed because of repeat)
  if(Repeat == TRUE){
    index <- which(file_tmp[,2] %in% names(which(Rtmp > 1)))  
    ID <- cbind(file[,2],file_tmp[,2],0)
    ID[index,3] <- 'Repeat'
    #ahp_plot
    ahp_plot[,2] <- file_tmp[,2]
    #sub_wplot  
    tmp <- ID[which(ID[,1] %in% sub_wplot[,1]),]
    tmp <- tmp[which(tmp[,3] == '0'),]
    index <- which(sub_wplot[,1] %in% tmp[,1])
    sub_wplot[index,1] <- tmp[,2]
    #sub_rainbowplot
    colnames(sub_rainbowplot) <- append(sub_wplot[,1], 'total_priorities')
  }
  weight_plot <- list(criteria_wplot = criteria_wplot, subcriteria_wplot = sub_wplot)
  rainbow_plot <- list(criteria_rainbowplot = criteria_rainbowplot, subcriteria_rainbowplot = sub_rainbowplot)      
  if(simulation > 0) {
    return(new("pipelineObj", ahp_plot = ahp_plot, weight_plot = weight_plot, rainbow_plot = rainbow_plot, ahp_weights = cweight, simulation = simulation, saaty_inconsistency = inconsis))
  } else {
    inconsis <- list() 
    return(new("pipelineObj", ahp_plot = ahp_plot, weight_plot = weight_plot, rainbow_plot = rainbow_plot, ahp_weights = cweight, simulation = simulation, saaty_inconsistency = inconsis))
  }
}
