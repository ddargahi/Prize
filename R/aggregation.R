#' gaggregate
#' 
#' @description Aggregating individual judgments (pairwise comparison matrices - PCMs) into a group judgement or group priority.
#' @param srcfile a matrix with one or two columns. Column one (required) includes the path (location) to each individual pairwise comparison matrix and column two (optional) includes the individual weights. The matrix rowname is individuals identifier.
#' @param method two methods are available for aggregation of individual opinions, (1) arithmetic, which compute the arithmetic mean of individual priorities, (2) geometric, which computes the geometric mean of individual PCMs. If individuals are assigned with a weight, the weighted arithmetic/geometric mean will be computed. The default method is 'geometric'.  
#' @param simulation simulation size for computation of Satty's inconsistency. The default value is 500.
#' @return An S4 object including group PCM/prioritise; 
#' @return If geometric mean is used, the returning object includes: aggregated group PCM (AIJ), group consistency ratio (GCR), individual consistency ratios (ICR), consensus index (CI), and priority matrix (IP).
#' @return If aritmetic mean is used, the returning object includes: agrregated group priority (AIP), individual consistency ratios (ICR), and priority matrix (IP). 
#' @author Daryanaz Dargahi
#' @references E. Forman and K. Peniwati. Aggregating individual judgments and priorities with the analytic hierarchy process. European Journal of Operational Research, 108(1):165-169, 1998.
#' @examples
#' 
#' mat <- matrix(nrow = 4, ncol = 1, data = NA)
#' mat[,1] <- c(system.file('extdata','ind1.tsv',package = 'Prize'), 
#'             system.file('extdata','ind2.tsv',package = 'Prize'), 
#'             system.file('extdata','ind3.tsv',package = 'Prize'),
#'             system.file('extdata','ind4.tsv',package = 'Prize'))
#' rownames(mat) <- c('ind1','ind2','ind3', 'ind4')
#' colnames(mat) <- c('individual_judgement') 
#' 
#' # non-weighted aggregation
#' res <- gaggregate(srcfile = mat, method = 'geometric', simulation = 500)
#' 
#' # weighted aggregation
#' # Decision makers are assigned with a priority value based on their specialization and perspectives.
#' mat <- cbind(mat, c(0.35,0.25,0.15,0.25))  
#' colnames(mat)[2] <- 'individual_weight'
#' 
#' res <- gaggregate(srcfile = mat, method = 'geometric', simulation = 500)
#' 
#' @export 

gaggregate <- function(srcfile, method = 'geometric', simulation = 500){ # weight = NULL, method = 'geometric'

  if(class(srcfile) %in% c('matrix', 'data.frame')){
    x <- as.matrix(srcfile)
    judge <- dim(x)[1] #number of judges
    ICRname <- as.character(rownames(x))
    
    if(dim(x)[2] > 1 && !all(is.na(x[,2]))){
      weights <- as.numeric(x[,2])
      if(sum(as.numeric(weights)) != 1){
        stop ('Weights must add up to 1.')
      }
    } else {
      weights <- NULL
    }
    
    tmp <- list()
    if(file.exists(as.character(x[1,1]))){
      file <- read.delim(as.character(x[1,1]), sep = '\t', header = TRUE, row.names = 1)
      nRow <- dim(file)[1]
      nCol <- dim(file)[2]
      rname <- rownames(file)
      cname <- colnames(file)
    } else {
      stop(paste(as.character(x[1,1]), ' is missing.', sep = ''))
    }
    
    message(paste("Reading individual judgements.",sep = ''))
    for(i in seq_len(judge)){      
      if(file.exists(as.character(x[i,1]))){ 
        file <- read.delim(as.character(x[i,1]), sep = '\t', header = TRUE, row.names = 1)
        # check for zero
        if(length(which(x==0,arr.ind = TRUE)) > 0){
          stop('Zero is not allowed in the matrix, infinite values are being created in the square matrix.')
        }
        # check if the judgement matrices are square
        if(dim(file)[1] != dim(file)[2]){
          stop(paste(as.character(x[i,1]), 'is not a square/triangular matrix.', sep = ' '))
        }
        # check if the matrices are numeric
        if(!is.numeric(as.matrix(file))){
          stop(paste(as.character(x[i,1]), 'is not a numeric matrix.', sep = ' '))
        }
        # check the # of row and columns are similar in all judgements
        if(dim(file)[1] != nRow || dim(file)[2] != nCol){
          stop('Judgement matrices has different number of rows or columns.')
        }
        # check the criteria/sub order
        if(!all(rownames(file) == rname)){
          stop('Judgement matrices have different orders.')
        }        
        # check if PCM is triangular and compute a complete square PCM 
        if(all(is.na(file[upper.tri(file)])) || all(is.na(file[lower.tri(file)]))){
          out <- ahmatrix(file)
          file <- as.matrix(out@ahp_matrix)
        } 
        tmp <- append(tmp, list(file))          
      } else {
        stop(paste(as.character(x[i,1]), ' is missing.', sep = ''))
      }
    }      
  } else {
    stop('\'srcfile\' must be a matrix/datafram that include the path to individual judgement matrices (PCM) and their weights (optional).')
  }
  x <- tmp
    
  # aggregating judgement matrices
  if (method == 'geometric'){
    message(paste("Aggregating individual judgements with geometric mean (AIJ).", sep = ' '))
    if(is.null(weights)){
      # computing non-weighted geometric mean
      # 1. prod of all matrices
      # 2. n th root where n = judge
      tmp <- x[[1]]
      iahp <- ahp(x[[1]], simulation = simulation)
      CR <- iahp@saaty_inconsistency
      IW <- iahp@weight
      for(i in seq(from = 2, to = judge)){ # 2:judge
        tmp <- tmp * x[[i]]
        iahp <- ahp(x[[i]], simulation = simulation)
        CR <- append(CR, iahp@saaty_inconsistency)
        IW <- rbind(IW, iahp@weight)
      }
      gmean  <- tmp ^ (1/judge)
      names(CR) <- names(x)
      IW = rbind(IW, ahp(gmean, simulation = simulation)@weight)
      rownames(IW) <- append(rownames(srcfile), 'Group judgement')
    }
    else {
      # computing weighted geometric mean
      tmp <- x[[1]] ^ weights[1]
      iahp <- ahp(x[[1]], simulation = simulation)
      CR <- iahp@saaty_inconsistency
      IW <- iahp@weight
      for(i in seq(from = 2, to = judge)){ # 2:judge
        tmp <- tmp * (x[[i]] ^ weights[i])
        iahp <- ahp(x[[i]], simulation = simulation)
        CR <- append(CR, iahp@saaty_inconsistency)
        IW <- rbind(IW, iahp@weight)
      }
      gmean <- tmp
      names(CR) <- ICRname
      IW = rbind(IW, ahp(gmean, simulation = simulation)@weight)
      rownames(IW) <- append(rownames(srcfile), 'Group judgement')
    }
    rmean <- gmean
    names(CR) <- ICRname
    
    # measuring the consistency of the aggregated matrix
    message(paste("Computing group consistency ratio (GCR).",sep = ''))
    GCR <- numeric()
    group_consistency_ratio <- numeric()
    if(simulation > 0){
      GCR <- ahp(rmean, simulation = simulation)
      group_consistency_ratio <- GCR@saaty_inconsistency
    }
    
    # computing consensus index
    message(paste("Computing consensus index (CI).",sep = ''))
    consensus_measure <- numeric()
    for(i in seq_len(judge)){ # number of PCMs
      GCI <- 0
      for(j in seq_len(nRow)){
        for(k in seq_len(nRow)){
          GCI <- GCI + (rmean[j,k] * x[[i]][k,j])
        }
      }
      GCI <- GCI / (nRow ^ 2)
      consensus_measure <- append(consensus_measure, GCI)
    }  
    names(consensus_measure) <- ICRname
    return(new("geoAggreg", AIJ = rmean, GCR = group_consistency_ratio, CI = consensus_measure, ICR = CR, IP = IW))  
    
  } else if (method == 'arithmetic'){ 
    # computing non-weighted arithmetic mean
    message(paste("Aggregating individual priorities with arithmetic mean (AIP).", sep = ' '))
     if(is.null(weights)){
      # arithmetic mean
      temp <- ahp(x[[1]], simulation = simulation)
      tmp <- temp@weight
      CR <- temp@saaty_inconsistency
      IW <- temp@weight
      for(i in seq(from = 2, to = judge)){ #2:judge
        temp <- ahp(x[[i]], simulation = simulation)
        tmp <- tmp + (temp@weight)
        CR <- append(CR, temp@saaty_inconsistency)
        IW <- rbind(IW, temp@weight)
      }
      tmp <- tmp/judge
      amean  <- tmp
      names(CR) <- ICRname
      IW <- rbind(IW, amean)
      rownames(IW) <- append(rownames(srcfile), 'Group judgement')
    }
    else {
      # weighted arithmetic mean
      temp <- ahp(x[[1]], simulation = simulation)
      tmp <- temp@weight
      CR <- temp@saaty_inconsistency
      IW <- temp@weight
      tmp <- tmp * weights[1]
      for(i in seq(from = 2, to = judge)){ # 2:judge
        temp <- ahp(x[[i]], simulation = simulation)
        tmp <- tmp + (temp@weight * weights[i])
        CR <- append(CR, temp@saaty_inconsistency)
        IW <- rbind(IW, temp@weight)
      }
      amean <- tmp
      names(CR) <- ICRname
      IW <- rbind(IW, amean)
      rownames(IW) <- append(rownames(srcfile), 'Group judgement')
    }
    rmean <- amean
    names(CR) <- ICRname
    return(new("ariAggreg", AIP = rmean, ICR = CR, IP = IW))  
  } else {
    stop('Please choose one of the following methods: geometric, arithmetic')
  }
}


