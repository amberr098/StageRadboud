setSelectedMatrix <- function(data, molecules, samples, absnorm, avind){
  if(avind == "av"){
    matAv <- matrix(NA, nrow = length(samples), ncol = length(molecules))
    matSd <- matrix(NA, nrow = length(samples), ncol = length(molecules))
    
    avMa <- data$average
    sdMa <- data$standDev
    
    colMat <- 0
    for(mol in molecules){
      colMat <- colMat + 1
      mol_res <- paste0(mol, " Results")
      column <- which(colnames(avMa) == mol_res, arr.ind = TRUE)
      
      rowMat <- 0
      for(sam in samples){
        rowMat <- rowMat +1
        row <- which(rownames(avMa) == sam, arr.ind = TRUE)
        
        valueAv <- avMa[row, column]
        valueSd <- sdMa[row, column]
        matAv[rowMat,colMat] <- valueAv
        matSd[rowMat, colMat] <- valueSd
      }
    }
    colnames(matAv) <- molecules
    rownames(matAv) <- samples
    colnames(matSd) <- molecules
    rownames(matSd) <- samples
    print(matAv)
    average_stanDev_selected <- list()
    average_stanDev_selected$average <- matAv
    average_stanDev_selected$standDev <- matSd
    
    return(average_stanDev_selected)
  }
  
  else{
    mat <- matrix(NA, ncol = length(molecules)+1)
      for(sam in samples){
        row_temp <- which(data == sam, arr.ind = TRUE)
        row <- row_temp[1:nrow(row_temp),1]
        
        for(r in 1:length(row)){
          newRow <- c(sam)
          for(mol in molecules){
            mol_res <- paste0(mol, " Results")
            column <- which(colnames(data) == mol_res, arr.ind = TRUE)
            
            value <- data[row[r], column]
            newRow <- c(newRow, as.numeric(as.character(value)))
          }
          mat <- rbind(mat, newRow)
        }
      }

    rownames(mat)  <- mat[,1]
    mat2 <- mat[,-1]
    mat3 <- mat2[-1,]
    colnames(mat3) <- molecules
    
    return(mat3)
  }

}