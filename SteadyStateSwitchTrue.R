setSelectedMatrix <- function(data, molecules, samples, absnorm, avind, type){
  # Average optie is gekozen
  if(avind == "av"){
    matAv <- matrix(NA, nrow = length(samples), ncol = length(molecules))
    matSd <- matrix(NA, nrow = length(samples), ncol = length(molecules))

    avMa <- data$average
    sdMa <- data$standDev
    
    colMat <- 0
    for(mol in molecules){
      colMat <- colMat + 1
      # Ophalen index van de gekozen moleculen door de gebruiker
      mol_res <- paste0(mol, " Results")
      column <- which(colnames(avMa) == mol_res, arr.ind = TRUE)
      
      rowMat <- 0
      for(sam in samples){
        
        if(type == FALSE){
          sam <- paste0("_", sam)
        }
        
        # Ophalen van de index van de gekozen samples door de gebruiker
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
    
    average_stanDev_selected <- list()
    average_stanDev_selected$average <- matAv
    average_stanDev_selected$standDev <- matSd
    
    return(average_stanDev_selected)
  }
  
  # Individual optie is gekozen.
  else{
    ind_matrix <- matrix(NA, ncol = length(molecules)+1)
    tags_column <- which(data == "Name", arr.ind = TRUE)
    # De waardes van de kolom Name waarin de tags gezocht moeten worden wanneer de Types niet zijn ingevuld.
    namesColumn <- data[,tags_column[1,2]]

    # Als type FALSE is, dan staan er in het bestand geen types aangegeven maar staat er alleen "Sample"
    # In dat geval moet er anders gezocht worden naar de tags.
      for(sam in samples){
        
        if(type == FALSE){
          sam <- paste0("_", sam)
          patternTag <- paste0(".*",sam)
          row <- grep(patternTag, namesColumn)
        }else{
          row_temp <- which(data == sam, arr.ind = TRUE)
          row <- row_temp[1:nrow(row_temp),1]
        }
        
        for(r in 1:length(row)){
          newRow <- c(sam)
          for(mol in molecules){
            mol_res <- paste0(mol, " Results")
            column <- which(colnames(data) == mol_res, arr.ind = TRUE)
            value <- data[row[r], column]
            newRow <- c(newRow, as.numeric(as.character(value)))
            
          }
          ind_matrix <- rbind(ind_matrix, newRow)
        }
      }
    
    # De rijnamen hetzelfde maken als de eerste kolom van de matrix.
    rownames(ind_matrix)  <- ind_matrix[,1]
    
    # De eerste kolom verwijderen, de rijnamen en eerste kolom zijn hetzelfde. Eerste rij bevat alleen NAs
    ind_matrix_rownames <- ind_matrix[,-1]
  
    # Verwijderen van de eerste rij die alleen NAs bevat.
    final_ind_matrix <- ind_matrix_rownames[-1,]
    colnames(final_ind_matrix) <- molecules
    
    return(final_ind_matrix)
  }

}