changeFormat <- function(norm_Responses){
  # Bepalen van de kolomnamen
  pattern_firstCol <- " Results"
  index_firstCol <- grep(pattern_firstCol, colnames(norm_Responses))[1]
  coln <- colnames(norm_Responses)[index_firstCol:ncol(norm_Responses)]
  adding_coln <- c()
  # Ophalen van de kolom met de samples erin en het patroon bepalen voor het vinden van de benodigde samples
  index_col_Name <- which(norm_Responses == "Name", arr.ind = T)[1,2]
  pattern_sample <- ".*_.*"
  index_col_Type <- which(norm_Responses == "Type", arr.ind = T)[1,2]
  rown <- c()
  preFormatData <- list()
  
  for(name in norm_Responses[,index_col_Name]){
    sample <- grepl(pattern_sample, name)
    
    # Als sample gelijk is aan TRUE, dan is het een te visualiseren sample
    if(isTRUE(sample)){
      # Ophalen van de index van de rij van de te visualiseren sample
      index_row_sample <- grep(name, norm_Responses[,index_col_Name])
      # Toevoegen van de rij met alleen de waardes, dus niet ook nog de Type etc.
      new_row <- norm_Responses[index_row_sample, index_firstCol:ncol(norm_Responses)]
      preFormatData <- rbind(preFormatData, as.character(new_row))
      
      rowName <- norm_Responses[index_row_sample,index_col_Type]
      rown <- c(rown, as.character(rowName))
    }
  }
  
  for(col in coln){
    if(grepl(" Results", col) == TRUE){
      col <- gsub(" Results", "", col)
      adding_coln <- c(adding_coln, col)
    }else{
      adding_coln <- c(adding_coln, col)
    }
  }
  # Maken van de gewenste dataframe
  formatData <- matrix(as.numeric(unlist(preFormatData)),nrow=nrow(preFormatData))
  rownames(formatData) <- rown
  colnames(formatData) <- adding_coln
  
  return(formatData)
}

# Voor de bestanden met tijd (12C en 13C data) worden alleen de samples gepakt waar ook de tijd bijstaat
changeFormatTime <- function(average_df){
  pattern_time <- "_\\d{1,2}[min|h|sec]"
  preRatio <- list()
  rown <- c()
  # Controleren of _ en een tijd in de sample naam staat.
  for(index_name in 1:nrow(average_df)){
    name <- rownames(average_df)[index_name]
    if(grepl(pattern_time, name) == TRUE){
      new_row <- average_df[index_name,]
      preRatio <- rbind(preRatio, new_row)
      
      rown <- c(rown, gsub(pattern_time, "", name))
    }
  }
  
  ratios <- as.matrix(preRatio)
  rownames(ratios) <- rown
  return(ratios)
}
