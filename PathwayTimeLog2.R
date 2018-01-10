getCondition <- function(choiceUser, ratios){
  rows_choiceUser <- grep(choiceUser, rownames(ratios))
  pattern <- "13C.{1,2}-"
  
  preRatios <- list()
  Results_coln <- c()
  compound_coln <- c()
  coln <- c()
  
  # Alleen de data pakken die de gebruiker wilt
  for(row in rows_choiceUser){
    preRatios <- rbind(preRatios, ratios[row,])
  }
  
  # " Results" verwijderen uit de kolomnamen
  for(name in colnames(ratios)){
    if(grepl(" Results", name) == TRUE){
      name <- gsub(" Results", "", name)
      Results_coln <- c(Results_coln, name)
    }else{
      Results_coln <- c(Results_coln, name)
    }
  }
  
  # "13C.. " verwijderen uit de kolomnamen
  for(name in Results_coln){
    if(grepl(pattern, name) == TRUE){
      name <- gsub(pattern, "", name)
      compound_coln <- c(compound_coln, name)
    }else{
      compound_coln <- c(compound_coln, name)
    }
  }
  
  for(name in compound_coln){
    if(grepl("-", name) == TRUE){
      name <- gsub("-", "_", name)
      coln <- c(coln, name)
    }else{
      coln <- c(coln, name)
    }
  }
  
  ratios_df <- as.data.frame(preRatios)
  colnames(ratios_df) <- coln
  
  log2 <- getLog2(ratios_df)
  return(log2)
}

getLog2 <- function(ratios_df){
  preLog2 <- list()
  for(row in 1:nrow(ratios_df)){
    new_row <- c()
    for(col in 1:ncol(ratios_df)){
      value <- as.numeric(ratios_df[row,col])
      new_row <- c(new_row, log2(value))
    }
    
    preLog2 <- rbind(preLog2, new_row)
  }
  
  log2 <- as.data.frame(preLog2)
  colnames(log2) <- colnames(ratios_df)
  
  return(log2)
}