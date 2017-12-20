getMolecules <- function(Resp_dataframe){
  pattern <- "13C.{1,2}-"
  firstCol <- grep(pattern, colnames(Resp_dataframe))[1]
  molecule_list <- c()
  
  # Vanaf de eerste kolomnaam, die overeenkomt met het patroon, alle kolomnamen pakken
  for(col in firstCol:ncol(Resp_dataframe)){
    molecule_list <- c(molecule_list, gsub(" Results", "", colnames(Resp_dataframe)[col]))
  }
  
  return(molecule_list)
}

getSamples <- function(Resp_dataframe){
  col_ind_name <- which(Resp_dataframe == "Name", arr.ind = TRUE)[1,2]
  allSamples_Time <- Resp_dataframe[,col_ind_name]
  allSamples_ <- c()
  
  # Ophalen van alle samples waarin een _ staat. 
  for(sam in allSamples_Time){
    if(!sam == "Name"){
      allSamples_ <- c(allSamples_, regmatches(sam,regexpr(".*_", sam)))
    }
  }

  allSamples <- c()
  # Unieke samples pakken voor de keuze voor de gebruiker. 
  for(sam_ in unique(allSamples_)){
    sample <- gsub("_", "", sam_)
    allSamples <- c(allSamples, sample)
  }
  return(allSamples)
}

getMolecules_Norm <- function(Resp_dataframe){
  pattern <- "13C.{1,2}-"
  firstCol <- grep(pattern, colnames(Resp_dataframe))[1]
  molecule_list <- c()
  
  # Wanneer het patroon overeenkomt met de kolomnaam, wordt de kolomnaam opgeslagen in molecule_list
  for(col in firstCol:ncol(Resp_dataframe)){
    if(grepl(pattern, colnames(Resp_dataframe)[col]) == TRUE){
      molecule_list <- c(molecule_list, gsub(" Results", "", colnames(Resp_dataframe)[col]))
    }
  }
  
  return(molecule_list)
}