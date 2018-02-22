# Een matrix met alle compounds als kolomnamen en de twee gekozen condities door de gebruiker als rijnamen.
# De matrix is gevuld met de data zoals het in het ingevoerde csv bestand staat. Deze functie wordt alleen 
# bij de bestanden met Time data aangeroepen
getDataConditions <- function(condition1, condition2, Resp_dataframe){
  preDataConditions <- list()
  
  index_col_name <- which(Resp_dataframe == "Name", arr.ind = TRUE)[1,2]
  getIndexMatrix <- as.matrix(Resp_dataframe)

  for(row in 1:nrow(Resp_dataframe)){
    if(grepl("_", Resp_dataframe[row,index_col_name]) == TRUE){
      new_name <- unlist(strsplit(as.character(Resp_dataframe[row,index_col_name]), "_"))[1]
      getIndexMatrix[row,index_col_name] <- new_name
    }
  }
  
  # Alle rijen van de data van de gewenste condities ophalen
  indexes_condition1 <- grep(condition1, Resp_dataframe[,index_col_name])
  indexes_condition2 <- grep(condition2, Resp_dataframe[,index_col_name])

  preDataConditions <- setDataConditions(indexes_condition1, preDataConditions, Resp_dataframe, getIndexMatrix, condition1)
  preDataConditions <- setDataConditions(indexes_condition2, preDataConditions, Resp_dataframe, getIndexMatrix, condition2)

  # Ophalen van de kolom en rijnamen
  pattern <- "13C.{1,2}-"
  first_col <- grep(pattern, colnames(Resp_dataframe))[1]
  coln <- colnames(Resp_dataframe[first_col:ncol(Resp_dataframe)])
  rown <- getRownames(Resp_dataframe, indexes_condition1, indexes_condition2, index_col_name, condition1, condition2, getIndexMatrix)
  print(length(rown))
  # Maken van de matrix met alleen de gekozen samples
  dataConditions <- matrix(as.character(unlist(preDataConditions)),nrow=nrow(preDataConditions))
  print(nrow(dataConditions))
  rownames(dataConditions) <- rown
  colnames(dataConditions) <- coln
  
  # De data in de matrix van dataConditions numeric gemaakt
  dataConditionNumeric <- getNumericData(dataConditions)
  return(dataConditionNumeric)
}

# De data van de twee gekozen condities toevoegen aan een lijst waarvan later een matrix wordt gemaakt. 
setDataConditions <- function(indexes, preDataConditions, Resp_dataframe, getIndexMatrix, condition){
  pattern <- "13C.{1,2}-"
  first_col <- grep(pattern, colnames(Resp_dataframe))[1]
  index_col_name <- which(Resp_dataframe == "Name", arr.ind = TRUE)[1,2]
  
  for(row in indexes){
    if(identical(as.character(getIndexMatrix[row, index_col_name]), as.character(condition)) == TRUE){
      preDataConditions <- rbind(preDataConditions, Resp_dataframe[row,first_col:ncol(Resp_dataframe)])
    }
  }

  return(preDataConditions)
}

# Ophalen van de rijnamen/samples voor de matrix
getRownames <- function(Resp_dataframe, indexes_condition1, indexes_condition2, index_col_name, condition1, condition2, getIndexMatrix){
  rown <- c()
  
  for(index in indexes_condition1){
    if(identical(as.character(getIndexMatrix[index, index_col_name]), as.character(condition1)) == TRUE){
      name_condition1 <- Resp_dataframe[index, index_col_name]
      rown <- c(rown, as.character(name_condition1))
    }
  }
  
  for(index in indexes_condition2){
    if(identical(as.character(getIndexMatrix[index, index_col_name]), as.character(condition2)) == TRUE){
      name_condition2 <- Resp_dataframe[index, index_col_name]
      rown <- c(rown, as.character(name_condition2))
    }
  }

  return(rown)
}

# Van characters nummers maken, soms hebben datasets , inplaats van . als scheidingsteken. 
getNumericData <- function(dataConditions){
  preDataConditionNumeric <- list()
  
  # Vervangen van de , door een . wanneer er een , in de value staat
  for(row in 1:nrow(dataConditions)){
    new_row <- c()
    for(col in 1:ncol(dataConditions)){
      if(grepl(",",dataConditions[row,col]) == TRUE){
        numericValue <- gsub(",", ".", dataConditions[row,col])
        new_row <- c(new_row, numericValue)
      }else{
        new_row <- c(new_row, dataConditions[row,col])
      }
    }
    preDataConditionNumeric <- rbind(preDataConditionNumeric, new_row)
  }
  
  dataConditionNumeric <- matrix(as.numeric(unlist(preDataConditionNumeric)),nrow=nrow(preDataConditionNumeric))
  rownames(dataConditionNumeric) <- rownames(dataConditions)
  colnames(dataConditionNumeric) <- colnames(dataConditions)
  
  return(dataConditionNumeric)
}