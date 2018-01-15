getDataConditions <- function(condition1, condition2, Resp_dataframe){
  preDataConditions <- list()
  
  index_col_name <- which(Resp_dataframe == "Name", arr.ind = TRUE)[1,2]
  # Alle rijen van de data van de gewenste condities ophalen
  indexes_condition1 <- grep(condition1, Resp_dataframe[,index_col_name])
  indexes_condition2 <- grep(condition2, Resp_dataframe[,index_col_name])

  preDataConditions <- setDataConditions(indexes_condition1, preDataConditions, Resp_dataframe)
  preDataConditions <- setDataConditions(indexes_condition2, preDataConditions, Resp_dataframe)
  
  # Ophalen van de kolom en rijnamen
  pattern <- "13C.{1,2}-"
  first_col <- grep(pattern, colnames(Resp_dataframe))[1]
  coln <- colnames(Resp_dataframe[first_col:ncol(Resp_dataframe)])
  rown <- getRownames(Resp_dataframe, indexes_condition1, indexes_condition2, index_col_name)
  
  # Maken van de matrix met alleen de gekozen samples
  dataConditions <- matrix(as.character(unlist(preDataConditions)),nrow=nrow(preDataConditions))
  rownames(dataConditions) <- rown
  colnames(dataConditions) <- coln
  
  # De data in de matrix van dataConditions numeric gemaakt
  dataConditionNumeric <- getNumericData(dataConditions)
  return(dataConditionNumeric)
}

# De data van de twee gekozen condities toevoegen aan een lijst waarvan later een matrix wordt gemaakt. 
setDataConditions <- function(indexes, preDataConditions, Resp_dataframe){
  pattern <- "13C.{1,2}-"
  first_col <- grep(pattern, colnames(Resp_dataframe))[1]
  
  for(row in indexes){
    preDataConditions <- rbind(preDataConditions, Resp_dataframe[row,first_col:ncol(Resp_dataframe)])
  }
  
  return(preDataConditions)
}

# Ophalen van de rijnamen/samples voor de matrix
getRownames <- function(Resp_dataframe, indexes_condition1, indexes_condition2, index_col_name){
  rown <- c()
  
  for(index in indexes_condition1){
    name_condition1 <- Resp_dataframe[index, index_col_name]
    rown <- c(rown, as.character(name_condition1))
  }
  
  for(index in indexes_condition2){
    name_condition2 <- Resp_dataframe[index, index_col_name]
    rown <- c(rown, as.character(name_condition2))
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