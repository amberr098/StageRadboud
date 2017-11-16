# Bewerken van het .csv bestand 

# Verwijderen van RT kolommen en terugzetten van de kolomnamen.
setColumnNames <- function(data){

  # Alle indexen van RT kolommen, deze kolommen bevatten ook de namen van de moleculen
  index_RTColumns <- which(data == "RT", arr.ind = TRUE)
  
  all_colnames <- c()
  for(row in 1:nrow(index_RTColumns)){
    
    # De indexen van de kolommen met RT staan in een matrix die er zo worden uitgehaald.
    column_NumberRT <- index_RTColumns[row,2]
    
    # Op basis van de indexen van RT worden de kolom namen opgehaald.
    column_Names <- colnames(data[column_NumberRT])
    
    all_colnames <- c(all_colnames, column_Names)
  }
  
  # Verwijderen van de RT kolommen op basis van de index van RT kolommen
  data_noRT <- data[,-index_RTColumns[,2]]
  first_Column_Name <- index_RTColumns[1,2]

  for(i in 1:length(all_colnames)){
    colnames(data_noRT)[first_Column_Name] <- all_colnames[i]
    first_Column_Name = first_Column_Name + 1
  }
  
  return(data_noRT)
}

# Het toevoegen van een kolom waarin de totale response komt te staan van een sample.
getTotalResponses <- function(data_noRT){
  resp_Columns <- which(data_noRT == "Resp.", arr.ind = TRUE)

  # Vector waarin alle totale responses worden opgeslagen.
  total_responses <- c("total response")

  # Kopie van data_NoRT zodat er een kolom bijgevoegd kan worden.
  data_totalResponse <- data.frame(data_noRT, check.names = FALSE)

  # alle response waardes van 1 rij.
  for(i in 1:nrow(data_totalResponse)){
    row_allResponses <- data_totalResponse[i,resp_Columns[1,2]:ncol(data_totalResponse)]
    row_allResponses <- as.matrix(row_allResponses)
    # Eerste rij met kolomnamen niet meenemen, de andere rijen bij elkaar optellen en toevoegen aan vector.
    if(!row_allResponses[1] == "Resp."){
      sum_response <- sum(as.integer(row_allResponses))
      total_responses <- append(total_responses, sum_response)
    }
  }
  # Toevoegen van de nieuwe kolom. 
  data_totalResponse["total response"] <- total_responses
  return(data_totalResponse)
}

  