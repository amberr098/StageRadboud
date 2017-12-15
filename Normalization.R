Normalization_Resp <- function(data_totalResponse){

  # Verwijderen van rows met NA 
  data_totalResponse <- na.omit(data_totalResponse)
  
  # Index kolom van total response. 
  index_total_response <- match("total response", colnames(data_totalResponse))
  
  # In matrix de kolommen en rijen van Resp. all_response zijn alle kolommen met response values.
  resp_columns <- which(data_totalResponse == "Resp.", arr.ind = TRUE)
  all_response <- resp_columns[,2]
  
  norm_Responses <- as.matrix(data_totalResponse)
  for (row in 1:nrow(data_totalResponse)) {
    # Het ophalen van de waarde van de total response van 1 rij.
    total_response <- data_totalResponse[row,index_total_response]
    
    if(!total_response == "total response"){
      # Numeric voor een berekening
      total_response <- as.numeric(as.character(total_response))
      
      # Een voor een ophalen van de kolommen met response
      for(response in all_response){
        response_value <- as.numeric(as.character(data_totalResponse[row,response]))
        # Normalisatie 
        norm_resp <- (response_value/total_response)*100
        # De oorspronkelijke waarde vervangen door de genormaliseerde waarde. 
        norm_Responses[row,response] <- norm_resp
      }
    }
  }
  # Verwijderen van de totale response kolom.
  norm_Responses <- norm_Responses[,-index_total_response]
  return(norm_Responses)
}

  

