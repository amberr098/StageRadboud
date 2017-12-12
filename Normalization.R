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

normalisation_C13C12 <- function(average_df, switch_norm){
  pattern <- "13C.{1,2}-"
  allC13_index <- grep(pattern, colnames(average_df))
  allMolecules <- c()
  temp_divided_values <- list()
  nextCol <- 0
  mols <- c()
  totalMol <- c()
  
  for(col in allC13_index){
    nextCol <- col + 1
    
    C13_molecule <- colnames(average_df)[col]
    molecule <- gsub(pattern, "", C13_molecule)
    
    # Checken of er meerdre C's zijn van één molecuul: C3, C6, C9 etc.
    check <- grep(molecule, colnames(average_df))
    if(!col == ncol(average_df)){
      if(length(check) < 3){
        # Als het molecuul overeenkomt met de nextCol dan moet daar de deling van genomen worden
        C13_values <- average_df[,col]
        C12_values <- average_df[,nextCol]
        
        # FALSE betekent dat C13/C12 voor normalisatie gedaan moet worden.
        # TRUE betekend dat C13/total gedaan moet worden.
        if(switch_norm == FALSE){
          # Voor de kolomnamen
          allMolecules <- c(allMolecules, C13_molecule)
          
          divided_values <- as.numeric(C13_values)/as.numeric(C12_values)
          temp_divided_values <- cbind(temp_divided_values, divided_values)
        }else{
          # Voor kolomnamen
          allMolecules <- c(allMolecules, C13_molecule)
        
          divided_values <- as.numeric(C13_values)/(as.numeric(C13_values) +as.numeric(C12_values))
          temp_divided_values <- cbind(temp_divided_values, divided_values)
        }
        
      }else{
        if(switch_norm == FALSE){
          allMolecules <- c(allMolecules, C13_molecule)
          
          C13_values <- average_df[,col]
          col_molecule <- match(molecule, colnames(average_df))
          C12_values <- average_df[,col_molecule]
          
          divided_values <- as.numeric(C13_values)/as.numeric(C12_values)
          temp_divided_values <- cbind(temp_divided_values, divided_values)
        }else{
          # Elke kolomnaam opslaan die meer dan 2 keer voorkomt
          if(!molecule %in% totalMol){
            totalMol <- c(totalMol, molecule)
          }
        }
      }
    }
  }
  # EXCEPTIE MAKEN WANNEER TOTALMOL LEEG IS!!!!!!!
  
  # elke mol variabelen is een nucleotide suiker met meerdere variantie (C3, C6, C9 etc.)
  for(mol in totalMol){
    # Hierin worden alle kolommen opgeslagen die bij elkaar horen qua nucleotide suiker
    list_for_mat <- list()
    
    all_cols_mol <- grep(mol, colnames(average_df))
    coln <- c()
    # Ophalen van dezelfde nucleotide suikers met andere Cs
    for(col in all_cols_mol){
      list_for_mat <- cbind(list_for_mat, average_df[,col])
      coln <- c(coln, colnames(average_df)[col])
    }
    
    # Matrix maken van de list zodat de som berekent kan worden van een rij. 
    all_cols_mat <- as.matrix(list_for_mat)
    
    sum_rows <- c()
    for(row in 1:nrow(all_cols_mat)){
      total <- sum(as.numeric(all_cols_mat[row,]))
      sum_rows <- c(sum_rows, total)
    }
    
    # Toevoegen van een kolom met de totale som van de rij
    list_for_mat<- cbind(list_for_mat, sum_rows)

    # Maken van een dataframe van de lijst met de totale som als laatste kolom. 
    coln <- c(coln, "Total")
    totals_df <- as.data.frame(list_for_mat)
    colnames(totals_df) <- coln
    
    # Normaliseren door de C varianten (C3, C6, C9 bijv.) te delen door het totaal. 
    total_col <- which(colnames(totals_df) == "Total")
    C13mol <- grep(pattern, colnames(totals_df))
    for(mol in C13mol){
      allMolecules <- c(allMolecules, colnames(totals_df)[mol])
      C13_values <- totals_df[,mol]
      total_values <- totals_df[,total_col]
      
      divided_values <- as.numeric(C13_values)/as.numeric(total_values)
      temp_divided_values <- cbind(temp_divided_values, divided_values)
    }
  }
  
  # data_norm is de genormaliseerde data. Hangt af van de switch welke data erin staat. 
  data_norm <- as.data.frame(temp_divided_values)
  colnames(data_norm) <- allMolecules
  rownames(data_norm) <- rownames(average_df)
}

