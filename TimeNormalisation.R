# Alle 13C kolommen delen door de 12C kolom
C13_dividedBy_C12 <- function(Resp_dataframe){
  pattern <- "13C.{1,2}-"
  firstCol <- grep(pattern, colnames(Resp_dataframe))[1]
  
  # Matrix maken van dataframe om alle , door . te veranderen zodat de waardes gezien worden als nummers.
  Resp_matrix <- as.matrix(Resp_dataframe)
  
  # Alle , vervangen door .
  for(row in 1:nrow(Resp_matrix)){
    for(col in 1:ncol(Resp_matrix)){
      if(grepl(",", Resp_matrix[row,col]) == TRUE){
        Resp_matrix[row,col] <- as.numeric(gsub(",",".",Resp_matrix[row,col]))
      }
    }
  }

  norm_list <- list()
  column_names_matrix <- c()
  for(col in firstCol:ncol(Resp_matrix)){
    # Alle 13C moleculen ophalen
    if(grepl(pattern, colnames(Resp_matrix)[col]) == FALSE){
      coln <- colnames(Resp_matrix)[col]
      coln_numbers <- grep(coln,colnames(Resp_matrix))

      # Als coln_number kleiner is dan 3, dan bevat het molecuul alleen één 13C en één 12C kolom
      if(length(coln_numbers)< 3){
        col1 <- coln_numbers[1]
        col2 <- coln_numbers[2]
        
        # Alle waarden van de 13C en 12C kolommen ophalen en de Resp. verwijderen door [-1]
        C13_values <- Resp_matrix[,col1][-1]
        C12_values <- Resp_matrix[,col2][-1]
       
        # De twee kolommen door elkaar delen
        norm_values <- as.numeric(C13_values)/as.numeric(C12_values)
        
        # De waardes toevoegen aan een lijst
        norm_list <- cbind(norm_list, norm_values)
        
      }else{
        # coln_number heeft meer dan 3 waarden wat inhoudt dat er meerdere 13C varianten zijn
        if(grepl(pattern, colnames(Resp_matrix)[col]) == FALSE){
          C12_col <- col
          for(num in coln_numbers){
            if(!num == C12_col){
              # Ophalen van de waarden van de 13C en 12C kolommen
              C13_values <- Resp_matrix[,num][-1]
              C12_values <- Resp_matrix[,C12_col][-1]
              
              # Kolommen delen door elkaar: elke C13 variant delen door C12 kolom
              norm_values <- as.numeric(C13_values)/as.numeric(C12_values)
              norm_list <- cbind(norm_list, norm_values)
              
            }
          }
        }
      }
    }else{
      column_names_matrix <- c(column_names_matrix, colnames(Resp_matrix)[col])
    }
  }
  
  # Verkrijgen van de rij namen. [-1] voor het verwijderen van de cel "Name"
  name_col <- which(Resp_matrix == "Name", arr.ind = TRUE)[1,2]
  rown <- Resp_matrix[,name_col][-1]
  
  norm_matrix <- as.matrix(norm_list)
  colnames(norm_matrix) <- column_names_matrix
  rownames(norm_matrix) <- rown
  
  return(norm_matrix)
}

# Het gemiddelde van duplicated samples berekenen
getAverageC13C12 <- function(norm_matrix){
  average_list <- list()
  unique_names <- unique(rownames(norm_matrix))
  
  # alle soorten samples een voor een zoeken.
  for(name in unique_names){
    average_row <- c()
    col_numbers <- grep(name, rownames(norm_matrix))
    for(col in 1:ncol(norm_matrix)){
      values <- c()
      for(row in col_numbers){
        # De waardes van dezelfde samples in een vector zetten
        values <- c(values, as.numeric(norm_matrix[row,col]))
      }
      # Het gemiddelde nemen van de values en toeveogen aan een vector
      average_row <- c(average_row, mean(values))
    }
    # Rij toevoegen aan een lijst met alle gemiddelde waarden. 
    average_list <- rbind(average_list, average_row)
  }
  
  average_df <- as.data.frame(average_list)
  colnames(average_df) <- colnames(norm_matrix)
  rownames(average_df) <- unique_names
  return(average_df)
}

# De standaard deviatie van duplicated samples berekenen
getStanDevC13C12 <- function(norm_matrix){
  sd_list <- list()
  unique_names <- unique(rownames(norm_matrix))
  
  # alle soorten samples een voor een zoeken.
  for(name in unique_names){
    sd_row <- c()
    col_numbers <- grep(name, rownames(norm_matrix))
    for(col in 1:ncol(norm_matrix)){
      values <- c()
      for(row in col_numbers){
        # De waardes van dezelfde samples in een vector zetten
        values <- c(values, as.numeric(norm_matrix[row,col]))
      }
      # De standaard deviatie nemen van de values en toeveogen aan een vector
      sd_row <- c(sd_row, sd(values))
    }
    # Rij toevoegen aan een lijst met alle gemiddelde waarden. 
    sd_list <- rbind(sd_list, sd_row)
  }
  
  sd_df <- as.data.frame(sd_list)
  colnames(sd_df) <- colnames(norm_matrix)
  rownames(sd_df) <- unique_names
  return(sd_df)
}

# Alle 13C kolommen delen door het totaal van de 13C+12C kolom.
C13_dividedBy_total <- function(Resp_dataframe){
  pattern <- "13C.{1,2}-"
  firstCol <- grep(pattern, colnames(Resp_dataframe))[1]
  
  # Matrix maken van dataframe om alle , door . te veranderen zodat de waardes gezien worden als nummers.
  Resp_matrix <- as.matrix(Resp_dataframe)
  
  # Alle , veranderen in .
  for(row in 1:nrow(Resp_matrix)){
    for(col in 1:ncol(Resp_matrix)){
      if(grepl(",", Resp_matrix[row,col]) == TRUE){
        Resp_matrix[row,col] <- as.numeric(gsub(",",".",Resp_matrix[row,col]))
      }
    }
  }
  
  total_counts_matrix <- getTotalCounts(Resp_matrix, pattern, firstCol)
  norm_values_matrix <- getNormValues(Resp_matrix, total_counts_matrix, pattern, firstCol)
  return(norm_values_matrix)
}

# Matrix maken met alle totale waarden van de 12C kolom + 13C kolom
getTotalCounts <- function(Resp_matrix, pattern, firstCol){
  list_total_counts <- list()
  coln <- c()
  
  
  for(col in firstCol:ncol(Resp_matrix)){
    # Als het molecuul in de kolomnaam niet overeenkomt met het patroon, is het een 12C molecuul
    if(grepl(pattern,colnames(Resp_matrix)[col]) == FALSE){
      total_counts_vector <- c()
      coln <- c(coln, colnames(Resp_matrix)[col])
      C12_molecule <- colnames(Resp_matrix)[col]
      
      # Ophalen van het 13C molecuul 
      cols_molecules <- grep(C12_molecule, colnames(Resp_matrix))
      
      # Elke rij pakken
      for(row in 1:nrow(Resp_matrix)){
        total_counts <- 0
        
        # De kolommen die bij elkaar horen bij elkaar optellen
        for(col in cols_molecules){
          
          value <- Resp_matrix[row,col]
          if(!value == "Resp."){
            total_counts <- total_counts + as.numeric(value)
          }else{
            total_counts <- NULL
          }
        }
        
        # alle totale waarden toevoegen aan een vector
        if(!is.null(total_counts)){
          total_counts_vector <- c(total_counts_vector, total_counts)
        }
      }
      # Vector met de totale waarden van een kolom in een lijst zetten
      list_total_counts <- cbind(list_total_counts, total_counts_vector)
    }
  }
  
  col_index_name <- which(Resp_matrix == "Name", arr.ind = TRUE)[1,2]
  rown <- Resp_matrix[,col_index_name][-1]
  
  total_counts_matrix <- as.matrix(list_total_counts)
  colnames(total_counts_matrix) <- coln
  rownames(total_counts_matrix) <- rown
  return(total_counts_matrix)
}

# De 13C kolommen delen door de totale waarden van dat molecuul (13C+12C)
getNormValues <- function(Resp_matrix, total_counts_matrix,pattern, firstCol){
  list_norm_values <- list()
  coln_norm <- c()
  
  for(col in firstCol:ncol(Resp_matrix)){
    # Alle 13C kolommen ophalen
    if(grepl(pattern,colnames(Resp_matrix)[col]) == TRUE){
      norm_values_vector <- c()
      coln_norm <- c(coln_norm, colnames(Resp_matrix)[col])
      C13_molecule <- colnames(Resp_matrix)[col]
      molecule <- gsub(pattern, "", C13_molecule)
      # Ophalen van de kolom met de totale waarden van het 13C molecuul.
      col_molecule_total <- which(colnames(total_counts_matrix) == molecule, arr.ind = T)
      for(row in 1:nrow(Resp_matrix)){
        # Elke rij het 13C molecuul delen door het totaal van dat 13C molecuul + 12C molecuul
        if(!Resp_matrix[row,col] == "Resp."){
          abs_val <- Resp_matrix[row,col]
          total_val <- total_counts_matrix[row-1,col_molecule_total]
          norm_value  <- as.numeric(abs_val)/as.numeric(total_val)
          norm_values_vector <- c(norm_values_vector, norm_value)
        }
        
      }
      list_norm_values <- cbind(list_norm_values, norm_values_vector)
    }
  }
  col_index_name <- which(Resp_matrix == "Name", arr.ind = TRUE)[1,2]
  rown <- Resp_matrix[,col_index_name][-1]
  
  norm_values_matrix <- as.matrix(list_norm_values)
  colnames(norm_values_matrix) <- coln_norm
  rownames(norm_values_matrix) <- rown
  return(norm_values_matrix)
}

# Average berekenen van de C13 kolommen die gedeeld zijn door het totaal
getAverageC13Total <- function(norm_values_matrix){
  all_samples <- unique(rownames(norm_values_matrix))
  list_average <- list()
  coln_average <- c()
  
  # Elke unieke sample een voor een.
  for(sam in all_samples){
    all_mean_row <- c()
    
    # Ophalen van de samples die hetzelfde zijn
    rows_sam <- grep(sam, rownames(norm_values_matrix))
    for(col in 1:ncol(norm_values_matrix)){
      mean_of_vector <- c()
      
      for(row in rows_sam){
        # Alle waarden die bij elkaar horen in een vector zetten 
        mean_of_vector <- c(mean_of_vector, as.numeric(norm_values_matrix[row,col])) 
      }
      
      # Gemiddelde nemen van de vector waarin de waarden staan van hetzelfde sample
      average <- mean(mean_of_vector)
      all_mean_row <- c(all_mean_row, average)
    }
    list_average <- rbind(list_average, all_mean_row)
  }
  
  average_df <- as.data.frame(list_average)
  colnames(average_df) <- colnames(norm_values_matrix)
  rownames(average_df) <- all_samples
  
  return(average_df)
}

getStanDevC13Total <- function(norm_values_matrix){
  all_samples <- unique(rownames(norm_values_matrix))
  list_sd <- list()
  coln_sd <- c()
  
  # Elke unieke sample een voor een.
  for(sam in all_samples){
    all_sd_row <- c()
    
    # Ophalen van de samples die hetzelfde zijn
    rows_sam <- grep(sam, rownames(norm_values_matrix))
    for(col in 1:ncol(norm_values_matrix)){
      sd_of_vector <- c()
      
      for(row in rows_sam){
        # Alle waarden die bij elkaar horen in een vector zetten 
        sd_of_vector <- c(sd_of_vector, as.numeric(norm_values_matrix[row,col])) 
      }
      # De standaard deviatie nemen van de vector met de waardes van dezelfde samples
      sd <- sd(sd_of_vector)
      all_sd_row <- c(all_sd_row, sd)
    }
    list_sd <- rbind(list_sd, all_sd_row)
  }
  
  standev_df <- as.data.frame(list_sd)
  colnames(standev_df) <- colnames(norm_values_matrix)
  rownames(standev_df) <- all_samples
  
  return(standev_df)
}