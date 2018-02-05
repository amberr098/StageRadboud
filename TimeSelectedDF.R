getSelectedDataframe <- function(samples, molecules, average_df, standev_df){
  pattern <- "13C.{1,2}-"
  list_for_df <- list()
  
  for(mol in molecules){
    # Aan het molecule Results toevoegen zodat het herkend kan worden in de average_df
    col_ind <- which(colnames(average_df) == paste0(mol, " Results"), arr.ind = T)
    for(sam in samples){
      samples_row <- grep(sam, rownames(average_df))
   
      for(row in samples_row){
        rown <- rownames(average_df)[row]
        
        # Van 0h, 1h bijv 0, 1 maken.
        sam_time <- unlist(strsplit(rown, "_"))
        time <- sam_time[2]
        num_time <- as.numeric(gsub("\\D", "", time))
        
        # Average en standaard deviatie ophalen. 
        average <- average_df[row,col_ind]
        sd <- standev_df[row, col_ind]
        half_sd <- as.numeric(sd)/2

        # Het splitsen van het molecuul en de variant: 13C6-CMP-Neu5Gc in molecuul: CMP-neu5Gc, variant: 13C6
        if(grepl(pattern,mol) == TRUE){
          C13_variant_temp <- regmatches(mol, regexpr(pattern, mol))
          C_variant <- gsub("-", "", C13_variant_temp)
          C_molecule <- gsub(pattern, "", mol)
        }else{
          C_variant <- "12C"
          C_molecule <- mol
        }
        new_row <- c(sam_time[1], num_time, C_molecule, C_variant, average,sd, half_sd)
        list_for_df <- rbind(list_for_df, new_row)
      }
    }
  }
  
  selected_dataframe <- as.data.frame(list_for_df)
  return(selected_dataframe)
}

getShowDataframe <- function(samples, molecules, average_df, standev_df){
  list_for_df <- list()
  
  # Index van de kolomnaam ophalen op basis van het gekozen molecuul.
  for(mol in molecules){
    mol <- paste0(mol, " Results")
    col <- which(colnames(average_df) == mol)
    
    # Indexen van de rijnamen ophalen op basis van het gekozen sample.
    for(sam in samples){
      sample_rows <- grep(sam, rownames(average_df))
      for(row in sample_rows){
        mol_name_temp <- colnames(average_df)[col]
        mol_name <- gsub(" Results", "", mol_name_temp)
        
        sam_name_temp <- rownames(average_df)[row]
        sam_name <- gsub("_", " ", sam_name_temp)
        
        average <- average_df[row,col]
        sd <- standev_df[row,col]
        
        # Steeds een nieuwe rij toevoegen aan de lijst. 
        new_row <- c(sam_name, mol_name, average, sd)
        list_for_df <- rbind(list_for_df, new_row)
      }
    }
  }
  
  # Dataframe maken van de lijst
  showDataFrame <- as.data.frame(list_for_df)
  colnames(showDataFrame) <- c("Samples", "Molecules", "Average", "SD")
  
  return(showDataFrame)
}