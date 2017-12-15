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
