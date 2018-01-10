placeIDs <- function(){
  # Ophalen de dataframe met daarin de compounds en bijbehorende ids
  source("PathwaySavedCompounds.R")
  saved_compounds <<- savedIdentifiers()

  # Ophalen van de compounds die veranderd moeten worden in IDs
  replacingCompounds <<- colnames(log2Dataframe)

  index_col_Compound <- which(colnames(saved_compounds) == "Compound")
  index_col_ID <- which(colnames(saved_compounds) == "ID")
  replaced_ids <- c()
  
  # Voor elke compound het bijbehorende ID zoeken en in een lijst zetten
  for(compound in replacingCompounds){
    row_index_compound <- which(saved_compounds == compound)
    id <- saved_compounds[row_index_compound, index_col_ID]
    replaced_ids <- c(replaced_ids, as.character(id))
  }

  # De compounds als kolomnamen vervangen door IDs
  tableIDs <<- log2Dataframe
  colnames(tableIDs) <<- replaced_ids

  return(tableIDs)
}