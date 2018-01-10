# Checken of de benodigde compounds al een keer zijn opgeslagen of niet
searchIdentifiers <- function(allCompounds){
  unsaved_compounds <- c()
  
  # Ophalen van alle opgeslagen identifiers
  source("PathwaySavedCompounds.R")
  saved_compounds <- savedIdentifiers()
  
  index_col_compound <- which(colnames(saved_compounds) == "Compound")
  index_col_id <- which(colnames(saved_compounds) == "ID")
  
  allSavedCompounds <- saved_compounds[,index_col_compound]
  # Checken of alle benodigde compounds al een keer opgeslagen zijn, zo niet dan worden ze weergeven in de webinterface
  for(compound in allCompounds){
    if(compound %in% allSavedCompounds){
      index_row_id <- grep(compound, as.matrix(saved_compounds))
    }else{
      if(grepl(" Results", compound) == TRUE){
        compound <- gsub(" Results", "",compound)
      }

      # De compounds die nog niet eerder zijn opgeslagen worden weergeven in de webinterface
      unsaved_compounds <- c(unsaved_compounds, compound)
      unsavedIdentifiers(compound)
    }
  }

  return(unsaved_compounds)
}
# Plaatsen van de niet opgeslagen compounds in de webinterface
unsavedIdentifiers <- function(compound){
  insertUI(selector = "#placeholder_kegg_ids",
           ui = textInput(inputId = compound, label = compound))
}