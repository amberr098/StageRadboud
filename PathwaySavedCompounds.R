# Het openen van het bestand waarin de opgeslagen compounds staan met de IDs
savedIdentifiers <- function(){
  saved_compounds <- read.csv("compound_id_Excel.csv", sep = " ")
  return(saved_compounds)
}

# Het toevoegen van compounds wanneer dat wordt ingevuld in de webinterface. 
addingCompounds <- function(compound, id){
  new_row <- list(compound, id)
  write.table(new_row, "compound_id_Excel.csv",append=TRUE, row.names = FALSE, col.names = FALSE)
}