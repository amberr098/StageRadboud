savedIdentifiers <- function(){
  saved_compounds <- read.csv("compound_id_Excel.csv", sep = " ")
  return(saved_compounds)
}

addingCompounds <- function(compound, id){
  new_row <- list(compound, id)
  write.table(new_row, "compound_id_Excel.csv",append=TRUE, row.names = FALSE, col.names = FALSE)
}