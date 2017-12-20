getMolecules <- function(data){
  molecule_list <- c()
  # Alle indexen van de kolommen ophalen met RT want hierboven staan de kolomnamen.
  index_RT <- which(data == "RT", arr.ind = TRUE)
  
  # Alle kolomnamen/moleculen ophalen als keuze voor de gebruiker. 
  for(index in index_RT[,2]){
    molecule_name <- colnames(data)[index]
    molecule_name <- gsub(" Results", "", molecule_name)
    molecule_list <- c(molecule_list, molecule_name)
  }
  
  return(molecule_list)
}

# Het ophalen van de type van de samples
getSamples <- function(data){
  # Index van de kolom Type.
  index_Type <- which(data == "Type", arr.ind = TRUE)
  column_Type <- index_Type[1,2]
  
  # Index van de kolom Name
  index_Name <- which(data == "Name", arr.ind = TRUE)
  column_Name <- index_Name[1,2]
  
  pattern_Tag = "_.*"
  
  all_Tags <- c()
  # Zoeken naar de tags van de samples.
  for(name in data[, column_Name]){
    # tag zijn alle _a, _b, etc.
    tag <- regmatches(name, regexpr(pattern_Tag, name))
    all_Tags <- c(all_Tags, tag)
  }
  
  # Alleen de unieke tags overhouden
  unique_tags <- unique(all_Tags)
  
  all_Types <- c()
  check <- NULL
  # Tags zoals ze gevisualiseerd zullen worden.
  vis_Tags <- c()
  
  # Voor elke tag het rij nummer ophalen zodat uit die rij de Type gehaald kan worden.
  for(tag in unique_tags){
    
    row_number_tag <- which(grepl(tag, data[, column_Name]))
    # Type is een factor met dezelfde type namen, dus alleen de eerste wordt gepakt.
    type <- data[row_number_tag, column_Type]
    # Als de type gelijk is aan sample dan zijn er geen Types ingevoerd en worden de tags weergeven.
    if(!type[1] == "Sample"){
      all_Types <- c(all_Types, as.character(type[1]))
      check <- TRUE
    }else{
      tag <- gsub("_", "", tag)
      vis_Tags <- c(vis_Tags, tag)
      check <- FALSE
    }
  }
  
  if(check == TRUE){
    return(all_Types)
  }else if(check == FALSE){
    return(vis_Tags)
  }
}