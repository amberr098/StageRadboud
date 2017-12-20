# Het bij elkaar voegen van dezelfde samples en het gemiddelde en de standaard deviatie
# berekenen per tag. Wordt zowel gedaan bij de niet-genormaliseerde waarden als de 
# genormaliseerde waarden. 

getSameSamples <- function(data){
  # index_Name is een matrix met de indexen van de cel waarin Name staat. Hetzelfde voor index_Type.
  index_Name <- which(data == "Name", arr.ind = TRUE)
  index_Type <- which(data == "Type", arr.ind = TRUE)
  col_Type <- index_Type[1,2]

  pattern_Tag <- "_.*"
  all_Tags <- c()
  all_Types <- c()
  
  # De kolommen met de waarde van response (Resp.) opgeslagen in een matrix
  resp_columns <- which(data == "Resp.", arr.ind = TRUE)
  all_molecule_names <- colnames(data[resp_columns[1,2]:ncol(data)])
  
  # Er wordt gekeken of het bestand Types bevat, zo ja dan worden de types de rijnamen.
  row_tag <- 0
  for(name in data[, index_Name[1,2]]){
    row_tag <- row_tag + 1 

    if(grepl(pattern_Tag, name) == TRUE){
      if(!data[row_tag,col_Type] == "Sample"){
        all_Types <- c(all_Types, as.character(data[row_tag,col_Type]))
      }else{
        # tag zijn alle _a, _b, etc.
        tag <- regmatches(name, regexpr(pattern_Tag, name))
        all_Tags <- c(all_Tags, tag)
      }
    }
  }

  # Alleen de unieke tags overhouden voor de grep functie
  unique_tags <- unique(all_Tags)
  unique_types <- unique(all_Types)

  if(is.null(unique_tags)){
    column_names <- unique_types
  }else{
    column_names <- unique_tags
  }
  # Maken van een lege matrix met alleen de kolomnamen (molecuul namen) en rijnamen (unieke tags)
  basic_matrix <- setMatrix(column_names, all_molecule_names)
  average_matrix <- basic_matrix
  standDev_matrix <- basic_matrix

  # Als er alleen tags zijn ingevoerd, dan wordt er verder gewerkt met tags.
  if(!is.null(unique_tags)){
    # Herkennen welke samples dezelfde tag hebben.
    for(tags in unique_tags){
      index_Same_Samples <- which(grepl(tags, data[, index_Name[1,2]]))

      # De kolomnummers van de kolommen Resp. ophalen uit de matrix. 
      for(column in resp_columns[,2]){
        name_Molecule <- colnames(data[column])
        row_Val <- c()
        for(row in index_Same_Samples){
          val <- data[row, column]
          row_Val <- c(as.character(row_Val), as.character(val))
          
        }
        average_matrix <- setAverageMatrix(row_Val, name_Molecule, average_matrix, tags)
        standDev_matrix <- setStandDevMatrix(row_Val, name_Molecule, standDev_matrix, tags)
      }
    }
  }else{
    for(type in unique_types){
      
      index_Same_Sample <- which(grepl(type, data[, col_Type]))
      
      for(column in resp_columns[,2]){

        name_Molecule <- colnames(data[column])
        row_Val <- c()
        for(row in index_Same_Sample){
          val <- data[row, column]
          row_Val <- c(as.character(row_Val), as.character(val))
        }
        average_matrix <- setAverageMatrix(row_Val, name_Molecule, average_matrix, type)
        standDev_matrix <- setStandDevMatrix(row_Val, name_Molecule, standDev_matrix, type)
      }
    }
  }
  
  # Om twee dataframes te returnen, worden ze beide toegevoegd aan een lijst. 
  average_stanDev<- list()
  average_stanDev$average <- average_matrix
  average_stanDev$standDev <- standDev_matrix
  
  return(average_stanDev)
}

# Een lege matrix maken met alleen de kolomnamen en rijnamen gedefinieerd. 
setMatrix <- function(unique_tags, all_molecule_names){
  basic_matrix <- matrix(NA, nrow = length(unique_tags), ncol = length(all_molecule_names))
  colnames(basic_matrix) <- all_molecule_names
  rownames(basic_matrix) <- unique_tags
  
  return(basic_matrix)
}

# De lege matrix vullen met de gemiddeldes. 
setAverageMatrix <- function(row_Val, name_Molecule, average_matrix, tags){
  average <- mean(as.numeric(row_Val))
  index_column <- match(name_Molecule, colnames(average_matrix))
  index_row <- match(tags, rownames(average_matrix))
  average_matrix[index_row,index_column] <- average
  
  return(average_matrix)
}

# De lege matrix vullen met standaard deviaties. 
setStandDevMatrix <- function(row_Val, name_Molecule, standDev_matrix, tags){
  standDev <- sd(as.numeric(row_Val))
  index_column <- match(name_Molecule, colnames(standDev_matrix))
  index_row <- match(tags, rownames(standDev_matrix))
  
  standDev_matrix[index_row,index_column] <- standDev
  return(standDev_matrix)
}
