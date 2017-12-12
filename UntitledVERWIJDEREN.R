pattern <- "13C.{1,2}-"
firstCol <- grep(pattern, colnames(Resp_dataframe))[1]

# Matrix maken van dataframe om alle , door . te veranderen zodat de waardes gezien worden als nummers.
Resp_matrix <- as.matrix(Resp_dataframe)

for(row in 1:nrow(Resp_matrix)){
  for(col in 1:ncol(Resp_matrix)){
    if(grepl(",", Resp_matrix[row,col]) == TRUE){
      Resp_matrix[row,col] <- as.numeric(gsub(",",".",Resp_matrix[row,col]))
    }
  }
}

###### AVERAGE - ABSOLUTE########
allNames <- c()
col_Name <- which(Resp_matrix[1,] == "Name")
for(row in 1:nrow(Resp_matrix)){
  if(!Resp_matrix[row,col_Name] == "Name"){
    allNames <- c(allNames, Resp_matrix[row,col_Name])
  }
}

# Bevat ook samples zonder een tijd.
unique_names <- unique(allNames)
pattern_time <- "_\\d{1,2}[min|h|sec]"

temp_average_df <- list()
rown <- c()
coln <- colnames(Resp_matrix)[firstCol:ncol(Resp_matrix)]

for(name in unique_names){
  # Alleen de samples pakken met een tijd.
  if(grepl(pattern_time, name) == TRUE){
    rown <- c(rown,name)
    same_samples_index <- grep(name, Resp_matrix[,col_Name])
    average_row <- c()
    for(col in firstCol:ncol(Resp_matrix)){
      values <- c()
      for(row in same_samples_index){
        values <- c(values,as.numeric(Resp_matrix[row,col]))
      }
      # Alle gemiddelden van de rij in een vector zetten
      average_row <- c(average_row, mean(values))
    }
    temp_average_df <- rbind(temp_average_df, average_row)
  }
}
# Dataframe maken van de gemiddelden. 
average_df <- as.data.frame(temp_average_df)
colnames(average_df) <- coln
rownames(average_df) <- rown

########################################### NORM

pattern <- "13C.{1,2}-"
allC13_index <- grep(pattern, colnames(average_df))

allMolecules <- c()
temp_divided_values <- list()
temp_divided_by_total <- list()
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
      # Voor de kolomnamen
      allMolecules <- c(allMolecules, C13_molecule)
      
      # Als het molecuul overeenkomt met de nextCol dan moet daar de deling van genomen worden
      C13_values <- average_df[,col]
      C12_values <- average_df[,nextCol]
      
      divided_values <- as.numeric(C13_values)/as.numeric(C12_values)
      temp_divided_values <- cbind(temp_divided_values, divided_values)
      
      divided_by_total <- as.numeric(C13_values)/(as.numeric(C13_values) +as.numeric(C12_values))
      print(divided_by_total)
      temp_divided_by_total <- cbind(temp_divided_by_total, divided_by_total)
      
      mols <- c(mols, C13_molecule)
    }else{
      if(!molecule %in% totalMol){
        totalMol <- c(totalMol, molecule)
      }
      
      allMolecules <- c(allMolecules, C13_molecule)
      
      C13_values <- average_df[,col]
      col_molecule <- match(molecule, colnames(average_df))
      C12_values <- average_df[,col_molecule]
      
      divided_values <- as.numeric(C13_values)/as.numeric(C12_values)
      temp_divided_values <- cbind(temp_divided_values, divided_values)
    }
  }
}

C13C12_norm <- as.data.frame(temp_divided_values)
colnames(C13C12_norm) <- allMolecules
rownames(C13C12_norm) <- rownames(average_df)

View(C13C12_norm)

for(mol in totalMol){
  list_for_mat <- list()
  
  all_cols_mol <- grep(mol, colnames(average_df))
  coln <- c()
  for(col in all_cols_mol){
    list_for_mat <- cbind(list_for_mat, average_df[,col])
    coln <- c(coln, colnames(average_df)[col])
  }
  
  all_cols_mat <- as.matrix(list_for_mat)
  
  sum_rows <- c()
  for(row in 1:nrow(all_cols_mat)){
    total <- sum(as.numeric(all_cols_mat[row,]))
    sum_rows <- c(sum_rows, total)
  }
  
  list_for_mat<- cbind(list_for_mat, sum_rows)
  
  coln <- c(coln, "Total")
  totals_df <- as.data.frame(list_for_mat)
  colnames(totals_df) <- coln
  
  pattern <- "13C.{1,2}-"
  
  total_col <- which(colnames(totals_df) == "Total")
  C13mol <- grep(pattern, colnames(totals_df))
  for(mol in C13mol){
    mols <- c(mols, colnames(totals_df)[mol])
    C13_values <- totals_df[,mol]
    total_values <- totals_df[,total_col]

    divided_by_total <- as.numeric(C13_values)/as.numeric(total_values)
    temp_divided_by_total <- cbind(temp_divided_by_total, divided_by_total)
  }
}

divided_by_total <- as.data.frame(temp_divided_by_total)
colnames(divided_by_total) <- mols
rownames(divided_by_total) <- rownames(average_df)
View(divided_by_total)




























