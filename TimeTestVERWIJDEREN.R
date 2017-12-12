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

############# C13/C12 ###################
pattern <- "13C.{1,2}-"
allC13_index <- grep(pattern, colnames(average_df))
allMolecules <- c()
temp_divided_values <- list()
temp_divided_by_total <- list()
nextCol <- 0
checkColumns <- c()
ta <- c()


for(col in allC13_index){
  t <- 0
  nextCol <- col + 1
  temp <- list()
  
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
      temp_divided_by_total <- cbind(temp_divided_by_total, divided_by_total)
    }else{
      allMolecules <- c(allMolecules, C13_molecule)

      col_molecule <- match(molecule, colnames(average_df))
      C12_values <- average_df[,col_molecule]
      C13_values <- average_df[,col]
      
      divided_values <- as.numeric(C13_values)/as.numeric(C12_values)
      temp_divided_values <- cbind(temp_divided_values, divided_values)
    }
  }
}

C13C12_norm <- as.data.frame(temp_divided_values)
colnames(C13C12_norm) <- allMolecules
rownames(C13C12_norm) <- rownames(average_df)

View(C13C12_norm)

x <- as.data.frame(temp_divided_by_total)