getAverageData <- function(Resp_dataframe){
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

  return(average_df)
}

getStandevData <- function(Resp_dataframe){
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
  
  # Get SD
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
  
  temp_standev_df <- list()
  rown <- c()
  coln <- colnames(Resp_matrix)[firstCol:ncol(Resp_matrix)]
  
  for(name in unique_names){
    # Alleen de samples pakken met een tijd.
    if(grepl(pattern_time, name) == TRUE){
      rown <- c(rown,name)
      same_samples_index <- grep(name, Resp_matrix[,col_Name])
      standev_row <- c()
      for(col in firstCol:ncol(Resp_matrix)){
        values <- c()
        for(row in same_samples_index){
          values <- c(values,as.numeric(Resp_matrix[row,col]))
        }
        # Alle gemiddelden van de rij in een vector zetten
        standev_row <- c(standev_row, sd(values))
      }
      temp_standev_df <- rbind(temp_standev_df, standev_row)
    }
  }
  # Dataframe maken van de standaard deviaties 
  standev_df <- as.data.frame(temp_standev_df)
  colnames(standev_df) <- coln
  rownames(standev_df) <- rown
  return(standev_df)
}