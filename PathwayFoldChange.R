# Een dataframe gegenereerd waarin de twee gekozen condities door elkaar gedeeld worden. kolomnamen zijn de compounds,
# rijnaam is "Ratio"
getRatio <- function(CondMatrix){
  preRatioDataframe <- list()
  
  # Dit moet waar zijn, want er zijn maar twee condities die met elkaar vergeleken worden
  if(nrow(CondMatrix) == 2){
    # Ophalen van de rijen met de waarden van de condities
    cond1 <- as.numeric(unlist(CondMatrix[1,]))
    cond2 <- as.numeric(unlist(CondMatrix[2,]))
    
    # Ratio bereken van de twee condities
    ratio <- cond1/cond2
    preRatioDataframe <- rbind(ratio)
  }
  
  ratioDataframe <- as.data.frame(preRatioDataframe)
  colnames(ratioDataframe) <- colnames(CondMatrix)
  rownames(ratioDataframe) <- "Ratio"

  getLog2(ratioDataframe, ratio)
}

# De log2 wordt berekent van de ratios van de twee gekozen condities. 
getLog2 <- function(ratioDataframe, ratio){
  preLog2Dataframe <- list()
  
  # Er is maar 1 rij met de ratio erin. 
  if(nrow(ratioDataframe) == 1){
    preLog2Dataframe <- rbind(log2(ratio))
  }
  
  log2Dataframe <<- as.data.frame(preLog2Dataframe)
  colnames(log2Dataframe) <<- colnames(ratioDataframe)
  rownames(log2Dataframe) <<- "log2"
  
  View(log2Dataframe)
}

# C13 kolommen delen door C12 kolommen
getRatioTime <- function(dataConditionNumeric){
  pattern <- "13C.{1,2}-"
  coln <- c()
  preRatios <- list()
  
  # Ophalen van alle 13C kolommen
  all13C <- grep(pattern, colnames(dataConditionNumeric))

  for(index_C13 in all13C){
    C13 <- colnames(dataConditionNumeric)[index_C13]
    compound <- gsub(pattern, "", C13)
    index_C12 <- which(colnames(dataConditionNumeric) == compound)
    
    # De values op halen van de kolommen die gedeeld door elkaar moeten worden
    valuesC13 <- dataConditionNumeric[,index_C13]
    valuesC12 <- dataConditionNumeric[,index_C12]
    
    C12 <- gsub(" Results", "", colnames(dataConditionNumeric)[index_C12])
    C12 <- gsub("-", "_", C12)
    
    C13divC12 <- valuesC13/valuesC12

    preRatios <- cbind(preRatios, C13divC12)
    coln <- c(coln, C12)
  }
  
  ratios <- matrix(as.numeric(unlist(preRatios)),nrow=nrow(preRatios))
  rownames(ratios) <- rownames(dataConditionNumeric)
  colnames(ratios) <- coln
  
  return(ratios)
}

# Aan de hand van de tijden de fold changes van de twee condities berekenen. 
getFoldChangeTime <- function(average_ratios){
  rown <- c()
  new_col <- c()
  
  # Sample en time apart nemen zodat dezelfde tijden door elkaar gedeeld kunnen worden
  for(name in rownames(average_ratios)){
    sample_time <- unlist(strsplit(name, "_"))
    sample <- sample_time[1]
    time <- sample_time[2]
    
    new_col <- c(new_col, time)
    rown <- c(rown, sample)
  }
  
  # Kolom met de tijd toevoegen en de dataframe omzetten naar matrix
  average_ratios$Time <- new_col
  average_ratios_matrix <- matrix(as.character(unlist(average_ratios)),nrow=nrow(average_ratios))
  colnames(average_ratios_matrix) <- colnames(average_ratios)
  rownames(average_ratios_matrix) <- rown

  # De data met dezelfde tijden door elkaar delen.
  index_col_Time <- which(colnames(average_ratios_matrix) == "Time")
  unique_times <- unique(average_ratios_matrix[,index_col_Time])
  
  preFoldChanges <- list()
  for(time in unique_times){
    # Index time zijn er altijd twee want er worden twee condities met elkaar vergeleken
    indexes_time <- which(average_ratios_matrix[,index_col_Time] == time, arr.ind = T)
    
    index_row1 <- indexes_time[1]
    row1 <- average_ratios_matrix[index_row1,]
    
    index_row2 <- indexes_time[2]
    row2 <- average_ratios_matrix[index_row2,]
    
    # Verwijderen van de values waarin de tijd staat (staat achteraan dus de lengte geeft de laatste index aan die verwijderd moet worden)
    values1 <- as.numeric(row1[-length(row1)])
    values2 <- as.numeric(row2[-length(row2)])
    
    FoldChange <- values1/values2
    preFoldChanges <- rbind(preFoldChanges, FoldChange)
  }
  
  FoldChanges <- as.data.frame(preFoldChanges)
  rownames(FoldChanges) <- unique_times
  colnames(FoldChanges) <- colnames(average_ratios[-length(average_ratios)])
  
  # Log2 van de foldchanges berekenen
  log2 <- getLog2Time(FoldChanges)
}

# Log2 berekenen van de dataset met Time waarden.
getLog2Time <- function(FoldChanges){
  preLog2 <- list()

  for(index_row in 1:nrow(FoldChanges)){
    values <- log2(unlist(FoldChanges[index_row,]))
    preLog2 <- rbind(preLog2, values)
  }
  
  log2 <- as.matrix(preLog2)
  colnames(log2) <- colnames(FoldChanges)
  rownames(log2) <- rownames(FoldChanges)
  
  return(log2)
}