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

getLog2 <- function(ratioDataframe, ratio){
  preLog2Dataframe <- list()
  
  # Er is maar 1 rij met de ratio erin. 
  if(nrow(ratioDataframe) == 1){
    preLog2Dataframe <- rbind(log2(ratio))
  }
  
  log2Dataframe <<- as.data.frame(preLog2Dataframe)
  colnames(log2Dataframe) <<- colnames(ratioDataframe)
  rownames(log2Dataframe) <<- "log2"
}