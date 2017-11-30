getPlotAvAbs <- function(av_matrix, sd_matrix){
  allComb_mat <- matrix(NA, ncol = 4, nrow = ncol(av_matrix)*nrow(av_matrix))
  colnames(allComb_mat) <- c("Molecules", "Samples", "Average", "SD")
  count <- 0
  for(col in 1:ncol(av_matrix)){
    molecule <- colnames(av_matrix)[col]
    for(row in 1:nrow(av_matrix)){
      count <- count + 1 
      
      sample <- rownames(av_matrix)[row]
      average <- av_matrix[row,col]
      sd <- sd_matrix[row,col]
      
      # Alle mogelijke combinaties van molecule en sample in een matrix.
      allComb_mat[count,1] <- molecule
      allComb_mat[count,2] <- sample
      allComb_mat[count,3] <- average
      allComb_mat[count,4] <- sd
    }
  }
  allComb_DatF <- as.data.frame(allComb_mat, stringsAsFactors=FALSE)
  p <- setPlotAvAbs(allComb_DatF)
  return(p)
}

setPlotAvAbs <- function(allComb_DatF){
  std <- as.numeric(as.character(allComb_DatF$SD))/2
  
  # bepalen van de standaard error.
  y_minStd <- as.numeric(as.character(allComb_DatF$Average))-std
  y_maxStd <- as.numeric(as.character(allComb_DatF$Average))+std
  
  # Bepalen hoever de hoogste en laagste bar van de rand afzitten.
  y_minBar <- min(y_minStd)*0.15 + min(y_minStd)
  y_maxBar <- max(y_maxStd)*0.15 + max(y_maxStd)
  
  if(y_minBar > 0){
    y_minBar <- 0
  }
  
  library(scales)
  p <- ggplot(allComb_DatF, aes(x = Samples, y = Average, fill = Samples))+
    geom_bar(position = position_dodge(), stat = "identity") +
    facet_wrap(~Molecules)+
    geom_errorbar(aes(ymin = y_minStd, ymax = y_maxStd), width=.2, position=position_dodge(.9))
  
  return(p)
}

getPlotIndAbs <- function(selected_matrix){
  allComb_m <- matrix(NA, ncol = 3, nrow = ncol(selected_matrix)*nrow(selected_matrix))
  colnames(allComb_m) <- c("Molecules", "Samples", "Values")
  
  count <-  0
  for(col in 1:ncol(selected_matrix)){
    molecule <- colnames(selected_matrix)[col]
    for(row in 1:nrow(selected_matrix)){
      count <- count + 1
      sample <- rownames(selected_matrix)[row]
      value <- selected_matrix[row,col]
      
      allComb_m[count,1] <- molecule
      allComb_m[count,2] <- sample
      allComb_m[count,3] <- value
    }
  }
  allComb_DF <- as.data.frame(allComb_m)
  return(allComb_DF)
}