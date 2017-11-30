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
      allComb_mat[count,3] <- as.numeric(average)
      allComb_mat[count,4] <- as.numeric(sd)
    }
  }
  allComb_DatF <- as.data.frame(allComb_mat, stringsAsFactors=FALSE)
  p <- setPlotAvAbs(allComb_DatF)
  return(p)
}

setPlotAvAbs <- function(allComb_DatF){
  library(scales)
  
  sd <- as.numeric(allComb_DatF$SD)
  half_sd <- as.numeric(as.character(sd))/2

  # De kolommen 3 en 4 numeric maken (Average en SD kolom)
  allComb_DatF[3] <- lapply(allComb_DatF[3], function(x) as.numeric(as.character(x)))
  allComb_DatF[4] <- lapply(allComb_DatF[4], function(x) as.numeric(as.character(x)))
  
  # De hoogte van de plots bepalen
  max_y <- max(allComb_DatF$Average + sd)*0.15 + max(allComb_DatF$Average + sd)
  min_y <- min(allComb_DatF$Average - sd)*0.15 + min(allComb_DatF$Average - sd)
  
  if(min_y > 0){
    min_y <- 0
  }
  
  p <- ggplot(allComb_DatF, aes(x = Samples, y = Average, fill = Samples, ymin = Average-half_sd, ymax = Average+half_sd))+
    geom_bar(stat = "identity", position = position_dodge()) +
    facet_wrap(~Molecules)+
    geom_errorbar() +
    scale_y_continuous(labels = comma, limits = c(min_y, max_y))
  
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
  p <- setPlotIndAbs(allComb_DF)
  
  return(p)
}

setPlotIndAbs <- function(allComb_DF){
  library(scales)
  print(allComb_DF)
  # De kolom 3 numeric maken (Value kolom)
  allComb_DF[3] <- lapply(allComb_DF[3], function(x) as.numeric(as.character(x)))
  
  # De maximum en minimum van de hoogte van de plot bepalen
  max_y <- max(allComb_DF$Values)*0.15 + max(allComb_DF$Values)
  min_y <- min(allComb_DF$Values)*0.15 + min(allComb_DF$Values)
  
  if(min_y > 0){
    min_y <- 0
  }

  p <- ggplot(allComb_DF, aes(x = Samples, y = Values, fill = Samples))+
    geom_bar(stat = "identity", position = position_dodge(),colour="black",size=0.1, width = 0.4) +
    facet_wrap(~Molecules)+
    scale_y_continuous(label = comma, limits = c(min_y, max_y))
  
  return(p)
}