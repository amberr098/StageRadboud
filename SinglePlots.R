# Deze functies worden aangeroepen wanneer de switch voor het laten zien van de aparte moleculen
# aan staat.

# Deze functie wordt geactiveerd wanneer de average optie wordt gekozen ipv individual
# De gewenste structuur van de dataframe voor de plot wordt hier gemaakt.
getPlotAverage <- function(av_matrix, sd_matrix, yscl){
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
  p <- setPlotAverage(allComb_DatF, yscl)
  return(p)
}

# Wordt aangeroepen in de getPlotAverage functie en plot de data.
setPlotAverage <- function(allComb_DatF, yscl){
  library(scales)
  allComb_DatF <- allComb_DatF[order(allComb_DatF$Molecules),]
  # Toevoegen van de helft van de SD aan de dataframe.
  allComb_DatF["Half"] <- as.numeric(as.character(allComb_DatF$SD))/2

  # De kolommen 3 en 4 numeric maken (Average en SD kolom)
  allComb_DatF[3] <- lapply(allComb_DatF[3], function(x) as.numeric(as.character(x)))
  allComb_DatF[4] <- lapply(allComb_DatF[4], function(x) as.numeric(as.character(x)))

  p <- ggplot(allComb_DatF, aes(x = Samples, y = Average, fill = Samples, ymin = Average-Half, ymax = Average+Half))+
    geom_bar(stat = "identity", position = position_dodge()) +
    facet_wrap(~Molecules, scales = yscl)+
    geom_errorbar(width = 0.1) +
    scale_y_continuous(labels = comma)+
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
}

# Deze functie wordt geactiveerd wanneer de individual optie wordt gekozen ipv average
# De gewenste structuur van de dataframe voor de plot wordt hier gemaakt.
getPlotIndividual <- function(selected_matrix, yscl){
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
  allComb_DF <- as.data.frame(allComb_m,stringsAsFactors=FALSE)
  p <- setPlotIndividual(allComb_DF, yscl)
  return(p)
}

# Wordt aangeroepen in getPlotIndividual en plot de data.
setPlotIndividual <- function(allComb_DF, yscl){
  library(scales)
  allComb_DF <- allComb_DF[order(allComb_DF$Molecules),]
  # De kolom 3 numeric maken (Value kolom)
  allComb_DF[3] <- lapply(allComb_DF[3], function(x) as.numeric(as.character(x)))

  # De maximum en minimum van de hoogte van de plot bepalen
  max_y <- max(allComb_DF$Values)*0.15 + max(allComb_DF$Values)
  min_y <- min(allComb_DF$Values)*0.15 + min(allComb_DF$Values)

  if(isTRUE(min_y > 0)){
    min_y <- 0
  }
  
  p <- ggplot(allComb_DF, aes(x = Samples, y = Values, fill = Samples, group = factor(Values)))+
    geom_bar(position = position_dodge(), stat = "identity", colour="white", size = 0.1, width = 0.5) +
    facet_wrap(~Molecules, scales = yscl) +
    scale_y_continuous(labels = comma) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  return(p)
}

# Het maken van de data tabel die wordt gebruikt voor het laten zien van de geselecteerde
# data in de webinterface. 
getAverageData <- function(selected_matrix){
  av_mat <- selected_matrix$average
  sd_mat <- selected_matrix$standDev
  
  showData_matrix <- matrix(NA, ncol=4, nrow=(nrow(av_mat)*ncol(av_mat)))
  colnames(showData_matrix) <- c("Samples", "Molecules", "Average", "SD")
  countRows <- 0
  
  for(row in 1:nrow(av_mat)){
    sample <- rownames(av_mat)[row]
    for(col in 1:ncol(av_mat)){
      countRows <- countRows + 1
      molecule <- colnames(av_mat)[col]
      average <- av_mat[row,col]
      standev <- sd_mat[row,col]
      
      showData_matrix[countRows, 1] <- sample
      showData_matrix[countRows, 2] <- molecule
      showData_matrix[countRows, 3] <- average
      showData_matrix[countRows, 4] <- standev
    }
  }
  
  return(showData_matrix)
}