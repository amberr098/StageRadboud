library(ggplot2)

# Ophalen van de geselecteerde data 
getSelectedMatrix <- function(plot_data, table_type, molecules, samples){
  if(table_type == "av"){
    # Zowel het gemiddelde als de standaard deviatie worden toegevoegd aan het dataframe.
    source("AverageOption.R")
    both_df <- getMatrix(plot_data, molecules, samples)
    return(both_df)
  }else{
    source("IndividualOption.R")
    matrix <- individualSamples(plot_data, molecules, samples)
    return(matrix)
  }
}

setOnePlot <- function(plotMatrix, table_type){
  # Plot ophalen wanneer de switch uit staat. 
  if(table_type == "av"){
    source("AverageOption.R")
    p <- plotBar(plotMatrix)
    return(p)
  }else{
    source("IndividualOption.R")
    p <- plotGraph(plotMatrix)
    return(p)
  }
}

setMultiplePlots <- function(selected_matrix, avind, yscl){
  # Plot ophalen wanneer de switch aan staat. 
  if(avind == "av"){
    source("SinglePlots.R")
    p <- getPlotAverage(selected_matrix$average, selected_matrix$standDev, yscl)
    return(p)
  }
  else if(avind == "ind"){
    source("SinglePlots.R")
    p <- getPlotIndividual(selected_matrix, yscl)
    return(p)
  }
}

# Ophalen van de data zoals het moet worden weergeven in de datatabel
showDataTable <- function(dataTable){
  asColumn <- rownames(dataTable)
  # Wordt geen warning weergeven door suppressWarnings(). 
  df <- suppressWarnings(data.frame(Samples = asColumn, dataTable))
  colnam <- colnames(dataTable)
  new_colnam <- gsub("\\.", " ", colnam)
  colnames(df) <- c("Samples", new_colnam)
  return(df)
}
  