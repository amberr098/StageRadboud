library(ggplot2)

getSelectedMatrix <- function(plot_data, table_type, molecules, samples){
  if(table_type == "av"){
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

setMultiplePlots <- function(selected_matrix, avind){
  
  if(avind == "av"){
    source("SinglePlots.R")
    p <- getPlotAvAbs(selected_matrix$average, selected_matrix$standDev)
    return(p)
  }
  else if(avind == "ind"){
    source("SinglePlots.R")
    p <- getPlotIndAbs(selected_matrix)
    return(p)
  }
}



showDataTable <- function(dataTable){
  asColumn <- rownames(dataTable)
  # Wordt geen warning weergeven door suppressWarnings(). 
  df <- suppressWarnings(data.frame(Samples = asColumn, dataTable))
  colnam <- colnames(dataTable)
  new_colnam <- gsub("\\.", " ", colnam)
  colnames(df) <- c("Samples", new_colnam)
  return(df)
}
  