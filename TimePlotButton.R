setTimePlot <- function(average_df, standev_df, input, output, session){

  # Ophalen van het dataframe waarin alle geselecteerde waarden staan op basis van de keuze vande gebruiker.
  source("TimeSelectedDF.R")
  selected_dataframe <<- getSelectedDataframe(input$sampleChoices, input$moleculeChoices, average_df, standev_df)
  colnames(selected_dataframe) <- c("Sample", "Time", "Molecule", "Variant", "Average", "SD", "Half SD")
  
  # Creeert de plot van de geselecteerde data.
  source("TimePlot.R")
  p <- getPlot(selected_dataframe)
  
  # Ophalen van de gewenste dataframe zoals het moet komen te staan in het subtabblad Data. 
  source("TimeSelectedDF.R")
  showDataFrame <- getShowDataframe(input$sampleChoices, input$moleculeChoices, average_df, standev_df)
  
  # Het toevoegen van de geselecteerde data in het subtabblad Data. 
  output$dataTable <- renderDataTable({
    showDataFrame
  })
  
  # Maken van de plot wanneer er titels of subtitels zijn toegevoegd.
  source("TimeSetTitles.R")
  setTitles(input, output, session, p)
}