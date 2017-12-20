# Aan de hand van de keuzes van de gebruiker is de specifieke data opgehaald waaruit de gegevens worden
# gehaald die door de gebruiker gewenst zijn, dus de keuze voor de moleculen en samples worden opgehaald
# met de bijbehorende waarden.
setSelectedData <- function(input, output, specific_data){
  # Ophalen en weergeven van de geselecteerde waardes.
  if(!is.null(input$MolCheckBox) && !is.null(input$SamCheckBox)){
    # Hangt van de switch af hoe het het geselecteerde dataframe eruit moet komen te zien.
    if(input$ShowSingleMolecule == FALSE){
      source("Visualization.R")
      selected_matrix <<- getSelectedMatrix(specific_data, input$av_ind, input$MolCheckBox,input$SamCheckBox)
      
      if(input$av_ind == "ind"){
        temp_selectmatrix <- showDataTable(selected_matrix)
        
        # Visualisatie waardes in datatabel weergeven als 10.000,5 ipv 10000.5
        selectmatrix <- format.data.frame(temp_selectmatrix, big.mark = ".", decimal.mark = ",")
        output$dataTable <- renderDataTable({
          selectmatrix
        })
      }else{
        output$dataTable <- renderDataTable({
          # Visualisatie waardes in datatabel weergeven als 10.000,5 ipv 10000.5
          format_selected_matrix <- format.data.frame(selected_matrix, big.mark = ".", decimal.mark = ",", scientific = TRUE)
          format_selected_matrix
        })
      }
    }else{
      
      # Controleren of het bestand waardes in de kolom Type heeft.
      checkType <- (which(data == "Type", arr.ind = TRUE))
      if(data[2,checkType[1,2]] == "Sample"){
        type <- FALSE
      }else{
        type <- TRUE
      }
      
      # Ophalen van de geselecteerde data.
      source("SteadyStateSwitchTrue.R")
      selected_matrix <<- setSelectedMatrix(specific_data, input$MolCheckBox, input$SamCheckBox, input$abs_norm, input$av_ind, type)
      if(input$av_ind == "av"){
        # Format voor de te visualiseren datatabel maken.
        source("SinglePlots.R")
        showData <- getAverageData(selected_matrix)
        output$dataTable <- renderDataTable({
          showData
        })
      }else{
        output$dataTable <- renderDataTable({
          selected_matrix
        })
      }
    }
  }
  return(selected_matrix)
}