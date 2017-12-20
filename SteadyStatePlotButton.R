# wordt aangeroepen wanneer er op de plot button geklikt wordt
activatedPlotButton <- function(input, output, session){
  # Exception als er geen moleculen of sampels zijn geselecteerd maar er wel op de plot button geklikt wordt
  if(!is.null(input$MolCheckBox) && !is.null(input$SamCheckBox)){
    updateTabsetPanel(session = session, inputId = "tabs", selected = "Results")
  }else if(is.null(input$MolCheckBox) && is.null(input$SamCheckBox)){
    showNotification("Select molecule(s) and sample(s)", type = "error")
  }else{
    if(is.null(input$MolCheckBox)){
      showNotification("Select molecule(s)", type = "error")
      
    }else if(is.null(input$SamCheckBox)){
      showNotification("Select sample(s)", type = "error")
    }
  }
  
  # Alleen de Resp. kolommen ophalen.
  source("EditFile.R")
  data_NoRT <- setColumnNames(data)
  
  # Alle data die hoort bij de gekozen opties door de gebruiker
  source("SteadyStateSpecificData.R")
  specific_data <- getData(input$abs_norm, input$av_ind, data_NoRT, input$ShowSingleMolecule)
  
  # De geselecteerde data ophalen op basis van de geselecteerde moleculen en samples door de gebruiker.
  source("SteadyStateSelectedData.R")
  selected_matrix <- setSelectedData(input, output, specific_data)
  
  # Ophalen en visualiseren van de plot
  source("SteadyStateGraph.R")
  setGraph(input, output, selected_matrix)
  

}