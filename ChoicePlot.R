# Op basis van de keuze van de gebruiker (steady state plot of time plot) de opties toevoegen aan
# het tabblad Settings. 
getTypeOfPlot <- function(input, output){
  # Keuze van de gebruiker achterhalen (keuze uit: steady state plot of time plot)
  observeEvent(input$SubmitTypeOfPlot,{
    plotType <- input$typeOfPlot
    
    # Als de keuze steady state is wordt er een browse menu en upload button toegevoegd.
    # aan het tabblad Settings worden de keuzes toegevoegd. 
    if(plotType == "Stady state"){
      setSteadyStateOptions(input, output)
    }else{
      setTimeOptions(output)
    }
  })
}

# Toevoegen van de steady state opties aan het tabblad Settings.
setSteadyStateOptions <- function(input, output){
  output$setSelectFile <- renderUI({
    fileInput('file1', 'Select your file')
  })
  
  output$setUploadButton <- renderUI({
    actionButton("upload", "Upload data")
  })
  
  # Keuze uit absolute waarden of genormaliseerde waarden.
  output$abs_norm_option <- renderUI({
    pickerInput(
      inputId = "abs_norm", 
      label = "Absolute/normalized counts",
      choices = list("Absolute counts" = "abs", "Normalized counts" = "norm"),
      selected = "abs",
      multiple = FALSE
    )
  })
  
  # Keuze uit gemiddelde waarden of individuele waarden. 
  output$av_ind_option <- renderUI({
    pickerInput(
      inputId = "av_ind",
      label = "Average count/individual samples",
      choices = list("Average counts" = "av", "Individual samples" = "ind"),
      multiple = FALSE
    )
  })
  
  # Optie om alle geselecteerde moleculen in aparte grafieken te weergeven.
  output$switchShowSeperate <- renderUI({
    materialSwitch(
      inputId = "ShowSingleMolecule",
      label = "Show molecules seperated",
      value = FALSE,
      right = TRUE,
      status = "succes")
  })
  
  # Button waardoor op basis van de opties de grafiek geplot wordt. 
  output$plot_Button <- renderUI({
    actionButton(inputId = "plot", 
                 label = "plot", 
                 icon = icon("bar-chart-o"),
                 width = 100
    )
  })
  
  # Als de moleculen in een aparte grafiek moeten worden geplot, is er een keuze om alle y-as schalen hetzelfde te houden
  # of om elke grafiek een aparte schaal te geven.
  observeEvent(input$ShowSingleMolecule, {
    if(input$ShowSingleMolecule == TRUE){
      output$set_y_axis_scale <- renderUI({
        pickerInput(inputId = "scale_y_axis",
                    label = "Choose scale y-axis",
                    choices = c("Same y-axis scales" = "fixed", "Different y-axis scales" = "free"),
                    multiple = FALSE)
      })
    }else{
      output$set_y_axis_scale <- renderUI({
        NULL
      })
    }
  })
}

# Toevoegen van de time opties aan het tabblad Settings. 
setTimeOptions <- function(output){
  # Wanneer er voor de time plot is gekozen, wordt de browse optie en upload button geplaatst. 
  output$setSelectFile <- renderUI({
    fileInput('fileTime', 'Select your file')
  })
  
  output$setUploadButton <- renderUI({
    actionButton("uploadTime", "Upload data")
  })
}