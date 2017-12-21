# Checken van de ingevoerde file of het een .csv bestand is en het toevoegen van de opties
# aan de Settings tabblad. 

# Word aangeroepen wanneer er een stady state plot gemaakt moet worden en de data
# ge-upload wordt. Er wordt dus op de upload button gedruk.
checkFileSteadyState <- function(datapath, file, session, output){
  # Wanneer er op de upload button wordt geklikt, maar er geen file in ingevoerd. 
  if(is.null(datapath)){
    showNotification("Select a file.", type = "error")
  }
  
  req(file)
  pattern <- "*\\.csv"
  
  # Controleren of het ingeladen bestand een .csv bestand is
  if(grepl(pattern, datapath) == TRUE){
    csvFile <- TRUE
    first_line <- readLines(datapath, n=1)
    
    # Checken welke seperator het bestand heeft
    if(grepl(";", first_line)){
      sep <- ";"
    }else if(grepl(",", first_line)){
      sep <- ","
    }else if(grepl("\t", first_line)){
      sep <- "\t"
    }else{
      invisble("X")
    }
  }else{
    csvFile <- FALSE
  }
  
  
  if(csvFile == TRUE){
    source("OpenFile.R")
    # Globale variabelen van data maken
    data <<- openSteadyStateFile(datapath, sep)
    
    # Ophalen van alle moleculen en alle samples waaruit de gebruiker kan kiezen.
    source("SteadyStateMoleculesSamples.R")
    molecule_list <- getMolecules(data)
    sample_list<- getSamples(data)
    
    # Toevoegen van alle samples waaruit de gebruiker kan kiezen
    output$select_samples_option <- renderUI({
      pickerInput(
        inputId = "SamCheckBox", 
        label = "Select sample(s)",
        choices = c(sample_list), options = list('actions-box' = TRUE), 
        multiple = TRUE
      )
    })
    
    # Toevoegen van alle moleculen waaruit de gebruiker kan kiezen
    output$select_molecules_option <- renderUI({
      pickerInput(
        inputId = "MolCheckBox",
        label = "Select molecule(s)",
        choices = c(molecule_list), options = list('actions-box' = TRUE),
        multiple = TRUE
      )
    })
    
    # Toevoegen van de optie om een zwart/witte grafiek te krijgen. Wordt toegevoegd aan de resultaten tab
    output$black_white_option <- renderUI({
      checkboxInput(inputId = "black_white", label = "Black/White")
    })
    
    # Zorgt dat de Settings tab ge-update word 
    updateTabsetPanel(session = session, inputId = "tabs", selected = "Settings")
    return(data)
  }else{
    # Exceptie wanneer er geen .csv bestand is ingevoerd
    showNotification("No csv file.", type = "error")
  }
}

# Wordt aangeroepen wanneer er gekozen wordt voor een time plot
checkFileTime <- function(datapath, file, session, output){
  # Wanneer er op de upload button wordt geklikt, maar er geen file in ingevoerd. 
  if(is.null(datapath)){
    showNotification("Select a file.", type = "error")
  }
  
  req(file)
  pattern <- "*\\.csv"
  
  # Controleren of het ingeladen bestand een .csv bestand is
  if(grepl(pattern, datapath) == TRUE){
    csvFile <- TRUE
    first_line <- readLines(datapath, n=1)
    
    # Checken welke seperator het bestand heeft
    if(grepl(";", first_line)){
      sep <- ";"
    }else if(grepl(",", first_line)){
      sep <- ","
    }else if(grepl("\t", first_line)){
      sep <- "\t"
    }else{
      invisble("X")
    }
  }else{
    csvFile <- FALSE
  }
  
  # Wanneer er gevalideerd is dat er een .csv bestand is ingevoerd, dan wordt het bestand geopend 
  if(csvFile == TRUE){
    source("OpenFile.R")
    Resp_dataframe <- openTimeFile(datapath, sep)
  }else{
    # Exceptie wanneer er geen .csv bestand is ingevoerd
    showNotification("No csv file.", type = "error")
    Resp_dataframe <- NULL
  }
  
  # Als Resp_dataframe gelijk is aan NULL, dan is er geen .csv bestand ingeladen. 
  if(!is.null(Resp_dataframe)){
    Resp_dataframe <- setOptionsTime(output, session, Resp_dataframe)
    return(Resp_dataframe)
  }else{
    invisible()
  }
}

setOptionsTime <- function(output, session, Resp_dataframe){

  # Toevoegen van de opties voor de gebruiker: absolute waarden/genormaliseerde waarden.
  output$abs_norm_option <- renderUI({
    pickerInput(
      inputId = "abs_norm_time", 
      label = "Absolute/normalized counts",
      choices = list("Absolute counts" = "abs", "Normalized counts" = "norm"),
      selected = "abs",
      multiple = FALSE
    )
  })
  source("TimeOptions.R")
  sample_list <- getSamples(Resp_dataframe)
  
  # Toevoegen van de optie om een sample te kiezen
  output$select_samples_option <- renderUI({
    pickerInput(inputId = "sampleChoices",
                label = "Select sample(s)",
                choices = c(sample_list), 
                options = list('actions-box' = TRUE),
                multiple = TRUE)
  })
  
  # Toevoegen van de plot button
  output$plot_Button <- renderUI({
    actionButton(inputId = "plotButton",
                 label = "Plot")
  })
  
  # Naar de tab Settings gaan wanneer er op de uploadbutton is geklikt
  updateTabsetPanel(session = session, inputId = "tabs", selected = "Settings")
  
  return(Resp_dataframe)
}