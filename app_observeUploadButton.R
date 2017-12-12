# Checken van de ingevoerde file of het een .csv bestand is en het toevoegen van de opties
# aan de Settings tabblad. 

# Word aangeroepen wanneer er een stady state plot gemaakt moet worden en de data
# ge-upload wordt. Er wordt dus op de upload button gedruk.
checkFile <- function(datapath, file, session, output){
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
    data <<- openStStFile(datapath, sep)
    
    source("SettingsPlot.R")
    molecule_list <- getMolecules(data)
    sample_list<- getSamples(data)

    output$select_samples_option <- renderUI({
      pickerInput(
        inputId = "SamCheckBox", 
        label = "Select sample(s)",
        choices = c(sample_list), options = list('actions-box' = TRUE), 
        multiple = TRUE
      )
    })

    output$select_molecules_option <- renderUI({
      pickerInput(
        inputId = "MolCheckBox",
        label = "Select molecule(s)",
        choices = c(molecule_list), options = list('actions-box' = TRUE),
        multiple = TRUE
      )
    })
    
    # Zorgt dat de Settings tab ge-update word 
    updateTabsetPanel(session = session, inputId = "tabs", selected = "Settings")
    return(data)
  }else{
    # Exceptie wanneer er geen .csv bestand is ingevoerd
    showNotification("No csv file.", type = "error")
  }
}

setMoleculesSamples <- function(Resp_matrix, output){
  # Molecuul lijst ophalen.
  pattern_molecule <- "13C.*-"
  # Start index voor de molecuulnamen
  first_molecule_index <- grep(pattern_molecule, colnames(Resp_matrix))[1]
  
  # Eind index voor de molecuulnamen
  length_colnames <- length(colnames(Resp_matrix)) 
  all_molecules <- colnames(Resp_matrix)[first_molecule_index:length_colnames]
  
  # Toevoegen van de keuze voor molecuul in de webinterface
  output$select_molecules_option <- renderUI({
    pickerInput(
      inputId = "MolCheckBox_time",
      label = "Select molecule(s)",
      choices = c(all_molecules), options = list('actions-box' = TRUE),
      multiple = TRUE
    )
  })
  
  # Sample lijst ophalen.
  col_mat_name <- which(Resp_matrix == "Name", arr.ind = TRUE)
  col_ind_name <- col_mat_name[1,2]
}

# Wordt aangeroepen wanneer er gekozen wordt voor een time plot
checkFileTime <- function(datapath, file, session, output){
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
    Resp_dataframe <- openTimeFile(datapath, sep)
    return(Resp_dataframe)
  }
  
}