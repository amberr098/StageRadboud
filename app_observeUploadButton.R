# Checken van de ingevoerde file of het een .csv bestand is en het toevoegen van de opties
# aan de Settings tabblad. 

checkFile <- function(datapath, file, session){
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
    data <<- open(datapath, sep)
    
    source("SettingsPlot.R")
    molecule_list <- getMolecules(data)
    sample_list<- getSamples(data)
    
    # Het updaten van de keuzes aan de hand van het ingevoerde bestand
    updatePickerInput(session, "MolCheckBox", 
                      label = "Select molecule(s)", 
                      choices = c(molecule_list))
    
    updatePickerInput(session, "SamCheckBox", 
                      label = "Select sample(s)", 
                      choices = c(sample_list))
    
    updateSelectInput(session, "abs_norm",
                      label = "Absolute/normalized counts",
                      choices = list("Absolute counts" = "abs", "Normalized counts" = "norm"),
                      selected = "abs")
    
    # Zorgt dat de Settings tab ge-update word 
    updateTabsetPanel(session = session, inputId = "tabs", selected = "Settings")
    return(data)
  }else{
    # Exceptie wanneer er geen .csv bestand is ingevoerd
    showNotification("No csv file.", type = "error")
  }
}
