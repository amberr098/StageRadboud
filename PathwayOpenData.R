# File checken op de seperator en checken of het wel een .csv bestand is.
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
    # Openen van het bestand
    pathwayData <- openFile(datapath, sep)
    return(pathwayData)
  }else{
    return(NULL)
  }
  
  
}

# Bestand openen wanneer er gecontroleerd is dat het een .csv bestand is. 
openFile <- function(path, separator){
  # Op basis van de keuze van de gebruiker een .csv file openen.
  if(!is.null(separator)){
    data <- read.csv(path, sep = separator, check.names = FALSE)
  }else{
    # Er wordt niets geprint met invisible.
    invisible("geen sep")
  }
  
  return(data)
}