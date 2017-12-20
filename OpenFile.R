openSteadyStateFile <- function(path, separator){

  # Op basis van de keuze van de gebruiker een .csv file openen.
  if(!is.null(separator)){
    data <- read.csv(path, sep = separator, check.names = FALSE)
  }else{
    # Er wordt niets geprint met invisible.
    invisible("geen sep")
  }
  
  return(data)
}

openTimeFile <- function(path, separator){
  # Ophalen van het bestand.
  dataTime <- read.csv(path, sep = separator, check.names = FALSE)
  
  # Ophalen van een dataframe met alleen de Resp kolommen.
  source("EditFile.R")
  Resp_dataframe <- getRespColumns(dataTime)

  return(Resp_dataframe)
}