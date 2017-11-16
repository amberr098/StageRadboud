open <- function(path, separator){

  # Op basis van de keuze van de gebruiker een .csv file openen.
  if(!is.null(separator)){
    data <- read.csv(path, sep = separator, check.names = FALSE)
  }else{
    # Er wordt niets geprint met invisible.
    invisible("geen sep")
  }
  
  return(data)
}