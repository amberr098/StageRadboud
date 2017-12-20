# Ophalen van de data aan de hand van de gekozen opties van de gebruiker. 
getData <- function(abs_norm, av_ind, data_NoRT, sinMol){

  if(abs_norm == "abs"){
    # Als er is gekozen voor Absolute + Average
    if(av_ind == "av"){
      source("SteadyStateSameSamples.R")
      average_stanDev <- getSameSamples(data_NoRT)
      return(average_stanDev)
    }else{
      # Als er is gekozen voor Absolute + Individual
      source("EditFile.R")
      data_totalResponse <- getTotalResponses(data_NoRT)
      return(data_totalResponse)
    }
  }

  if(abs_norm == "norm"){
    source("EditFile.R")
    data_totalResponse <- getTotalResponses(data_NoRT)
    
    source("SteadyStateNormalisation.R")
    norm_Responses <- Normalisation_Resp(data_totalResponse)

    # Als er is gekozen voor Normalized + Average
    if(av_ind == "av"){
      source("SteadyStateSameSamples.R")
      average_stanDev_norm <- getSameSamples(as.data.frame(norm_Responses))

      return(average_stanDev_norm)
    }else{
      # Als er is gekozen voor Normalized + Individual
      return(norm_Responses)

    }
  }
}
