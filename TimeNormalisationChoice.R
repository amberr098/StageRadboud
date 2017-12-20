choiceNormalisation <- function(input, output){
  # Als de gebruiker kiest voor de genormaliseerde waarden, wordt er een switch zichtbaar waarin 
  # de gebruiker kan bepalen wat voor een normalisatie er uitgevoerd moet worden. 
  if(input$abs_norm_time == "norm"){
    output$switchNormalisation <- renderUI({
      materialSwitch(inputId = "switch_norm",
                     label = "13C divided by total",
                     status = "success")
    })
    
    # Ophalen van de moleculen waaruit de gebruiker een keuze heeft bij de optie Normalisation
    # Hierin staan niet de 12C moleculen omdat deze genormaliseerd zijn.
    source("TimeOptions.R")
    molecule_list <- getMolecules_Norm(Resp_dataframe)
    
    # Toevoegen van de optie om een molecuul te kiezen voor de optie norm: alleen 13C moleculen
    output$select_molecules_option <- renderUI({
      pickerInput(inputId = "moleculeChoices",
                  label = "Select molecule(s)",
                  choices = c(molecule_list), 
                  options = list('actions-box' = TRUE),
                  multiple = TRUE)
    })
    
  }else{
    # Als de gebruiker kiest voor de optie absolute waarden dan wordt er geen switch zichtbaar.  
    output$switchNormalisation <- renderUI({
      NULL
    })
    
    # De molecuul lijst bevat nu wel 12C moleculen. 
    source("TimeOptions.R")
    molecule_list <- getMolecules(Resp_dataframe)
    
    # Toevoegen van de optie om een molecuul te kiezen voor de optie abs: zowel 13C als 12C moleculen
    output$select_molecules_option <- renderUI({
      pickerInput(inputId = "moleculeChoices",
                  label = "Select molecule(s)",
                  choices = c(molecule_list), 
                  options = list('actions-box' = TRUE),
                  multiple = TRUE)
    })
    
    # Alle gemiddelde voor een sample krijgen voor de optie Absolute
    source("TimeAverageMatrix.R")
    average_df <<- getAverageData(Resp_dataframe)
    standev_df <<- getStandevData(Resp_dataframe)
  }
}