timeMain <- function(input, output, session){
  observeEvent(input$uploadTime,{
    source("ActivatedUploadButton.R")
    Resp_dataframe <<- checkFileTime(input$fileTime$datapath, input$fileTime, session, output)
  })
  

  # Als er gekozen wordt voor de optie genormaliseerde waarden, dan moet er bepaald worden
  # hoe er genormaliseerd wordt: C13/C12 of C13/totaal C13 C12
  observeEvent(input$abs_norm_time, {
    source("TimeNormalisationChoice.R")
    choiceNormalisation(input, output)
  })
  
  # Ophalen van de data wanneer er voor genormaliseerde waardes is gekozen. 
  observeEvent(input$switch_norm, {
    source("TimeNormalisation.R")
    # Als de input TRUE geeft, is de normalisatie 13C/totaal. Anders is het 13C/12C
    if(input$switch_norm == FALSE){
      norm_matrix <- C13_dividedBy_C12(Resp_dataframe)
      average_df <<- getAverageC13C12(norm_matrix)
      standev_df <<- getStanDevC13C12(norm_matrix)
      
    }else{
      norm_values_matrix <- C13_dividedBy_total(Resp_dataframe)
      average_df <<- getAverageC13Total(norm_values_matrix)
      standev_df <<- getStanDevC13Total(norm_values_matrix)
    }
  })
  
  # Wanneer er op de Plot button wordt geklikt word de plot gegenereert en in de webinterface geplaatst
  observeEvent(input$plotButton, {
    # Exception als er geen moleculen of sampels zijn geselecteerd maar er wel op de plot button geklikt wordt
    if(!is.null(input$moleculeChoices) && !is.null(input$sampleChoices)){
      updateTabsetPanel(session = session, inputId = "tabs", selected = "Results")
      error <- FALSE
    }else if(is.null(input$moleculeChoices) && is.null(input$sampleChoices)){
      showNotification("Select molecule(s) and sample(s)", type = "error")
      error <- TRUE
    }else{
      if(is.null(input$moleculeChoices)){
        showNotification("Select molecule(s)", type = "error")
        error <- TRUE
      }else if(is.null(input$sampleChoices)){
        showNotification("Select sample(s)", type = "error")
        error <- TRUE
      }
    }
    
    # Als er geen error is, kan de plot gemaakt worden
    if(error == FALSE){
      source("TimePlotButton.R")
      setTimePlot(average_df, standev_df, input, output, session)
    }else{
      invisible()
    }
    
  })
}