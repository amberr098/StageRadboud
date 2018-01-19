# Vanuit de main worden alle functies aangeroepen die een rol spelen bij de visualisatie van
# de steady state pathway.
main <- function(datapath, file, session, output, input){
  source("PathwayOpenData.R")
  pathwayData <- checkFileSteadyState(datapath, file, session, output)
  
  if(is.null(pathwayData)){
    invisible("geen csv bestand")
  }else{
    checkedFile(pathwayData, datapath, file, session, output, input)
  }
  
}

# Deze functie roep alle andere functies aan wanneer er gecheckt is dat er een csv bestand is ingevoerd.
checkedFile <- function(pathwayData, datapath, file, session, output, input){
  # Alleen de Resp. kolommen ophalen en de totale response van een sample berekenen.
  source("EditFile.R")
  dataNoRT <- setColumnNames(pathwayData)
  data_totalResponse <- getTotalResponses(dataNoRT)
  
  # Normaliseren van de Resp. data
  source("SteadyStateNormalisation.R")
  norm_Responses <<- Normalisation_Resp(data_totalResponse)
  
  # Verkijgen van de juiste vorm van matrix
  source("PathwayChangeFormat.R")
  formatData <- changeFormat(norm_Responses)
  
  # Condities plaatsen en fold change berekenen
  source("PathwayConditions.R")
  setConditions(formatData, input, output, session)
  
  # Checken of de benodigde compounds al een keer eerder zijn opgeslagen of niet
  source("PathwayIdentifiers.R")
  unsaved_compounds <<- searchIdentifiers(colnames(formatData))
  
  # Wanneer alle benodigde compounds al een keer opgeslagen zijn, wordt er een tekst weergeven
  if(is.null(unsaved_compounds)){
    output$txt_no_ids <- renderUI({
      h5("All compounds already saved")
    })
    
    output$save_compounds_actionbutton <- renderUI({
      actionButton(inputId = "save_compounds", label = "GO")
    })
  }else{
    output$save_compounds_actionbutton <- renderUI({
      actionButton(inputId = "save_compounds", label = "Save")
    })
  }
  
  
  # Opslaan en verwijderen van de compounds, compound namen veranderen in de IDs, maken van de KEGG pathways
  observeEvent(input$save_compounds, {
    count_save_button <<- count_save_button + 1
    
    # Toevoegen van de compounds die worden opgeslagen door de gebruiker aan het tabblad met alle opgeslagen compounds
    for(comp in unsaved_compounds){
      id <- input[[comp]]
      
      if(!id == ""){
        source("PathwaySavedCompounds.R")
        addingCompounds(comp, id)
      }
    }
    
    # Opslaan en verwijderen van compounds.
    source("PathwayShowSavedCompounds.R")
    allCompound_ID <<- setAllCompounds(count_save_button, session)
    
    # Plaatsen van de remove button
    output$remove_compounds_actionbutton <- renderUI({
      actionButton(inputId = "remove_selected_compounds", label = "Remove")
    })
    
    # Plaatsen van de zoekbalk
    output$search_compound_option <- renderUI({
      searchInput(
        inputId = "search_query_user",
        label = NULL,
        placeholder = "Enter a compound",
        btnSearch = icon("search"),
        btnReset = icon("remove"),
        width = "30%"
      )
    })
    
    # Veranderen van de compound namen met de IDs
    source("PathwayKeggTable.R")
    tableIDs<-placeIDs()
    
    # Plaatsen van de kegg Map met de foldchange
    source("PathwayKeggMaps.R")
    newtable <- getMostCommonMaps(input, output, tableIDs, session)
    # Pathway maken en weergeven op de webinterface op basis van de keuze van de gebruiker
    makePathway(input, newtable, output, FALSE)
  })
  
  observeEvent(input$remove_selected_compounds, {
    source("PathwayShowSavedCompounds.R")
    unsaved_compounds <<- removeSelectedCompounds(input$selected_compounds, session, allCompounds, unsaved_compounds)
    
    # Alle textvelden leeg maken zodat de compound niet twee keer wordt toegevoegd
    for(unsaved_compound in unsaved_compounds){
      updateTextInput(session = session, inputId = unsaved_compound, value = "")
    }
    
  })
  
  # Weergeven van de zoekopdracht van de gebruiker
  observeEvent(input$search_query_user_search,{
    query <- input$search_query_user
    if(!query == ""){
      source("PathwayShowSavedCompounds.R")
      searched_compounds(query,session)
    }
  })
  
  observeEvent(input$search_query_user_reset, {
    # count_save_button wordt hier altijd 2 gemaakt zodat als er maar 1 keer op save is gedrukt en er wordt een zoekopdracht verwijderd,
    # dat dan niet weer de compounds opnieuw worden tegevoegd maar dat ze worden aangepast
    source("PathwayShowSavedCompounds.R")
    setAllCompounds(count_save_button = 2, session)
  })
}