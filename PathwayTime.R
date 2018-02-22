unsaved_compounds <- NULL

# Hier worden alle functies aangeroepen die te maken hebben met de visualisatie van de pathway met tijdspunten
main <- function(output, input, session, datapath, file){
  source("ActivatedUploadButton.R")
  Resp_dataframe <- checkFileTime(datapath, file, session, output)

  if(length(Resp_dataframe) == 0){
    invisible("geen csv file")
  }else{
    checkedFile(Resp_dataframe, output, input, session, datapath, file)
  }
}

# Deze functie wordt alleen aangeroepen wanneer er gecontroleerd is dat het ingevoerde bestand een .csv bestand is.
checkedFile <- function(Resp_dataframe, output, input, session, datapath, file){
  
  # Plaatsen van de selectInput waarin de keuze staat voor een conditie
  source("PathwayConditions.R")
  setConditionsTime(Resp_dataframe, output, input)
  
  # Wordt geactiveerd wanneer er op de "Calculate fold change" button wordt geklikt
  observeEvent(input$calcFoldChange, {
    
    # De data van de gekozen condities ophalen
    source("PathwayConditionData.R")
    dataConditionNumeric <- getDataConditions(input$userCondition1, input$userCondition2, Resp_dataframe)
    
    # ratio bepalen door de 13C kolommen te delen door de 12C kolommen.
    source("PathwayFoldChange.R")
    ratios <- getRatioTime(dataConditionNumeric)
    
    # Gemiddelde van duplicaten nemen
    source("TimeNormalisation.R")
    average_df <- getAverageC13C12(ratios)
 
    # Fold change berekenen en de log2 daarvan nemen. 
    source("PathwayFoldChange.R")
    log2 <- getFoldChangeTime(average_df)
    
    # log2Dataframe aanmaken zodat deze gebruikt kan worden in de file PathwayKeggMaps.R
    log2Dataframe <<- log2
    
    source("PathwayIdentifiers.R")
    unsaved_compounds <<- searchIdentifiers(colnames(log2))
    
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
  })
  
  
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
    
    source("PathwayKeggTable.R")
    tableIDs <- placeIDs()
    
    source("PathwayKeggMaps.R")
    newtable <<- getMostCommonMaps(input, output, tableIDs, session)
    # Pathway maken en weergeven op de webinterface op basis van de keuze van de gebruiker
    makePathway(input, newtable, output, TRUE)
  })
  
  observeEvent(input$remove_selected_compounds, {
    source("PathwayShowSavedCompounds.R")
    unsaved_compounds <<- removeSelectedCompounds(input$selected_compounds, session, colnames(log2), unsaved_compounds)
    
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