# Het weergeven van alle opgeslagen compounds met IDs in de webinterface. 
setAllCompounds <- function(count_save_button, session){
  # Ophalen alle opgeslagen compounds
  source("PathwaySavedCompounds.R")
  saved_compounds <- savedIdentifiers()
  
  col_Compound <- which(colnames(saved_compounds) == "Compound")
  col_ID <- which(colnames(saved_compounds) == "ID")
  
  allChoices <- c()
  allValuesSet <<- c()
  
  # De lijst maken met de weergaven van de compounds met de ids en de values van deze compound_id opslaan in een lijst.
  for(row in 1:nrow(saved_compounds)){
    comp <- saved_compounds[row,col_Compound]
    id <- saved_compounds[row,col_ID]
    
    comp_id <- paste0(comp, " ", id)
    
    allChoices <- c(allChoices, comp_id)
    allValuesSet <<- c(allValuesSet, as.character(comp))
  }
  
  # Alleen wanneer er voor de eerste keer op de save button geklikt wordt moeten de opgeslagen compounds worden weergeven.
  # Wanneer er daarna op de save button wordt geklikt, wordt de lijst alleen aangepast en niet opnieuw geplaatst.
  if(count_save_button == 1){
    insertUI(selector = "#placeholder_saved_ids",
             ui = checkboxGroupInput(inputId = "selected_compounds", label = "Remove compounds", choiceNames = allChoices, choiceValues = allValuesSet)
    )
  }else{
    updateCheckboxGroupInput(session,inputId = "selected_compounds", label = "Remove compounds", choiceNames = allChoices, choiceValues = allValuesSet)
  }
  
  
  return(allChoices)
}

# De mogelijkheid om compounds te verwijderen in de webinterface. Zodra de compound wel nodig is voor de pathway, wordt 
# de compound weergeven bij de lijst met compounds die nog ingevuld moeten worden.
removeSelectedCompounds <- function(all_selected_compounds, session, allCompounds, unsaved_compounds){
  # Ophalen alle opgeslagen compounds
  source("PathwaySavedCompounds.R")
  saved_compounds <- savedIdentifiers()
  
  # Wanneer er een compound is verwijderd, wordt hij ook verwijderd uit de lijst. 
  for(compound in all_selected_compounds){
    row_compound <- which(saved_compounds == compound)
    saved_compounds <- saved_compounds[-row_compound,]
  }
  
  allChoices <- c()
  allValuesRem <<- c()
  # Opnieuw maken van de lijst met de opgeslagen compounds: kunnen er een aantal verwijderd zijn.
  if(!nrow(saved_compounds) == 0){
    col_Compound <- which(colnames(saved_compounds) == "Compound")
    col_ID <- which(colnames(saved_compounds) == "ID")
    
    
    for(row in 1:nrow(saved_compounds)){
      comp <- saved_compounds[row,col_Compound]
      id <- saved_compounds[row,col_ID]
      
      comp_id <- paste0(comp, " ", id)
      
      if(!is.na(comp_id) && !comp_id == ""){
        allChoices <- c(allChoices, comp_id)
        allValuesRem <<- c(allValuesRem, as.character(comp))
      }
    }
    # Updaten van de opgeslagen moleculen, niet opnieuw plaatsten. 
    updateCheckboxGroupInput(session, inputId = "selected_compounds", label = "Remove compounds", choiceNames = allChoices, choiceValues = allValuesRem)
    write.table(saved_compounds, "compound_id_Excel.csv",append=FALSE, row.names = FALSE, col.names = TRUE)
  }else{
    allChoices <- character(0)
    allValuesRem <- character(0)
    updateCheckboxGroupInput(session, inputId = "selected_compounds", label = "NO COMPOUNDS SAVED", choiceNames = allChoices, choiceValues = allValuesRem)
    write.table(saved_compounds, "compound_id_Excel.csv",append=FALSE, row.names = FALSE, col.names = TRUE)
  }
  
  unsaved_compounds <- adaptInputID(allCompounds, unsaved_compounds)
  return(unsaved_compounds)
}

# Als er een compound wordt verwijderd die nodig is voor de dataset, dan wordt de compound weer weergeven in het tabblad
# waar de identifiers ingevuld moeten worden
adaptInputID <- function(allCompounds, unsaved_compounds){
  removed_compounds <- setdiff(allValuesSet, allValuesRem)
  
  for(compound in removed_compounds){
    # Alleen toevoegen wanneer het een compound is die zich in de dataset bevind
    if(compound %in% allCompounds){
      # Alleen de compound toevoegen, aan het scherm waar de identifiers worden ingevuld, als de compound er nog niet staat
      if(!compound %in% unsaved_compounds){
        unsavedIdentifiers(compound)
        unsaved_compounds <- c(unsaved_compounds, compound)
      }
    }
  }
  return(unsaved_compounds)
}

# Wordt aangeroepen wanneer er een zoekopdracht wordt ingegeven door de gebruiker
searched_compounds <- function(query,session){
  source("PathwaySavedCompounds.R")
  saved_compounds <- savedIdentifiers()
  
  index_col_Compounds <- which(colnames(saved_compounds) == "Compound")
  index_col_IDs <- which(colnames(saved_compounds) == "ID")
  
  # Ophalen in welke rij de gezochte query staat.
  allSavedCompounds <- tolower(saved_compounds[,index_col_Compounds])  
  index_query <- grep(tolower(query), allSavedCompounds)
  searched_compounds <- allSavedCompounds[index_query]
  
  # Als er een overeenkomst is gevonden met de zoekopdracht en de opgeslagen compounds
  if(!identical(index_query, integer(0))==TRUE){
    # Per rij de compound en het bijbehorende ID ophalen.
    allChoices <- c()
    allValues <- c()
    for(row in index_query){
      comp <- saved_compounds[row, index_col_Compounds]
      id <- saved_compounds[row, index_col_IDs]
      
      comp_id <- paste0(comp, " ", id)
      
      allChoices <- c(allChoices, comp_id)
      allValues <- c(allValues, as.character(comp))
    }
    
    updateCheckboxGroupInput(session,inputId = "selected_compounds", label = "Remove compounds", choiceNames = allChoices, choiceValues = allValues)
  }else{
    # Geen overeenkomst gevonden tussen zoekopdracht en opgeslagen compounds
    showModal(modalDialog("No compound found", easyClose = TRUE, 
                          footer = tagList(
                            modalButton("OK")
                          )
    )
    )
  }
  
  
}