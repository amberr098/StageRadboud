# Het ophalen van de KEGG mappen waarin de compounds voorkomen
getMostCommonMaps <- function(input, output, tableIDs, session){
  # De rijnamen worden de compound IDs die worden opgehaald en opgeslagen in allIDs
  newtable <- t(tableIDs)
  colnames(newtable) <- NULL
  allIDs <- rownames(newtable)
  newtable[newtable == -Inf] <- 0
  newtable[newtable == Inf] <- 0
  
  # Achterhalen hoevaak er 10 IDs gepakt moeten worden (keggGet kan maar per 10 IDs)
  numberIDs <- length(allIDs)
  numberOfLoops <- ceiling(numberIDs/10)
  
  start <- 1
  end <- 0
  allmaps <- c()
  for(loop in 1:numberOfLoops){
    # Als er geen 10 IDs zijn, dan is het van 1 tot de totale aantal IDs
    if(numberOfLoops == 1){
      ids <- allIDs[start:numberIDs]
      allmaps <- getMaps(ids, allmaps)
    }else{
      # Als het aantal van eind groter is dan de totale IDs dan worden de laatste paar IDs gepakt
      if(end+10 > numberIDs){
        ids <- allIDs[start:numberIDs]
        allmaps <- getMaps(ids, allmaps)
        # Als er nog meer dan 10 IDs over zijn worden de volgende 10 gepakt
      }else{
        end <- end + 10
        ids <- allIDs[start:end]
        allmaps <- getMaps(ids, allmaps)
      }
      start <- start + 10
    }
  }
  # De meest voorkomende map vooraanzetten gevolgd door de op een na meest voorkomende map etc.
  mostCommonMaps <- names(sort(summary(as.factor(allmaps)), decreasing=T))
  
  allOptions <- c()
  # Ophalen van de informatie voor de visualisatie van de map keuzes
  for(map in mostCommonMaps){
    # Ophalen van de naam van de map en de aantal voorkomens van de map 
    nameMap <- keggGet(map)[[1]]$NAME
    count_maps <- length(grep(map, allmaps))
    
    optionsMap <- paste0(nameMap, " ", "(", map, ", ", count_maps ,")")
    allOptions <- c(allOptions, optionsMap)
    
  }
  # Toevoegen van de keuzes voor de maps aan de webinterface
  placeMaps(output, allOptions, session)
  
  return(newtable)
}

# Alle mappen ophalen waarin de moleculen voorkomen
getMaps <- function(allIDs, allmaps){
  getkegg <- keggGet(allIDs)
  
  for(index in 1:length(allIDs)){
    maps <- getkegg[[index]]$PATHWAY
    getMapsID <- as.matrix(maps)
    
    mapsID <- rownames(getMapsID)
    allmaps <- c(allmaps, mapsID)
  }

  return(allmaps)
}

# Toevoegen van alle opties van de mappen aan de webinterface
placeMaps <- function(output, allOptions, session){
  output$maps_radiobuttons <- renderUI({
    radioButtons(inputId = "KEGGMAP", label = "Choose a KEGG map", choices = allOptions)
  })
  
  output$map_button <- renderUI({
    actionButton(inputId = "sendMap", label = "Map")
  })
}

# Maken van de pathway op basis van de keuze van de gebruiker
makePathway <- function(input, newtable,output, state){
  observeEvent(input$sendMap, {
    maxValue <- getMaxValue(newtable)
    minValue <- getMinValue(newtable)
    
    # Ophalen van de keuze van de gebruiker
    keggMap <- input$KEGGMAP
    patternMap <- "(map.*)"
    # Alleen de map id overhouden
    match <- regmatches(keggMap, regexpr(patternMap, keggMap))
    idmap <- gsub(",.*", "", match)
    id <- gsub("map", "", idmap)
    
    # Checken of de pathway gedownload kan worden van KEGG
    checkFile_name <- paste0("ko", id, ".xml")
    download.kegg(id, species = "ko")
    
    if((file.info(checkFile_name)$size == 0) == T){
      showModal(modalDialog("Can't download the pathway"
      ))
    }else{
     
      pvout <- pathview(cpd.data = newtable, pathway.id = id, kegg.native = T, species = "ko", low = "green", mid = "grey", high = "red", multi.state = state, limit = list(cpd = c(-10, 12)))
  
      if(state == FALSE){
        filename <- paste0("ko",id,".pathview.png")
      }else{
        filename <- paste0("ko", id, ".pathview.multi.png")
      }

      # Toevoegen van de map aan de webinterface
      output$keggmapper <- renderImage({
        list(src = filename,
             contentType = 'image/png')
        
      })
      
      print("map geplaatst")
    }
  })
}

# Bepalen van de maximale waarde die voorkomt in de tabel. Deze wordt altijd naar boven afgerond
getMaxValue <- function(newtable){
  log2data <- matrix(as.numeric(unlist(newtable)),nrow=nrow(newtable))
  index_maxValue <- which(log2data == max(log2data), arr.ind = TRUE)

  maxValue <- newtable[index_maxValue[1,1], index_maxValue[1,2]]
  ceiling_maxValue <- ceiling(as.numeric(maxValue))

  return(ceiling_maxValue)
}

# Bepalen van de minimale waarde die voorkomt in de tabel. Deze wordt altijd naar beneden afgerond.
getMinValue <- function(newtable){
  log2data <- matrix(as.numeric(unlist(newtable)),nrow=nrow(newtable))
  index_minValue <- which(log2data == min(log2data), arr.ind = TRUE)
  minValue <- newtable[index_minValue[1,1], index_minValue[1,2]]
  floor_minValue <- floor(as.numeric(minValue))

  return(floor_minValue)
}
