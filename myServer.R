data <- NULL
titleAlign <- NULL 
subtitleAlign <- NULL

myServer <- function(input, output, session) {
  observeEvent(input$SubmitTypeOfPlot,{
    plotType <- input$typeOfPlot

    if(plotType == "Stady state"){
      
      output$setSelectFile <- renderUI({
        fileInput('file1', 'Select your file')
      })
      
      output$setUploadButton <- renderUI({
        actionButton("upload", "Upload data")
      })
      
      
      source("SetSettings.R")
      setSettingsStSt(output)
      
      observeEvent(input$ShowSingleMolecule, {
        if(input$ShowSingleMolecule == TRUE){
          output$set_y_axis_scale <- renderUI({
            pickerInput(inputId = "scale_y_axis",
                        label = "Choose scale y-axis",
                        choices = c("Same y-axis scales" = "fixed", "Different y-axis scales" = "free"),
                        multiple = FALSE)
          })
        }else{
          output$set_y_axis_scale <- renderUI({
            NULL
          })
        }
      })
    }else{
      output$setSelectFile <- renderUI({
        fileInput('fileTime', 'Select your file')
      })
      
      output$setUploadButton <- renderUI({
        actionButton("uploadTime", "Upload data")
      })
    }
  })

############## STADY STATES #######################
  # Upload button op Dataset tab
  observeEvent(input$upload, {
    source("app_observeUploadButton.R")
    checkFile(input$file1$datapath, input$file1, session, output)
  })
  
  # Plot  button op Settings tab
  observeEvent(input$plot, {
    # Exception als er geen moleculen of sampels zijn geselecteerd maar er wel op de plot button geklikt wordt
    if(!is.null(input$MolCheckBox) && !is.null(input$SamCheckBox)){
      updateTabsetPanel(session = session, inputId = "tabs", selected = "Results")
    }else if(is.null(input$MolCheckBox) && is.null(input$SamCheckBox)){
      showNotification("Select molecule(s) and sample(s)", type = "error")
    }else{
      if(is.null(input$MolCheckBox)){
        showNotification("Select molecule(s)", type = "error")

      }else if(is.null(input$SamCheckBox)){
        showNotification("Select sample(s)", type = "error")
      }
    }

    source("EditFile.R")
    data_NoRT <- setColumnNames(data)

    source("PlotData.R")
    # Alle data die hoort bij de gekozen instellingen
    specific_data <- getData(input$abs_norm, input$av_ind, data_NoRT, input$ShowSingleMolecule)
    
    # Ophalen en weergeven van de geselecteerde waardes.
    if(!is.null(input$MolCheckBox) && !is.null(input$SamCheckBox)){
      # Hangt van de switch af hoe het het geselecteerde dataframe eruit moet komen te zien.
      if(input$ShowSingleMolecule == FALSE){
        source("Visualization.R")
        selected_matrix <- getSelectedMatrix(specific_data, input$av_ind, input$MolCheckBox,input$SamCheckBox)
        
        if(input$av_ind == "ind"){
          temp_selectmatrix <- showDataTable(selected_matrix)
  
          # Visualisatie waardes in datatabel weergeven als 10.000,5 ipv 10000.5
          selectmatrix <- format.data.frame(temp_selectmatrix, big.mark = ".", decimal.mark = ",")
          output$dataTable <- renderDataTable({
            selectmatrix
          })
        }else{
          output$dataTable <- renderDataTable({
            # Visualisatie waardes in datatabel weergeven als 10.000,5 ipv 10000.5
            format_selected_matrix <- format.data.frame(selected_matrix, big.mark = ".", decimal.mark = ",", scientific = TRUE)
            format_selected_matrix
          })
        }
      }else{
        
        # Controleren of het bestand waardes in de kolom Type heeft.
        checkType <- (which(data == "Type", arr.ind = TRUE))
        if(data[2,checkType[1,2]] == "Sample"){
          type <- FALSE
        }else{
          type <- TRUE
        }
        
        # Ophalen van de geselecteerde data.
        source("switchTrue.R")
        selected_matrix <<- setSelectedMatrix(specific_data, input$MolCheckBox, input$SamCheckBox, input$abs_norm, input$av_ind, type)
        
        if(input$av_ind == "av"){
          # Format voor de te visualiseren datatabel maken.
          source("SinglePlots.R")
          showData <- getAverageData(selected_matrix)
          
          output$dataTable <- renderDataTable({
            showData
          })
        }else{
          output$dataTable <- renderDataTable({
            selected_matrix
          })
        }
      }
    }
    
    if(input$ShowSingleMolecule == FALSE){
      source("Visualization.R")
      p <- setOnePlot(selected_matrix, input$av_ind)
      
    }else{
      yscl <- input$scale_y_axis
      source("Visualization.R")
      p <- setMultiplePlots(selected_matrix, input$av_ind, yscl)
      
    }
   
    ranges <- reactiveValues(x = NULL, y = NULL)
    
    # De output wanneer er ingezoomed wordt. 
    output$Graphic <- renderPlot(height="auto", {
      
      # Het openen van het menu waar meer instellingen staan (titel en subtitel)
      observeEvent(input$SubmitTitles, {
          
          # Zorgen dat de titel gealigned wordt op basis van welke button ze klikken (links: 0, center: 0.5). 
          alignValueTitle <- reactiveValues(click = NULL)
          
          observeEvent(input$alignLeftTitle, {
            alignValueTitle$click <- 0
          })
          
          observeEvent(input$alignCenterTitle, {
            alignValueTitle$click <- 0.5
          })
          
          alignValueSubtitle <- reactiveValues(click = NULL)
          
          observeEvent(input$alignLeftSubtitle,{
            alignValueSubtitle$click <- 0
          })
          
          observeEvent(input$alignCenterSubtitle, {
            alignValueSubtitle$click <- 0.5
          })
          
          # toevoegen van titel en subtitel
          output$Graphic <- renderPlot({
            titleAlign <<- alignValueTitle$click
            subtitleAlign <<- alignValueSubtitle$click
            plotInput()
            
          })
      })
      p + coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
    })
      
      # Wanneer er een vak word geselecteerd in de plot en er twee keer op geklikt wordt
      observeEvent(input$plot1_dblclick, {
        brush <- input$plot1_brush
        if (!is.null(brush)) {
          ranges$x <- c(brush$xmin, brush$xmax)
          ranges$y <- c(brush$ymin, brush$ymax)
        }else {
          ranges$x <- NULL
          ranges$y <- NULL
        }
      })
      
      # Barplot maken. Een globale functie maken van plotInput (<<-), plotInput wordt ook aangeroepen bij downloaden plot 
      plotInput <<- function(){
        source("app_BoldItalic.R")
        typeTitle <- getTypeTitle(input$LTB, input$LTI)
        typeSubtitle <- getTypeSubtitle(input$LTBS, input$LTIS)
       
        
        p + ggtitle(input$setTitle) +
          labs(subtitle = input$setSubtitle) +
          theme(plot.title = element_text(size = input$numT, hjust = titleAlign, face = typeTitle)) +
          theme(plot.subtitle = element_text(size = input$numS, hjust = subtitleAlign, face = typeSubtitle)) +
          coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) 
      }
  })
  
  # Download button op Results tab waarbij er een keuze gemaakt moet worden tussen 72 of 300 dpi
  observeEvent(input$download, {
    showModal(modalDialog(
      radioButtons(inputId = "resolution",
                   label = "Resolution",
                   choices = c("72 dpi" = 72, "300 dpi" = 300)), 
      footer = tagList(
        modalButton("Cancel"),
        downloadButton("downloadPlot")
      ),
      
      easyClose = TRUE
    )
    )
  })
  
  # Downloaden van de plot nadat de keuze is gemaakt voor 72 of 300 dpi
  output$downloadPlot <- downloadHandler(
    filename = "ShinyPlot.png",
    
    content = function(file){
      res <- as.numeric(input$resolution)
      ggsave(file, plotInput(), dpi= res, height=7, width=15, units="in", device = "png", limitsize = FALSE)
    }
  )
  
  # Cancel button in de popup voor de keuze voor 72 of 300 dpi
  observeEvent(input$cancel, {
    removeModal()
  })

########### TIME PLOTS ##################
  observeEvent(input$uploadTime,{
    source("app_observeUploadButton.R")
    Resp_dataframe <<- checkFileTime(input$fileTime$datapath, input$fileTime, session, output)
    
    # Alle gemiddelde voor een sample krijgen 
    source("TimeAverageMatrix.R")
    average_df <<- getAverageData(Resp_dataframe)
    
    # Toevoegen van de opties voor de gebruiker.
    output$abs_norm_option <- renderUI({
      pickerInput(
        inputId = "abs_norm", 
        label = "Absolute/normalized counts",
        choices = list("Absolute counts" = "abs", "Normalized counts" = "norm"),
        selected = "abs",
        multiple = FALSE
      )
    })
    updateTabsetPanel(session = session, inputId = "tabs", selected = "Settings")
  })
  
  # Als er gekozen wordt voor de optie genormaliseerde waarden, dan moet er bepaald wordne
  # hoe er genormaliseerd wordt: C13/C12 of C13/totaal C13 C12
  observeEvent(input$abs_norm, {
    if(input$abs_norm == "norm"){
      output$switchNormalisation <- renderUI({
        materialSwitch(inputId = "switch_norm",
                       label = "13C divided by total",
                       status = "success")
      })
    }else{
      output$switchNormalisation <- renderUI({
        NULL
      })
    }
  })
  
  
  observeEvent(input$switch_norm, {
    source("Normalization.R")
    normalisation_C13C12(average_df, input$switch_norm)
  })
}
