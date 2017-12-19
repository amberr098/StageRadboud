data <- NULL
typeTitle <- NULL
typeSubtitle <- NULL
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
    
    # Toevoegen van de optie om een zwart/witte grafiek te krijgen. Wordt toegevoegd aan de resultaten tab
    output$black_white_option <- renderUI({
      checkboxInput(inputId = "black_white", label = "Black/White")
    })
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
      
      
      alignValueTitle <- reactiveValues(click = NULL)
      
      observeEvent(input$leftTitle, {
        alignValueTitle$click <- 0
      })
      
      observeEvent(input$centerTitle, {
        alignValueTitle$click <- 0.5
      })
      
      alignValueSubtitle <- reactiveValues(click = NULL)
      
      observeEvent(input$leftSubtitle, {
        alignValueSubtitle$click <- 0
      })
      
      observeEvent(input$centerSubtitle, {
        alignValueSubtitle$click <- 0.5
      })
      
      observeEvent(input$AddTitles,{
        source("TitleSettings.R")
        typeTitle <<- getTypeTitle(input$boldTitle, input$italicTitle)
        typeSubtitle <<- getTypeSubtitle(input$boldSubtitle, input$italicSubtitle)
        
        titleAlign <<- alignValueTitle$click
        subtitleAlign <<- alignValueSubtitle$click
        
        output$Graphic <- renderPlot({
          # Door de observeEvent van de black/white checkbox worden de titles ook op de zwart/witte plot geplaatst. 
          observeEvent(input$black_white, {
            if(input$black_white == TRUE){
              output$Graphic <- renderPlot({
                p + ggtitle(input$titleInput) +
                  labs(subtitle = input$subtitleInput) +
                  theme(plot.title = element_text(size = input$sizeTitle, hjust = titleAlign, face = typeTitle)) +
                  theme(plot.subtitle = element_text(size = input$sizeSubtitle, hjust = subtitleAlign, face = typeSubtitle))+
                coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
                scale_fill_grey()
            })
          }else{
            output$Graphic <- renderPlot({
              p + ggtitle(input$titleInput) +
                labs(subtitle = input$subtitleInput) +
                theme(plot.title = element_text(size = input$sizeTitle, hjust = titleAlign, face = typeTitle)) +
                theme(plot.subtitle = element_text(size = input$sizeSubtitle, hjust = subtitleAlign, face = typeSubtitle))+
                coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
            })
          }
          })
        })
      })
      
      # Ook zwart en witte plot gemaakt wanneer er geen titel is toegevoegd, dus wanneer er niet op de "Add" button is geklikt.  
      observeEvent(input$black_white, {
        if(input$black_white == TRUE){
          output$Graphic <- renderPlot({
            p + ggtitle(input$titleInput) +
              labs(subtitle = input$subtitleInput) +
              theme(plot.title = element_text(size = input$sizeTitle, hjust = titleAlign, face = typeTitle)) +
              theme(plot.subtitle = element_text(size = input$sizeSubtitle, hjust = subtitleAlign, face = typeSubtitle))+
              coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
              # Scale_fill_grey zorgt ervoor dat de plot zwart/wit wordt.
              scale_fill_grey()
          })
        }else{
          output$Graphic <- renderPlot({
            p + ggtitle(input$titleInput) +
              labs(subtitle = input$subtitleInput) +
              theme(plot.title = element_text(size = input$sizeTitle, hjust = titleAlign, face = typeTitle)) +
              theme(plot.subtitle = element_text(size = input$sizeSubtitle, hjust = subtitleAlign, face = typeSubtitle))+
              coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
          })
        }
      })
  })
  


########### TIME PLOTS ##################
  observeEvent(input$uploadTime,{
    source("app_observeUploadButton.R")
    Resp_dataframe <<- checkFileTime(input$fileTime$datapath, input$fileTime, session, output)
   
    # Toevoegen van de opties voor de gebruiker: absolute waarden/genormaliseerde waarden.
    output$abs_norm_option <- renderUI({
      pickerInput(
        inputId = "abs_norm_time", 
        label = "Absolute/normalized counts",
        choices = list("Absolute counts" = "abs", "Normalized counts" = "norm"),
        selected = "abs",
        multiple = FALSE
      )
    })
    source("TimeOptions.R")
    sample_list <- getSamples(Resp_dataframe)
    
    # Toevoegen van de optie om een sample te kiezen
    output$select_samples_option <- renderUI({
      pickerInput(inputId = "sampleChoices",
                  label = "Select sample(s)",
                  choices = c(sample_list), 
                  options = list('actions-box' = TRUE),
                  multiple = TRUE)
    })
    
    # Toevoegen van de plot button
    output$plot_Button <- renderUI({
      actionButton(inputId = "plotButton",
                   label = "Plot")
    })
    
    updateTabsetPanel(session = session, inputId = "tabs", selected = "Settings")
  })
  

  
  # Als er gekozen wordt voor de optie genormaliseerde waarden, dan moet er bepaald wordne
  # hoe er genormaliseerd wordt: C13/C12 of C13/totaal C13 C12
  observeEvent(input$abs_norm_time, {
    if(input$abs_norm_time == "norm"){
      output$switchNormalisation <- renderUI({
        materialSwitch(inputId = "switch_norm",
                       label = "13C divided by total",
                       status = "success")
      })
      
      output$select_columns_to_show <- renderUI({
        NULL
      })
      
      source("TimeOptions.R")
      molecule_list <- getMolecules_Norm(Resp_dataframe)
      
      # Toevoegen van de optie om een molecuul te kiezen
      output$select_molecules_option <- renderUI({
        pickerInput(inputId = "moleculeChoices",
                    label = "Select molecule(s)",
                    choices = c(molecule_list), 
                    options = list('actions-box' = TRUE),
                    multiple = TRUE)
      })
      
    }else{
      output$switchNormalisation <- renderUI({
        NULL
      })
      
      source("TimeOptions.R")
      molecule_list <- getMolecules(Resp_dataframe)
      
      
      # Toevoegen van de optie om een molecuul te kiezen
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
  })
  
  
  observeEvent(input$switch_norm, {
    source("NormalisationTime.R")
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
  
  observeEvent(input$plotButton, {
    source("TimeSelectedDF.R")
    selected_dataframe <<- getSelectedDataframe(input$sampleChoices, input$moleculeChoices, average_df, standev_df)
    colnames(selected_dataframe) <- c("Sample", "Time", "Molecule", "Variant", "Average", "SD", "Half SD")
    
    source("TimePlot.R")
    p <- getPlot(selected_dataframe)
    
    source("TimeSelectedDF.R")
    showDataFrame <- getShowDataframe(input$sampleChoices, input$moleculeChoices, average_df, standev_df)
    
    output$dataTable <- renderDataTable({
      showDataFrame
    })
    
    updateTabsetPanel(session = session, inputId = "tabs", selected = "Results")
    
    ranges <- reactiveValues(x = NULL, y = NULL)
  
    # De output wanneer er ingezoomed wordt. 
    output$Graphic <- renderPlot(height="auto", {
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
    
    alignValueTitle <- reactiveValues(click = NULL)

    observeEvent(input$leftTitle, {
      alignValueTitle$click <- 0
    })

    observeEvent(input$centerTitle, {
      alignValueTitle$click <- 0.5
    })

    alignValueSubtitle <- reactiveValues(click = NULL)

    observeEvent(input$leftSubtitle, {
      alignValueSubtitle$click <- 0
    })

    observeEvent(input$centerSubtitle, {
      alignValueSubtitle$click <- 0.5
    })

    observeEvent(input$AddTitles,{
      source("TitleSettings.R")
      typeTitle <- getTypeTitle(input$boldTitle, input$italicTitle)
      typeSubtitle <- getTypeSubtitle(input$boldSubtitle, input$italicSubtitle)
      
      titleAlign <- alignValueTitle$click
      subtitleAlign <- alignValueSubtitle$click
      
      output$Graphic <- renderPlot({
        p + ggtitle(input$titleInput) +
          labs(subtitle = input$subtitleInput) +
          theme(plot.title = element_text(size = input$sizeTitle, hjust = titleAlign, face = typeTitle)) +
          theme(plot.subtitle = element_text(size = input$sizeSubtitle, hjust = subtitleAlign, face = typeSubtitle))+
          coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
      })
    })
  })
  
  #### ALGEMEEN ####
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
      ggsave(file, p, dpi= res, height=7, width=15, units="in", device = "png", limitsize = FALSE)
    }
  )
  
  # Cancel button in de popup voor de keuze voor 72 of 300 dpi
  observeEvent(input$cancel, {
    removeModal()
  })
}


