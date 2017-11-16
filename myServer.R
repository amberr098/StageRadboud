data <- NULL
titleAlign <- NULL 
subtitleAlign <- NULL

myServer <- function(input, output, session) {

  observeEvent(input$upload, {
    source("app_observeUploadButton.R")
    checkFile(input$file1$datapath, input$file1, session)
  })
  
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
    specific_data <- getData(input$abs_norm, input$av_ind, data_NoRT)
    
    if(!is.null(input$MolCheckBox) && !is.null(input$SamCheckBox)){
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
    }
    p <- setPlot(selected_matrix, input$av_ind)
    ranges <- reactiveValues(x = NULL, y = NULL)
    
    # De output wanneer er ingezoomed wordt. 
    output$Graphic <- renderPlot(height="auto", {
      
      # Het openen van het menu waar meer instellingen staan (titel en subtitel)
      observeEvent(input$showSettings, {
        if((input$showSettings %% 2) == 0){
          source("app_TitleSettings.R")
          hideSettings(output)
          
        
        }else{
          source("app_TitleSettings.R")
          showSettings(output)
          
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
        }
      })
      p + coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
    })
      
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
      
      # Barplot maken. Er een globale functie van maken (<<-) omdat hij ook aangeroepen wordt bij de download button
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
  

  
  
  # Downloaden van de plot
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
  
  output$downloadPlot <- downloadHandler(
    filename = "ShinyPlot.png",
    
    content = function(file){
      res <- as.numeric(input$resolution)
      ggsave(file, plotInput(), dpi= res, height=7, width=15, units="in", device = "png", limitsize = FALSE)
    }
  )
  
  observeEvent(input$cancel, {
    removeModal()
  })
}
