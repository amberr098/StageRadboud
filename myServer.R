data <- NULL
count_save_button <<- 0
myServer <- function(input, output, session) {
  # Gebruiker maakt een keus tussen de steady state plot of de time plot. 
  source("ChoicePlot.R")
  getTypeOfPlot(input, output)

  #### STEADY STATES ####
  observeEvent(input$upload, {
    source("SteadyState.R")
    SteadyStateMain(input, output, session)
    
    #### PATHWAYS ####
    source("PathwaySteadyState.R")
    main(input$file1$datapath, input$file1, session, output, input)
  })
  
  #### TIME PLOTS ####
  observeEvent(input$uploadTime, {
    source("Time.R")
    timeMain(input, output, session)

        #### PATHWAYS ####
    source("PathwayTime.R")
    main(output, input, session, input$fileTime$datapath, input$fileTime)
  })
  

  
  #### DOWNLOAD BUTTON ####
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
      ggsave(file, dpi= res, height=7, width=15, units="in", device = "png", limitsize = FALSE)
    }
  )
  
  # Cancel button in de popup voor de keuze voor 72 of 300 dpi
  observeEvent(input$cancel, {
    removeModal()
  })
}


