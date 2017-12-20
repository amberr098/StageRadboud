SteadyStateMain <- function(input, output, session){
  # Upload button op Dataset tab
  observeEvent(input$upload, {
    source("ActivatedUploadButton.R")
    checkFileSteadyState(input$file1$datapath, input$file1, session, output)
  })
  
  # Plot button op Settings tab
  observeEvent(input$plot, {
    source("SteadyStatePlotButton.R")
    activatedPlotButton(input, output, session)
  })
  
}