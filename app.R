library(shiny)
library(shinythemes)
library(shinyWidgets)

source("myUI.R", local = TRUE)
source("myServer.R")

# Run the application 
shinyApp(ui = myUI, server = myServer)