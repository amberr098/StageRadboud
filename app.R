library(shiny) # Web interface
library(shinythemes) # Layout webinterface
library(shinyWidgets) # Andere widgets
library(shinycssloaders) # Loading tekens
library(KEGGREST) # Verbinding maken met KEGG (geen pathways)
library(pathview) # KEGG pathways

source("myUI.R", local = TRUE)
source("myServer.R")
  
# Run the application
shinyApp(ui = myUI, server = myServer)