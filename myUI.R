source("app_TabDataset.R")
source("app_TabSettings.R")
source("app_TabResults.R")

myUI<- fluidPage(
  theme = shinytheme("flatly"),
  
  navbarPage("Visualize", id = "tabs",
             # De drie variabelen afkomsting van de sources die hierboven zijn aangeroepen.
             tabDataset,
             tabSettings,
             tabResults
  )
)