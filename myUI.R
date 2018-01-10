source("app_TabDataset.R")
source("app_TabSettings.R")
source("app_TabResults.R")
source("app_TabPathways.R")

myUI<- fluidPage(
  theme = shinytheme("flatly"),
  
  navbarPage("Visualize", id = "tabs",
             # De vier variabelen afkomsting van de sources die hierboven zijn aangeroepen.
             tabDataset,
             tabSettings,
             tabResults,
             tabPathways
  )
)