# Eerste tabblad (Dataset)

# Bevat radiobuttons waarbij de gebruiker kiest voor een stady state plot of een time plot. 
# Aan de hand van de keuze verschijnt er een browse optie om het gewenste bestand te kiezen
# en wordt er een upload button toegevoegd. 
tabDataset <- tabPanel("Dataset", 
                       icon = icon("table"), 
                       fluidRow(
                         column(4, radioButtons(inputId = "typeOfPlot",
                                                label = "Select plot",
                                                choices = c("Stady state", "Time plot"))),
                         column(4, uiOutput("setSelectFile"))
                         ),
                       fluidRow(
                         column(4, actionButton(inputId = "SubmitTypeOfPlot",
                                                label = "Submit")),
                         column(4, uiOutput("setUploadButton"))
                         
                       )
                      
                       )