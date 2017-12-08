# Eerste tabblad (Dataset)
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