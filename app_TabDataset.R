# Eerste tabblad (Dataset)
tabDataset <- tabPanel("Dataset", 
                       icon = icon("table"), 
                       fluidRow(
                         column(4, fileInput('file1', 'Select your file'),
                                fluidRow(
                                  column(12, actionButton("upload", "Upload data"))
                                  )
                                )
                         )
                       )