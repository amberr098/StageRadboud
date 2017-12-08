# Deze tab is onderverdeeld in twee tabs: plot en data. Plot weergeeft de grafiek van de geselecteerde data en data weergeeft een tabel met de geselecteerde data.
tabResults <- tabPanel("Results", icon = icon("bar-chart-o"),
                       
                       tabsetPanel(type = "tabs", 
                                   
                                   # In deze tab wordt de plot weergeven op basis van de opties van gebruiker.
                                   tabPanel("Plot",
                                            div(style = "margin-top: 20px;",
                                                plotOutput("Graphic", 
                                                           dblclick = "plot1_dblclick",
                                                           brush = brushOpts(
                                                             id = "plot1_brush",
                                                             resetOnNew = TRUE),
                                                           width = "100%",
                                                           height = "500px"),
                                                fluidRow(
                                                  column(12, div(style = "position:absolute;right:1em;",
                                                                actionButton(inputId = "download",
                                                                             label = "Download",
                                                                             icon = icon("download"))
                                                  ))
                                                ),
                                                
                                                fluidRow(
                                                  div(style = "margin-top: 40px; ",
                                                      fluidRow(
                                                        column(12, HTML('<hr style="color: grey;">'))
                                                      ),
                                                      
                                                      fluidRow(
                                                        div(style = "margin-left: 20px; ",
                                                            column(6, h4("Title settings"))
                                                        ),
                                                        column(5, h4("Subtitle settings"))
                                                      ),
                                                      
                                                      column(4, textInput(inputId = "setTitle",
                                                                          label = NULL,
                                                                          placeholder = "Title...",
                                                                          width = "600px"),
                                                             
                                                             fluidRow(
                                                               column(4, actionBttn(inputId = "alignLeftTitle",
                                                                                    label = NULL,
                                                                                    icon = icon("align-left"),
                                                                                    color = "succes",
                                                                                    style = "stretch",
                                                                                    block = TRUE)),
                                                               
                                                               column(4, actionBttn(inputId = "alignCenterTitle",
                                                                                    label = NULL,
                                                                                    icon = icon("align-center"),
                                                                                    color = "succes",
                                                                                    style = "stretch",
                                                                                    block = TRUE), offset = 4)
                                                             ), offset = 0,
                                                             
                                                             div(style = "margin-top: 20px; margin-bottom: 40px;",
                                                                 fluidRow(
                                                                   column(4, actionBttn(inputId = "LTB", 
                                                                                        label = NULL,
                                                                                        icon = icon("bold"),
                                                                                        color = "succes",
                                                                                        style = "stretch",
                                                                                        block = TRUE,
                                                                                        no_outline = FALSE)),
                                                                   
                                                                   column(4, actionBttn(inputId = "LTI",
                                                                                        label = NULL,
                                                                                        icon = icon("italic"),
                                                                                        color = "succes",
                                                                                        style = "stretch",
                                                                                        block = TRUE,
                                                                                        no_outline = FALSE), offset = 4)
                                                                 ))
                                                      ),
                                                      column(1, numericInput("numT", 
                                                                             label = NULL, 
                                                                             value = 12, 
                                                                             width = "200px")
                                                             , offset = 0),
                                                      
                                                      column(4, textInput(inputId = "setSubtitle",
                                                                          label = NULL,
                                                                          placeholder = "Subtitle...",
                                                                          width = "600px"), offset = 1,
                                                             fluidRow(
                                                               column(4, actionBttn(inputId = "alignLeftSubtitle",
                                                                                    label = NULL,
                                                                                    icon = icon("align-left"),
                                                                                    color = "succes",
                                                                                    style = "stretch",
                                                                                    block = TRUE)),
                                                               column(4, actionBttn(inputId = "alignCenterSubtitle",
                                                                                    label = NULL,
                                                                                    icon = icon("align-center"),
                                                                                    color = "succes",
                                                                                    style = "stretch",
                                                                                    block = TRUE), offset = 4)
                                                             ),
                                                             div(style = "margin-top: 20px; margin-bottom: 40px;",
                                                                 fluidRow(
                                                                   column(4, actionBttn(inputId = "LTBS",
                                                                                        label = NULL,
                                                                                        icon = icon("bold"),
                                                                                        color = "succes",
                                                                                        style = "stretch",
                                                                                        block = TRUE,
                                                                                        no_outline = FALSE)),
                                                                   column(4, actionBttn(inputId = "LTIS",
                                                                                        label = NULL,
                                                                                        icon = icon("italic"),
                                                                                        color = "succes",
                                                                                        style = "stretch",
                                                                                        block = TRUE,
                                                                                        no_outline = FALSE), offset = 4)
                                                                 ))
                                                      ),
                                                      column(1, numericInput("numS", 
                                                                             label = NULL, 
                                                                             value = 9, 
                                                                             width = "200px"), 
                                                             offset = 0)
                                                  )
                                                  
                                                ),
                                                fluidRow(
                                                  column(4, actionButton(inputId = "SubmitTitles",
                                                                         label = "Add"))
                                                )
                                            )
                                   ),
                                   
                                   # In deze tab wordt de data van de geselecteerde samples en moleculen weergeven.
                                   tabPanel("Data",
                                            div(style = "margin-top: 20px;",
                                                dataTableOutput("dataTable")
                                            )
                                   )
                       )
)