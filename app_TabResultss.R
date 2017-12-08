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
                                                  column(6, actionBttn(inputId = "showSettings",
                                                                      label = "",
                                                                      icon = icon("gear"),
                                                                      style = "jelly",
                                                                      color = "succes",
                                                                      size = "md")),
                                                  
                                                  column(6, div(style = "position:absolute;right:1em;",
                                                                actionButton(inputId = "download",
                                                                             label = "Download",
                                                                             icon = icon("download"))
                                                                ))
                                                  ),
                                                
                                                # De opties die er zichtbaar worden wanneer er op de groene settings cirkel wordt geklikt. Settings voor de title en subtitle
                                                fluidRow(
                                                  div(style = "margin-top: 20px; ",
                                                      fluidRow(
                                                        column(12, uiOutput("line"))
                                                        ),
                                                      
                                                      fluidRow(
                                                        div(style = "margin-left: 20px; ",
                                                            column(6, uiOutput("textT"))
                                                            ),
                                                        column(5, uiOutput("textS"))
                                                        ),
                                                      
                                                      column(4, uiOutput("TitleBar"),
                                                             fluidRow(
                                                               column(4, uiOutput("left")),
                                                               column(4, uiOutput("center"), offset = 4)
                                                               ), offset = 0,
                                                             
                                                             div(style = "margin-top: 20px; margin-bottom: 40px;",
                                                                 fluidRow(
                                                                   column(4, uiOutput("bold")),
                                                                   column(4, uiOutput("italic"), offset = 4)
                                                                   ))
                                                             ),
                                                      column(1, uiOutput("LetterTT"), offset = 0),
                                                      column(4, uiOutput("Subtitle"), offset = 1,
                                                             fluidRow(
                                                               column(4, uiOutput("leftS")),
                                                               column(4, uiOutput("centerS"), offset = 4)
                                                               ),
                                                             div(style = "margin-top: 20px; margin-bottom: 40px;",
                                                                 fluidRow(
                                                                   column(4, uiOutput("boldS")),
                                                                   column(4, uiOutput("italicS"), offset = 4)
                                                                   ))
                                                             ),
                                                      column(1, uiOutput("LetterTS"), offset = 0)
                                                      )
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