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
                                                  column(6, uiOutput("black_white_option")),
                                                  
                                                  column(6, div(style = "position:absolute;right:1em;",
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
                                                        column(6, h4("Title")),
                                                        column(6, h4("Subtitle"))
                                                      ),
                                                      fluidRow(
                                                        column(3, textInput(inputId = "titleInput", label = NULL, width = "100%"),
                                                               fluidRow(
                                                                 column(6, actionBttn(inputId = "boldTitle", label = NULL, icon = icon("bold"), color = "succes", style = "bordered", block = TRUE, no_outline = FALSE, size = "sm")),
                                                                 column(6, actionBttn(inputId = "italicTitle", label = NULL, icon = icon("italic"), color = "succes", style = "bordered", block = TRUE, no_outline = FALSE,size = "sm"))
                                                               ),
                                                               div(style = "margin-top: 20px; ",
                                                                 fluidRow(
                                                                   column(6, actionBttn(inputId = "leftTitle", label = NULL, icon = icon("align-left"), color = "succes", style = "bordered", block = TRUE, no_outline = FALSE, size = "sm")),
                                                                   column(6, actionBttn(inputId = "centerTitle", label = NULL, icon = icon("align-center"), color = "succes", style = "bordered", block = TRUE, no_outline = FALSE, size = "sm"))
                                                                 ))
                                                               ),
                                                        column(1, numericInput(inputId = "sizeTitle", label = NULL, value = 12)),
                                                        
                                                        column(3, textInput(inputId = "subtitleInput", label = NULL, width = "100%"), offset = 2,
                                                               fluidRow(
                                                                 column(6, actionBttn(inputId = "boldSubtitle", label = NULL, icon = icon("bold"),color = "succes", style = "bordered", block = TRUE, no_outline = FALSE, size = "sm")),
                                                                 column(6, actionBttn(inputId = "italicSubtitle", label = NULL, icon = icon("italic"), color = "succes", style = "bordered", block = TRUE, no_outline = FALSE, size = "sm"))
                                                               ),
                                                               
                                                               div(style = "margin-top: 20px; ",
                                                                 fluidRow(
                                                                   column(6, actionBttn(inputId = "leftSubtitle", label = NULL, icon = icon("align-left"), color = "succes", style = "bordered", block = TRUE, no_outline = FALSE, size = "sm")),
                                                                   column(6, actionBttn(inputId = "centerSubtitle", label = NULL, icon = icon("align-center"), color = "succes", style = "bordered", block = TRUE, no_outline = FALSE, size = "sm"))
                                                                 ))
                                                               ),
                                                        column(1, numericInput(inputId = "sizeSubtitle", label = NULL, value = 9))
                                                      ),
                                                      
                                                      div(style = "margin-top: 20px; margin-bottom: 20px; ",
                                                        fluidRow(
                                                          column(12, actionButton(inputId = "AddTitles", label = "Add"))
                                                        )
                                                      )
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