tabPathways <- tabPanel("Pathways",
                        tabsetPanel(type = "tabs",
                                  
                                    tabPanel("Conditions",
                                             div(style = "margin-top: 20px;",
                                                 uiOutput("choiceNormalisation"),
                                                 
                                                 fluidRow(
                                                   column(3, uiOutput("choiceCondition1")),
                                                   column(3, uiOutput("choiceCondition2")),
                                                   div(style = "margin-top: 24px; ",
                                                       column(3, uiOutput("calculateFoldChange"))
                                                   )
                                                 )
                                             )
                                             ),
                                    tabPanel("Identifiers",
                                             div(style = "margin-top:20px; ", 
                                                 tags$div(id="placeholder_kegg_ids"),
                                                 uiOutput("txt_no_ids"),
                                                 uiOutput("save_compounds_actionbutton")
                                             )
                                             ),
                                    tabPanel("Maps",
                                             fluidRow(
                                               column(12, withSpinner(uiOutput("maps_radiobuttons")))
                                             ),
                                             
                                             fluidRow(uiOutput("map_button"))
                                             ),
                                    
                                    tabPanel("Pathway",
                                             withSpinner(imageOutput("keggmapper"))),
                                    
                                    tabPanel("Saved Identifiers",
                                             uiOutput("search_compound_option"),
                                             
                                             tags$div(id="placeholder_saved_ids"),
                                             
                                             uiOutput("remove_compounds_actionbutton"))
                                    )
                        )
             
