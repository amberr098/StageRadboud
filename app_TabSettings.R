# Deze tab bevat de opties voor de gebruiker voor de uiteindelijke visualisatie. 
# De uiOutputs worden toegevoegd in het bestand SetSettings.R 
tabSettings <- tabPanel("Settings",icon = icon("gear"),
                        tabsetPanel(type= "tabs",
                                    
                                    # Opties voor de plot toevoegen
                                    tabPanel("Settings plot",
                                             div(style = "margin-top: 20px;",
                                                 fluidRow(
                                                   column(4, uiOutput("abs_norm_option"),
                                                     
                                                     uiOutput("av_ind_option"),
                                                     
                                                     uiOutput("set_y_axis_scale"),
                                                     
                                                     uiOutput("switchNormalisation")),
                                                   
                                                   column(4, uiOutput("select_molecules_option"),
                                                     
                                                     uiOutput("switchShowSeperate")),
                                                   
                                                   column(4, uiOutput("select_samples_option"))
                                                   )
                                                 )
                                             ),
                                    
                                    # Plot button
                                    div(style = "position:absolute;right:2em;",
                                        uiOutput("plot_Button"))
                                    ))