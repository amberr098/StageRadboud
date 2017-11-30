# Deze tab bevat de opties voor de gebruiker voor de uiteindelijke visualisatie. 
tabSettings <- tabPanel("Settings",icon = icon("gear"),
                        tabsetPanel(type= "tabs",
                                    
                                    # Opties voor de plot toevoegen
                                    tabPanel("Settings plot",
                                             div(style = "margin-top: 20px;",
                                                 fluidRow(
                                                   column(4, pickerInput(
                                                     inputId = "abs_norm", 
                                                     label = "Absolute/normalized counts",
                                                     choices = list("Absolute counts" = "abs", "Normalized counts" = "norm"),
                                                     selected = "abs",
                                                     multiple = FALSE
                                                     ),
                                                     
                                                     pickerInput(
                                                       inputId = "av_ind", 
                                                       label = "Average count/individual samples", 
                                                       choices = list("Average counts" = "av", "Individual samples" = "ind"), 
                                                       multiple = FALSE
                                                       )),
                                                   
                                                   column(4, pickerInput(
                                                     inputId = "MolCheckBox", 
                                                     label = "Select molecule(s)", 
                                                     choices = c(), options = list('actions-box' = TRUE), 
                                                     multiple = TRUE
                                                     ),
                                                     
                                                     materialSwitch(
                                                       inputId = "ShowSingleMolecule",
                                                       label = "Show samples seperated",
                                                       value = FALSE,
                                                       right = TRUE,
                                                       status = "succes")),
                                                   
                                                   column(4, pickerInput(
                                                     inputId = "SamCheckBox", 
                                                     label = "Select sample(s)",
                                                     choices = c(), options = list('actions-box' = TRUE), 
                                                     multiple = TRUE
                                                     ))
                                                   )
                                                 )
                                             ),
                                    
                                    # Plot button
                                    div(style = "position:absolute;right:2em;",
                                        actionButton(inputId = "plot", 
                                                     label = "plot", 
                                                     icon = icon("bar-chart-o"),
                                                     width = 100
                                                     ))
                                    ))