# Deze tab bevat de opties voor de gebruiker voor de uiteindelijke visualisatie. 
# De uiOutputs worden toegevoegd in het bestand SetSettings.R 
tabSettings <- tabPanel("Settings",icon = icon("gear"),
                        tabsetPanel(type= "tabs",
                                    
                                    # Opties voor de plot toevoegen
                                    tabPanel("Settings plot",
                                             div(style = "margin-top: 20px;",
                                                 fluidRow(
                                                   
                                                   # Deze optie is zowel voor de steady state en time plot. 
                                                   # Keuze voor absolute en genormaliseerde waarden
                                                   column(4, uiOutput("abs_norm_option"),
                                                     
                                                     # Alleen een optie voor de steady state plot.
                                                     # Keuze uit de average waarden of de individuele waarden.
                                                     uiOutput("av_ind_option"),
                                                     
                                                     # Deze optie is alleen voor de steady state en is pas van toepassing
                                                     # wanneer de switch (switchShowSeperate) is geactiveerd.
                                                     # Kan gekozen worden voor dezelfde schaal voor alle plotjes of verschillende schalen. 
                                                     uiOutput("set_y_axis_scale"),
                                                     
                                                     # Alleen een optie bij de Time plot wanneer de keuze tussen absolute
                                                     # en genormaliseerde waarde (abs_norm_option) de absolute waarden zijn. 
                                                     uiOutput("switchNormalisation")
                                                     ),
                                                   
                                                   # Een optie voor zowel de steady state als time plot
                                                   # Hier komen alle moleculen te staan waar de gebruiker uit kan kiezen voor de plot.
                                                   column(4, uiOutput("select_molecules_option"),
                                                     
                                                     # Een switch die alleen een optie is bij de steady state plot
                                                     # Hierbij kan de keuze gemaakt worden voor alle moleculen in 1 plot 
                                                     # of elk molecuul in een aparte plot
                                                     uiOutput("switchShowSeperate")),
                                                   
                                                   # Optie voor steady state en time plot waarin alle samples staan waaruit
                                                   # de gebruiker kan kiezen.
                                                   column(4, uiOutput("select_samples_option"))
                                                   )
                                                 )
                                             ),
                                    
                                    # Plot button waardoor de Resultaten tab wordt gevisualiseerd.
                                    div(style = "position:absolute;right:2em;",
                                        uiOutput("plot_Button"))
                                    ))