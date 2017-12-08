setSettingsStSt <- function(output){
  output$abs_norm_option <- renderUI({
    pickerInput(
      inputId = "abs_norm", 
      label = "Absolute/normalized counts",
      choices = list("Absolute counts" = "abs", "Normalized counts" = "norm"),
      selected = "abs",
      multiple = FALSE
    )
  })

  output$av_ind_option <- renderUI({
    pickerInput(
      inputId = "av_ind",
      label = "Average count/individual samples",
      choices = list("Average counts" = "av", "Individual samples" = "ind"),
      multiple = FALSE
    )
  })

  output$switchShowSeperate <- renderUI({
    materialSwitch(
      inputId = "ShowSingleMolecule",
      label = "Show molecules seperated",
      value = FALSE,
      right = TRUE,
      status = "succes")
  })

  
  output$plot_Button <- renderUI({
    actionButton(inputId = "plot", 
                 label = "plot", 
                 icon = icon("bar-chart-o"),
                 width = 100
    )
  })
}

setSettingsTime <- function(output){
  ######### nog hetzelfde as setSettingsStSt ########## 
  
  output$abs_norm_option <- renderUI({
    pickerInput(
      inputId = "abs_norm_time", 
      label = "Absolute/normalized counts",
      choices = list("Absolute counts" = "abs", "Normalized counts" = "norm"),
      selected = "abs",
      multiple = FALSE
    )
  })
  
  output$av_ind_option <- renderUI({
    pickerInput(
      inputId = "av_ind_time",
      label = "Average count/individual samples",
      choices = list("Average counts" = "av", "Individual samples" = "ind"),
      multiple = FALSE
    )
  })
  
  output$switchShowSeperate <- renderUI({
    materialSwitch(
      inputId = "ShowSingleMolecule_time",
      label = "Show molecules seperated",
      value = FALSE,
      right = TRUE,
      status = "succes")
  })
  
  
  output$plot_Button <- renderUI({
    actionButton(inputId = "plot_time", 
                 label = "plot", 
                 icon = icon("bar-chart-o"),
                 width = 100
    )
  })

}