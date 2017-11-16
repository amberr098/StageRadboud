hideSettings <- function(output){
  output$line <- renderUI({
    NULL
  })
  output$textT <- renderUI({
    NULL
  })
  
  output$textS <- renderUI({
    NULL
  })
  
  output$TitleBar <- renderUI({
    NULL
  })
  output$Subtitle <- renderUI({
    NULL
  })
  
  output$LetterTT <- renderUI({
    NULL
  })
  
  output$LetterTS <- renderUI({
    NULL
  })
  
  output$left <- renderUI({
    NULL
  })
  
  output$center <- renderUI({
    NULL
  })
  
  output$bold <- renderUI({
    NULL
  })
  
  output$italic <- renderUI({
    NULL
  })
  
  output$leftS <- renderUI({
    NULL
  })
  
  output$centerS <- renderUI({
    NULL
  })
  
  output$boldS <- renderUI({
    NULL
  })
  
  output$italicS <- renderUI({
    NULL
  })
}

showSettings <- function(output){
  output$line <- renderUI({
    HTML('<hr style="color: grey;">')
  })
  
  output$textT <- renderUI({
    h4("Title settings")
  })
  
  output$textS <- renderUI({
    h4("Subtitle settings")
  })
  
  output$TitleBar <- renderUI({
    textInput(inputId = "setTitle",
              label = NULL,
              placeholder = "Title...",
              width = "600px")
    
  })
  
  output$LetterTT <- renderUI({
    numericInput("numT", label = NULL, value = 12, width = "200px")
  })
  
  output$left <- renderUI({
    actionBttn(inputId = "alignLeftTitle",
               label = NULL,
               icon = icon("align-left"),
               color = "succes",
               style = "stretch",
               block = TRUE)
  })
  
  
  output$center <- renderUI({
    actionBttn(inputId = "alignCenterTitle",
               label = NULL,
               icon = icon("align-center"),
               color = "succes",
               style = "stretch",
               block = TRUE)
  })
  
  
  output$bold <- renderUI({
    actionBttn(inputId = "LTB",
               label = NULL,
               icon = icon("bold"),
               color = "succes",
               style = "stretch",
               block = TRUE,
               no_outline = FALSE)
  })
  
  output$italic <- renderUI({
    actionBttn(inputId = "LTI",
               label = NULL,
               icon = icon("italic"),
               color = "succes",
               style = "stretch",
               block = TRUE,
               no_outline = FALSE)
  })
  
  output$Subtitle <- renderUI({
    textInput(inputId = "setSubtitle",
              label = NULL,
              placeholder = "Subtitle...",
              width = "600px")
  })
  
  output$LetterTS <- renderUI({
    numericInput("numS", label = NULL, value = 9, width = "200px")
  })
  
  output$leftS <- renderUI({
    actionBttn(inputId = "alignLeftSubtitle",
               label = NULL,
               icon = icon("align-left"),
               color = "succes",
               style = "stretch",
               block = TRUE)
  })
  
  output$centerS <- renderUI({
    actionBttn(inputId = "alignCenterSubtitle",
               label = NULL,
               icon = icon("align-center"),
               color = "succes",
               style = "stretch",
               block = TRUE)
  })
  
  output$boldS <- renderUI({
    actionBttn(inputId = "LTBS",
               label = NULL,
               icon = icon("bold"),
               color = "succes",
               style = "stretch",
               block = TRUE,
               no_outline = FALSE)
  })
  
  output$italicS <- renderUI({
    actionBttn(inputId = "LTIS",
               label = NULL,
               icon = icon("italic"),
               color = "succes",
               style = "stretch",
               block = TRUE,
               no_outline = FALSE)
  })
}