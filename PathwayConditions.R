# Ophalen en plaatsen van de condities
setConditions <- function(dataMatrix, input, output, session){
  conditionChoicesUser <- unique(rownames(dataMatrix))
  
  output$choiceCondition1 <- renderUI({
    pickerInput(inputId = "userCondition1", label = "First condition", choices = conditionChoicesUser, multiple = FALSE)
  })
  
  output$choiceCondition2 <- renderUI({
    pickerInput(inputId = "userCondition2", label = "Second condition", choices = conditionChoicesUser, multiple = FALSE)  
  })
  
  output$calculateFoldChange <- renderUI({
    actionButton(inputId = "calcFoldChange", label = "Calculate Fold Change")
  })
  
  observeEvent(input$calcFoldChange,{
    # Wanneer er twee dezelfde condities zijn ingegeven komt er een popup scherm.
    if(input$userCondition1 == input$userCondition2){
      showModal(modalDialog("Select two different conditions", easyClose = TRUE, 
                            footer = tagList(
                              modalButton("OK")
                            )
      )
      )
    }else{
      choicesUser <- c(input$userCondition1, input$userCondition2)
      log2Dataframe <- getConditions(dataMatrix, choicesUser)
      updateTabsetPanel(session, "kegg_tabs", "KEGG identifiers")
    }
  })
  
  
}

# Maken van de matrix met de gekozen condities: wanneer er meerdere dezelfde samples zijn wordt het gemiddelde van dat sample genomen.
getConditions <- function(dataMatrix, choicesUser){
  rown_matrix <- rownames(dataMatrix)
  preCondMatrix <- list()
  
  for(cond in choicesUser){
    row_index_cond <- grep(cond, rown_matrix)
    
    # Als de lengte van row_index_cond groter is dan 1, dan zijn er dus meerdere dezelfde samples waarvan het gemiddelde genomen moet worden
    if(length(row_index_cond) == 1){
      # Geen duplicates
      new_row <- dataMatrix[row_index_cond,]
      
    }else{
      # Hierin komen de gemiddelden te staan die worden toegevoegd als rij aan de matrix
      new_row <- c()
      
      # Gemiddelde nemen van de duplicates
      for(col in 1:ncol(dataMatrix)){
        # Vector van de waarden waarvan het gemiddelde genomen wordt
        vector_average <- c()
        for(row in row_index_cond){
          vector_average <- c(vector_average, dataMatrix[row,col])
        }
        # Gemiddelde nemen van de vector en opslaan in een nieuwe vector.
        average <- mean(vector_average)
        new_row <- c(new_row, average)
      }
    }
    
    preCondMatrix <- rbind(preCondMatrix, new_row)
    
  }
  
  CondMatrix <- as.data.frame(preCondMatrix)
  colnames(CondMatrix) <- colnames(dataMatrix)
  rownames(CondMatrix) <- choicesUser
  
  source("PathwayFoldChange.R")
  getRatio(CondMatrix)
  
}

setConditionsTime <- function(ratios, output){
  conditionChoicesUser <- unique(rownames(ratios))
  
  output$choiceCondition1 <- renderUI({
    pickerInput(inputId = "userCondition1", label = "First condition", choices = conditionChoicesUser, multiple = FALSE)
  })
  
  output$calculateFoldChange <- renderUI({
    actionButton(inputId = "calcFoldChange", label = "Calculate Fold Change")
  })
}