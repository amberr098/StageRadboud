typeTitle <- NULL
typeSubtitle <- NULL
titleAlign <- NULL
subtitleAlign <- NULL

setGraph <- function(input, output, selected_matrix){
  # Ophalen van de plot op basis van de switch: per molecuul 1 plot of alle moleculen in 1 plot.
  if(input$ShowSingleMolecule == FALSE){
    # Basis plot zonder de titel en subtitel erbij
    source("Visualization.R")
    p <- setOnePlot(selected_matrix, input$av_ind)
    
  }else{
    # Basis plot zonder de titel en subtitel erbij
    yscl <- input$scale_y_axis
    source("Visualization.R")
    p <- setMultiplePlots(selected_matrix, input$av_ind, yscl)
    
  }
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  # De output wanneer er ingezoomed wordt. 
  output$Graphic <- renderPlot(height="auto", {
    p + coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
  })
  
  # Wanneer er een vak word geselecteerd in de plot en er twee keer op geklikt wordt
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
    }else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  # Ophalen van de opties voor het alignen
  alignValueTitle <- reactiveValues(click = NULL)
  
  observeEvent(input$leftTitle, {
    alignValueTitle$click <- 0
  })
  
  observeEvent(input$centerTitle, {
    alignValueTitle$click <- 0.5
  })
  
  alignValueSubtitle <- reactiveValues(click = NULL)
  
  observeEvent(input$leftSubtitle, {
    alignValueSubtitle$click <- 0
  })
  
  observeEvent(input$centerSubtitle, {
    alignValueSubtitle$click <- 0.5
  })
  
  observeEvent(input$AddTitles,{
    source("TitleSettings.R")
    typeTitle <<- getTypeTitle(input$boldTitle, input$italicTitle)
    typeSubtitle <<- getTypeSubtitle(input$boldSubtitle, input$italicSubtitle)
    
    titleAlign <<- alignValueTitle$click
    subtitleAlign <<- alignValueSubtitle$click
    
    output$Graphic <- renderPlot({
      plotInput()
    })
  })
  
  plotInput <<- function(){
    # Door de observeEvent van de black/white checkbox worden de titles ook op de zwart/witte plot geplaatst. 
    observeEvent(input$black_white, {
      if(input$black_white == TRUE){
        output$Graphic <- renderPlot({
          p + ggtitle(input$titleInput) +
            labs(subtitle = input$subtitleInput) +
            theme(plot.title = element_text(size = input$sizeTitle, hjust = titleAlign, face = typeTitle)) +
            theme(plot.subtitle = element_text(size = input$sizeSubtitle, hjust = subtitleAlign, face = typeSubtitle))+
            coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
            scale_fill_grey()
        })
      }else{
        output$Graphic <- renderPlot({
          p + ggtitle(input$titleInput) +
            labs(subtitle = input$subtitleInput) +
            theme(plot.title = element_text(size = input$sizeTitle, hjust = titleAlign, face = typeTitle)) +
            theme(plot.subtitle = element_text(size = input$sizeSubtitle, hjust = subtitleAlign, face = typeSubtitle))+
            coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
        })
      }
    })
  }
  
  # Ook de plot maken wanneer er niet op de "Add" button geklikt wordt. 
  plotInput()
}