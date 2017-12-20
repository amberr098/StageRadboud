setTitles <- function(input, output, session, p){
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
  
  # Een value meegeven wanneer er op een button is geklikt voor het alignen.
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
  
  # Het toevoegen van de titel en subtitel wanneer er op de Add button is geklikt. 
  observeEvent(input$AddTitles,{
    # Het krijgen van de waarde voor de face parameter van de ggplot. Achterhalen of de titel/subtitel bold en/of italic moet zijn.
    source("TitleSettings.R")
    typeTitle <- getTypeTitle(input$boldTitle, input$italicTitle)
    typeSubtitle <- getTypeSubtitle(input$boldSubtitle, input$italicSubtitle)
    
    titleAlign <- alignValueTitle$click
    subtitleAlign <- alignValueSubtitle$click
    
    # Toevoegen van de plot aan de webinterface 
    output$Graphic <- renderPlot({
      p + ggtitle(input$titleInput) +
        labs(subtitle = input$subtitleInput) +
        theme(plot.title = element_text(size = input$sizeTitle, hjust = titleAlign, face = typeTitle)) +
        theme(plot.subtitle = element_text(size = input$sizeSubtitle, hjust = subtitleAlign, face = typeSubtitle))+
        coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
    })
  })
}