library(reshape2)
library(ggplot2)

individualSamples <- function(plot_data, molecules, samples){
  Row_names <- c()
  # Ophalen van de index van de kolommen Name en Type
  index_Name <- which(plot_data == "Name", arr.ind = TRUE)
  column_Name <- index_Name[1,2]
  index_Type <- which(plot_data == "Type", arr.ind = TRUE)
  column_Type <- index_Type[1,2]
  column_number <- NULL
  
  # Rij namen ophalen, afhankelijk van de kolom Type.
  # if wordt geactiveerd wanneer er geen Types zijn ingevuld, dus alleen de tags er staan als keuze in de Webinterface. 
  if(plot_data[2,column_Type] == "Sample"){
    angle_plot <<- NULL
    column_number <- column_Name
    for(sam_temp in samples){
      sam <- paste0("_", sam_temp)
      index_sam <- which(grepl(sam, plot_data[,column_Name]))
      for (index_row in index_sam) {
        Row_names <- c(Row_names, sam_temp)
      }
    }

    # Else wordt geactiveerd wanneer er wel Types zijn ingevuld en er dus wel samples benoemd staan in de webinterface. 
  }else{
    angle_plot <<- 90
    column_number <- column_Type
    for(sam in samples){
      index_sam <- which(plot_data[,column_Type] == sam, arr.ind = TRUE)
      for (index_row in index_sam) {
        Row_names <- c(Row_names, sam)
      }
    }
  }
  selected_matrix <- matrix(NA, nrow = length(Row_names), ncol = length(molecules))
  colnames(selected_matrix) <- molecules
  rownames(selected_matrix) <- Row_names

  # Als column number gelijk is aan 5 dan zijn er types ingevuld, wanneer column number gelijk is aan 3 zijn er geen types ingevuld. 
  for(sam in samples){
    # Index van de samples ophalen afhankelijk van de Type kolom
    if(column_number == 5){
      index_sam <- which(plot_data[,column_number] == sam)
      
    }else{
      sam_ <- paste0("_",sam)
      index_sam <- which(grepl(sam_, plot_data[,column_Name]))
    }
    
    # Index van de moleculen ophalen
    for (mol in molecules) {
      count <- 0
      selected_mol <- paste(mol, "Results")
      index_mol <- match(selected_mol, colnames(plot_data))
      
      # Waardes ophalen van de geselecteerde data van de gebruiker.
      for (index_row in index_sam) {
  
        value <- plot_data[index_row, index_mol]
        
        col <- match(mol, colnames(selected_matrix))
        row <- match(sam, rownames(selected_matrix))
        
        if(is.na(selected_matrix[row,col])){
          selected_matrix[row,col] <- as.numeric(as.character(value))
        }else{
          count <- count + 1
          selected_matrix[row+count,col] <- as.numeric(as.character(value))
        }
      }
    }
  }
  return(selected_matrix)
}

plotGraph <- function(matrix){
  # Elke rijnaam een unieke naam geven zodat er een dataframe van gemaakt kan worden
  # (dataframe moet unieke rijnamen hebben). Dataframe is nodig voor een grouped barplot. 
  unique_names <- rownames(matrix)

  df <- as.data.frame(matrix)
  rownames(df) <- make.names(unique_names, unique = TRUE)
  columnNames <- colnames(df)

  asColumn <- rownames(df)
  for(i in 1:nrow(df)){
    rownames(df)[i] <- i
  }

  df<-data.frame(Samples = asColumn,df)
  colnames(df) <- c("Samples", columnNames)
 
  df_m <- melt(df, id.vars='Samples')

  max_y <- max(df_m$value)*0.15 + max(df_m$value)
  min_y <- min(df_m$value)*0.15 + min(df_m$value)
  
  if(min_y > 0){
    min_y <- 0
  }
  
  labels_x <- gsub("\\.", " ", df_m$Samples)
  labels_leg <- gsub("\\.", " ", df_m$variable)

  require(scales)
  
  p <- ggplot(df_m, aes(x= Samples, y =value, fill=variable))+
    geom_bar(position = "dodge", stat = "identity") +
    theme(axis.text.x = element_text(angle = angle_plot, hjust = 1, size = 12),
          axis.text.y = element_text(size = 12),
          text = element_text(size = 12),
          legend.text=element_text(size=12),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          legend.title = element_text(size=14)) +
    scale_y_continuous(labels = comma, limits = c(min_y, max_y)) +
    scale_x_discrete(labels = labels_x) +
    scale_fill_discrete(labels = unique(labels_leg), name = "Molecules") + 
    labs(x = "Samples", y = "Values")
  return(p)
}