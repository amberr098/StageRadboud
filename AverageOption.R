library(reshape2)
library(ggplot2)

getMatrix <- function(plot_data, molecules, samples){
  pattern_Tag <- "_."
  selected_df <- data.frame()

  av_matrix <- plot_data$average
  sd_matrix <- plot_data$standDev

  # De keuzes die gemaakt worden zijn zonder _, dus _ wordt weggehaald in de rijnamen van de matrixen. 
  index <- 1
  for(name in rownames(av_matrix)){
    if(grepl(pattern_Tag, name) == TRUE){
      tag <- gsub("_", "", name)
      rownames(av_matrix)[index] <- tag
      rownames(sd_matrix)[index] <- tag
      index <- index + 1 
      angle_plot <<- NULL
    }else{
      angle_plot <<- 90
    }
  }
  
  count <- 1
  for(sam in samples){
    row_av <- match(sam, rownames(av_matrix))
    row_sd <- match(sam, rownames(sd_matrix))
    
    for(mol in molecules){
      mol_res <- paste(mol, "Results")
      col_av <- match(mol_res, colnames(av_matrix))
      col_sd <- match(mol_res, colnames(sd_matrix))
      
      selected_df[count,1] <- sam
      selected_df[count,2] <- mol
      selected_df[count,3] <- av_matrix[row_av, col_av]
      selected_df[count,4] <- sd_matrix[row_sd, col_sd]
      count <- count + 1
    }
    
  }

  colnames(selected_df) <- c("Samples", "Molecules", "Average", "SD")
  return(selected_df)
}


plotBar <- function(both_df){
  standDev <- both_df$SD/2 
  max_y <- max(both_df$Average + standDev)*0.15 + max(both_df$Average + standDev)
  min_y <- min(both_df$Average - standDev)*0.15 + min(both_df$Average - standDev)
  
  if(min_y > 0){
    min_y <- 0
  }

  library(scales)
  p <- ggplot(both_df, aes(x = Samples, y = Average, fill = Molecules)) +
    geom_bar(position = position_dodge(), stat = "identity") +
    geom_errorbar(aes(ymin = both_df$Average - standDev, ymax = both_df$Average + standDev), width=.2, position=position_dodge(.9)) + 
    theme(axis.text.x = element_text(angle = angle_plot, hjust = 1, size = 12),
          axis.text.y = element_text(size = 12),
          text = element_text(size = 12),
          legend.text=element_text(size=12),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          legend.title = element_text(size=14)) +
    scale_y_continuous(labels = comma ,limits = c(min_y, max_y))
  return(p)
}