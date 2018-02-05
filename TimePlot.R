library(ggplot2)
getPlot <- function(selected_dataframe){
  colnames(selected_dataframe) <- c("Samples", "Time", "Molecule", "Variant", "Average", "SD", "Half SD")
  
  time_index_col <- which(colnames(selected_dataframe) == "Time")
  x_axis_times <- unlist(unique(selected_dataframe[,time_index_col]))

  # De types meegeven van de gegevens in de tabel: as.numeric, as.character
  selected_data <- data.frame(Samples = as.character(selected_dataframe$Samples),
                              Time = as.numeric(selected_dataframe$Time),
                              Molecule = as.character(selected_dataframe$Molecule),
                              Variant = as.character(selected_dataframe$Variant),
                              Average = as.numeric(selected_dataframe$Average),
                              SD = as.numeric(selected_dataframe$SD),
                              Half_SD = as.numeric(selected_dataframe$`Half SD`))

  # Plot maken van de selected_data
  p <- ggplot(selected_data, aes(x = Time, 
                                 y = Average, 
                                 group = interaction(Samples, Variant),
                                 colour = Variant, 
                                 linetype = Samples, 
                                 ymin = Average-Half_SD, 
                                 ymax = Average+Half_SD))+
    geom_line()+
    geom_point()+
    geom_errorbar(width=0.1)+
    facet_wrap(~Molecule, scales = "free")+
    scale_x_continuous(breaks = x_axis_times)
  return(p)
}
