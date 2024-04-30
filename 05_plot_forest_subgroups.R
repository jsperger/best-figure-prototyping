

#Subgroup forest plot
PlotSubgroupForest <- function(value_data, 
                               segments_data){
  
  
  
  # Create the main plot without y-axis labels and ticks
  main_plot <- ggplot()+
    geom_segment(data = segments_data, aes(y = subgroup, yend = subgroup,
                                           x = personalized_treatments, xend = trial_average,
                                           color = color), size = 1) +
    geom_point(data = value_data, aes(y = subgroup, x = value, 
                                shape = method), size = 3)  +
    scale_color_manual(values = c("Positive" = "#E69F00", "Negative" = "#0072B2"), name = "Difference") +
    # geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper, height = 0.2)) +
    theme_minimal() +
    theme(
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()
    ) +
    labs(x = "Negative Change in 24-week PEG", 
         y = 'Subgroup') + 
    scale_shape_discrete(name = "Method")

  
  # Create the label plot
  label_plot <- ggplot(value_data, aes(y = subgroup, x = 0)) +
    geom_text(aes(label = subgroup), hjust = 0) +
    theme_void() + 
    theme(plot.margin = margin(0,0,0,0)) + 
    labs(y = 'Subgroup')
  
  # Combine the two plots
  combined_plot <- cowplot::plot_grid(label_plot, main_plot, rel_widths = c(1, 2), align = 'h')
  
  # Print the combined plot
  return(combined_plot)
}
