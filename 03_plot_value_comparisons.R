

# Define the function to create the dot plot
PlotValueComparison<- function(in.data) {
  # Sort data by the point estimate in descending order for plotting
  in.data <- in.data %>%
    mutate(TreatmentPolicy = factor(TreatmentPolicy, levels = TreatmentPolicy)) %>%
    arrange(PointEstimate) %>%
    mutate(TreatmentPolicy = factor(TreatmentPolicy, levels = TreatmentPolicy))

  # Create the ggplot
  p <- ggplot(in.data, aes(x = PointEstimate, y = TreatmentPolicy, color = TreatmentPolicy)) +
    geom_point(size = 4) +  # Plot the point estimates
    geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), height = 0) +
    # Adding curves to simulate parentheses at each end
    #geom_curve(data = in.data, aes(x = LowerCI, y = TreatmentPolicy, xend = LowerCI - 0.05, yend = TreatmentPolicy), curvature = -0.5, size = 0.5, arrow = arrow(type = "closed", length = unit(2, "mm"))) +
    #geom_curve(data = in.data, aes(x = UpperCI, y = TreatmentPolicy, xend = UpperCI + 0.05, yend = TreatmentPolicy), curvature = 0.5, size = 0.5, arrow = arrow(type = "closed", length = unit(2, "mm")))+# Add horizontal error bars for the confidence intervals
    scale_x_continuous(limits = c(0, NA)) +  # Set the x-axis limits
    labs(x = "Expected Reduction in 24-week PEG", y = "Treatment Policy", color = "Treatment\nPolicy") +  # Label the axes
    theme_minimal() +  # Use a minimal theme
    theme(axis.text.y = element_text(size = 10),  # Increase font size for y axis texts
          axis.text.x = element_text(size = 12),# Increase font size for x axis texts
          panel.spacing.y = unit(0.5, "lines"),  # Adjust spacing between panels +
          axis.ticks.length.y = unit(0.5, "cm"),
          plot.margin = margin(t = 3.5, r = 0.5, b = 2.5, l = 0.5, unit = "cm"))  # Adjust plot margins)

  # Return the ggplot object
  return(p)
}
