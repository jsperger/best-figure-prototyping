#' Creates an example plot where the treatment effect of one treatment
#' diminishes with age, while the other increases with age, but the first
#' treatment is always better regardless of age.
CreateExDTRRespDifPlot <- function() {
 # Create a data frame with the necessary data
age <- seq(18, 90, by = 1)
acupuncture <- seq(2.5, 1.5, length.out = length(age))
tcm <- seq(0.25, 1.25, length.out = length(age))

plot_data <- data.frame(
  Age = rep(age, 2),
  Expected_PEG_Reduction = c(acupuncture, tcm),
  Treatment = rep(c("Acupuncture", "Meditation"), each = length(age))
)

# Create the plot
pmed_plot <- ggplot(
    data = plot_data,
    aes(x = Age, y = Expected_PEG_Reduction, color = Treatment)) +
  geom_line(linewidth = 1.2) +
  scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, by = 1)) +
  scale_x_continuous(limits = c(18, 90), breaks = seq(18, 90, by = 12)) +
  labs(
    subtitle =
      "Why both treatment recommendations and response predictions are needed",
    x = "Age",
    y = "Expected PEG Reduction",
    color = "Treatment Group"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
  )

    return(pmed_plot)
}


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
