                                        # Descriptive figures

PlotStackedBarChartByTreatment <- function(bar_data) {
  # Ensure the necessary columns are present
  if (!all(c("Treatment", "Responder", "Count") %in% colnames(bar_data))) {
    stop("Bar_Data must contain 'Treatment', 'Responder', and 'Count' columns.")
  }
  # Define pattern types
  pattern_types <- c(
      "1" = "stripe", "2" = "crosshatch",
      "3" = "circle", "4" = "none")

  # Create the stacked bar chart
    p <- ggplot(bar_data,
                aes(x = Treatment,
                    y = Count,
                    fill = factor(Responder),
                    pattern = factor(Responder))) +
    geom_bar_pattern(stat = "identity", position = "stack",
                     pattern_density = 0.1, pattern_spacing = 0.02,
                     pattern_key_scale_factor = 0.6) +
    ggpattern::scale_pattern_manual(values = pattern_types) +
        labs(x = "Initial Treatment",
             y = "Count",
             fill = "Responder Status",
             pattern = "Responder Status") +
    theme_minimal() +
    theme(legend.position = "right",
          axis.text.x = element_text(angle = 45, hjust = 1))

  return(p)
}


