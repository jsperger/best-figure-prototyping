# Descriptive figures

BeanPlotOutcomeByTreatment <- function(wide_data, outcome_name, color_vec) {
  outcome_vals <- wide_data %>% pull(outcome_name)

  bp_formula <- as.formula(paste0(outcome_name, " ~ Trt"))
  bp <- beanplot::beanplot(bp_formula,
    data = wide_data,
    cutmin = min(outcome_vals),
    cutmax = max(outcome_vals),
    method = "jitter",
    col = color_vec[1:4]
  )

  return(bp)
}

GGBeanPlot <- function(wide_data, outcome_name, timepoint_name = "12-week",
                       violin_quantiles = NULL) {
  outcome_vals <- wide_data %>% pull(outcome_name)
  min_outcome <- min(outcome_vals)
  max_outcome <- max(outcome_vals)

  base_plot <- ggplot(aes(x = factor(Trt), y = .data[[outcome_name]]), data = wide_data)

  bean_plot <- base_plot +
    geom_violin(draw_quantiles = violin_quantiles) +
    geom_jitter(height = 0.25, width = 0.25, ) +
    scale_y_continuous(
      limits = c(min_outcome, max_outcome),
      breaks = seq(min_outcome, max_outcome, by = 1)
    ) + labs(x = "Assigned Treatment", y = paste(timepoint_name, outcome_name)) +
    theme(panel.grid.minor = element_blank())

  return(bean_plot)
}

PlotStackedBarChartByTreatment <- function(bar_data) {
  # Ensure the necessary columns are present
  if (!all(c("Treatment", "Responder", "Count") %in% colnames(bar_data))) {
    stop("Bar_Data must contain 'Treatment', 'Responder', and 'Count' columns.")
  }
  # Define pattern types
  pattern_types <- c(
    "1" = "stripe", "2" = "crosshatch",
    "3" = "circle", "4" = "none"
  )

  # Create the stacked bar chart
  p <- ggplot(
    bar_data,
    aes(
      x = Treatment,
      y = Count,
      fill = factor(Responder),
      pattern = factor(Responder)
    )
  ) +
    geom_bar_pattern(
      stat = "identity", position = "stack",
      pattern_density = 0.1, pattern_spacing = 0.02,
      pattern_key_scale_factor = 0.6
    ) +
    ggpattern::scale_pattern_manual(values = pattern_types) +
    labs(
      x = "Initial Treatment",
      y = "Count",
      fill = "Responder Status",
      pattern = "Responder Status"
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  return(p)
}
