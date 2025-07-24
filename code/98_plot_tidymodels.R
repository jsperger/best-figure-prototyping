# This file contains plotting functions that are compliant with the `tidymodels`
# ecosystem. The functions are designed to be generic and work with the
# standardized outputs of `broom::tidy()` and `recipes::tidy()`.

.PlotPredSlice <- function(in.data, num_predictors, grouping_var = NULL) {
  top_pred_data <- in.data %>%
    dplyr::group_by(Treatment) %>%
    dplyr::top_n(num_predictors, abs(Magnitude)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(Treatment, desc(abs(Magnitude))) %>%
    dplyr::mutate(Predictor = factor(Predictor, levels = unique(Predictor)))

  if (!is.null(grouping_var)) {
    top_pred_data <- top_pred_data %>%
      dplyr::arrange(Predictor, !!rlang::sym(grouping_var)) %>%
      dplyr::mutate(
        Predictor = factor(
          paste(Predictor, ":", !!rlang::sym(grouping_var)),
          levels = unique(paste(Predictor, ":", !!rlang::sym(grouping_var)))
        )
      )
  }

  return(top_pred_data)
}

#' @title Plot Predictor Lollipop Chart
#' @description Create a lollipop chart for variable importance.
#' @param object A data frame containing the predictor importance data.
#' @param num_predictors The number of top predictors to display.
#' @param grouping_var A character string specifying the grouping variable.
#' @return A ggplot object.
#' @export
autoplot.predictor_importance <- function(
  object,
  num_predictors = 5,
  grouping_var = NULL
) {
  # Filter and prepare data
  in.data <- .PlotPredSlice(
    in.data = object,
    num_predictors = num_predictors,
    grouping_var = grouping_var
  )

  # Define a set of shapes for the grouping variable levels
  shape_levels <- levels(as.factor(in.data[[grouping_var]]))
  shape_values <- c(16, 17, 18, 15, 4, 8, 3, 7, 12) # Predefined shapes, add more if needed

  # Create the lollipop chart
  p <- ggplot2::ggplot(in.data, ggplot2::aes(x = Magnitude, y = Predictor, group = Predictor)) +
    ggplot2::geom_segment(
      ggplot2::aes(xend = 0, yend = Predictor, color = Sign, linetype = Sign),
      size = .7
    ) +
    ggplot2::geom_point(ggplot2::aes(shape = !!rlang::sym(grouping_var), color = Sign), size = 2) +
    ggplot2::scale_shape_manual(
      values = setNames(shape_values[1:length(shape_levels)], shape_levels)
    ) +
    ggplot2::scale_linetype_manual(
      values = c("Positive" = "solid", "Negative" = "dashed")
    ) +
    ggplot2::facet_wrap(~Treatment, scales = "free_y") +
    ggplot2::labs(
      x = "Magnitude of Effect (z-score)",
      y = NULL,
      color = "Effect on \n Expected Treatment Efficacy",
      linetype = "Effect on \n Expected Treatment Efficacy",
      shape = paste("Levels of", grouping_var)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 0, hjust = 1))

  return(p)
}

#' @title Plot Value Comparison
#' @description Create a dot plot to compare treatment policy values.
#' @param object A data frame containing the value comparison data.
#' @return A ggplot object.
#' @export
autoplot.value_comparison <- function(object) {
  # Sort data by the point estimate in descending order for plotting
  in.data <- object %>%
    dplyr::mutate(
      TreatmentPolicy = factor(TreatmentPolicy, levels = TreatmentPolicy)
    ) %>%
    dplyr::arrange(PointEstimate) %>%
    dplyr::mutate(TreatmentPolicy = factor(TreatmentPolicy, levels = TreatmentPolicy))

  # Create the ggplot
  p <- ggplot2::ggplot(
    in.data,
    ggplot2::aes(x = PointEstimate, y = TreatmentPolicy, color = TreatmentPolicy)
  ) +
    ggplot2::geom_point(size = 4) + # Plot the point estimates
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = LowerCI, xmax = UpperCI), height = 0) +
    ggplot2::scale_x_continuous(limits = c(0, NA)) + # Set the x-axis limits
    ggplot2::labs(
      x = "Expected Reduction in 24-week PEG",
      y = "Treatment Policy",
      color = "Treatment\nPolicy"
    ) + # Label the axes
    ggplot2::theme_minimal() + # Use a minimal theme
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 10), # Increase font size for y axis texts
      axis.text.x = ggplot2::element_text(size = 12), # Increase font size for x axis texts
      panel.spacing.y = ggplot2::unit(0.5, "lines"), # Adjust spacing between panels +
      axis.ticks.length.y = ggplot2::unit(0.5, "cm"),
      plot.margin = ggplot2::margin(t = 3.5, r = 0.5, b = 2.5, l = 0.5, unit = "cm")
    ) # Adjust plot margins)

  # Return the ggplot object
  return(p)
}

#' @title Plot DTR Assignments
#' @description Create a Sankey diagram to visualize DTR assignments.
#' @param object A data frame containing the DTR assignment data.
#' @param group_augment A logical indicating whether to group subsequent layers.
#' @return A Sankey diagram object.
#' @export
autoplot.dtr_assignments <- function(object, group_augment = FALSE) {
  #Step needed if we want to group subsequent layers, could be an option in the future?
  if (group_augment) {
    .trt_assignment_df <- object %>%
      dplyr::mutate(
        stage2_action_allocation = paste(
          stage1_allocation,
          stage2_action_allocation,
          sep = " - "
        )
      )
  } else {
    .trt_assignment_df <- object
  }

  # Creating nodes for the Sankey Diagram
  .nodes <- data.frame(
    name = c(
      as.character(unique(.trt_assignment_df$stage1_allocation)),
      as.character(unique(.trt_assignment_df$stage2_action_allocation)),
      as.character(unique(.trt_assignment_df$stage2_treatment_allocation))
    )
  )
  # browser()
  # Creating links for the Sankey Diagram
  .links <- dplyr::bind_rows(
    .trt_assignment_df %>%
      dplyr::count(stage1_allocation, stage2_action_allocation) %>%
      dplyr::rename(source = stage1_allocation, target = stage2_action_allocation),
    .trt_assignment_df %>%
      dplyr::count(stage2_action_allocation, stage2_treatment_allocation) %>%
      dplyr::rename(
        source = stage2_action_allocation,
        target = stage2_treatment_allocation
      )
  ) %>%
    dplyr::rename(count = n) %>%
    dplyr::mutate(
      source = match(source, .nodes$name) - 1,
      target = match(target, .nodes$name) - 1
    ) %>%
    dplyr::select(source, target, count)

  # Creating the Sankey Diagram
  sankey <- networkD3::sankeyNetwork(
    Links = .links,
    Nodes = .nodes,
    Source = "source",
    Target = "target",
    Value = "count",
    NodeID = "name",
    units = "TWh",
    fontSize = 12,
    nodeWidth = 30
  )

  # Print the Sankey diagram
  return(sankey)
}

#' @title Plot Subgroup Forest
#' @description Create a forest plot to visualize subgroup treatment effects.
#' @param object A data frame containing the subgroup value data.
#' @param segments_data A data frame containing the segments data.
#' @return A ggplot object.
#' @export
autoplot.subgroup_forest <- function(object, segments_data) {
  # Create the main plot without y-axis labels and ticks
  main_plot <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = segments_data,
      ggplot2::aes(
        y = subgroup,
        yend = subgroup,
        x = personalized_treatments,
        xend = trial_average,
        color = color
      ),
      size = 1
    ) +
    ggplot2::geom_point(
      data = object,
      ggplot2::aes(y = subgroup, x = value, shape = method),
      size = 3
    ) +
    ggplot2::scale_color_manual(
      values = c("Positive" = "#E69F00", "Negative" = "#0072B2"),
      name = "Difference"
    ) +
    # geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper, height = 0.2)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    ) +
    ggplot2::labs(x = "Negative Change in 24-week PEG", y = 'Patient Characteristic') +
    ggplot2::scale_shape_discrete(name = "Method")

  # Create the label plot
  label_plot <- ggplot2::ggplot(object, ggplot2::aes(y = subgroup, x = 0)) +
    ggplot2::geom_text(ggplot2::aes(label = subgroup), hjust = 0) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0)) +
    ggplot2::labs(y = 'Patient Characteristic')

  # Combine the two plots
  combined_plot <- cowplot::plot_grid(
    label_plot,
    main_plot,
    rel_widths = c(1, 2),
    align = 'h'
  )

  # Print the combined plot
  return(combined_plot)
}
