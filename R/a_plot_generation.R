#' Lollipop plot for variable importance of regression coefficients
PlotPredPubr <- function(
  in_data,
  tx_label = NULL,
  num_predictors = 10,
  drop_intercept = TRUE
) {
  checkmate::assertTRUE(rlang::is_installed("ggpubr"))

  response_label <- .CreateLabels(names(
    workflows::extract_mold(in_data)$outcomes
  ))

  model_coefs <- broom::tidy(in_data)

  lambda_pen <- rlang::quo_get_expr(
    in_data$fit$actions$model$spec[1]$args$penalty
  )

  alpha_mix <- rlang::quo_get_expr(
    in_data$fit$actions$model$spec[1]$args$mixture
  )

  model_type_label <- bquote(paste(
    "Magnitude (Elastic Net Coefficient; ",
    lambda,
    "= ",
    .(lambda_pen),
    ", ",
    alpha,
    "= ",
    .(alpha_mix),
    ")"
  ))

  if (alpha_mix == 0) {
    model_type_label <- bquote(paste(
      "Magnitude (Ridge Coefficient; ",
      lambda,
      "= ",
      .(lambda_pen),
      ")"
    ))
  }
  if (alpha_mix == 1) {
    model_type_label <- bquote(paste(
      "Magnitude (Lasso Coefficient; ",
      lambda,
      "= ",
      .(lambda_pen),
      ")"
    ))
  }

  plot_label <- ifelse(
    test = is.null(tx_label),
    yes = glue::glue("Effect on {response_label}"),
    no = glue::glue("Predictors of {response_label} on {tx_label}")
  )

  renamed_data <- .PlotStripVarPrefixes(model_coefs)

  plot_data <- renamed_data |>
    .RenameTidyCoefsForPlotting(
      drop_intercept = TRUE
    ) |>
    dplyr::arrange(desc(Magnitude)) |>
    dplyr::slice_head(n = num_predictors) |>
    dplyr::mutate(SignSymbol = ifelse(Sign == "Positive", "+", "−")) |>
    dplyr::arrange(desc(abs(Magnitude)))

  # Create the plot
  pred_summary_plot <- ggpubr::ggdotchart(
    plot_data,
    x = "Predictor",
    y = "Magnitude",
    color = "Sign", # Color by groups
    palette = c("#E69F00", "#0072B2", "#D55E00"), # Custom color palette
    sorting = "descending", # Sort value in descending order
    add = "segments", # Add segments from y = 0 to dots
    add.params = list(linetype = plot_data$Sign),
    rotate = TRUE, # Rotate vertically
    dot.size = 3, # Large dot size
    label = "SignSymbol", # Add dot labels
    font.label = list(color = "white", size = 12, vjust = 0.37), # Adjust label
    ggtheme = ggpubr::theme_pubr() # ggplot2 theme
  ) +
    ggplot2::labs(
      y = model_type_label,
      x = NULL,
      color = glue::glue("Effect on {response_label}"),
      linetype = glue::glue("Effect on {response_label}"),
      subtitle = plot_label
    ) + # Label axes
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 8, angle = 30, hjust = 1),
      axis.text.x = ggplot2::element_text(size = 8),
      legend.position = "bottom"
    ) # Smaller text, angled

  return(pred_summary_plot)
}

#' dot plot for effect of predictor on treatment
PlotValueComparison <- function(in_data) {
  # Sort data by the point estimate in descending order for plotting
  in_data <- in_data %>%
    dplyr::mutate(
      TreatmentPolicy = factor(TreatmentPolicy, levels = TreatmentPolicy)
    ) %>%
    dplyr::arrange(PointEstimate) %>%
    dplyr::mutate(
      TreatmentPolicy = factor(TreatmentPolicy, levels = TreatmentPolicy)
    )

  # Create the ggplot
  p <- ggplot2::ggplot(
    in_data,
    ggplot2::aes(
      x = PointEstimate,
      y = TreatmentPolicy,
      color = TreatmentPolicy
    )
  ) +
    ggplot2::geom_point(size = 4) + # Plot the point estimates
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = LowerCI, xmax = UpperCI),
      height = 0
    ) +
    # Adding curves to simulate parentheses at each end
    #geom_curve(data = in_data, aes(x = LowerCI, y = TreatmentPolicy, xend = LowerCI - 0.05, yend = TreatmentPolicy), curvature = -0.5, size = 0.5, arrow = arrow(type = "closed", length = unit(2, "mm"))) +
    #geom_curve(data = in_data, aes(x = UpperCI, y = TreatmentPolicy, xend = UpperCI + 0.05, yend = TreatmentPolicy), curvature = 0.5, size = 0.5, arrow = arrow(type = "closed", length = unit(2, "mm")))+# Add horizontal error bars for the confidence intervals
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
      plot.margin = ggplot2::margin(
        t = 3.5,
        r = 0.5,
        b = 2.5,
        l = 0.5,
        unit = "cm"
      )
    ) # Adjust plot margins)

  # Return the ggplot object
  return(p)
}

#' sankey diagram of DTR assignments
PlotDTRAssignments <- function(
  in_data,
  group_augment = F
) {
  #Step needed if we want to group subsequent layers, could be an option in the future?
  if (group_augment) {
    .trt_assignment_df <- in_data %>%
      mutate(
        stage2_action_allocation = paste(
          stage1_allocation,
          stage2_action_allocation,
          sep = " - "
        )
      )
  } else {
    .trt_assignment_df <- in_data
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
  .links <- bind_rows(
    .trt_assignment_df %>%
      count(stage1_allocation, stage2_action_allocation) %>%
      rename(source = stage1_allocation, target = stage2_action_allocation),
    .trt_assignment_df %>%
      count(stage2_action_allocation, stage2_treatment_allocation) %>%
      rename(
        source = stage2_action_allocation,
        target = stage2_treatment_allocation
      )
  ) %>%
    rename(count = n) %>%
    mutate(
      source = match(source, .nodes$name) - 1,
      target = match(target, .nodes$name) - 1
    ) %>%
    select(source, target, count)

  # Creating the Sankey Diagram
  sankey <- sankeyNetwork(
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

GGBeanPlot <- function(
  in_data,
  response_label,
  tx_timepoint_name,
  response_timepoint_name,
  violin_quantiles = NULL
) {
  names(in_data) = .CreateLabels(names(in_data))

  outcome_vals <- in_data |>
    dplyr::select(paste(response_timepoint_name, response_label, "Score"))
  min_outcome <- min(outcome_vals)
  max_outcome <- max(outcome_vals)

  if (tx_timepoint_name == "Baseline") {
    Trt <- in_data |> dplyr::select("Treatment Initial")
  }
  if (tx_timepoint_name == "Week 12") {
    Trt <- in_data |> dplyr::select("Treatment Week 12")
  }

  data <- tibble::tibble(
    Trt = tibble::deframe(Trt),
    outcome_vals = tibble::deframe(outcome_vals)
  )

  base_plot <- ggplot2::ggplot(
    ggplot2::aes(x = factor(Trt), y = outcome_vals),
    data = data
  )

  bean_plot <- base_plot +
    ggplot2::geom_violin(draw_quantiles = violin_quantiles) +
    ggplot2::geom_jitter(height = 0.25, width = 0.25) +
    ggplot2::scale_y_continuous(
      limits = c(min_outcome - 1, max_outcome + 1),
      breaks = seq(floor(min_outcome) - 1, ceiling(max_outcome) + 1)
    ) +
    ggplot2::labs(
      x = "Assigned Treatment",
      y = paste(response_timepoint_name, response_label, "Score"),
      title = paste(
        names(outcome_vals),
        "by",
        tx_timepoint_name,
        "Treatment Assignment"
      )
    ) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::theme_bw()

  return(bean_plot)
}

PerformancePlot <- function(
  in_data,
  metric
) {
  bp <- workflowsets::autoplot(in_data, metric = metric)
  metric_name <- NULL
  if (metric == "rmse") {
    metric_name <- "RMSE"
  }
  if (metric == "rsq") {
    metric_name <- "R-Squared"
  }
  perf_plot <- bp +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(paste0("Ordered Model Performance by ", metric_name)) +
    ggplot2::xlab("Rank") +
    ggplot2::ylab(metric_name)

  return(perf_plot)
}
