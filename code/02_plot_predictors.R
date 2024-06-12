colorblind_palette  <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
            "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#'
PlotPredictorSummary <- function(in.data, num_predictors = 5) {

    in.data <- .PlotPredSlice(in.data = in.data, num_predictors = num_predictors)

    ## Creating a long format for plotting
  data_long <- in.data %>%
    pivot_longer(cols = c("Magnitude", "Uniqueness"), names_to = "Metric", values_to = "Value") %>%
    mutate(Offset = ifelse(Metric == "Uniqueness", -0.3, 0))  # Add offset for uniqueness

  Offset <- 0.3  # Offset for the lollipop sticks
  # Create the plot
  pred_summary_plot <- ggplot(data_long, aes(y = Predictor, x = Value, group = interaction(Predictor, Metric))) +
    geom_segment(aes(yend = as.numeric(Predictor) + Offset, xend = 0), color = "grey50", size = 0.5) +  # Draw the lollipop sticks with offset
    geom_point(aes(color = ifelse(Metric == "Magnitude", Sign, NA)), size = 3, position = position_nudge(y = Offset)) +  # Plot points with offset
    facet_wrap(~Treatment, scales = "free_x") +  # Separate plot for each treatment, horizontally
    labs(x = "Value", y = NULL) +  # Label axes
    scale_color_manual(values = c("Positive" = "blue", "Negative" = "red"), na.value = "black") +  # Define colors, default black for uniqueness
    theme_minimal() +  # Minimal theme
    theme(axis.text.y = element_text(angle = 0, hjust = 1))  # Text alignment

  return(pred_summary_plot)
}

#' @title
PlotPredImportance <- function(in.data, num_predictors = 5) {
    in.data <- .PlotPredSlice(in.data = in.data, num_predictors = num_predictors) %>%
      arrange(desc(abs(Magnitude)))

      # Create the plot
  pred_summary_plot <- ggplot(in.data, aes(y = Predictor, x = Magnitude, group = interaction(Predictor, Magnitude))) +
    geom_segment(aes(xend = 0, yend = Predictor, color = Sign, linetype = Sign), size = .5) +
    geom_point(aes(shape = Sign, color = Sign), size = 1) +
    facet_wrap(~Treatment, scales = "free_x") +  # Separate plot for each treatment, horizontally
    labs(x = "Magnitude of Effect", y = NULL,
         color = "Effect on \nTreatment Efficacy",
    linetype = "Effect on \nTreatment Efficacy",
         shape = "Effect on \nTreatment Efficacy") +  # Label axes
    theme(axis.text.y = element_text(angle = 0, hjust = 1))  # Text alignment

  return(pred_summary_plot)
}

#' @title
PlotPredPubr <- function(in.data, num_predictors = 5) {
  in.data <- .PlotPredSlice(in.data = in.data, num_predictors = num_predictors) %>%
    mutate(SignSymbol = ifelse(Sign == "Positive", "+", "-")) %>%
    arrange(desc(abs(Magnitude)))

  # Create the plot
  pred_summary_plot <- ggdotchart(in.data, x = "Predictor", y = "Magnitude",
                                  color = "Sign",                                # Color by groups
                                  palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
                                  sorting = "descending",                       # Sort value in descending order
                                  add = "segments",                             # Add segments from y = 0 to dots
                                  add.params = list(linetype = in.data$Sign),
                                  rotate = TRUE,                                # Rotate vertically
                                  group = "Treatment",                                # Order by groups
                                  dot.size = 3,                                 # Large dot size
                                  label = "SignSymbol",                        # Add dot labels
                                  font.label = list(color = "white", size = 10, vjust = 0.37),               # Adjust label parameters
                                  ggtheme = theme_pubr()                        # ggplot2 theme
  ) +
    scale_y_continuous(limits = c(0, 1.5), breaks = c(0, .5, 1, 1.5), labels = c("0", "0.5", "1", "1.5")) +  # Set the x-axis limits
    facet_grid(~Treatment) +
    labs(y = "Magnitude of Effect", x = NULL,
         color = "Effect on Treatment Efficacy",
         linetype = "Effect on Treatment Efficacy",
         label = "Effect on Treatment Efficacy") +  # Label axes
    theme(axis.text.y = element_text(size = 8, angle = 30, hjust = 1),
          axis.text.x = element_text(size = 8),
    legend.position = "bottom")  # Smaller text, angled

  return(pred_summary_plot)
}


# Define the function to create a lollipop chart for variable importance
PlotPredictorLollipop <- function(in.data, num_predictors = 5) {
  # Filter and prepare data
  in.data <- .PlotPredSlice(in.data = in.data, num_predictors = num_predictors)

  # Create the lollipop chart
  p <- ggplot(in.data, aes(x = Magnitude, y = Predictor, group = Predictor)) +
    geom_segment(aes(xend = 0, yend = Predictor, color = Sign, linetype = Sign), size = .7) +
    geom_point(aes(shape = Sign, color = Sign), size = 2) +
    scale_shape_manual(values = c("Positive" = 16, "Negative" = 4)) +  # Circle for positive, X for negative
    scale_linetype_manual(values = c("Positive" = "solid", "Negative" = "dashed")) +
    facet_wrap(~Treatment, scales = "free_y") +
    labs(x = "Magnitude of Effect", y = NULL,
         color = "Effect on \nTreatment Efficacy",
         linetype = "Effect on \nTreatment Efficacy",
         shape = "Effect on \nTreatment Efficacy",)

  return(p)
}




.PlotPredSlice <- function(in.data, num_predictors, grouping_var = NULL) {
  top_pred_data <- in.data %>%
          group_by(Treatment) %>%
          top_n(num_predictors, abs(Magnitude)) %>%
          ungroup() %>%
      arrange(Treatment, desc(abs(Magnitude))) %>%
      mutate(Predictor = factor(Predictor, levels = unique(Predictor)))

  if (!is.null(grouping_var)) {
                top_pred_data <- top_pred_data %>%
    arrange(Predictor, !!sym(grouping_var)) %>%
    mutate(Predictor = factor(paste(Predictor, ":", !!sym(grouping_var)),
      levels = unique(paste(Predictor, ":", !!sym(grouping_var)))))
        }

        return(top_pred_data)
}

PlotPredictorLollipopGrouped <- function(in.data, num_predictors = 5, grouping_var = NULL) {
  # Filter and prepare data
  in.data <- .PlotPredSlice(in.data = in.data, num_predictors = num_predictors, grouping_var = grouping_var)

  # Define a set of shapes for the grouping variable levels
  shape_levels <- levels(as.factor(in.data[[grouping_var]]))
  shape_values <- c(16, 17, 18, 15, 4, 8, 3, 7, 12)  # Predefined shapes, add more if needed

  # Create the lollipop chart
  p <- ggplot(in.data, aes(x = Magnitude, y = Predictor, group = Predictor)) +
    geom_segment(aes(xend = 0, yend = Predictor, color = Sign, linetype = Sign), size = .7) +
    geom_point(aes(shape = !!sym(grouping_var), color = Sign), size = 2) +
    scale_shape_manual(values = setNames(shape_values[1:length(shape_levels)], shape_levels)) +
    scale_linetype_manual(values = c("Positive" = "solid", "Negative" = "dashed")) +
    facet_wrap(~Treatment, scales = "free_y") +
    labs(x = "Magnitude of Effect (z-score)", y = NULL,
         color = "Effect on \n Expected Treatment Efficacy",
         linetype = "Effect on \n Expected Treatment Efficacy",
         shape = paste("Levels of", grouping_var)) +
    theme_minimal() +
    theme(axis.text.y = element_text(angle = 0, hjust = 1))

  return(p)
}






### Define the function to create a plot showing top Standardized Effect Size for Patient Feature  ### 

PlotVarImportance.BZ <- function(in.data, num_predictors = 5) {
  
  in.data <- .PlotPredSlice(in.data = in.data, num_predictors = num_predictors) %>%
    mutate(SignSymbol = ifelse(Sign == "Positive", "+", "-")) %>%
    arrange(desc(abs(Magnitude)))
  
  # Assuming your data is in a data frame
  plot.data <- in.data %>%
    mutate(
      Magnitude = ifelse(Sign == "Negative", -Magnitude, Magnitude),
      Treatment = factor(Treatment, levels = c("ESC", "EBEM", "ACT", "Duloxetine")),
      Predictor = factor(Predictor, levels = unique(Predictor)))
  
  
  # Create the variable importance plot
  p <- ggplot(plot.data, aes(x = Magnitude, y = Predictor
                             , color = Treatment
                             # , shape = Treatment
                             # , linetype = Treatment
  )
  ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    # geom_text(aes(label = TreatmentAbbrev), hjust = 1.5, vjust = 1.5, size=2) +
    # geom_text(aes(label = Treatment), hjust = 1.5, vjust = 1.5, size=2) +
    geom_segment(aes(x = 0, xend = Magnitude, yend = Predictor)
                 # , arrow = arrow(length = unit(0.15, "cm"))
                 # , size = 1
                 # ,position = position_dodge(width = 0.8)
                 , color = "black"
    ) +
    geom_point(size = 3
               , shape = 19
               # , position = position_dodge(width = 0.8)
    ) +
    # scale_color_manual(values = c("ESC (1)" = "red", "EBEM (2)" = "blue", "ACT (3)" = "green", "Duloxetine (4)" = "orange"),
    #                    name = "Treatment") +
    # scale_shape_manual(values = c("ESC" = 4, "EBEM" = 9, "ACT" = 13, "Duloxetine" = 0),
    #                    name = "Treatment") +
    # scale_linetype_manual(values = c("ESC" = "solid", "EBEM" = "dashed", "ACT" = "dotted", "Duloxetine" = "twodash"),
    #                       name = "Treatment") +
    labs(
      # title = "Variable Importance Plot",
      x = "Standardized Effect Size", y = "Patient Feature") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position = "bottom",
          axis.text.y = element_text(size = 10))
  
  return(p)
  
}