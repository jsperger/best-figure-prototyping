



PlotTreatSqn.BZ <- function(in.data) {
  
  # Create a summary data frame with count and proportion for each treatment sequence
  dtr_summary <- in.data %>%
    group_by(stage1_allocation, stage2_action_allocation, stage2_treatment_allocation) %>%
    summarise(
      count = n(),
      proportion = n() / nrow(in.data),
      .groups = "drop"
    ) %>%
    ungroup() %>%
    mutate(
      sequence = paste(stage1_allocation, stage2_action_allocation, stage2_treatment_allocation, sep = " -> "),
      label = paste0(sequence, ", N = ", count, " (", scales::percent(proportion), ")")
    )
  
  # Define the desired order for Stage 1 and Stage 2 treatments
  desired_order <- c("S1: ESC", "S1: EBEM", "S1: Duloxetine", "S1: ACT")
  desired_order_stage2 <- c("S2: ESC", "S2: EBEM", "S2: Duloxetine", "S2: ACT")
  
  # Calculate the total count and proportion of each Stage 1 treatment
  stage1_stats <- dtr_summary %>%
    group_by(stage1_allocation) %>%
    summarise(
      total_count = sum(count),
      total_proportion = round(sum(proportion),digits=2)
    ) %>%
    ungroup()
  
  # Merge the Stage 1 statistics with the original data
  dtr_summary_merged <- left_join(dtr_summary, stage1_stats, by = "stage1_allocation")
  
  # Update y-axis labels to include total count and rounded proportion
  dtr_summary_merged <- dtr_summary_merged %>%
    mutate(
      stage1_allocation = factor(stage1_allocation, levels = desired_order),
      stage2_treatment_allocation = factor(stage2_treatment_allocation, levels = desired_order_stage2),
      stage1_allocation_label = paste(stage1_allocation, " (Total: ", total_count, ", ", round(total_proportion * 100), "%)", sep = "")
    )
  
  # Update y-axis labels to include total count and proportion
  dtr_summary_merged <- dtr_summary_merged %>%
    mutate(stage1_allocation_label = paste(stage1_allocation, "\n",
                                           " (Total: ", total_count, ", ", scales::percent(total_proportion), ")", sep = ""))
  
  # Create a horizontal bar chart with counts on the x-axis
  ggplot(dtr_summary_merged, aes(x = count, y = stage1_allocation_label, total_count, fill = stage2_treatment_allocation)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(aes(label = paste0(stage2_action_allocation, ": ", "\n",count, " (", scales::percent(proportion, accuracy = 0.1), ")")),
              position = position_stack(vjust = 0.5),
              color = "white", size = 3, fontface = "bold") +
    scale_fill_brewer(type = "qual", palette = "Set2") +
    labs(
      # title = "Treatment Sequences",
         x = "Number of Participants", y = "Stage 1 Treatment") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5),
      axis.title.y = element_text(face = "bold", hjust = 0.5),
      axis.text.y = element_text(face = "bold", hjust = 0.5),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank()
    ) +
    guides(fill = guide_legend(title = "Stage 2 Treatment"))
}







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