
#Define the function to create a Sankey diagram

PlotDTRAssignments <- function(in.data, 
                               group_augment = F){
 

  #Step needed if we want to group subsequent layers, could be an option in the future?  
  if(group_augment){
    .trt_assignment_df <- in.data %>%
      mutate(stage2_action_allocation = paste(stage1_allocation, stage2_action_allocation, sep = " - "))
  } else {
    .trt_assignment_df <- in.data
  }
  
   
  # Creating nodes for the Sankey Diagram
  .nodes <- data.frame(name=c(as.character(unique(.trt_assignment_df$stage1_allocation)), 
                             as.character(unique(.trt_assignment_df$stage2_action_allocation)), 
                             as.character(unique(.trt_assignment_df$stage2_treatment_allocation))))
  # browser()
  # Creating links for the Sankey Diagram
  .links <- bind_rows(
    .trt_assignment_df %>% 
      count(stage1_allocation, stage2_action_allocation) %>% 
      rename(source = stage1_allocation, 
             target = stage2_action_allocation), 
    .trt_assignment_df %>% 
      count(stage2_action_allocation, stage2_treatment_allocation)%>% 
      rename(source = stage2_action_allocation, 
             target = stage2_treatment_allocation)
  ) %>% 
    rename(count = n) %>%
    mutate(source = match(source, .nodes$name) - 1,
           target = match(target, .nodes$name) - 1) %>%
    select(source, target, count)
  
  # Creating the Sankey Diagram
  sankey <- sankeyNetwork(Links = .links, Nodes = .nodes, Source = "source",
                          Target = "target", Value = "count", NodeID = "name",
                          units = "TWh", fontSize = 12, nodeWidth = 30)
  
  # Print the Sankey diagram
  return(sankey)
  
}



### Define the function to create a stacked bar chart for treatment sequence from stage 1 to stage ### 

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

