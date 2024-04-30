
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



