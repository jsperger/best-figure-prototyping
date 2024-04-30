
#Define the function to create a Sankey diagram

PlotDTRAssignments <- function(in.data){
 

  #Step needed if we want to group subsequent layers, could be an option in the future?   
  .trt_assignment_df <- in.data %>%
    mutate(stage2_action_allocation = paste(stage1_allocation, stage2_action_allocation, sep = " - "))
   
  # Creating nodes for the Sankey Diagram
  .nodes <- data.frame(name=c(as.character(unique(.trt_assignment_df$stage1_allocation)), 
                             as.character(unique(.trt_assignment_df$stage2_action_allocation))))
  
  # Creating links for the Sankey Diagram
  .links <- .trt_assignment_df %>%
    group_by(stage1_allocation, stage2_action_allocation) %>%
    summarise(count = n(), .groups = 'drop') %>%
    mutate(source = match(stage1_allocation, .nodes$name) - 1, 
           target = match(stage2_action_allocation, .nodes$name) - 1) %>%
    select(source, target, count)
  
  # Creating the Sankey Diagram
  sankey <- sankeyNetwork(Links = .links, Nodes = .nodes, Source = "source",
                          Target = "target", Value = "count", NodeID = "name",
                          units = "TWh", fontSize = 12, nodeWidth = 30)
  
  # Print the Sankey diagram
  return(sankey)
  
}



