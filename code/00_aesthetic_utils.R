ThemePlot <- function(plot_obj, scale_colors = colorblind_palette){
  themed_plot <- plot_obj +
    scale_color_manual(values = scale_colors)  +
    theme_minimal()
  return(themed_plot)
}