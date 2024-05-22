library(styler)


StyleSpacing <- function(filename) {
    style_lims <- c("spaces", "indention", "line_breaks")
    style_file(filename, scope = I(style_lims), strict = TRUE)
    return(NULL)
}
