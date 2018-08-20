error_plot <- function(){
  library(jpeg)
  #Change img directory to actual location of error message
  img <- readJPEG("shiny_code_repo/plotting_scripts/error.jpg")
  g <- rasterGrob(img, interpolate=TRUE)
  error_obj<- ggplot(data.frame(matrix(c(1,10,1,10), nrow = 2, ncol = 2)))+ 
    annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
    geom_text(x = .5, y = .5, label = "Missing Data", size = 20)
  return(error_obj)
}
# 
# display_plot <- function(){
#   library(jpeg)
#   #Change img directory to actual location of error message
#   img <- readJPEG("www/error.jpg")
#   g <- rasterGrob(img, interpolate=TRUE)
#   error_obj<- ggplot(data.frame(matrix(c(1,10,1,10), nrow = 2, ncol = 2)))+ 
#     annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
#     geom_text(x = .5, y = .5, label = "An error has occured. Please contact the SPL Data Coordinators.")
#   return(error_obj)
# }