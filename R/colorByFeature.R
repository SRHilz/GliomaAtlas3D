#' Map numeric values to a blue-to-red color gradient
#' 
#' Takes a vector of values and returns an analogous vector of colors from a heatmap gradient of the vector values
#' @import RColorBrewer
#' @import grDevices
#' @param vector a vector of numeric values with sample names
#' @export 

colorByFeatureMain <- function(vector){
  rbPal <- colorRampPalette(c("blue","red"))
  if (length(unique(vector)) > 1){
    mappedColors <- rbPal(length(vector))[as.numeric(cut(vector,breaks = length(vector)))][1:length(vector)]
  } else {
    mappedColors <- rep('blue', length(vector))
  }
  names(mappedColors) <- names(vector)
  return(mappedColors)
}


