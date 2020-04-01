#' Plots samples within a 3D model
#' 
#' Given a vector of named sample coordinates and colors, first determines if any samples are missing color info, then plots samples in an existing 3D model
#' @import misc3d
#' @import rgl
#' @param sampleCoordinates Coordinates of samples with columns x y and z, each row is a sample with a row name
#' @param colors vector of colors, usually corresponding to intensity of metric of interest; should be lenght of number of samples and also have names
#' @export 

plot3dSamples <- function(sampleCoordinates, colors){
  sampleColors <- c()
  for (name in rownames(sampleCoordinates)){
    if(!is.na(colors[as.character(name)])){
      singleColor <- colors[name]
    } else {
      singleColor <- '#a7a457'
      print(name)
    }
    sampleColors <- append(sampleColors, singleColor)
  }
  points3d(x=sampleCoordinates[,1], y=sampleCoordinates[,2], z=sampleCoordinates[,3], level = 1, size = 7, color=sampleColors)
  text3d(x=sampleCoordinates[,1], y=sampleCoordinates[,2], z=sampleCoordinates[,3], texts = rownames(sampleCoordinates), cex=1, adj=-.3)
}