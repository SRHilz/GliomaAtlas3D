#' Plots background tumor and brain template
#' 
#' Given xyz binary masks of the tumor and brain, plots the background 3d model on which samples can later be added
#' @import misc3d
#' @import rgl
#' @param tumorModel xyz binary mask originally derived from dicoms of tumor
#' @param brainModel xyz binary mask originally derived from dicoms of brain
#' @param voxelToMM from adj.rds, number of mm per voxel for x, y, and z
#' @export 

plotTemplate <- function(tumorModel, brainModel, voxelToMM){
  dtemp <- dim(tumorModel)
  maximumDimension <- max(dtemp)
  dtempScaled <- dtemp/maximumDimension # the aspect that fits the original voxel matrix, scaled to 1
  aspectInMM <- dtempScaled * c(voxelToMM$x, voxelToMM$y, voxelToMM$z) # and this then converts the voxel dimensions to mm
  message('Creating brain contour and plotting brain')
  plot3d(brainModel, alpha=0.01, col='#726665', axes=F,  xlab = "", ylab = "", zlab= "", aspect = aspectInMM)
  message('Creating tumor contour and plotting tumor')
  tumor <- contour3d(tumorModel, x=1:dtemp[1], y=1:dtemp[2], z=1:dtemp[3], level = 1, alpha = .2, add = TRUE, draw = TRUE, color = 'yellow')
}


