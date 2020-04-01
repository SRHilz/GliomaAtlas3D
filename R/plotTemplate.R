#' Plots background tumor and brain template
#' 
#' Given xyz binary masks of the tumor and brain, plots the background 3d model on which samples can later be added
#' @import misc3d
#' @import rgl
#' @param tumorModel xyz binary mask originally derived from dicoms of tumor
#' @param brainModel xyz binary mask originally derived from dicoms of brain
#' @export 

plotTemplate <- function(tumorModel, brainModel){
  dtemp <- dim(tumorModel)
  print('Creating brain contour and plotting brain')
  brain <- contour3d(brainModel, x=1:dtemp[1], y=1:dtemp[2], z=1:dtemp[3], level = 1, alpha = .05, add = FALSE, draw = TRUE, color = '#B08883')
  print('Creating tumor contour and plotting tumor')
  tumor <- contour3d(tumorModel, x=1:dtemp[1], y=1:dtemp[2], z=1:dtemp[3], level = 1, alpha = .2, add = TRUE, draw = TRUE, color = 'yellow')
}


