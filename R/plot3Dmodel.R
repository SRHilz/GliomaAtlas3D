#' Plots 3D model
#' 
#' Given a patientID, sf#, and colors, plots model of tumor with samples
#'
#' @import misc3d
#' @import rgl
#' @param patientID Patient ID in long form (i.e. Patient300 not P300)
#' @param sf sf number with sf lower
#' @param colors vector of colors, usually corresponding to intensity of metric of interest; should be lenght of number of samples
#' \donttest{@examples
#' plot3Dmodel('Patient300','sf10711',rep('#FF0000',10))}
#' @return dolphins
#' @export plot3Dmodel

plot3Dmodel <- function(patientID, sf, colors, tumorModelsPath){
  # Specify patient and load the config file for that patient. Config file contains paths to imaging files + ordering of samples + sample names + colors
  modelsPath <- paste0(tumorModelsPath,'/',patientID,'/',sf)
  
  # Read in sample models
  sampleCoordinates <- readRDS(paste0(modelsPath, '/coordinates_samples.rds'))
  
  # Read in tumor model for patient
  tumorModel <- readRDS(paste0(modelsPath, '/tumor_t2.rds'))
  
  # Read in brain model for patient
  brainModel <- readRDS(paste0(modelsPath, '/brain.rds'))
  
  # Plot background of brain and tumor
  plotTemplate(tumorModel, brainModel) 
  
  # Plot samples
  plot3dSamples(sampleCoordinates, colors)
}
