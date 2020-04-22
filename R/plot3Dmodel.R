#' Plots 3D model
#' 
#' Given a patientID, sf#, and colors, plots model of tumor with samples
#'
#' @param patientID Patient ID in long form (i.e. Patient300 not P300)
#' @param sf sf number with sf lower
#' @param colors vector of colors, usually corresponding to intensity of metric of interest; should be lenght of number of samples
#' @param tumorModelsPath path to models
#'
#' @examples
#' plot3Dmodel("Patient300", sf="sf10711", colors=rep("#FF0000", times=10))
#'
#' @return dolphins
#'
#' @import misc3d
#' @import rgl
#' @importFrom utils file_test
#'
#' @export plot3Dmodel
plot3Dmodel <- function(patientID, sf, colors, tumorModelsPath = system.file(package = "GliomaAtlas3D", "exdata", "models")) {
  stopifnot(file_test("-d", tumorModelsPath))
  # Specify patient and load the config file for that patient. Config file contains paths to imaging files + ordering of samples + sample names + colors
  modelsPath <- paste0(tumorModelsPath,'/',patientID,'/',sf)
  stopifnot(file_test("-d", modelsPath))
  
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
