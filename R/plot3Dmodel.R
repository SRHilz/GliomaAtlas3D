#' Plots 3D model
#' 
#' Given a patientID, sf#, and colors, plots model of tumor with samples
#'
#' @param patientID Patient ID in long form (i.e. Patient300 not P300)
#' @param sf sf number with sf lower
#' @param colors vector of colors, usually corresponding to intensity of metric of interest; should be length of number of samples
#' @param tumorModelsPath path to models
#' @examples
#' plot3DModel("Patient300", sf="sf10711", colors=setNames(rep("#FF0000",10), seq(10)))
#' @import misc3d
#' @import rgl
#' @importFrom utils file_test
#'
#' @export plot3DModel
plot3DModel <- function(patientID, sf, colors, tumorModelsPath = system.file(package = "GliomaAtlas3D", "exdata", "models")) {
  stopifnot(file_test("-d", tumorModelsPath))
  # Specify patient and load the config file for that patient. Config file contains paths to imaging files + ordering of samples + sample names + colors
  modelsPath <- file.path(tumorModelsPath,patientID,sf)
  stopifnot(file_test("-d", modelsPath))
  
  # Read in sample models
  sampleCoordinates <- readRDS(file.path(modelsPath, 'coordinates_samples.rds'))
  
  # Read in tumor model for patient
  tumorModel <- readRDS(file.path(modelsPath, 'tumor_t2.rds'))
  
  # Read in brain model for patient
  brainModel <- readRDS(file.path(modelsPath, 'coordinates_brain_periphery.rds'))
  
  # Read in adjustment file
  voxelToMM <- readRDS(file.path(modelsPath, 'adj.rds'))
  
  # Plot background of brain and tumor
  plotTemplate(tumorModel, brainModel, voxelToMM) 
  
  # Plot samples
  plot3DSamples(sampleCoordinates, colors)
}
