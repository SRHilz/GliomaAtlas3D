#' Creates a dataframe of available data for each patient
#' 
#' Looks through all files in the dataset path and creates a dataframe containing the datasets that exist for each tumor
#' @import gtools
#' @param datasets_path path to where datasets are located 
#' @export 

getDatasets <- function(datasets_path){
  tumor_datasets <- data.frame(patient = c(), sf = c())
  patients <- list.files(file.path(datasets_path))
  for (p in patients){
    sfnums <- list.files(file.path(datasets_path, p))
    for (sf in sfnums){
      datasets <- list.files(file.path(datasets_path, p, sf))
      temp <- data.frame(patient = c(p), sf = c(sf))
      temp2 <- data.frame(t(rep(1, length(datasets))))
      colnames(temp2) <- datasets
      tumor_datasets <- smartbind(tumor_datasets, cbind(temp, temp2))
    }
  }
  return(tumor_datasets)
}