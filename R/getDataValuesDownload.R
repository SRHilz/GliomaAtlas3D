#' Retrieve the values for a particular patients dataset
#' 
#' Creates a vector of final data values for a particular patient, tumor, and dataset
#' @param patientsFinal Patient ID in long form (i.e. Patient300 not P300)
#' @param sfConversion Coverts patientID to sf# in a one-to-one relationship
#' @param datasetD name of dataset (i.e. Histology, Amplification, Copy Number, etc)
#' @param typeD only required if dataset is Histology
#' @param rowSelectionD only required if dataset is Copy Number, RNAseq, Cell Types, Cancer-associated Processes, or Expansions
#' @param thresholdD only required if dataset is Amplification
#' @param conversion converts dataset to name of data file, where value is dataset and name is file name (ex c(cn.rds='Copy Number', ))
#' @param tumorDatasetsPath path to datasets
#' @param sampleData contains spatial information about each sample
#' @export 

getDataValuesDownload <- function(patientsFinal, sfConversion, datasetD, typeD, rowSelectionD, thresholdD, conversion, tumorDatasetsPath, sampleData){
  if (datasetD=="Histology"){
    if (typeD=='BV Hyperplasia'){
      fname <- 'bv_hyper.rds'
    } else {
      fname <- 'per_nec.rds'
    }
  } else {
    fname <-  conversion[[datasetD]][1]
  }
  units <- gsub(' ','_',conversion[[datasetD]][2])
  toReturn <- data.frame(patient=character(), tumor=character(), sample=character(), value=character(), stringsAsFactors=F)
  for (patient in patientsFinal){
    tumor <- as.character(sfConversion[patient])
    dataPath <- file.path(tumorDatasetsPath, patient, tumor, fname)
    if (file.exists(dataPath)){
      data <- readRDS(dataPath)#data has rownames=gene names and colnames=sample names of format PNNNvN
      if (datasetD=='Amplification'){
        data <- cn_to_amp(data, thresholdD)
      }
      if (is.data.frame(data)){ # Handling purity & histology datasets (vector instead of dataframe)
        vector <- as.numeric(data[rowSelectionD,])
        valueName <- paste0(gsub(' ','_',datasetD),'_',rowSelectionD)
      } else { # All other datasets
        vector <- as.numeric(data)
        valueName <- paste0(gsub(' ','_',datasetD))
      }
      SampleName <- gsub('P[0-9]{3}v', '', names(data))
      spatialDataTumor <- sampleData[which(tolower(sampleData$SFNumber) == tumor),c('SampleName','L.Coordinate','P.Coordinate','S.Coordinate','DistCentroid','DistPeriph','DistVR')]
      spatialDataTumor$SampleName <- gsub('v', '', spatialDataTumor$SampleName)
      toMerge <- data.frame(cbind(patient, tumor, SampleName, vector), stringsAsFactors = F)
      toBind <- merge(toMerge, spatialDataTumor, by='SampleName')
      toBind <- toBind[order(toBind$SampleName),]
      toReturn <- rbind(toReturn, toBind)
    }
  }
  
  toReturn <- toReturn[,c('patient','tumor','SampleName','vector','L.Coordinate','P.Coordinate','S.Coordinate','DistCentroid','DistPeriph','DistVR')]
  colnames(toReturn) <- c('Patient','Tumor','Sample',valueName,'L.Coordinate','P.Coordinate','S.Coordinate','DistCentroid','DistPeriph','DistVR')
    
  return(toReturn)
}