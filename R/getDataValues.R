#' Retrieve the values for a particular patients dataset
#' 
#' Creates a vector of final data values for a particular patient, tumor, and dataset
#' @param patient Patient ID in long form (i.e. Patient300 not P300)
#' @param tumor sf number with sf lower
#' @param dataset name of dataset (i.e. Histology, Amplification, Copy Number, etc)
#' @param type only required if dataset is Histology
#' @param gene only required if dataset is Copy Number or RNAseq
#' @param threshold only required if dataset is Amplificaiton
#' @param conversion converts dataset to name of data file, where value is dataset and name is file name (ex c(cn.rds='Copy Number', ))
#' @export 

getDataValues <- function(patient, tumor, dataset, type, gene, threshold, conversion, tumorDatasetsPath){
  if (dataset=="Histology"){
    if (type=='BV Hyperplasia'){
      fname <- 'bv_hyper.rds'
    } else {
      fname <- 'per_nec.rds'
    }
  } else {
    fname <-  names(conversion[which(conversion==dataset)])
  }
  data <- readRDS(paste0(tumorDatasetsPath,'/', patient, '/', tumor, '/', fname))#data has rownames=gene names and colnames=sample names of format PNNNvN
  if (dataset=='Amplification'){
    data <- cn_to_amp(data, threshold)
  }
  if (is.null(dim(data))){ # Handling purity & histology datasets (vector instead of dataframe)
    vector <- as.numeric(data)
  } else { # All other datasets
    vector <- as.numeric(data[gene,])
  }
  names(vector) <- gsub('P[0-9]{3}v', '', names(data))
  return(vector)
}