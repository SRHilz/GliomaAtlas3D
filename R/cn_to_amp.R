#' Converts continuous cn values to binary amp or not based on threshold
#' 
#' Takes a vector of cn values and a threshold, determines if values are over threshold and thus if amplification is present
#' @param cn_df a data frame of copy number values across samples
#' @param threshold a numeric value, the threshold over which you consider something to be amplified
#' @export 

cn_to_amp <- function(cn_df, threshold){
  # Binarizes copy number matrix, if TCN > threshold 
  amp_df <- apply(cn_df, c(1, 2), function(x) ifelse(x > threshold, 1, 0))
  return(amp_df)
}