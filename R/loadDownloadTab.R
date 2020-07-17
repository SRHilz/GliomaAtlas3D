#' UI-elements for data tab
#'
#' @import shiny
#' @import rgl
#' @importFrom utils packageVersion
#' @export 

loadDownloadTab <- function(){
  downloadTab <- tabPanel(title = "Download", id = "download",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          # User input - select patient subset
                          checkboxGroupInput("patientSubsetDownload", 
                                             label = "Filter patients by tumor type", 
                                             choices = list("IDH-mut" = 1, 
                                                            "IDH-wt" = 2,
                                                            "codel" = 3,
                                                            "non-codel" = 4,
                                                            "grade II or III" = 5,
                                                            "grade IV" = 6,
                                                            "newly diagnosed" = 7,
                                                            "recurrent" = 8),
                                             selected = c(1,2,3,4,5,6,7,8)),
                          
                          # User input - select dataset
                          uiOutput("datasetDownloadUI"),
                          
                          # User input - select type ID (type of histological data)
                          uiOutput("typeDownloadUI"),
                          
                          # User input - select copy number threshold
                          uiOutput("thresholdDownloadUI"),
                          
                          # User input - select row if data has more than one 
                          uiOutput("rowSelectionDownloadUI"),
                          
                          downloadButton("downloadData", "Download"),
                          
                          checkboxInput("tumorMetadataDownload","I'd like to also download tumor-level metadata", FALSE)
                          
                        ),
                        
                        mainPanel(
                          # Display data selected to download
                          tableOutput("tableDownload")
                        )
                      ),
  )
  invisible(downloadTab)
}