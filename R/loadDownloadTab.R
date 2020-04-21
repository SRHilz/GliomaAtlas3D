#' UI-elements for about tab
#'
#' @import shiny
#' @export 

loadDownloadTab <- function(){
  downloadTab <- tabPanel(title = 'Download', id = 'download', 
           titlePanel("Coming soon!")
           )
  return(downloadTab)
}
