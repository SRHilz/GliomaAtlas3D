#' Shiny app server object
#'
#' @import shiny
#' @import shinythemes
#' @export 

# create the shiny application user interface for our web app
shinyAppUI <- function(){
  navbarPage("GliomaAtlas3D",
                 loadAboutTab(),
                 loadDataTab(),
                 loadDownloadTab(),
                 theme= shinytheme('flatly')
  )

}
