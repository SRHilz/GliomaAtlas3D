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
                 options(shiny.sanitize.errors = TRUE),
                 tags$head(tags$style(type="text/css",".shiny-output-error{visibility: hidden; }")),
                 tags$head(tags$style(".shiny-output-error:before{content: 'Loading data...';visibility: visible; }")),
                 theme= shinytheme('flatly')
  )

}
