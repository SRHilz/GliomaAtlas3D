#' launches the GLiomaAtlas3D app
#'
#' @export launchApp
#'
#' @return shiny application object
#'
#' @examples \donttest{launchApp()}
#'
#' @import shiny
#'


# wrapper for shiny::shinyApp()
launchApp <- function() {
  shinyApp(ui = shinyAppUI, server = shinyAppServer)
}