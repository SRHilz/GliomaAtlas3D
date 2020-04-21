#' launches the GLiomaAtlas3D app
#'
#' @export launchApp
#'
#' @return shiny application object
#'
#' @examples \donttest{
#' if (interactive()) launchApp()
#' }
#'
#' @import shiny
#'


# wrapper for shiny::shinyApp()
launchApp <- function() {
  shinyApp(ui = shinyAppUI, server = shinyAppServer)
}