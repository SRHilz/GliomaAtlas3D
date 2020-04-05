#' launches the GLiomaAtlas3D app
#'
#' @export launchApp
#'
#' @return shiny application object
#'
#' \donttest{@examples launchApp()}
#'
#' @import shiny
#'


# wrapper for shiny::shinyApp()
launchApp <- function() {
  shinyApp(ui = shinyAppUI, server = shinyAppServer)
}