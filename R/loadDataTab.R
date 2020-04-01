#' UI-elements for data tab
#'
#' @import shiny
#' @import rgl
#' @export 

loadDataTab <- function(){
    addResourcePath('www',system.file('shinyApp/www',package = "GliomaAtlas3D"))  
    dataTab <- tabPanel(title = "Explore", id = "data",
           
    sidebarLayout(
      sidebarPanel(
        
        # User input - select patient
        selectInput("patient",
                    label = "Patient",
                    choices = paste0('Patient',c('260','300','303','327','340','372','373','375','413','450','452','453','454','455','457')), 
                    selected = "Patient327"),
        # CN instead of "Copy Number" bc of rds naming convention (need to change later)
        
        # User input - select tumor
        uiOutput("tumorUI"),
        
        # User input - select dataset
        uiOutput("datasetUI"),
        
        # User input - select type ID (type of histological data)
        uiOutput("typeUI"),
        
        # User input - select copy number threshold
        uiOutput("thresholdUI"),
        
        # User input - select gene
        uiOutput("geneUI"),
        
        # Display data selected (at some point we can make this look a bit nicer and add in sample IDs)
        tags$strong("Data Values:"), # Placeholder
        htmlOutput("units"),
        htmlOutput("data_values") # Placeholder
      ),
      
      mainPanel(rglwidgetOutput("model3D"),
                img(src = "www/colorbar.png", height="15%", width="15%", align="center"),
                htmlOutput('colorbartext'))
    )
    
  )
    return(dataTab)
}
