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
                    choices = paste0('Patient',c('260','276','300','303','327','340','372','373','375','413','450','452','453','454','455','457','475','481','482','485')), 
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
        
        # User input - select row if data has more than one 
        uiOutput("rowSelectionUI"),
        
        # Display summary plot centroid
        tags$strong("Relationship with centroid:"), 
        plotOutput('centroidPlot'),
        
        # Display summary plot periphery
        tags$strong("Relationship with periphery:"), 
        plotOutput('peripheryPlot'),
        
        # Display summary plot ventricles
        tags$strong("Relationship with ventricles:"),
        plotOutput('VRPlot'),
        
        # Display data selected (at some point we can make this look a bit nicer and add in sample IDs)
        tags$strong("Data Values:"), # Placeholder
        htmlOutput("units"),
        htmlOutput("data_values") # Placeholder
        
      ),
      
      mainPanel(
        rglwidgetOutput("model3D"),
        img(src = "www/colorbar.png", height=30, width=100, align="center", alt='image failed to load'),
        htmlOutput('colorbartext')
        )
      )
    )
    invisible(dataTab)
}
