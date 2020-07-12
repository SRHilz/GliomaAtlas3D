#' UI-elements for data tab
#'
#' @import shiny
#' @import rgl
#' @importFrom utils packageVersion
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
        uiOutput("rowSelectionUI")
        
        # Display data selected (kept here for troubleshooting, handy to have if need to actually see values)
        #tags$strong("Data Values:"), # Placeholder
        #htmlOutput("data_values") # Placeholder
        
      ),
      
      mainPanel(
        fluidPage(
          fluidRow(
            column(8, offset=0, style='padding:0px;', wellPanel(
              tags$strong(tags$span(style="color:grey","Interactive 360° visualization (click and drag to rotate)")),
              rglwidgetOutput("model3D"),
              style = "background: white"
              )),
            column(4, align='center', fixedPanel(
              img(src = "www/colorbar.png", height=30, width=100, align="center", alt='image failed to load'),
              htmlOutput('colorbartext'),
              htmlOutput("units"),
              style = "background: white"
            ))
          ),
          fluidRow(
            column(4, offset=0, style='padding:0px;', align='center', wellPanel(
              # Display summary plot centroid
              tags$strong(tags$span(style="color:grey","Relationship with tumor centroid")), 
              plotOutput('centroidPlot', height="150pt", width="130pt"),
              style = "background: white"
              )),
            column(4, offset=0, style='padding:0px;', align='center', wellPanel(
              # Display summary plot periphery
              tags$strong(tags$span(style="color:grey","Relationship with tumor periphery")), 
              plotOutput('peripheryPlot', height="150pt", width="130pt"),
              style = "background: white"
            )),
            column(4, offset=0, style='padding:0px;', align='center', wellPanel(
              # Display summary plot ventricles
              tags$strong(tags$span(style="color:grey","Relationship with brain center")),
              plotOutput('VRPlot', height="150pt", width="130pt"),
              style = "background: white"
            ))
            )
          )
        )
      ),
    options(shiny.sanitize.errors = TRUE),
    tags$head(tags$style(type="text/css",".shiny-output-error{visibility: hidden; }")),
    tags$head(tags$style(".shiny-output-error:before{content: 'Loading data...';visibility: visible; }"))
    )
    invisible(dataTab)
}
