#' Shiny app server function
#' @import graphics
#' @import stats
#' @export
#' @param input provided by shiny
#' @param output provided by shiny
#'

# Define server logic required to generate 3D glioma visualizations
shinyAppServer <- function(input, output){
  
  datasetConversion <- list(RNAseq = c('rna.rds','Counts per million'),
                            `Tumor Cell Proportion` =  c('purity.rds','Proportion of cells'),
                            `Copy Number` = c('cn.rds','Number of copies'),
                            Amplification = c('cn.rds','Amplified at threshold (0=no, 1=yes'),
                            `Percent Necrosis` = c('per_nec.rds', '% of tissue with necrosis'),
                            `BV Hyperplasia` = c('bv_hyper.rds','Score (0=none, 1=mild, 2=extensive)'),
                            Histology = c('Histology','NA'),
                            `Cell Types` = c('celltypes.rds','enrichment score'),
                            `Cancer Processes` = c('cancerprocesses.rds','enrichment score'),
                            `Expansion Events` = c('expansions.rds','mean VAF'))
  # define paths
  root <- system.file(package = "GliomaAtlas3D", "exdata")
  sampleDataPath <-file.path(root, "metadata","sampledata_v11.rds")
  tumorDataPath <-file.path(root, "metadata","tumordata_v11.rds")
  tumorDatasetsPath <- file.path(root,"datasets")
  tumorModelsPath <- file.path(root,"models")
  
  # read in data to be used globally
  sampleData <- readRDS(sampleDataPath)
  tumorData <- readRDS(tumorDataPath)
  tumorDatasets <- getDatasets(tumorDatasetsPath)
  
  output$tumorUI <- renderUI({
    if (is.null(input$patient))
      return()
    
    sfNums <- tumorDatasets[tumorDatasets$patient==input$patient, 'sf']
    switch(input$patient, selectInput("tumor", "Tumor", choices = sfNums, selected = sfNums[1]))
  })
  
  output$datasetUI <- renderUI({
    availableDatasetFiles <- colnames(tumorDatasets[which(tumorDatasets[which(tumorDatasets$patient==input$patient),]==1)])
    availableDatasets <- names(datasetConversion)[unlist(lapply(datasetConversion, function(x) x[[1]] %in% availableDatasetFiles))]
    if ('Copy Number' %in% availableDatasets){
      availableDatasets <- append(availableDatasets, 'Amplification')
    }
    if ('Percent Necrosis' %in% availableDatasets | 'BV Hyperplasia' %in% availableDatasets){
      availableDatasets <- append(availableDatasets[!availableDatasets %in% c('Percent Necrosis','BV Hyperplasia')], 'Histology')
    }
    availableDatasets <- availableDatasets[order(availableDatasets)]
    switch(input$patient, selectInput("dataset", "Dataset", choices = availableDatasets, selected='Tumor Cell Proportion'))
  })
  
  output$typeUI <- renderUI({
    if (input$dataset!="Histology")
      return()
    
    switch(input$dataset,
           "Histology" = selectInput("type", "Type", choices = c("Percent Necrosis", "BV Hyperplasia"), 
                                     selected = "Percent Necrosis")
    )
  })
  
  output$thresholdUI <- renderUI({
    if (input$dataset!="Amplification"){
      return()
    } 
      switch(input$dataset, sliderInput("threshold", "Threshold", min = 0, max = 15, value = 5, step = 0.1)
    )
  })
  
  output$rowSelectionUI <- renderUI({
    if (is.null(input$tumor))
      return()
    
    if (input$dataset=="Histology"){
      fname <- datasetConversion[[input$type]][1]
    } else {
      fname <- datasetConversion[[input$dataset]][1]
    }
    data <- readRDS(paste0(tumorDatasetsPath,'/', input$patient, '/', input$tumor, '/', fname))
    if (input$dataset %in% c('Tumor Cell Proportion', 'Histology')){ # Don't need to select gene for purity or histology
      return()
    } else if (input$dataset %in% c('Cell Types')) {
      switch(input$tumor, selectInput("rowSelection", "Cell Type", choices = rownames(data), selected = rownames(data)[1]))
    } else if (input$dataset %in% c('Cancer Processes')) {
      switch(input$tumor, selectInput("rowSelection", "Cancer Process", choices = rownames(data), selected = rownames(data)[1]))
    } else if (input$dataset %in% c('Expansion Events')){
      switch(input$tumor, selectInput("rowSelection", "Expansion Event", choices = rownames(data), selected = rownames(data)[1]))
    } else {
      switch(input$tumor, selectInput("rowSelection", "Gene", choices = rownames(data), selected = rownames(data)[1]))# for RNAseq, Amplification, CN
    }
  })
  
  dataValues <- reactive({
    getDataValues(input$patient, input$tumor, input$dataset, input$type, input$rowSelection, input$threshold, datasetConversion, tumorDatasetsPath)
  })
  
  output$units <- renderUI({
    if (input$dataset=="Histology"){
      unitsForData <- datasetConversion[[input$type]][2]
    } else {
      unitsForData <- datasetConversion[[input$dataset]][2]
    }
    HTML(paste0('<i>',as.character(unitsForData),'</i>'))
  })
  
  output$data_values <- renderUI({
    outputVector <- c()
    for (n in names(dataValues())){
      localString <- paste0('<u>Sample ',n,':</u> ', round(dataValues()[n],2))
      outputVector <- append(outputVector, localString)
    }
    HTML(paste(outputVector, collapse = '<br>'))
  })
  
  output$colorbartext <- renderUI({ #ended with trying to get this to render in the main panel
    min <- as.character(round(min(dataValues()),2))
    max <- as.character(round(max(dataValues()),2))
    HTML(paste0('<p>', min, ' &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp ',max, '</p>'))
  })
  
  output$model3D <- renderRglwidget({ #ended with trying to get this to render in the main panel
    colors <- colorByFeatureMain(dataValues())
    try(rgl.close(), silent = TRUE)
    plot3DModel(input$patient, input$tumor, colors, tumorModelsPath)
    rglwidget()
  })
  
  output$centroidPlot <-  renderPlot({
    par(bg='#edf0f1')
    if (input$dataset=="Histology"){
      unitsForData <- datasetConversion[[input$type]][2]
    } else {
      unitsForData <- datasetConversion[[input$dataset]][2]
    }
    ylabText <- as.character(unitsForData)
    patientID <- gsub('Patient','P',input$patient)
    toPlot <- data.frame(sampleName=character(), values=numeric(), distances=numeric(), stringsAsFactors = F)
    for (n in names(dataValues())){
      sampleName <- paste0('v',n)
      sampleDataValue <- dataValues()[n]
      distance <- as.numeric(sampleData[which(sampleData$Patient == patientID & sampleData$SampleName == sampleName),]$DistCentroid)
      toBind <- data.frame(sampleName=sampleName, values=sampleDataValue, distances=distance)
      toPlot <- rbind(toPlot, toBind)
    }
    datax <- toPlot$distances
    datay <- toPlot$values
    mod <- lm(datay~datax)
    test <- cor.test(datax, datay)
    pvalue = round(test$p.value,2)
    r = round(as.numeric(test$estimate),2)
    plot(datax, datay, xlab='Distance from centroid (mm)', ylab=ylabText, col="grey", pch=19, cex=2)
    abline(mod, col="red", lwd=1)
    text(datax, datay, labels=toPlot$sampleName, cex=0.9, font=2)
    statistic <- paste0('(R = ',r,'; p-value = ',pvalue,')')
    legend('topleft', legend=c("    Linear fit", statistic), bty='n', bg="transparent",
           col=c("red", "white"), lty=c(1, 0), cex=0.8)
  })
  
  output$peripheryPlot <-  renderPlot({
    par(bg='#edf0f1')
    if (input$dataset=="Histology"){
      unitsForData <- datasetConversion[[input$type]][2]
    } else {
      unitsForData <- datasetConversion[[input$dataset]][2]
    }
    ylabText <- as.character(unitsForData)
    patientID <- gsub('Patient','P',input$patient)
    toPlot <- data.frame(sampleName=character(), values=numeric(), distances=numeric(), stringsAsFactors = F)
    for (n in names(dataValues())){
      sampleName <- paste0('v',n)
      sampleDataValue <- dataValues()[n]
      distance <- as.numeric(sampleData[which(sampleData$Patient == patientID & sampleData$SampleName == sampleName),]$DistPeriph)
      toBind <- data.frame(sampleName=sampleName, values=sampleDataValue, distances=distance)
      toPlot <- rbind(toPlot, toBind)
    }
    datax <- toPlot$distances
    datay <- toPlot$values
    mod <- lm(datay~datax)
    test <- cor.test(datax, datay)
    pvalue = round(test$p.value,2)
    r = round(as.numeric(test$estimate),2)
    plot(datax, datay, xlab='Distance from periphery (mm)', ylab=ylabText, col="grey", pch=19, cex=2)
    abline(mod, col="red", lwd=1)
    text(datax, datay, labels=toPlot$sampleName, cex=0.9, font=2)
    statistic <- paste0('(R = ',r,'; p-value = ',pvalue,')')
    legend('topleft', legend=c("    Linear fit", statistic), bty='n', bg="transparent",
           col=c("red", "white"), lty=c(1, 0), cex=0.8)
  })
  
  output$VRPlot <-  renderPlot({
    par(bg='#edf0f1')
    if (input$dataset=="Histology"){
      unitsForData <- datasetConversion[[input$type]][2]
    } else {
      unitsForData <- datasetConversion[[input$dataset]][2]
    }
    ylabText <- as.character(unitsForData)
    patientID <- gsub('Patient','P',input$patient)
    toPlot <- data.frame(sampleName=character(), values=numeric(), distances=numeric(), stringsAsFactors = F)
    for (n in names(dataValues())){
      sampleName <- paste0('v',n)
      sampleDataValue <- dataValues()[n]
      distance <- as.numeric(sampleData[which(sampleData$Patient == patientID & sampleData$SampleName == sampleName),]$DistVR)
      toBind <- data.frame(sampleName=sampleName, values=sampleDataValue, distances=distance)
      toPlot <- rbind(toPlot, toBind)
    }
    datax <- toPlot$distances
    datay <- toPlot$values
    mod <- lm(datay~datax)
    test <- cor.test(datax, datay)
    pvalue = round(test$p.value,2)
    r = round(as.numeric(test$estimate),2)
    plot(datax, datay, xlab='Distance from ventricles (mm)', ylab=ylabText, col="grey", pch=19, cex=2)
    abline(mod, col="red", lwd=1)
    text(datax, datay, labels=toPlot$sampleName, cex=0.9, font=2)
    statistic <- paste0('(R = ',r,'; p-value = ',pvalue,')')
    legend('topleft', legend=c("    Linear fit", statistic), bty='n', bg="transparent",
           col=c("red", "white"), lty=c(1, 0), cex=0.8)
  })
}