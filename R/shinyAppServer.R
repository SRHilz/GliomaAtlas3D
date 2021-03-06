#' Shiny app server function
#' @import graphics
#' @import stats
#' @import utils
#' @rawNamespace import(shinyjs, except = runExample)
#' @export
#' @param input provided by shiny
#' @param output provided by shiny
#'

# Define server logic required to generate 3D glioma visualizations
shinyAppServer <- function(input, output){

  datasetConversion <- list(RNAseq = c('rna.rds','Counts per million'),
  		       	    'Gene Promotor Accessibility' = c('genepromoters.rds','Relative enrichment'),
  		       	    'Gene Enhancer Accessibility' = c('geneenhancers.rds','Relative enrichment'),
                            `Tumor Cell Proportion` =  c('purity.rds','Proportion of cells'),
                            `Copy Number` = c('cn.rds','Number of copies'),
                            Amplification = c('cn.rds','Amplified at threshold (0=no, 1=yes'),
                            `Percent Necrosis` = c('per_nec.rds', '% of tissue with necrosis'),
                            `BV Hyperplasia` = c('bv_hyper.rds','Score (0=none, 1=mild, 2=extensive)'),
                            Histology = c('Histology','NA'),
                            `Cell Types` = c('celltypes.rds','Enrichment score'),
                            `Cancer Processes` = c('cancerprocesses.rds','Enrichment score'),
                            `Expansion Events` = c('expansions.rds','Mean VAF'))
  # define paths
  root <- system.file(package = "GliomaAtlas3D", "exdata")
  sampleDataPath <-file.path(root, "metadata","sampledata_v12.rds")
  tumorDataPath <-file.path(root, "metadata","tumordata_v12.rds")
  tumorDatasetsPath <- file.path(root,"datasets")
  tumorModelsPath <- file.path(root,"models")
  
  # read in data to be used globally
  sampleData <- readRDS(sampleDataPath)
  tumorData <- readRDS(tumorDataPath)
  tumorDatasets <- getDatasets(tumorDatasetsPath)
  
  patientsFinal <- reactive({
    req(input$patientSubset)
    # key: 1) IDH-mut 2) IDH-wt 3) codel 4) non-codel 5) grade II and III 6) grade IV 7) newly-diagnosed 8) recurrent
    
    patients <- tumorData$Patient #this is the starting list
    # IDH-mut vs IDH-wt logic
    if (any(c('1','2') %in% input$patientSubset)){
      if (!all(c('1','2') %in% input$patientSubset)){
        if ('1' %in% input$patientSubset){
          patients <- patients[patients %in% tumorData[which(tumorData$IDH_Mut == 1),]$Patient]
        } else {
          patients <- patients[patients %in% tumorData[which(tumorData$IDH_Mut == 0),]$Patient]
        }
      }
    }
    # 1p19q loss vs 1p19q intact logic
    if (any(c('3','4') %in% input$patientSubset)){
      if (!all(c('3','4') %in% input$patientSubset)){
        if ('3' %in% input$patientSubset){
          patients <- patients[patients %in% tumorData[which(tumorData$X1p19q == 1),]$Patient]
        } else {
          patients <- patients[patients %in% tumorData[which(tumorData$X1p19q == 0),]$Patient]
        }
      }
    }
    # grade logic 
    if (any(c('5','6') %in% input$patientSubset)){
      if (!all(c('5','6') %in% input$patientSubset)){
        if ('5' %in% input$patientSubset){
          patients <- patients[patients %in% tumorData[which(tumorData$Grade == '2' | tumorData$Grade == '3'),]$Patient]
        } else {
          patients <- patients[patients %in% tumorData[which(tumorData$Grade == '4'),]$Patient]
        }
      }
    }
    # type logic 
    if (any(c('7','8') %in% input$patientSubset)){
      if (!all(c('7','8') %in% input$patientSubset)){
        if ('7' %in% input$patientSubset){
          patients <- patients[patients %in% tumorData[which(tumorData$Tumor == 'Primary'),]$Patient]
        } else {
          patients <- patients[patients %in% tumorData[which(tumorData$Tumor == 'Recurrence1' | tumorData$X1p19q == 'Recurrence2' | tumorData$X1p19q == 'Recurrence3' | tumorData$X1p19q == 'Recurrence4'),]$Patient]
        }
      }
    }
    gsub('P','Patient',patients)
  })
  
  patientsFinalDownload <- reactive({
    req(input$patientSubsetDownload)
    # key: 1) IDH-mut 2) IDH-wt 3) codel 4) non-codel 5) grade II and III 6) grade IV 7) newly-diagnosed 8) recurrent
    
    patients <- tumorData$Patient #this is the starting list
    # IDH-mut vs IDH-wt logic
    if (any(c('1','2') %in% input$patientSubsetDownload)){
      if (!all(c('1','2') %in% input$patientSubsetDownload)){
        if ('1' %in% input$patientSubsetDownload){
          patients <- patients[patients %in% tumorData[which(tumorData$IDH_Mut == 1),]$Patient]
        } else {
          patients <- patients[patients %in% tumorData[which(tumorData$IDH_Mut == 0),]$Patient]
        }
      }
    }
    # 1p19q loss vs 1p19q intact logic
    if (any(c('3','4') %in% input$patientSubsetDownload)){
      if (!all(c('3','4') %in% input$patientSubsetDownload)){
        if ('3' %in% input$patientSubsetDownload){
          patients <- patients[patients %in% tumorData[which(tumorData$X1p19q == 1),]$Patient]
        } else {
          patients <- patients[patients %in% tumorData[which(tumorData$X1p19q == 0),]$Patient]
        }
      }
    }
    # grade logic 
    if (any(c('5','6') %in% input$patientSubsetDownload)){
      if (!all(c('5','6') %in% input$patientSubsetDownload)){
        if ('5' %in% input$patientSubsetDownload){
          patients <- patients[patients %in% tumorData[which(tumorData$Grade == '2' | tumorData$Grade == '3'),]$Patient]
        } else {
          patients <- patients[patients %in% tumorData[which(tumorData$Grade == '4'),]$Patient]
        }
      }
    }
    # type logic 
    if (any(c('7','8') %in% input$patientSubsetDownload)){
      if (!all(c('7','8') %in% input$patientSubsetDownload)){
        if ('7' %in% input$patientSubsetDownload){
          patients <- patients[patients %in% tumorData[which(tumorData$Tumor == 'Primary'),]$Patient]
        } else {
          patients <- patients[patients %in% tumorData[which(tumorData$Tumor == 'Recurrence1' | tumorData$X1p19q == 'Recurrence2' | tumorData$X1p19q == 'Recurrence3' | tumorData$X1p19q == 'Recurrence4'),]$Patient]
        }
      }
    }
    gsub('P','Patient',patients)
  })
  
  output$patientUI <-  renderUI({
    validate(
      need(patientsFinal(), "There are no patients with the combination of criteria specified. Please make a new selection.")
    )
    
    selectInput("patient",
                label = "Patient",
                choices = patientsFinal())
  })
  
  output$patientInfoUI <- renderUI({
    req(patientsFinal(), input$patient)
    p <- gsub('Patient','P',input$patient)
    patientTumorData <- tumorData[which(tumorData$Patient == p),]
    infoTag <- c('A')
    if (patientTumorData$Tumor == 'Primary'){
      infoTag <- append(infoTag, 'newly diagnosed')
    } else {
      infoTag <- append(infoTag, 'recurrent')
    }
    if (patientTumorData$Grade == '2'){
      infoTag <- append(infoTag, 'grade II')
      } else if (patientTumorData$Grade == '3'){
      infoTag <- append(infoTag, 'grade III')
      } else {
      infoTag <- append(infoTag, 'grade IV')
      }
    if (is.na(patientTumorData$IDH_Mut)){
      infoTag <- append(infoTag, 'glioma,')
    } else if (patientTumorData$IDH_Mut == 1) {
      infoTag <- append(infoTag, 'IDH-mut glioma,')
    } else {
      infoTag <- append(infoTag, 'IDH-wt glioma,')
    }
    if (is.na(patientTumorData$X1p19q)){
      infoTag <- append(infoTag, '')
    } else if (patientTumorData$X1p19q == 1) {
      infoTag <- append(infoTag, 'codel')
      } else {
      infoTag <- append(infoTag, 'non-codel')
    }
    if (is.na(patientTumorData$TERT)){
      infoTag <- append(infoTag, 'with unknown TERTp status')
    } else if (patientTumorData$TERT == 0){
      infoTag <- append(infoTag, 'and TERTp-wt')
    } else {
      infoTag <- append(infoTag, 'and TERTp-mut')
    }
    HTML(paste0('<center><i>',paste(infoTag, collapse=' '),'</i><center><br>'))
  })
  
  output$tumorUI <- renderUI({
    req(patientsFinal(), input$patient)
    
    sfNums <- tumorDatasets[tumorDatasets$patient==input$patient, 'sf']
    switch(input$patient, selectInput("tumor", "Tumor", choices = sfNums, selected = sfNums[1]))
  })
  
  output$datasetUI <- renderUI({
    req(patientsFinal(), input$patient)
    
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
  
  availableDatasetsDownload <- reactive({
    validate(
      need(patientsFinalDownload(), "There are no patients with the combination of criteria specified. Please make a new selection.")
    )
    
    patientTumorDatasets <- Filter(function(x)!all(is.na(x)), tumorDatasets[which(tumorDatasets$patient %in% patientsFinalDownload()),])
    availableDatasetFiles <- colnames(Filter(function(x)any(is.numeric(x)), patientTumorDatasets))
    availableDatasets <- names(datasetConversion)[unlist(lapply(datasetConversion, function(x) x[[1]] %in% availableDatasetFiles))]
    if ('Copy Number' %in% availableDatasets){
      availableDatasets <- append(availableDatasets, 'Amplification')
    }
    if ('Percent Necrosis' %in% availableDatasets | 'BV Hyperplasia' %in% availableDatasets){
      availableDatasets <- append(availableDatasets[!availableDatasets %in% c('Percent Necrosis','BV Hyperplasia')], 'Histology')
    }
    availableDatasets[order(availableDatasets)]
  })
  
  output$datasetDownloadUI <- renderUI({
    req(availableDatasetsDownload())
    
    selectInput("datasetDownload", "Dataset", choices = availableDatasetsDownload(), selected='Tumor Cell Proportion')
  })
  
  output$typeUI <- renderUI({
    req(input$dataset)
    
    if (input$dataset!="Histology")
      return()
    
    switch(input$dataset,
           "Histology" = selectInput("type", "Type", choices = c("Percent Necrosis", "BV Hyperplasia"), 
                                     selected = "Percent Necrosis")
    )
  })
  
  output$typeDownloadUI <- renderUI({
    req(input$datasetDownload)
    
    if (input$datasetDownload!="Histology")
      return()
    
    switch(input$datasetDownload,
           "Histology" = selectInput("typeDownload", "Type", choices = c("Percent Necrosis", "BV Hyperplasia"), 
                                     selected = "Percent Necrosis")
    )
  })
  
  output$thresholdUI <- renderUI({
    req(input$dataset)
    if (input$dataset!="Amplification"){
      return()
    } 
      switch(input$dataset, sliderInput("threshold", "Threshold", min = 0, max = 15, value = 5, step = 0.1)
    )
  })
  
  output$thresholdDownloadUI <- renderUI({
    req(input$datasetDownload)
    if (input$datasetDownload!="Amplification"){
      return()
    } 
    switch(input$datasetDownload, sliderInput("thresholdDownload", "Threshold", min = 0, max = 15, value = 5, step = 0.1)
    )
  })
  
  output$rowSelectionUI <- renderUI({
    req(input$dataset, input$patient, input$tumor)
  
    if (input$dataset %in% c('Tumor Cell Proportion', 'Histology')){ # Don't need to select gene for purity or histology
      return()
    } else {
      fname <- datasetConversion[[input$dataset]][1]
      data <- readRDS(file.path(tumorDatasetsPath, input$patient, input$tumor, fname))
      if (input$dataset %in% c('Cell Types')) {
        switch(input$dataset, selectInput("rowSelection", "Cell Type", choices = rownames(data), selected = rownames(data)[1]))
      } else if (input$dataset %in% c('Cancer Processes')) {
        switch(input$dataset, selectInput("rowSelection", "Cancer Process", choices = rownames(data), selected = rownames(data)[1]))
      } else if (input$dataset %in% c('Expansion Events')){
        switch(input$dataset, selectInput("rowSelection", "Expansion Event", choices = rownames(data), selected = rownames(data)[1]))
      } else {
        switch(input$dataset, selectInput("rowSelection", "Gene", choices = rownames(data), selected = rownames(data)[1]))# for RNAseq, Amplification, CN, Gene Promotor
      }
    }
  })
  
  output$rowSelectionDownloadUI <- renderUI({
    req(input$datasetDownload, patientsFinalDownload())
    
    if (input$datasetDownload %in% c('Tumor Cell Proportion', 'Histology')){ # Don't need to select gene for purity or histology
      return()
    } else {
      allRowNames <- c()
      for (p in patientsFinalDownload()){
        fname <- datasetConversion[[input$datasetDownload]][1]
        sf <- as.character(tumorDatasets[which(tumorDatasets$patient == p),]$sf)
        dataPath <- file.path(tumorDatasetsPath, p, sf, fname)
        if (file.exists(dataPath)){
          data <- readRDS(dataPath)
          allRowNames <- unique(append(allRowNames, rownames(data)))
        }
      }
      if (input$datasetDownload %in% c('Cell Types')) {
        switch(input$datasetDownload, selectInput("rowSelectionDownload", "Cell Type", choices = allRowNames, selected = allRowNames[1]))
      } else if (input$datasetDownload %in% c('Cancer Processes')) {
        switch(input$datasetDownload, selectInput("rowSelectionDownload", "Cancer Process", choices = allRowNames, selected = allRowNames[1]))
      } else if (input$datasetDownload %in% c('Expansion Events')){
        switch(input$datasetDownload, selectInput("rowSelectionDownload", "Expansion Event", choices = allRowNames, selected = allRowNames[1]))
      } else {
        switch(input$datasetDownload, selectInput("rowSelectionDownload", "Gene", choices = allRowNames, selected = allRowNames[1]))# for RNAseq, Amplification, CN, Gene Promoter
      }
    }
  })
  
  dataValues <- reactive({
    req(input$dataset, input$patient, input$tumor)
    
    if (input$dataset == 'Tumor Cell Proportion'){
      getDataValues(input$patient, input$tumor, input$dataset, NA, NA, NA, datasetConversion, tumorDatasetsPath)
    } else if(input$dataset == 'Histology'){
      req(input$type)
      getDataValues(input$patient, input$tumor, input$dataset, input$type, NA, NA, datasetConversion, tumorDatasetsPath)
    } else if (input$dataset == 'Amplification'){
      req(input$threshold, input$rowSelection)
      getDataValues(input$patient, input$tumor, input$dataset, NA, input$rowSelection, input$threshold, datasetConversion, tumorDatasetsPath)
    } else {
      req(input$rowSelection)
      getDataValues(input$patient, input$tumor, input$dataset, NA, input$rowSelection, NA, datasetConversion, tumorDatasetsPath)
    }
  })
  
  dataValuesDownload <- reactive({
    req(input$datasetDownload, patientsFinalDownload())

    sfConversion <- tumorDatasets$sf # works now because only one tumor per patient, will need to be updated when this is no longer true
    names(sfConversion) <- tumorDatasets$patient
    if (input$datasetDownload == 'Tumor Cell Proportion'){
      getDataValuesDownload(patientsFinalDownload(), sfConversion, input$datasetDownload, NA, NA, NA, datasetConversion, tumorDatasetsPath, sampleData)
    } else if(input$datasetDownload == 'Histology'){
      req(input$typeDownload)
      getDataValuesDownload(patientsFinalDownload(), sfConversion, input$datasetDownload, input$typeDownload, NA, NA, datasetConversion, tumorDatasetsPath, sampleData)
    } else if (input$datasetDownload == 'Amplification'){
      req(input$thresholdDownload, input$rowSelectionDownload)
      getDataValuesDownload(patientsFinalDownload(), sfConversion, input$datasetDownload, NA, input$rowSelectionDownload, input$thresholdDownload, datasetConversion, tumorDatasetsPath, sampleData)
    } else {
      req(input$rowSelectionDownload)
      getDataValuesDownload(patientsFinalDownload(), sfConversion, input$datasetDownload, NA, input$rowSelectionDownload, NA, datasetConversion, tumorDatasetsPath, sampleData)
    }
  })
  
  output$tableDownload <- renderTable({
    dataValuesDownload()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0('GliomaAtlas3D_SampleData_',input$datasetDownload, ".csv")
    },
    content = function(file) {
      write.csv(dataValuesDownload(), file, row.names = FALSE)
    }
  )
  
  tumorDataDownload <- reactive({
    req(patientsFinalDownload())
    
    patients <- gsub('Patient','P',patientsFinalDownload())
    tumorData[which(tumorData$Patient %in% patients)]# works currently because patient:tumor 1:1 not 1:many
  })
  
  output$downloadTumorData <- downloadHandler(
    filename = function() {
      'GliomaAtlas3D_TumorData.csv'
    },
    content = function(file) {
      write.csv(tumorDataDownload(), file, row.names = FALSE)
    }
  )
  
  observeEvent(input$tumorMetadataDownload, {
    if (input$tumorMetadataDownload == FALSE)
      shinyjs::hide("downloadTumorData")
    else
      shinyjs::show("downloadTumorData")
  })
  
  output$units <- renderUI({
    req(input$dataset)
    if (input$dataset=="Histology"){
      req(input$type)
      unitsForData <- datasetConversion[[input$type]][2]
    } else {
      unitsForData <- datasetConversion[[input$dataset]][2]
    }
    HTML(paste0(as.character(unitsForData)))
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
    req(dataValues())
    colors <- colorByFeatureMain(dataValues())
    try(rgl.close(), silent = TRUE)
    plot3DModel(input$patient, input$tumor, colors, tumorModelsPath)
    rglwidget()
  })
  
  output$centroidPlot <-  renderPlot({
    req(dataValues())
    par(bg='white', mgp=c(1.5,.5,0), mar=c(2.5,2.5,2,.4))
    if (input$dataset=="Histology"){
      req(input$type)
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
    plot(datax, datay, xlab='Dist. from centroid (mm)', ylab=ylabText, col="grey", pch=19, cex=2)
    abline(mod, col="forestgreen", lwd=1.5)
    text(datax, datay, labels=toPlot$sampleName, cex=0.9, font=2)
    statistic <- paste0('(R = ',r,'; p = ',pvalue,')')
    legend('topleft', legend=c("    Linear fit", statistic), bty='n', bg="transparent",
           col=c("forestgreen", "white"), lty=c(1, 0), lwd=c(1.5,0), cex=0.8, x.intersp=0)
  })
  
  output$peripheryPlot <-  renderPlot({
    req(dataValues())
    par(bg='white', mgp=c(1.5,.5,0), mar=c(2.5,2.5,2,.4))
    if (input$dataset=="Histology"){
      req(input$type)
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
    plot(datax, datay, xlab='Dist. from periphery (mm)', ylab=ylabText, col="grey", pch=19, cex=2)
    abline(mod, col="forestgreen", lwd=1.5)
    text(datax, datay, labels=toPlot$sampleName, cex=0.9, font=2)
    statistic <- paste0('(R = ',r,'; p = ',pvalue,')')
    legend('topleft', legend=c("    Linear fit", statistic), bty='n', bg="transparent",
           col=c("forestgreen", "white"), lty=c(1, 0), lwd=c(1.5,0), cex=0.8, x.intersp=0)
  })
  
  output$VRPlot <-  renderPlot({
    req(dataValues())
    par(bg='white', mgp=c(1.5,.5,0), mar=c(2.5,2.5,2,.4))
    if (input$dataset=="Histology"){
      req(input$type)
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
    plot(datax, datay, xlab='Dist. from ventricles (mm)', ylab=ylabText, col="grey", pch=19, cex=2)
    abline(mod, col="forestgreen", lwd=1.5)
    text(datax, datay, labels=toPlot$sampleName, cex=0.9, font=2)
    statistic <- paste0('(R = ',r,'; p = ',pvalue,')')
    legend('topleft', legend=c("    Linear fit", statistic), bty='n', bg="transparent",
           col=c("forestgreen", "white"), lty=c(1, 0), lwd=c(1.5,0), cex=0.8, x.intersp=0)
  })
}