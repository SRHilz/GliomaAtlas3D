#' UI-elements for about tab
#'
#' @import shiny
#' @export 

loadAboutTab <- function(){
  addResourcePath('www',system.file('shinyApp/www',package = "GliomaAtlas3D"))  
  aboutTab <- tabPanel(title = 'About', id = 'about', 
           titlePanel("The 3D Glioma Atlas"),
           sidebarLayout(
             sidebarPanel(
               h4("Citation Information"),
               p("For more information about the data in our app, please check out our publication:"),
               em("Stephanie Hilz, Chibo Hong, Llewellyn E. Jalbert, Robert K. Hu, Tali Mazor, Anupam Kumar, Andrew McKinney, Samuel J. Shelton, Patrick Schupp, Michael Y. Zhang, Michael Martin, Stephen T. Magill, Karen H.K. Wong, Josie L. Hayes, Yao Yu, Tracy Luks, Marram P. Olson, Marisa Lafontaine, Anny Shai, Henrik Bengtsson, Adam Olshen, Annette M. Molinaro, Michael W. McDermott, Manish K. Aghi, Edward F. Chang, Shawn L. Hervey-Jumper, Mitchel S. Berger, Daniel A. Lim, Aaron Diaz, Janine M. Lupo, Joanna J. Phillips, Susan M. Chang, Sarah J. Nelson, Michael C. Oldham, Joseph F. Costello. "),
                  strong(em("The evolution of glioma cell populations in three-dimensional space.")),
                  em("In preparation"),
               br(),
               br(),
               p("Please cite us if you use any data from our app in your own work."),
               br(),
               "The 3D Glioma Atlas was built by Stephanie Hilz and Robert Hu using ", 
               a("R", 
                 href = "https://www.r-project.org/about.html"),
               " and ",
               a("Shiny", 
                 href = "http://shiny.rstudio.com")
             ),
             mainPanel(
               h1("Introducing a spatial approach to cancer genomics"),
               br(),
               img(src = "www/concept.png", height="90%", width="90%", align="center"),
               br(),
               br(),
               p("To create the 3D Glioma Atlas, we maximally sampled the anatomical diversity present in each patient's tumor, registering the physical location of each sample to a preoperative magnetic resonance image (MRI). This allowed us to analyze genomic and histologic information from each sample in relation to other samples and the tumor as a whole."),
               br(),
               h4("How do I use the Atlas?"),
               br(),
               p("Using the Atlas is simple. Select a patient and you will see all tumors available for that patient. After selecting a tumor, you will see the available data types. Some of these will have additional inputs, such as gene names for RNAseq data, that you can additionally select. A 3D heatmap of samples shaded by that data type will then appear on the right."),
               br(),
               h4("Why 3D?"),
               br(),
               p("ITH is thought to be a major reason for targeted treatment failure. Previous studies have been restricted in their ability to provide a comprehensive understanding of ITH in solid tumors due to insufficient sampling, typically just one biopsy per patient, and a lack of knowledge of where within the tumor the sample was taken. "),
               br(),
               h4("Available data types"),
               br(),
               strong("- Tumor cell proportion: "),
               p("The proportion of cells in the sample that are tumor, based on whole exome sequencing data"),
               strong("- RNAseq: "),
               p("The transcript level of a given gene, based on RNA sequencing"),
               strong("- Copy Number: "),
               p("The number of copies per gene, based on whole exome data"),
               strong("- Amplification: "),
               p("Whether or not a gene is amplified at a given threshold, based on whole exome data"),
               strong("- Percent Necrosis: "),
               p("The percent of tissue estimated to be necrotic, based on histology"),
               strong("- BV Hyperplasia: "),
               p("A score indicating the extent of abnormal blood vessles, based on histology")
          
               
               
             )
           )
  )
  return(aboutTab)
}
