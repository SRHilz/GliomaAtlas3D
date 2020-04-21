![R-CMD-check](https://github.com/SRHilz/GliomaAtlas3D/workflows/R-CMD-check/badge.svg)

# GliomaAtlas3D - A 3D Map of Tumor-Wide Genetic and Microenvironmental Heterogeneity Present Across Adult Diffuse Glioma

The spatial organization of cells within a tumor provides a record of tumor growth and insights into the efficacy of future treatments, but is difficult to study in brain tumors due to limitations in sample quantity and ability to map samples back to their original location in the tumor. We generated the three-dimensional (3D) Glioma Atlas - an interactive data sharing platform - in which multi-platform genomic data is integrated with precise 3D spatial information for ~10 samples per tumor.


## Installation

```r
> if (!requireNamespace("remotes")) install.packages("remotes")
> remotes::install_github("SRHilz/GliomaAtlas3D")
```


## Launch Shiny App

From R, call:
```r
> shiny::runApp(system.file("shinyApp", package="GliomaAtlas3D"))
```

Alternatively, launch it directly from the terminal as:

```sh
$ Rscript -e 'shiny::runApp(system.file("shinyApp", package="GliomaAtlas3D"))'
```
