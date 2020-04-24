FROM rocker/shiny:3.6.3

RUN apt-get update && apt-get install -y libcurl4-openssl-dev libv8-dev libx11-dev mesa-common-dev libglu1-mesa-dev && \
    mkdir -p /var/lib/shiny-server/bookmarks/shiny

# Download and install library
RUN Rscript -e "install.packages(c('shinydashboard', 'shinyjs', 'remotes', 'V8'))"

# Get R package from GitHub
RUN Rscript -e "remotes::install_github('SRHilz/GliomaAtlas3D')"

EXPOSE 3838

# R package 'rgl': produce OpenGL figures in headless mode
ENV RGL_USE_NULL=true

CMD ["Rscript", "-e", "shiny::runApp(system.file('shinyApp', package='GliomaAtlas3D'), port=3838, host='0.0.0.0')"]
