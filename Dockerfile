FROM rocker/shiny:latest

LABEL maintainer="Hossam Hammady <hossam@rayyan.ai>"

# Install system dependencies
RUN apt-get update -qq && \
    apt-get install -y \
        libssl-dev \
        libfontconfig1-dev \
        libcurl4-openssl-dev \
        libxml2-dev \
        libharfbuzz-dev \
        libfribidi-dev \
        libgit2-dev \
        libfreetype6-dev \
        libpng-dev \
        libtiff-dev \
        libv8-dev \
        librsvg2-dev \
        libwebp-dev \
        pandoc

# Install R packages
RUN Rscript -e 'install.packages(c("DiagrammeR", "DiagrammeRsvg", "htmltools", "htmlwidgets", "scales", "shiny", "shinyjs", "stringr", "xml2", "DT", "rio", "zip", "rsvg", "webp", "devtools"))'

# Copy the shiny server configuration
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# Install the PRISMA2020 package
COPY . /tmp/app
RUN R CMD INSTALL /tmp/app

# Copy the shiny app files
RUN mkdir -p /srv/shiny-server/prisma/app && \
    echo "OK" > /srv/shiny-server/prisma/healthz.txt
COPY inst/shiny-examples/PRISMA_flowdiagram/. /srv/shiny-server/prisma/app/

# Clean up downloaded files
RUN rm -rf /tmp/*/downloaded_packages /tmp/app
