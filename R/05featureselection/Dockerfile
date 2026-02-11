FROM rocker/tidyverse:4.4.2

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libsqlite3-dev \
    libssl-dev \
    libxml2-dev \
    git \
    build-essential \
    pandoc \
    ghostscript \
    libicu-dev \
    && rm -rf /var/lib/apt/lists/*

################################################################################

# Arbeitsverzeichnis
# WORKDIR /app

# renv lockfile
# COPY renv.lock renv.lock

# Pakete aus renv.lock installieren
# RUN Rscript -e "\
# install.packages('jsonlite'); \
# lock <- jsonlite::fromJSON('renv.lock'); \
# pkgs <- names(lock$Packages); \
# install.packages(pkgs)"

# Output-Verzeichnis (wird ggf. vom Volume überschrieben)
# RUN mkdir -p /app/output

# R-Skript
# COPY R/rfe.R ./rfe.R

# Default-Command
# CMD ["Rscript", "rfe.R"]

################################################################################

# Set working directory
WORKDIR /app

# Create output directory
RUN mkdir -p /app/output

# Install renv
RUN Rscript -e "install.packages('renv')"

# Copy R script and renv lockfile
COPY R/rfe.R ./rfe.R
COPY renv.lock ./renv.lock

# Restore R packages from source
RUN Rscript -e "renv::restore(prompt = FALSE)"

# Run the R script
CMD ["Rscript", "rfe.R"]
