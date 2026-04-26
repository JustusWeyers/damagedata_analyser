## Base image
FROM rocker/verse:4.4.2

## Install system dependencies
RUN apt-get update && apt-get install -y \
   libcurl4-openssl-dev \
   libsqlite3-dev \
   libssl-dev \
   libxml2-dev \
   zlib1g-dev \
   libpng-dev \
   libjpeg-dev \
   libfreetype6-dev \
   git \
   build-essential \
   cmake \
   pkg-config \
   pandoc \
   ghostscript \
   libicu-dev \
   libfontconfig1-dev \
   gfortran \
   libopenblas-dev \
   liblapack-dev \
   libharfbuzz-dev \
   libfribidi-dev \
   && rm -rf /var/lib/apt/lists/*

## Set working directory
WORKDIR /app

## Copy renv.lock
COPY renv.lock /app/

## Install renv
RUN Rscript -e 'install.packages(c("renv"))'

## Set the renv path var to the renv lib
ENV RENV_PATHS_LIBRARY renv/library

## Restore the environment
RUN Rscript -e 'renv::restore(lockfile="/app/renv.lock")'

# Tinytex
RUN Rscript -e 'install.packages("tinytex"); tinytex::install_tinytex(force = TRUE)'

RUN Rscript -e 'tinytex::tlmgr_install(c("fvextra","upquote","lineno",\
"eso-pic","grfext","grffile","oberdiek","pdfpages","multirow","wrapfig",\
"colortbl","pdflscape","tabu","varwidth","threeparttable","threeparttablex",\
"environ","trimspaces","ulem","makecell"))'

RUN Rscript -e 'install.packages("remotes")'

RUN Rscript -e 'remotes::install_github("JustusWeyers/damagedata_analyser/R/packages/damageRF")'
  
## Copy project files into the image
COPY R/ /app/R

## Create data directory
RUN mkdir -p /app/data

## Create output directory
RUN mkdir -p /app/output

## Default command to run when the container starts
CMD ["Rscript", "-e", "rmarkdown::render('R/run_rf3.Rmd')"]
