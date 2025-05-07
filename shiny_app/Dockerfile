# # Base image with R 4.2 and Shiny server
# # this is the basic image for r and rstudio
# FROM rocker/shiny

# # Install system dependencies for ragg and other graphics-related packages
# # This is needed from the linux side for the r proper functioning. 
# # Strongly depends on libraries you use
# RUN apt-get update && apt-get install -y \
#     libfreetype6-dev \
#     libpng-dev \
#     libtiff5-dev \
#     libjpeg-dev \
#     && rm -rf /var/lib/apt/lists/*

# # Set working directory
# WORKDIR /app

# # Copy necessary files
# COPY /app/renv.lock ./renv.lock
# COPY /app ./

# # Install renv and restore packages
# RUN Rscript -e 'install.packages("renv")' && \
#     Rscript -e 'renv::restore()'


# # Expose Shiny server port
# EXPOSE 3838

# # Start Shiny app
# CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]

# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:latest

# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libjpeg-dev \
    libpng-dev \
    libtiff5-dev \
    pkg-config \
    build-essential \
    cmake \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# copy necessary files
## renv.lock file
COPY /app/renv.lock ./renv.lock
## app folder
COPY /app ./app

# install renv & restore packages
RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'renv::restore()'

# expose port
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]