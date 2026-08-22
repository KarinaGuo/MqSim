# Use the official rocker/tidyverse image as the base image.
# This image comes pre-installed with R, RStudio, and the entire tidyverse suite.
FROM rocker/tidyverse:latest

# Install necessary system dependencies required by various R packages:
# - libgdal-dev, libproj-dev, libgeos-dev, libudunits2-dev: Required for spatial packages like geodata and dartR
# - libgl1-mesa-dev, libglu1-mesa-dev: Required for 3D plotting packages like rgl
# - libxt-dev: Required for various plotting and GUI components
RUN apt-get update -qq && apt-get install -y \
    libgdal-dev \
    libproj-dev \
    libgeos-dev \
    libudunits2-dev \
    libgl1-mesa-dev \
    libglu1-mesa-dev \
    libxt-dev \
    && rm -rf /var/lib/apt/lists/*

# Install all required R packages for the simulation and plotting scripts.
# install2.r is a utility provided by the rocker image to install CRAN packages easily.
# We skip already installed packages and stop on error.
RUN install2.r --error --skipinstalled \
    testthat \
    sensitivity \
    doParallel \
    foreach \
    scales \
    patchwork \
    geodata \
    mgcv \
    gratia \
    qqman \
    rgl \
    plotly \
    lme4 \
    manipulateWidget \
    bigmemory \
    biganalytics \
    scatterplot3d \
    EMMREML \
    Matrix \
    ape \
    genetics \
    mvtnorm \
    MASS \
    gtools \
    gdata \
    combinat \
    gplots

# dartR is installed separately in case of complex spatial dependencies.
RUN install2.r --error --skipinstalled dartR

# Set the working directory inside the container
WORKDIR /project

# Command to run when the container starts (starts an interactive R session)
CMD ["R"]
