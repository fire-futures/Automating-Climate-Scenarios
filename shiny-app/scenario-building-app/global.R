# LOAD PACKAGES ----
library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(shinycssloaders)
library(markdown)
library(fresh)
library(gridExtra)
library(zip) #for creating zip files

#packages from workflow markdown doc
library(strex)
library(stringr)
library(lubridate)
library(purrr)
library(here)
library(sf)
library(caladaptr)  # remotes::install_github("ucanr-igis/caladaptr")
library(ggrepel)
library(rhandsontable) #For creating interactive table 
library(shinyBS)


# Assign default values to allow workflow functions to source
ui_grid_cells <- NULL
ui_gcm <- NULL
ui_rcp <- NULL


# SOURCE IN FUNCTIONS ----
# Construct the full path to the build_functions directory
#build_functions_dir <- file.path("/capstone/firefutures/climate-scenarios/build_functions")

# Get a list of all the R files in the build_functions directory
#r_files <- list.files(build_functions_dir, pattern = "\\.R$", full.names = TRUE)

# Use lapply() to source each file in the directory
#lapply(r_files, source)



scenarios <- c("Probable_1_CanESM2", "Probable_2_CNRM_CM5", "Probable_3_HadGEM2ES", "Probable_4_MIROC5")

models <- c("CanESM2", "CNRM-CM5", "HadGEM2-ES", "MIROC5")

RCP_assumptions <- c(45, 85)

segment_type <- c("year", "season")

grid_list <- seq(30693, 48698)
