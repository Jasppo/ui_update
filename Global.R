# Load packages ========================================================================

# Shiny components
library(shiny)
library(shinyjs)
library(bslib)
library(shinyWidgets)

# Plotting
library(ggplot2)
library(highcharter)
library(RColorBrewer)
library(directlabels)
library(scales)
library(classInt)
library(cowplot)

# Data manipulation
library(dplyr)
library(tidyr)

# Read in data
library(docxtractr)
library(readr)
library(readxl)
library(stringr)

# Map
library(sf)
library(leaflet)

# Markdown
library(markdown)

# Source scripts =======================================================

# Configuration settings
source("R/config.R")

# Standards
source("R/app_standards.R") 

# Input widgets and functions
source("R/input_widgets.R")

# Utility function for loading and caching files
source("R/utils/load_and_cache_files.R")

# Utility function for generating links to views, and restoring views
source("R/utils/createLinkTo_and_restore_view.R")

# Tab components (modules for tab-specific ui and server, plotting functions)
source("R/load_modules_and_plots.R")