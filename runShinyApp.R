# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                           Program Header Status  
#                   Header: 🗸  Comments: 🗸   Refactored: 🗸         
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ==============================================================================
#
# TITLE: ITL_GIT/runApp.R
#
#                               DESCRIPTION
# This program starts the application in the default browser. First, the
# workspace variables are cleared, then external packages are loaded.
# Lastly the app is launched via the shiny/app.R file.
#
#                             SUBDIRECTORIES
# ITL_GIT/shiny/ - this directory contains the remainder of the software.
#
#
# ==============================================================================

# Delete these lines when distributing _________________________________________
# print(paste0("DELETE THESE TWO LINES WHEN DISTRIBUTING"))

# ST WORKING DIRECTORIES


# PERSONAL GITHUB
# setwd("C:/Users/steph/Desktop/SEROHUB/seronistv1.0/ITL_GIT/")
# Git bash
# cd Desktop/SEROHUB/seronistv1.0/ITL_GIT/

# WORK GITHUB
# setwd("C:/Users/sst2/OneDrive - NIST/Desktop/SeroNIST/seronistv1.0/ITL_GIT/")
# Git bash
# cd OneDrive\ -\ NIST/Desktop/SeroNIST/seronistv1.0/ITL_GIT/

# PERSONAL LOCAL 
# setwd("C:/Users/steph/Desktop/ITL_GIT/") 
# ______________________________________________________________________________


# Also to clear working variables and run the shiny app
rm(list=ls())

# this message is printed on several lines (one per path) to make multiple paths
# easier to spot
message('library paths:\n', paste('... ', .libPaths(), sep='', collapse='\n'))

# Make sure in working directory above ./shiny 
source("shiny/source/a_StartHere/a_init_ExternalPackages.R")

# Run the actual app
shiny::runApp('./shiny',port=7777, launch.browser = T)

