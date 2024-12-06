# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                           Program Header Status  
#                   Header: 🗸  Comments: 🗸   Refactored: 🗸         
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ==============================================================================
#
# TITLE: init_ExternalPackages.R
#
#                               DESCRIPTION
# The purpose of this code is to install packages that the source code
# depends on for running. This code is run at the initial code startup & if
# any packages are missing from the environment they are installed here
#
#
#                             TABLE OF CONTENTS
#       1) PACKAGES FOR SPEEDUP
#       2) SQL 
#       3) DATA MANIPULATION
#       4) GGPLOT AND PLOTTING
#       5) GUI RELATED
#       6) OTHER
#
#                                 NOTES
# Any breakages in package install may require reboot and reinstallation, see here
# https://stackoverflow.com/questions/55415631/error-package-or-namespace-load-failed-for-tidyverse-in-loadnamespace
#
#
# ==============================================================================



# To add possibly
# shinycssloaders - for spinner while data loads
# shinydashboard 

# ==============================================================================
# 1) PACKAGES FOR SPEEDUP
# ==============================================================================

# # Parallelization of code made possible with this package
# if("doParallel" %in% row.names(installed.packages())==FALSE){
#   install.packages("doParallel",dependencies=TRUE);
#   library("doParallel");
# } else {
#   library(doParallel)
# }

# Used to interface between R and C++ code
if("Rcpp" %in% row.names(installed.packages())==FALSE){
  install.packages("Rcpp",dependencies=TRUE);
  library("Rcpp");
} else {
  library(Rcpp)
}

# ==============================================================================
# 2) SQL
# ==============================================================================

# SQL database package, ie) dbGetQuery(...)
if("DBI" %in% row.names(installed.packages())==FALSE){
  install.packages("DBI",dependencies=TRUE);
  library("DBI");
} else {
  library(DBI)
}

# Allows for use of sqlite files, must be included with the DBI package
if("RSQLite" %in% row.names(installed.packages())==FALSE){
  install.packages("RSQLite",dependencies=TRUE);
  library("RSQLite");
} else {
  library(RSQLite)
}

# ==============================================================================
# 3) DATA MANIPULATION
# ==============================================================================

# data.table is used to create and manipulate data frames
if("data.table" %in% rownames(installed.packages()) == FALSE) {
  install.packages("data.table")
  library(data.table)
} else {
  library(data.table)
}

# Grouping and other operations performed on data tables
if("dplyr" %in% row.names(installed.packages())==FALSE){
  install.packages("dplyr",dependencies=TRUE);
  library("dplyr");
} else {
  library(dplyr)
}

# readxl used to read excel .xlsx files
if("readxl" %in% row.names(installed.packages())==FALSE){
  install.packages("readxl")
  library(readxl)
} else {
  library(readxl)
}

### 010124 added
# 
# # rJava used to read excel .rJava files
# if("rJava" %in% row.names(installed.packages())==FALSE){
#   install.packages("rJava")
#   library(rJava)
# } else {
#   library(rJava)
# }
# 
# # xlsx used to read excel .xlsx files
# if("xlsx" %in% row.names(installed.packages())==FALSE){
#   install.packages("xlsx")
#   library(xlsx)
# } else {
#   library(xlsx)
# }

if("readr" %in% row.names(installed.packages())==FALSE){
  install.packages("readr")
  library(readr)
} else {
  library(readr)
}

# stringr package is used for reading and manipulating string data
if("stringr" %in% row.names(installed.packages())==FALSE){
  install.packages("stringr")
  library(stringr)
} else {
  library(stringr)
}

# # This package helps with data manipulation and piping shortcuts
# if("tidyverse" %in% row.names(installed.packages())==FALSE){
#   install.packages("tidyverse",dependencies=TRUE);
#   library("tidyverse");
# } else {
#   library(tidyverse)
# }

# stringr package is used for reading and manipulating string data
if("writexl" %in% row.names(installed.packages())==FALSE){
  install.packages("writexl")
  library(writexl)
} else {
  library(writexl)
}

# ==============================================================================
# 4) GGPLOT AND PLOTTING
# ==============================================================================

if("pracma" %in% row.names(installed.packages())==FALSE){
  install.packages("pracma")
  library(pracma)
} else {
  library(pracma)
}

if("akima" %in% row.names(installed.packages())==FALSE){
  install.packages("akima")
  library(akima)
} else {
  library(akima)
}

if("rgl" %in% row.names(installed.packages())==FALSE){
  install.packages("rgl")
  library(rgl)
} else {
  library(rgl)
}

# Needed for heatmap
# if("forcats" %in% row.names(installed.packages())==FALSE){
#   install.packages("forcats")
#   library(forcats)
# } else {
#   library(forcats)
# }

# if("ggpmisc" %in% row.names(installed.packages())==FALSE){
#   install.packages("ggpmisc")
#   library(ggpmisc)
# } else {
#   library(ggpmisc)
# }


# ggplot2 allows for interactive and downloadable plots
if("ggplot2" %in% row.names(installed.packages())==FALSE){
  install.packages("ggplot2")
  library(ggplot2)
} else {
  library(ggplot2)
}

# # ggrepel package used for plot annotations that are self repelling
# if("ggrepel" %in% row.names(installed.packages())==FALSE){
#   install.packages("ggrepel")
#   library(ggrepel)
# } else {
#   library(ggrepel)
# }

# # Needed for heatmap
# if("ggsci" %in% row.names(installed.packages())==FALSE){
#   install.packages("ggsci")
#   library(ggsci)
# } else {
#   library(ggsci)
# }

# # ggtext element_markdown theme for plot text customization 
# if("ggtext" %in% row.names(installed.packages())==FALSE){
#   install.packages("ggtext")
#   library(ggtext)
# } else {
#   library(ggtext)
# }

# plotly is used for generating plots with annotations and customizations
if("plotly" %in% row.names(installed.packages())==FALSE){
  install.packages("plotly",dependencies=TRUE);
  library("plotly")
} else {
  library("plotly")
}

# # Needed for heatmap
# if("scales" %in% row.names(installed.packages())==FALSE){
#   install.packages("scales")
#   library(scales)
# } else {
#   library(scales)
# }

# ==============================================================================
# 5) GUI RELATED
# ==============================================================================

# DT used for creating datatables within rshiny framework
# allows for more customizability than dataframes
if("DT" %in% row.names(installed.packages())==FALSE){
  devtools::install_github('rstudio/DT');
  library(DT);
} else {
  library(DT);
}

# # httr used for working with URLs and HTTP within rshiny framewwork
# if("httr" %in% row.names(installed.packages())==FALSE){
#   install.packages("httr",dependecies=TRUE);
#   library("httr")
# } else {
#   library("httr")
# }

# rShiny package used for making interactive web applications
if("shiny" %in% row.names(installed.packages())==FALSE){
  install.packages("shiny",dependencies=TRUE);
  library("shiny");
} else {
  library(shiny)
}

# rShiny package used for making interactive web applications
if("shinyFiles" %in% row.names(installed.packages())==FALSE){
  install.packages("shinyFiles",dependencies=TRUE);
  library("shinyFiles");
} else {
  library(shinyFiles)
}

# shinyjs used for integrating javascript in web app
if("shinyjs" %in% rownames(installed.packages())==FALSE){
  install.packages("shinyjs")
  library(shinyjs)
} else {
  library(shinyjs)
}

# shinythemes used for customizing themes of the web app
if("shinythemes" %in% rownames(installed.packages()) == FALSE){
  install.packages("shinythemes")
  library(shinythemes)
} else {
  library(shinythemes)
}

# shinythemes used for customizing themes of the web app
if("shinyBS" %in% rownames(installed.packages()) == FALSE){
  install.packages("shinyBS")
  library(shinyBS)
} else {
  library(shinyBS)
}


# shinycssloaders used for spinner while loading
if("shinycssloaders" %in% rownames(installed.packages()) == FALSE){
  install.packages("shinycssloaders")
  library(shinycssloaders)
} else {
  library(shinycssloaders)
}
# ==============================================================================
# 6) OTHER
# ==============================================================================
# optimx used for github install
# if("optimx" %in% row.names(installed.packages())==FALSE){
#   install.packages("optimx",dependencies=TRUE)
#   library(optimx)
# } else{
#   library(optimx)
# }
# # optimx used for github install
# if("optimParallel" %in% row.names(installed.packages())==FALSE){
#   install.packages("optimParallel",dependencies=TRUE)
#   library(optimParallel)
# } else{
#   library(optimParallel)
# }

if("emdbook" %in% row.names(installed.packages())==FALSE){
  install.packages("emdbook",dependencies=TRUE)
  library(emdbook)
} else{
  library(emdbook)
}


# devtools used for github install
if("devtools" %in% row.names(installed.packages())==FALSE){
  install.packages("devtools",dependencies=TRUE)
  library(devtools)
} else{
  library(devtools)
}


# reshape2 used for github install
if("reshape2" %in% row.names(installed.packages())==FALSE){
  install.packages("reshape2",dependencies=TRUE)
  library(reshape2)
} else{
  library(reshape2)
}

