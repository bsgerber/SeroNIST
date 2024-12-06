# ==============================================================================
#
# TITLE: global.R
#
#                               DESCRIPTION
# The purpose of this program is to define and load variables that remain
# available throughout the shiny experience. Importantly, this program is called 
# before the UI and server are called, so any prerequisite data and variables 
# incorporated into the UI or server must be defined here first. 
#
#
#                             TABLE OF CONTENTS
#       1) LOAD HELPER FUNCTIONS
#       1A)   CPP Functions
#       1B)   R Functions
#       1C)   SQL Functions
#       2) INIT GLOBAL VARIABLES
#       3) INIT REACTIVE VARIABLES
#       4) VARIABLES FOR SQL
#       5) VARIABLES FOR UI
#
#
# ==============================================================================



# ==============================================================================
# 1) LOAD HELPER FUNCTIONS
# ==============================================================================
# ------------------------------------------------------------------------------
#         1A) CPP Functions
# ------------------------------------------------------------------------------
# # Import cpp functions
# sourceCpp("source/ServerFunctions/RCPP/Functions/asm_LibrarySearch_CPP.cpp") 


# ------------------------------------------------------------------------------
#         1B) R Functions
# ------------------------------------------------------------------------------
# Import asm functions here
# source("source/Functions/asm-clc.R")

# # Clear console using helper function
# clc()


# ------------------------------------------------------------------------------
#         1C) SQL Functions
# ------------------------------------------------------------------------------



# ==============================================================================
# 2) INIT GLOBAL VARIABLES
# ==============================================================================
# File types accepted
# query_file_types = c(".txt",".jsp",".csv");

# Find master excel sheets available
data_directory = list.files("./Data")
# data_directory = list.dirs("./Data", recursive = FALSE)
# dataFiles = list.files("./Data", pattern = ".sqlite")

# Reset the database connection variable
con <<- NULL


# ==============================================================================
# 3) INIT REACTIVE VARIABLES
# ==============================================================================
# Init reactive values for shiny gui use
rv <- reactiveValues()
rv$addedClasses= 0
rv$loadExcelSheetPressed = FALSE
rv$addAntigen = NULL
rv$numDimensions = 0
rv$numDimCount = 0
rv$everRemoved = FALSE
rv$addClass = NULL
rv$numClass = 0
rv$numClassCount = 0
rv$sheetCount = 1
rv$prevHeaderRowSelect = 0

# rv$done = TRUE

# db primarily used for search history
db <- reactiveValues()
# Set database to empty on start
db$dbEmpty = TRUE

# ==============================================================================
# 4) VARIABLES FOR SQL
# ==============================================================================

# ==============================================================================
# 5) VARIABLES FOR UI
# ==============================================================================

# rui used for reactive variables (see section 5)
rui <- list()

# Hard coded values for UI 
rui$numPossibleAntigens = seq(0,20)
rui$numPossibleRowsDataStart = seq(0, 100)

# Hard coded text for data acquisition of various inputs
# See inputs defined in source/b_UI/sst_GUI_Viewer.R

rui$upload = "Upload a file: "
rui$showAllCols = "Show all columns"
rui$displayOnlyCols = "Display all columns"
rui$selectedAntigens = "Select number of antigens to analyze"
rui$dispOnlyNum = "Display only numeric columns"
rui$autopop = "Autopopulate antigen names"
rui$selectSampleID = "Select a column to be used as sample ID:"

rui$dimAddnHeader = "<span style=\"display:inline-block; width: 20px;\"></span>
<b>Antigen #</b>
<span style=\"display:inline-block; width: 45px;\"></span>
<b>Select antigen column</b>
<span style=\"display:inline-block; width: 270px;\"></span>
<b>Enter antigen name</b>
<span style=\"display:inline-block; width: 285px;\"></span>
<b>Remove</b>"

rui$classAddnHeader = "<span style=\"display:inline-block; width: 20px;\"></span>
<b>Class #</b>
<span style=\"display:inline-block; width: 45px;\"></span>
<b>Selected Classes</b>
<span style=\"display:inline-block; width: 210px;\"></span>
<b># Rows</b>
<span style=\"display:inline-block; width: 55px;\"></span>
<b>Enter class name</b>
<span style=\"display:inline-block; width: 300px;\"></span> 
<b>Remove</b>"



