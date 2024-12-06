# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                           Program Header Status  
#                             COMPLETED 091923
#                   Header: 🗸  Comments: 🗸   Refactored: 🗸        
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ==============================================================================
#
# TITLE: global.R
#
#                               DESCRIPTION
# The purpose of this program is to define and load variables that remain
# available throughout the shiny session. Importantly, this program is called 
# before the UI and server are called, so any prerequisite data and variables 
# incorporated into the UI or server must be defined here first. 
# This is also where all reactive variables are initialized, which are updated
# continuously in response to user interactions with the shiny app. 
#
#
#                             TABLE OF CONTENTS
#
#       1) LOAD HELPER FUNCTIONS
#               1a) CPP Functions
#               1b) R Functions
#               1c) SQL Functions
#
#       2) INIT GLOBAL VARIABLES
#
#       3) VARIABLES FOR SQL
#
#       4) VARIABLES FOR UI
#
#       5) INIT REACTIVE VARIABLES
#             5a) rv for general reactive values
#                     i) General metadata associated with rv$df
#                     ii) Pre-import metadata
#                     iii) Antigen metadata
#                     iv) Class metadta 
#                     v) Analysis related
#             5b) op for optimization related variables 
#             5c) db primarily used for search history
#             5d) Action log reactive var
#
#       6) LOCKING REACTIVE VARIABLES
#             6a) Locks for Pre-import
#             6b) Locks for import
#             6c) Locks for analysis
#
# ==============================================================================
# ==============================================================================
#
#                         1) LOAD HELPER FUNCTIONS
#
# ==============================================================================
# ------------------------------------------------------------------------------
# 1a) CPP Functions
# ------------------------------------------------------------------------------
# # Import cpp functions
# sourceCpp("") 


# ------------------------------------------------------------------------------
# 1b) R Functions
# ------------------------------------------------------------------------------
# Import asm functions here
# source("source/Functions/asm-clc.R")

# # Clear console using helper function
# clc()


# ------------------------------------------------------------------------------
# 1c) SQL Functions
# ------------------------------------------------------------------------------


# ==============================================================================
#
#                         2) INIT GLOBAL VARIABLES
#
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
# 
#                           3) VARIABLES FOR SQL
#
# ==============================================================================

# ==============================================================================
#
#                           4) VARIABLES FOR UI
#
# ==============================================================================

# rui used for reactive variables (see section 5)
rui <- list()

# Hard coded values for UI 
rui$numPossibleAntigens = seq(1,3)
rui$numPossibleRowsDataStart = seq(0, 100)
rui$tabNamesImport = c("Import", "Select Antigens", "Assign Classes", "Train")
rui$testTabNamesImport = c("Import", "Select Antigens", "Assign Classes", "Analyze")
rui$numTabsImport = length(rui$tabNamesImport)
rui$testData_numTabsImport = length(rui$testTabNamesImport)

# Hard coded text for data acquisition of various inputs
rui$upload = "Upload a file: "
rui$doneImporting = ""
rui$showAllCols = "Show all columns"
rui$displayOnlyCols = "Display all columns"
rui$selectedAntigens = "Select number of antigens"
rui$dispOnlyNum = "Display only numeric columns"
rui$autopop = "Autopopulate antigen names"
rui$selectSampleID = "Select a sample ID column:"
rui$selectMetadataCol = "Additional metadata columns: "
rui$selExcel = "Select which sheet"
rui$selHeader = "Specify header row number"
rui$selectSheet = "Select an excel sheet from your upload. The file cannot be open in Excel at the same time."
rui$specifyHeaderRow = "The header is a row in your Excel file that specifies column names used for labelling."
rui$updateDataSelect = "Upload your file with the settings chosen above to SeroNIST."
rui$updateAntigenSelect = "Tooltip text goes here."
rui$updateClassSelect = "Tooltip text goes here."
rui$sampleIDOptionalLabel = "[Optional] Select the column of data that contains unique labels for each sample."
rui$metadataOptionalLabel = "[Optional] Select additional columns of metadata to track in plots."
rui$sqlPrompt = "Enter a name for this database"
rui$sqlOptionalLabel = "[Optional]: The default name for the database if none provided is \"Default_Database.sqlite\""
rui$submitImportBSLabel = "Submit the file upload while applying the settings selected above."
rui$classSelectBSLabel = "Select one or more unique classes and press the Add Class button."
rui$classRowSelectedBSLabel = "This will display the rows selected for the current class to be added."

rui$allNamesForNeg = c("Negatives", "Negative", "negatives", "negative",
                       "Neg", "neg")
rui$forbiddenChars = c("/", "\\", ":", "*", "?", "\"", "<", ">", "|")
rui$noneSel = "None Selected"

rui$dimAddnHeader = 
  "<span style=\"font-size: 85%;\">
<span style=\"display:inline-block; width: 20px;\"></span>
<b>Antigen #</b>
<span style=\"display:inline-block; width: 15px;\"></span>
<b>Select antigen column</b>
<span style=\"display:inline-block; width: 40px;\"></span>
<b>Enter antigen name </b>
<span style=\"display:inline-block; width: 70px;\"></span>
<b></b></span>"

rui$classAddnHeader = "<span style=\"font-size: 85%;\">
<span style=\"display:inline-block; width: 20px;\"></span>
<b>Class #</b>
<span style=\"display:inline-block; width: 13px;\"></span>
<b>Class (# Rows)</b>
<span style=\"display:inline-block; width: 80px;\"></span>
<b>Enter class name</b>
<span style=\"display:inline-block; width: 70px;\"></span> 
<b></b></span>"

rui$classAddnHeader_B = "<span style=\"font-size: 85%;\">
<span style=\"display:inline-block; width: 20px;\"></span>
<b>Class #</b>
<span style=\"display:inline-block; width: 45px;\"></span>
<b>Rows selected</b>
<span style=\"display:inline-block; width: 210px;\"></span>
<b># Rows</b>
<span style=\"display:inline-block; width: 55px;\"></span>
<b>Enter class name</b>
<span style=\"display:inline-block; width: 300px;\"></span> 
<b></b></span>"

rui$optionAB = c("Add by column", "Add by row (manual)")

# Evaluator settings hyperparameter defaults
rui$rawOrLog = c("Raw data", "Log transform")
rui$trainingTabNames = c("Raw Data", "Log Data", "Initial Classifier", "Trained Classifier")
rui$testData_trainingTabNames = c("Raw Data", "Log Data", "Trained Classifier")
rui$trainingDataTableTabNames = c("Preview", "Raw Data", "Selected Data", "Classification")

rui$bfgsChoices = c("BFGS", "L-BFGS-B")
rui$bu_mmv = c(1, Inf, "100")
rui$ndeps_mmv = c(0, Inf, "1e-8")
rui$reltol_mmv = c(0, Inf, "1e-10")
rui$abstol_mmv = c(0, Inf, "1e-20")
rui$pgtol_mmv = c(0, Inf, "1e-16")
rui$maxit_mmv = c(1, Inf, "1e4")
rui$factr_mmv = c(0, Inf, "1e1")

rui$bfgsChoices = c("BFGS", "L-BFGS-B")
rui$bu_mmv = c(1, Inf, "100")
rui$ndeps_mmv = c(1e-12, 1e-2, "1e-8")
rui$reltol_mmv = c(1e-16, 1e-2, "1e-10")
rui$abstol_mmv = c(1e-25, 1e-2, "1e-20")
rui$pgtol_mmv = c(1e-25, 1e-2, "1e-16")
rui$maxit_mmv = c(1, Inf, "1e4")
rui$factr_mmv = c(0, Inf, "1e1")

rui$bu_default = as.numeric(rui$bu_mmv[3])
rui$ndeps_default = as.numeric(rui$ndeps_mmv[3])
rui$reltol_default = as.numeric(rui$reltol_mmv[3])
rui$abstol_default = as.numeric(rui$abstol_mmv[3])
rui$pgtol_default = as.numeric(rui$pgtol_mmv[3])
rui$maxit_default = as.numeric(rui$maxit_mmv[3])
rui$factr_default = as.numeric(rui$factr_mmv[3])



rui$bfgs_label = "Choose optimization method: "
rui$bu_label = "Uniform uncertainty regularization parameter"
rui$ndeps_label = "Finite difference approximation step size:"
rui$reltol_label = "Relative convergence tolerance:"
rui$abstol_label = "Absolute convergence tolerance:"
rui$pgtol_label =  "Helps control convergence tolerance for \"L-BFGS-B\" method:"
rui$maxit_label = "Max number of iterations:"
rui$factr_label =  "Controls convergence of \"L-BFGS-B\" method:"

rui$bfgs_bstt = "Method \"BFGS\" is a quasi-Newton method that uses function values and gradients to build up a picture of the surface to be optimized. Method \"L-BFGS-B\" allows a lower and/or upper bound. This uses a limited-memory modification ofthe BFGS quasi-Newton method. If non-trivial bounds are supplied, this method will be selected."
rui$bu_bstt = "Uniform uncertainty regularization parameter"
rui$ndeps_bstt = "A vector of step sizes for the finite-difference approximation to the gradient, on par/parscale scale. Ex) 1e-3."
rui$reltol_bstt = "Relative convergence tolerance. The algorithm stops if it is unable to reduce the value by a factor of reltol * (abs(val) + reltol) at a step. Typically about 1e-8."
rui$abstol_bstt = "The absolute convergence tolerance. Only useful for non-negative functions, as a tolerance for reaching zero."
rui$pgtol_bstt = "Helps control the convergence of the \"L-BFGS-B\" method. It is a tolerance on the projected gradient in the current search direction. This defaults to zero, when the check is suppressed."
rui$maxit_bstt = "The maximum number of iterations. Defaults to 100 for the derivative-based methods, and 500 for \"Nelder-Mead\"."
rui$factr_bstt = "Controls the convergence of the \"L-BFGS-B\" method. Convergence occurs when the reduction in the objective is within this factor of the machine tolerance. Ex) factr of 1e7 is a tolerance of about 1e-8."
rui$hyperparamRestore_bstt = "Restores all optional hyperparameters used in the optimization routine"



# Export settings 
# rui$whatToExport = c("Training Data", 
#                      "Assay Performance", 
#                      "Test Data", 
#                      "Raw Data",
#                      "Action Log",
#                      "All of the Above")
rui$whatToExport = c("All of the Above")
rui$wteLength = length(rui$whatToExport)
rui$exportNameChoices = c("Default naming convention", "Enter name")
# rui$expFileTypeChoices = c(".xlsx", ".csv", ".txt")
rui$expFileTypeChoices = c(".xlsx")
rui$exportFigureOptions = c(".jpg", ".png")


rui$exportTabs = c("Data", "Figures", "Session")







# ==============================================================================
#
#                       5) INIT REACTIVE VARIABLES
#
# ==============================================================================
# ------------------------------------------------------------------------------
# 5a) rv for general reactive values
# ------------------------------------------------------------------------------
# Init reactive values for shiny gui use
rv <- reactiveValues()

# List of strings that may prevent a numeric column from being read correctly
rv$na = c("", "-", "--", "NA", "N/A", "na", "n/a", "NULL", "null")

# i) General metadata associated with rv$df_____________________________________
rv$df = NULL
# contains subsetted rv$df
rv$filteredDF = NULL
rv$filteredDF_forAllPlot = NULL
rv$testData_filteredDF = NULL
rv$testData_filteredDF_forAllPlot = NULL
# df column names
rv$dataColNames = NULL
# Numeric columns
rv$nums = NULL
# Numeric colnames
rv$numDataColNames = NULL

# Current hyperparameters for optim() call
rv$bfgsChoice = rui$bfgsChoices[1]
rv$bu = as.numeric(rui$bu_default[3])
rv$ndeps = as.numeric(rui$ndeps[3])
rv$reltol = as.numeric(rui$reltol[3])
rv$abstol = as.numeric(rui$abstol[3])
rv$pgtol = as.numeric(rui$pgtol[3])
rv$maxit = as.numeric(rui$maxit[3])
rv$factr = as.numeric(rui$factr_default[3])


# Reactive variables to store latest confusion matrices
# Bound uncertainty confusion matrix
rv$boundUncertaintyRhoMax = NULL
rv$evaluateResultsSumm = NULL
rv$optimizedEvalResultsSum = NULL

# ii) Pre-import metadata ______________________________________________________
# Track all excel sheets if file type is xlsx
rv$currSheets = NULL
# Track number of excel sheets in upload
rv$sheetCount = 1
# Previous header row selection (to resolve 0 -> "No Header" conflict)
rv$prevHeaderRowSelect = 0
# Current header row
rv$headerRow = 0
# Total number of rows in file
rv$rawNumRows = 0
# Column range depends on default value determined earlier 
rv$minMaxCol = rui$colRangeDefault



# Track all excel sheets if file type is xlsx
rv$testData_currSheets = NULL
# Track number of excel sheets in upload
rv$testData_sheetCount = 1
# Previous header row selection (to resolve 0 -> "No Header" conflict)
rv$testData_prevHeaderRowSelect = 0
# Current header row
rv$testData_headerRow = 0
# Total number of rows in file
rv$testData_rawNumRows = 0
# Column range depends on default value determined earlier 
rv$testData_minMaxCol = rui$colRangeDefault


# iii) Optional metadata _______________________________________________________
# User selects a column to assign a unique ID column
rv$sampleIDCol = NULL
# User selects column(s) for additional metadata to track in plots
rv$metadataCols = NULL

# iv) Antigen metadata _________________________________________________________
# Save dataframe containing antigen metadata
rv$addAntigen = NULL
# Track previous antigen data
rv$prevAntigenData = NULL
# Keep track of number of antigens assigned
rv$numDimensions = 0
# Track previous number of antigens for indexing
rv$prevNumAnt = 0
# Diff tracks if adding (+) or removing (-) an antigen
rv$diffAnt = 0
# This number creates a unique set of UI IDs every time antigens are edited
rv$numDimCount = 0


# Save dataframe containing antigen metadata
rv$testData_addAntigen = NULL
# Track previous antigen data
rv$testData_prevAntigenData = NULL
# Keep track of number of antigens assigned
rv$testData_numDimensions = 0
# Track previous number of antigens for indexing
rv$testData_prevNumAnt = 0
# Diff tracks if adding (+) or removing (-) an antigen
rv$testData_diffAnt = 0
# This number creates a unique set of UI IDs every time antigens are edited
rv$testData_numDimCount = 0



# v) Class metadta _____________________________________________________________
# Save dataframe containing class assignments
rv$addClass = NULL
# Use counter for number of classes added
rv$addedClasses= 0
# This 2nd counter is used to create a unique set of UI IDs every time classes
# are edited
rv$numClassCount = 0


# Save dataframe containing class assignments
rv$testData_addClass = NULL
# Use counter for number of classes added
rv$testData_addedClasses= 0
# This 2nd counter is used to create a unique set of UI IDs every time classes
# are edited
rv$testData_numClassCount = 0


# v) Analysis related __________________________________________________________
# Track if user wants raw or log transformed data for analysis
rv$rawOrLog = rui$rawOrLog[1]
# Counter used to keep track of outmat arrays in matrix viewer
rv$currA2IDX = 1

# The number of sigvals is saved here
rv$maxA2IDX = 1
# Store all optimization iteration values
rv$allIterVals = NULL

# ------------------------------------------------------------------------------
# 5b) op for optimization related variables
# ------------------------------------------------------------------------------
op <- reactiveValues()

op$outmat = NULL
op$L = NULL


# ------------------------------------------------------------------------------
# 5c) db primarily used for search history
# ------------------------------------------------------------------------------

tst <- reactiveValues()

# Store test data modeling results 
tst$allIterVals = NULL

tst$currACounter_q1 = 1
tst$currACounter_q2 = 1
tst$currACounter_q3 = 1




# ------------------------------------------------------------------------------
# 5d) db primarily used for search history
# ------------------------------------------------------------------------------
db <- reactiveValues()
# Set database to empty on start
db$dbEmpty = TRUE
db$name = "Default_Database.sqlite"

# ------------------------------------------------------------------------------
# 5d) Action log reactive var
# ------------------------------------------------------------------------------
# Reactive value containing action history
actionLog <- reactiveValues()
actionLog$sessionStart = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
actionLog$dispMessage = ""
actionLog$updateCount = 0

# ==============================================================================
#
#                       6) LOCKING REACTIVE VARIABLES
#
# ==============================================================================
# ------------------------------------------------------------------------------
# 6a) Locks for Pre-import
# ------------------------------------------------------------------------------
lock_preImport <- reactiveValues()
lock_preImport$done0 = FALSE
# dataImport_fileLoadButton.R lock when "Load" button pressed until rv$df done
lock_preImport$done1 = FALSE
# Locks on column contingent data for dropdown menus
lock_preImport$done2 = FALSE
# dataImport_addAntigens.R 
lock_preImport$done3 = FALSE

# ------------------------------------------------------------------------------
# 6b) Locks for import
# ------------------------------------------------------------------------------
lock_import <- reactiveValues()
lock_import$done1 = FALSE
lock_import$done2 = FALSE
lock_import$done3 = FALSE

# ------------------------------------------------------------------------------
# 6c) Locks for analysis
# ------------------------------------------------------------------------------
lock_analysis <- reactiveValues()
lock_analysis$done1 = FALSE
lock_analysis$done2 = FALSE
lock_analysis$done3 = FALSE
lock_analysis$done4 = FALSE
lock_analysis$done5 = FALSE
lock_analysis$done6 = FALSE

# ------------------------------------------------------------------------------
# 6a) Locks for Test Data Pre-import
# ------------------------------------------------------------------------------
lock_testData <- reactiveValues()
# dataImport_fileLoadButton.R lock when "Load" button pressed until rv$df done
lock_testData$done1 = FALSE
# Locks on column contingent data for dropdown menus
lock_testData$done2 = FALSE
# dataImport_addAntigens.R 
lock_testData$done3 = FALSE
lock_testData$done4 = FALSE
# Used specifically for getting q values from optim routine
lock_testData$done5 = FALSE

