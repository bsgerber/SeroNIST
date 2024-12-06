# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                           Program Header Status  
#                               COMPLETE 092123
#                   Header: 🗸  Comments: 🗸   Refactored: 🗸         
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ==============================================================================
#
# TITLE: app_Server.R 
#
#                              DESCRIPTION
# This program loads server functions responsible for the SeroNIST backend. 
# This includes observeEvents, eventReactives, inputs and outputs that comprise 
# the app server. This does not include non-specific helper functions written in
# R, C++ or SQL. 
#
#
#                            TABLE OF CONTENTS
#         1) General Server Functions
#         2) DATA IMPORT & TRAINING
#
#
#                                 NOTES
#       - Server functions called from this program can be found in the 
#           "source/b_Server/" folder. 
#       - User interface (UI) programs can be found in the 
#           "source/b_UI/" folder. 
#
# ==============================================================================

appServer <- function(input,output,session){
  
  # ============================================================================
  # 1) GENERAL SERVER FUNCTIONS
  # ============================================================================
  volumes = getVolumes()
  roots <- c(home = '~')
  
  shinyDirChoose(input, "exportDirChoose", roots = volumes(), session = session)
  
  output$chosenExportDir <- renderText({
    rv$exportDir <- parseDirPath(volumes, input$exportDirChoose)
    
  })
  
  # ============================================================================
  # 2) DATA IMPORT & TRAINING
  # ============================================================================
  
  # File upload and import settings
  source('source/b_Server/dataImport_fileLoadButton.R', local=TRUE)
  
  # Add antigens column by column and relevant observeEvents
  source('source/b_Server/dataImport_addAntigens.R', local=TRUE)
  
  # Add classes by subcategory and relevant observeEvents
  source('source/b_Server/dataImport_addClasses.R', local=TRUE)
  
  # Add classes by subcategory and relevant observeEvents
  source('source/b_Server/dataImport_submitImport.R', local=TRUE)
  
  # Other observeEvents involved in Data import UI elements
  source('source/b_Server/dataImport_supportingObserveEvents.R', local=TRUE)
  
  source('source/b_Server/trainingModel_performOptimization.R', local=TRUE)
  
  source('source/b_Server/trainingModel_setInitialConditions.R', local=TRUE)
  
  # Other observeEvents involved in training model 
  source('source/b_Server/trainingModel_supportingObserveEvents.R', local=TRUE)
  
  # File upload and import settings
  source('source/b_Server/testData_fileLoadButton.R', local=TRUE)
  
  # Add antigens column by column and relevant observeEvents
  source('source/b_Server/testData_addAntigens.R', local=TRUE)
  
  # Add classes by subcategory and relevant observeEvents
  source('source/b_Server/testData_addClasses.R', local=TRUE)
  
  # Add classes by subcategory and relevant observeEvents
  source('source/b_Server/testData_submitImport.R', local=TRUE)
  

  
  # ============================================================================
  # ) MATH FUNCTIONS
  # ============================================================================
  
  source('source/c_MathFunctions/allMathFunctions.R', local=TRUE)
  
  # ============================================================================
  # ) SQL FUNCTIONS
  # ============================================================================
  
  source('source/c_SQLFunctions/sqlConnectionManager.R', local=TRUE)
  
  # ============================================================================
  # ) GRAPHS
  # ============================================================================
  source('source/b_Server/plotData_testData.R', local=TRUE)
  
  source('source/b_Server/plotData_supportingFunctions.R', local=TRUE)
  
  source('source/b_Server/testData_performOptimization.R', local = TRUE)
  
  
  # ============================================================================
  #  EXPORT TAB
  # ============================================================================
  # Get dimensions of a data structure with this helper getDataDetails()
  
  source('source/b_Server/exportData_submitExport.R', local = TRUE)
  # ============================================================================
  #  OTHER SERVER FUNCTIONS
  # ============================================================================
  # Get dimensions of a data structure with this helper getDataDetails()
  source('source/d_Other/getDataDetails.R', local=TRUE)
  
  # To close the application and shutoff database connection
  source('source/d_Other/stopApp.R', local=TRUE)
  
}

