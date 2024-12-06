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
#
# ==============================================================================



appServer <- function(input,output,session){
  
  # ============================================================================
  # 1) GENERAL SERVER FUNCTIONS
  # ============================================================================
  
  # To close the application and shutoff database connection
  source('source/b_Server/stopApp.R', local=TRUE)
  
  
  # ============================================================================
  # 2) DATA IMPORT & TRAINING
  # ============================================================================
  
  # File upload and import settings
  source('source/b_Server/dataImport_fileLoadButton.R', local=TRUE)
  
  # Add antigens column by column and relevant observeEvents
  source('source/b_Server/dataImport_addAntigens.R', local=TRUE)
  
  # Add classes by subcategory and relevant observeEvents
  source('source/b_Server/dataImport_addClasses.R', local=TRUE)
  
  # Other observeEvents involved in Data import UI elements
  source('source/b_Server/dataImport_supportingObserveEvents.R', local=TRUE)
  
  
  
}







