## =============================================================================

# TITLE: ITL_GIT/shiny/app.R 

# DESCRIPTION: 
    # The only .R file found in the shiny/ directory, this program sources the
    # first files necessary to render the shiny application by calling 
    # subroutines found in shiny/source/. See below for a glossary of the 
    # folders utilized within the source/ directory.  

# FILES
    # 1) app.R - run shiny app.

# SUBDIRECTORIES 
    # 1) ITL_GIT/shiny/CodeBackups - source code backups in .zip format.
    #
    # 2) ITL_GIT/shiny/Data - User may place data inputs into this folder within
    #                         a subfolder. Do not place data in here directly.
    # 3) ITL_GIT/shiny/Exports - Application exports are sent here. 
    # 
    # 4) ITL_GIT/shiny/Libraries - .sqlite libraries built from the application
    #                             are sent to this directory
    # 5) ITL_GIT/shiny/Misc - miscellaneous supporting files. 
    #
    # 6) ITL_GIT/shiny/source - The majority of source code files found here. 

# GLOSSARY OF DIRECTORIES WITHIN shiny/source/:
    # 1) source/ - source code main root directory found in the shiny/ folder.
    #
    # 2) source/a_StartHere/ - Contains programs called from app.R, see below.
    #
    # 3) source/b_Server/ - Contains the app server, which calls subroutines to 
    #                      make computations and render the app.
    # 4) source/b_UI/ - Contains the app UI (user interface) for app appearance.
    #
    # 5) source/c_C++Functions - Contains subroutines written in rcpp.
    #
    # 6) source/c_RFunctions - contains subroutines written in base R language, 
    #                         these functions are meant to be general purpose.
    # 7) source/c_ServerFunctions - Subroutines for rendering the application.
    #                        
    # 8) source/c_SQLFunctions - Subroutines for accessing SQL database backend.

## =============================================================================

## Load essential programs for app
    # Clear environment
    rm(list=ls())
    
    # load global variables, UI, server
    source("source/a_StartHere/global.R")
    source('source/a_StartHere/app_UI.R', local = TRUE)
    source('source/a_StartHere/app_Server.R', local = TRUE)
  

  ## Package shiny app
    shinyApp(
      ui = appUI,
      server = appServer
    )
    
    
    