
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                           Program Header Status  
#                             COMPLETED 092023
#                   Header: 🗸  Comments: 🗸   Refactored: 🗸      
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ==============================================================================
# 
# TITLE: b_Server/testData_fileLoadButton.R
#
#                               DESCRIPTION
# This program contains the first observeEvents that respond to a file upload &
# "Load" button press. When a file is selected using the "Browse" button, 
# metadata for that file are immediately collected and used to render additional
# data acquisition tools with ranges that are specific to the file and depend 
# on the file extension (excel or csv). 
# When pre-import settings are ready, the "Load" button is pressed to commit 
# those settings and import the upload into a reactive variable in R. 
# 
#                             TABLE OF CONTENTS
#       1) FILE UPLOAD
#             1a) Update Excel Sheet
# 
#       2) LOAD BUTTON PRESS
#             2a) Import Data
#
#       3) HELPER FUNCTIONS FOR FILE UPLOAD
#             3a) Update Pre-Import Settings
#             3b) Import data
#             3c) Render Sample ID Available Columns
#                   i) Find columns that contain all unique entries
#                   ii) Render sample ID dropdown menu
#
#             3d) Render dropdown menus for class assignment
#
#             3e) Render raw datatable
#                   i) Identify numeric vs int columns
#                   ii) Render Sample ID dropdown menu
#                   iii) Raw datatable output

# ==============================================================================
# ==============================================================================
#
#                               1) FILE UPLOAD
#
# ==============================================================================
# This observeEvent gets the metadata needed to upload a file when one is chosen
observeEvent(input$testData_fileUploadInput, {
  # Clear reactive variables from previous file upload 
  lock_testData$done1 = FALSE
  rv$testDataFileType = NULL
  rv$testDataFileUploadPath = NULL
  rv$testData_numDimensions = 0
  rv$testData_sheetCount = NULL
  # Clear outputs with every new file upload
  output$testDataAcquisitionTabs = NULL
  output$testDataInitialImportPlotTabs = NULL
  output$testDataTrainingTabs = NULL
  
  # Clear outputs that render as a result of file upload
  output$testData_separatingLine <- renderUI({NULL})
  output$testData_optionalSettingsTitle <- renderUI({NULL})
  output$testData_selectSampleIDCol <- renderUI({NULL})
  output$testData_selectMetadataCols <- renderUI({NULL})
  output$testData_doneImportBtn <- renderUI({NULL})
  # output$testData_evalTabOptimBtn <- renderUI({NULL})
  
  # Clear previous class assignments
  if (isTruthy(rv$testData_addClass)){
    testData_clearAddedClasses()
  }
  
  # Get the datapath from the file upload
  # datapath = paste0("Data/", input$testData_fileUploadInput[1])
  datapath = paste0(input$testData_fileUploadInput$datapath)
  
  # Get the file format
  numberofocc = length(strsplit(datapath, "\\.")[[1]])
  rv$testDataFileType = strsplit(datapath,"\\.")[[1]][numberofocc]
  # Write to reactive variable
  rv$testDataFileUploadPath = datapath
  
  # Render tabs for data acquisition (bulk of data import tab)
  output$testDataAcquisitionTabs <- renderTestDataAcquisitionTabs()
  
  # Each time a new file is uploaded, preimport inputs must update their ranges
  updateTestDataUIBeforeBtnPress()

  # Provide access to excel tools only if the current file upload is excel
  if (rv$testDataFileType == "xlsx"){
    # Render UI for excel sheet dropdown menu select
    output$testData_selectSheet <- renderUI({
      selectInput("testData_selectSheet", "Select Excel sheet to upload",
                  choices = rv$testData_currSheets,
                  selected = rv$testData_currSheets[1])
    })
    enable("testData_selectSheet")
  } else {
    disable("testData_selectSheet")
    
    output$testData_selectSheet <- renderUI({NULL})
  }
  
  testData_renderPreviewDataTable()
  
  # Adjust number of antigens to match training data if available
  updateSelectInput(session = session, 
                    inputId = "testData_numDimensions",
                    rui$selectedAntigens,
                    choices = rui$numPossibleAntigens,
                    selected = rv$numDimensions)
  if (lock_import$done3){
    disable("testData_numDimensions")
  }
})

# ------------------------------------------------------------------------------
# 1a) Update Excel Sheet
# ------------------------------------------------------------------------------
# This observeEvent updates pre-import UI based on which excel sheet is chosen
observeEvent(input$testData_selectSheet, {
  # This comes into play at time of actual file upload
  rv$testData_sheetCount = input$testData_selectSheet
  # update dropdown menu everytime a new sheet is chosen
  updateTestDataUIBeforeBtnPress()
  
  testData_renderPreviewDataTable()
})

# ==============================================================================
#
#                             2) LOAD BUTTON PRESS
# 
# ==============================================================================
# This obvserveEvent responds to the initial "Load" button that the user presses
# after they have uploaded their file and completed pre-upload steps (such as 
# selecting desired Excel sheet, header location, columns to import).
# The file is then read into R after applying the user's settings.
observeEvent(input$testData_updateDataSelect, {
  # # Use helper function to go to next tab automatically
  # testData_tabSwitchNext()
  
  # # Reset the datatable loaded previously ### Need this eventually
  output$testData_rawDataTable <- NULL
  
  # Reset rv$addAntigen and rv$addClass
  rv$testData_addAntigen = NULL
  rv$testData_addClass = NULL
  rv$testData_df = NULL
  rv$testData_originalDF = NULL
  # Refresh UI to catch recent changes, this is necessary since the user may 
  # have made different pre-import selections before pressing "Load"
  updateTestDataUIBeforeBtnPress()
  
  # Import the file with user settings and file extension to update rv$df
  # [lock_testData$done1]
  testData_fileToReactiveVar()
  
  # Get more info of file upload's columns for sample ID and class dropdown menus
  # [lock_testData$done2]
  testData_computeUniqueCols()
  
  testData_renderDependentDropdownMenus()
  
  
  
  # Render initial datatable of file upload
  testData_renderRawDataTable()
  
  # Switch tabs 
  updateTabsetPanel(session = session, inputId = "testDataTableTabs",
                    selected = rui$trainingDataTableTabNames[2])
  
  # # Update action log
  # updateActionLog(paste0("File loaded.<br/>",
  #                        "File type: ", rv$filetype, "<br/>",
  #                        "Header Row Number: ", rv$headerRow, "<br/>"
  # )
  # )
  
}) # end updateDataSelect aka "Load" button



# ==============================================================================
#
#                   3) HELPER FUNCTIONS FOR FILE UPLOAD
#
# ==============================================================================
# ------------------------------------------------------------------------------
# 3a) Update Pre-Import Settings
# ------------------------------------------------------------------------------
updateTestDataUIBeforeBtnPress <- function(){
  req(isTruthy(input$testData_fileUploadInput))
  
  if (rv$testDataFileType == "xlsx"){
    # Get available sheets in the upload if excel
    rv$testData_currSheets = excel_sheets(rv$testDataFileUploadPath)
    # By default use first sheet unless observeEvent triggered
    sheetCount = 1
    
    # Avoid crashes only proceed when ready
    if (isTruthy(input$testData_selectSheet)){
      # Update sheetcount if on first sheet
      if (input$testData_selectSheet %in% rv$testData_currSheets){
        sheetCount = which(input$testData_selectSheet %in% rv$testData_currSheets)
      } 
    }
    
    # Load a temp copy of the data to get number of rows in the selected sheet
    tempdf = suppressMessages(
      read_xlsx(rv$testDataFileUploadPath, sheetCount)
    )
  } else if (rv$testDataFileType == "csv"){
    tempdf = read.csv(rv$testDataFileUploadPath)
    
  } 
  
  # Get metadata to help fill in some reactive inputs
  numRowsDF = nrow(tempdf)
  rui$testData_numPossibleRowsDataStart = seq(1, numRowsDF)
  rui$testData_numPossibleRowsDataStart = c("No Header", rui$testData_numPossibleRowsDataStart)
  
  # Avoid crashes
  if (isTruthy(input$testData_specifyHeaderRow)){
    # Update header row select tracker
    if (input$testData_specifyHeaderRow == "No Header"){
      rv$testData_prevHeaderRowSelect = 0
    } else {
      rv$testData_prevHeaderRowSelect = as.numeric(input$testData_specifyHeaderRow)
    }
  } 
  
  # render the UI for specifying header row number
  output$testData_specifyHeaderRow <- renderUI({
    # suppressMessages fails, too many options under selectInput choices
    # Only show first 100 rows to prevent harmless warning 
    selectInput("testData_specifyHeaderRow", 
                label = rui$selHeader,
                choices = rui$testData_numPossibleRowsDataStart[1:100], # Only show first 
                selected = rv$testData_prevHeaderRowSelect
    ) # end selectInput
  }) # end renderUI
  
}


# ------------------------------------------------------------------------------
# 3b) Import Data into reactive variable
# ------------------------------------------------------------------------------
testData_fileToReactiveVar <- function(){
  # Block other actions that require latest rv$df
  lock_testData$done1 = FALSE
  # adjust header location and rows to skip based on user input 
  headerRow = as.numeric(rv$testData_headerRow)
  headerRowSkip = headerRow - 1
  
  if (headerRowSkip < 0){
    # Don't skip negative number of rows
    headerRowSkip = 0
  } 
  
  # Handle excel files
  if (rv$testDataFileType == "xlsx"){
    
    # If no header is present (select 0 for header row)
    if (headerRow == 0){
      # Read excel sheet without column names (default to numeric names)
      df = suppressMessages(
        read_xlsx(rv$testDataFileUploadPath,
                  rv$testData_sheetCount, skip = headerRowSkip,
                  col_names = FALSE, na = rv$na
        )
      )
    } else {
      df = read_xlsx(rv$testDataFileUploadPath,
                     rv$testData_sheetCount, skip = headerRowSkip,
                     na = rv$na
      )
    }
    # Handle csv files
  } else if (rv$testDataFileType == "csv"){
    # If no header, don't apply column names
    if (headerRow == 0){
      df = read_csv(rv$testDataFileUploadPath, col_names = FALSE, na = rv$na)
      # If the first row is the header, use that row as column names
    } else if (headerRow == 1){
      df = read_csv(rv$testDataFileUploadPath, col_names = TRUE, na = rv$na)
    } else {
      # Apply a skip if header row is not the first row
      df = read_csv(rv$testDataFileUploadPath, col_names = TRUE, skip = headerRowSkip,
                    na = rv$na)
    }
  }
  
  
  # Save df reactively
  rv$testData_originalDF = df
  
  # IMPORTANT - The following are for general updates to metadata inherent to 
  #             the file upload only. 
  #             ie) user-selected settings aren't included until "Submit" pressed
  # Get column names of data
  rv$testData_dataColNames = colnames(df)
  # Get numeric columns
  rv$testData_nums <- unlist(lapply(df, is.numeric), use.names = FALSE)
  
  # Get names of numeric columns
  rv$testData_numDataColNames = colnames(df[, rv$testData_nums])
  
  # Free lock to continue
  lock_testData$done1 = TRUE
}

# ------------------------------------------------------------------------------
# 3c) Render Sample ID Available Columns
# ------------------------------------------------------------------------------
testData_computeUniqueCols <- function(){
  req(lock_testData$done1)
  lock_testData$done2 = FALSE
  
  # Initialize data structures for unique columns
  colIsUnique = c()
  uniqueCols = c()
  # df = data.frame(df)
  # Save column names of import
  dataColNames = colnames(rv$testData_originalDF)
  
  # Get total number of rows
  numTotalEntries = dim(rv$testData_originalDF)[1]
  
  # i) Find columns that contain all unique entries ____________________________
  # Loop through all columns
  for (i in 1:length(dataColNames)){
    # Get current column
    currColName = dataColNames[i]
    currCol = rv$testData_originalDF[, currColName]
    # Get number of unique entries in current column i
    uniqueEntries = unique(currCol)
    
    # numUniqueEntries = dim(uniqueEntries)[1]
    numUniqueEntries = length(uniqueEntries)
    
    
    # See if the column is valid for a sample ID column (every value is)
    if (numUniqueEntries == numTotalEntries){
      colIsUnique = c(colIsUnique, "TRUE")
      # Store list of all entirely unique columns
      uniqueCols = c(uniqueCols, currColName)
    } else {
      colIsUnique = c(colIsUnique, "FALSE")
    }
  } # end i loop
  
  # ii) Render Sample ID dropdown menu _________________________________________
  # Save to reactive variable
  # All columns that are entirely unique
  rv$testData_uniqueCols = uniqueCols
  # all column names
  rv$testData_dataColNames = dataColNames
  
  # Free lock
  lock_testData$done2 = TRUE
  
}


# ------------------------------------------------------------------------------
# 3d) Render dropdown menus for class assignment
# ------------------------------------------------------------------------------
testData_renderDependentDropdownMenus <- function(){
  req(lock_testData$done2)
  
  # Highlight invalid columns that would not be suitable for a unique ID column
  concatStmt = colorInvalidColumns("#testData_selectSampleIDCol",
                                   rv$testData_dataColNames, 
                                   rv$testData_uniqueCols)
  
  # Render the dropdown menu to select a column for unique/sample ID
  output$testData_selectSampleIDCol <- renderUI({
    div(tags$head(
      # Highlight options for selectInput
      tags$style(HTML(concatStmt))),
      selectInput("testData_selectSampleIDCol",
                  rui$selectSampleID, 
                  choices = c(rui$noneSel, 
                              rv$testData_dataColNames),
                  selected = rui$noneSel)
    )
  })
  # } # end conditional block
  
  # Render the dropdown menu for additional metadata columns to track
  output$testData_selectMetadataCols <- renderUI({
    # Highlight options for selectInput
    # tags$style(HTML(concatStmt))),
    selectInput("testData_selectMetadataCols",
                rui$selectMetadataCol, 
                choices = rv$testData_dataColNames,
                multiple = TRUE
    )
  })
  
  # Render dropdown menu for column selection for class assignments
  # output$testData_selectClassCol <- renderUI({
  #   selectInput("testData_selectClassCol",
  #               "Select a column containing class assignments:",
  #               choices = rv$testData_dataColNames,
  #               selected = rv$testData_dataColNames[1])
  # })
  output$testData_selectClassCol <- renderUI({
    selectInput("testData_selectClassCol",
                "Select a column containing class assignments:",
                choices = c("None", rv$testData_dataColNames),
                selected = "None")
  })
  
  # Render line to divide import from optional settings
  output$testData_separatingLine <- renderUI({
    p("______________________________________________________________________________________________")
  })
  output$testData_optionalSettingsTitle <- renderUI({
    strong("Optional Settings:")
  })
  
  # Render "Next" button to go to next tab after optional settings
  output$testData_doneImportBtn <- renderUI({
    actionButton("testData_doneImportBtn", "Next", width = "240px")
  })
  
  ## 112023 added to allow another file upload in same session
  ## Error fixed 020124
  ### 021324 not sure why this was used, removed
  # rv$testData_selectClassCol = rv$dataColNames[1]

  testData_renderSelectclassDropdown()
}



testData_renderPreviewDataTable <- function(){
  # Handle excel files
  if (rv$testDataFileType == "xlsx"){
    
    df = suppressMessages(
      read_xlsx(rv$testDataFileUploadPath,
                rv$testData_sheetCount, 
                na = rv$na, 
                col_names = FALSE
      ))
    
    # Handle csv files
  } else if (rv$testDataFileType == "csv"){
    # If no header, don't apply column names
    df = read_csv(rv$testDataFileUploadPath, col_names = FALSE, na = rv$na)
  }
  
  
  # Save df reactively
  rv$testData_previewDF = df
  
  # # i) Identify numeric vs int columns _________________________________________
  df = rv$testData_previewDF
  # # Get numeric columns
  df2 = sapply(df, is.double)
  
  # Use this function to determine if a column contains only integers
  is_whole <- function(x){
    # Be prepared to handle non-numeric data (will crash)
    if (is.numeric(x)){
      all(floor(x) == x)
    } else {FALSE}
  }
  # Find which columns are integers (will return either NA or TRUE)
  df3 = sapply(df, is_whole)
  # Get the index of columns that contain only integers
  wholeidx = which(is.na(df3))
  trueidx = which(isTruthy(df3))
  # Set those columns to false so they are not adjusted for sigfigs
  df2[wholeidx] = FALSE
  df2[trueidx] = FALSE
  
  # Render the tabs separating different datatables 
  if (lock_testData$done1){
    output$testDataTableTabs <- renderTestDataTableTabs(2, session)
  } else {
    # Rendering datatable tabs for first time
    output$testDataTableTabs <- renderTestDataTableTabs(1, session)
  }
  
  
  
  # ii) Raw datatable output ___________________________________________________
  output$testData_previewDataTable <- DT::renderDataTable({
    # req(lock_preImport$done2)
    # For numeric columns (in df2), set sigfigs depending but only for double
    df[, df2] = df[, df2]  %>%
      mutate(across(everything(),
                    .fns = function(x) {format(round(x, 4), nsmall = 4)}))
    
    df
    
  }, 
  escape = TRUE,
  # filter = "top", # search box
  selection = "multiple", 
  # extensions = c("FixedHeader", "FixedColumns"), # Causes extra long scroll
  extensions = c("FixedColumns"),
  server = FALSE, # disable this for numeric sort
  options = list(
    paging = FALSE,
    scrollX = TRUE,
    scrollY = "500px",
    autoWidth = TRUE,
    fixedHeader = T,
    fixedColumns = list(leftColumns = 1),
    columnDefs = list(
      list(width = '100px', targets = 2:ncol(df) - 1)
    )
    
  ) # end option list
  ) # end renderDataTable
  
  
  
  
}




# ------------------------------------------------------------------------------
# 3e) Render raw datatable
# ------------------------------------------------------------------------------
testData_renderRawDataTable <- function(){
  # rv$df must be done loading before proceeding
  req(lock_testData$done1 && lock_testData$done2)
  
  # i) Identify numeric vs int columns _________________________________________
  df = rv$testData_originalDF
  # # Get numeric columns
  df2 = sapply(df, is.double)
  
  # Use this function to determine if a column contains only integers
  is_whole <- function(x){
    # Be prepared to handle non-numeric data (will crash)
    if (is.numeric(x)){
      all(floor(x) == x)
    } else {FALSE}
  }
  # Find which columns are integers (will return either NA or TRUE)
  df3 = sapply(df, is_whole)
  # Get the index of columns that contain only integers
  wholeidx = which(is.na(df3))
  trueidx = which(isTruthy(df3))
  # Set those columns to false so they are not adjusted for sigfigs
  df2[wholeidx] = FALSE
  df2[trueidx] = FALSE
  
  # ii) Raw datatable output ___________________________________________________
  output$testData_rawDataTable <- 
    DT::renderDataTable({
      req(lock_testData$done2)
      # For numeric columns (in df2), set sigfigs depending but only for double
      df[, df2] = df[, df2]  %>%
        mutate(across(everything(),
                      .fns = function(x) {format(round(x, 4), nsmall = 4)}))
      
      df
    }, 
    escape = TRUE,
    filter = "top", # search box
    selection = "multiple", 
    # extensions = c("FixedHeader", "FixedColumns"), # Causes extra long scroll
    extensions = c("FixedColumns"),
    server = FALSE, # disable this for numeric sort
    options = list(
      paging = FALSE,
      scrollX = TRUE,
      scrollY = "500px",
      autoWidth = TRUE,
      fixedHeader = T,
      fixedColumns = list(leftColumns = 1),
      columnDefs = list(
        list(width = '100px', targets = 2:ncol(df) - 1)
      )
      
    ) # end option list
    ) # end renderDataTable
  
  # Update tabs
  output$testDataTableTabs <- renderTestDataTableTabs(2, session)
  
  # MAke proxy to clear row selections
  rdtt_proxy <<- dataTableProxy('testData_rawDataTable')
}

