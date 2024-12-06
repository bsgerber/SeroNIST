# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                           Program Header Status  
#                             COMPLETED 092023
#                   Header: 🗸  Comments: 🗸   Refactored: 🗸      
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ==============================================================================
# 
# TITLE: b_Server/dataImport_fileLoadButton.R
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
observeEvent(input$fileUploadInput, {
  # Clear reactive variables from previous file upload 
  rv$filetype = NULL
  rv$fileUploadPath = NULL
  rv$numDimensions = 0
  
  ## added 010524
  lock_preImport$done1 = FALSE
  rv$sheetCount = NULL
  
  # Clear other outputs
  output$dataAcquisitionTabs = NULL
  output$initialImportPlotTabs = NULL
  output$dataTrainingTabs = NULL
  
  # output$testDataAcquisitionTabs = NULL
  # output$testData_initialImportPlotTabs = NULL
  # output$testDataTrainingTabs = NULL
  
  
  # Clear outputs that render as a result of file upload
  output$separatingLine <- renderUI({NULL})
  output$optionalSettingsTitle <- renderUI({NULL})
  output$selectSampleIDCol <- renderUI({NULL})
  output$selectMetadataCols <- renderUI({NULL})
  output$doneImportBtn <- renderUI({NULL})
  output$evalTabOptimBtn <- renderUI({NULL})
  
  # output$testData_separatingLine <- renderUI({NULL})
  # output$testData_optionalSettingsTitle <- renderUI({NULL})
  # output$testData_selectSampleIDCol <- renderUI({NULL})
  # output$testData_selectMetadataCols <- renderUI({NULL})
  # output$testData_doneImportBtn <- renderUI({NULL})
  # output$testData_evalTabOptimBtn <- renderUI({NULL})
  # 
  
  # Clear previous class assignments
  if (isTruthy(rv$addClass)){
    clearAddedClasses()
  }
  # if (isTruthy(rv$testData_addClass)){
  #   testData_clearAddedClasses()
  # }
  
  # Get the datapath from the file upload
  # datapath = paste0("Data/", input$fileUploadInput[1])
  datapath = paste0(input$fileUploadInput$datapath)
  
  # Get the file format
  numberofocc = length(strsplit(datapath, "\\.")[[1]])
  rv$filetype = strsplit(datapath,"\\.")[[1]][numberofocc]
  # Write to reactive variabl
  rv$fileUploadPath = datapath
  
  # Render tabs for data acquisition (bulk of data import tab)
  output$dataAcquisitionTabs <- renderDataAcquisitionTabs()
  ## 112023 added for multiple file per session support
  setupAntigenUI()
  
  
  # Each time a new file is uploaded, preimport inputs must update their ranges
  updateUIBeforeBtnPress()
  
  # Provide access to excel tools only if the current file upload is excel
  if (rv$filetype == "xlsx"){
    # Render UI for excel sheet dropdown menu select
    output$selectSheet <- renderUI({
      selectInput("selectSheet", 
                  label = rui$selExcel,
                  choices = rv$currSheets,
                  selected = rv$currSheets[1])
    })
    enable("selectSheet")
  } else {
    disable("selectSheet")
    output$selectSheet <- renderUI({NULL})
      
  }
  
  
  
  renderPreviewDataTable()
  
})

# ------------------------------------------------------------------------------
# 1a) Update Excel Sheet
# ------------------------------------------------------------------------------
# This observeEvent updates pre-import UI based on which excel sheet is chosen
observeEvent(input$selectSheet, {
  
  # This comes into play at time of actual file upload
  rv$sheetCount = input$selectSheet
  # update dropdown menu everytime a new sheet is chosen
  # updateUIBeforeBtnPress() ### reomved 010524
  
  renderPreviewDataTable()
})


renderPreviewDataTable <- function(){
  # Handle excel files
  if (rv$filetype == "xlsx"){
    df = suppressMessages(
      read_xlsx(rv$fileUploadPath, rv$sheetCount, na = rv$na, col_names = FALSE
      ))
    
    # Handle csv files
  } else if (rv$filetype == "csv"){
    # If no header, don't apply column names
    df = read_csv(rv$fileUploadPath, col_names = FALSE, na = rv$na)
  }
  
  # Save df reactively
  rv$previewDF = df
  
  # # i) Identify numeric vs int columns _________________________________________
  df = rv$previewDF
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
  if (lock_preImport$done1){
    output$dataTableTabs = renderDataTableTabs(2, session)
  } else {
    # Rendering datatable tabs for first time
    output$dataTableTabs = renderDataTableTabs(1, session)
  }
  
  # ii) Raw datatable output ___________________________________________________
  output$previewDataTable <- DT::renderDataTable({
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

# ==============================================================================
#
#                             2) LOAD BUTTON PRESS
# 
# ==============================================================================
# This obvserveEvent responds to the initial "Load" button that the user presses
# after they have uploaded their file and completed pre-upload steps (such as 
# selecting desired Excel sheet, header location, columns to import).
# The file is then read into R after applying the user's settings.
observeEvent(input$updateDataSelect, {
  # # Use helper function to go to next tab automatically
  # tabSwitchNext()
  
  # Reset antigens
  updateSelectInput(session = session, inputId = "numDimensions",
                    rui$selectedAntigens,
                    choices = rui$numPossibleAntigens,
                    selected = 1)
  
  # Clear previous class assignments
    clearAddedClasses()
  
  # Reset the datatable loaded previously
  output$initialImportPlotTabs = NULL
  output$dataTrainingTabs = NULL
  output$evalTabOptimBtn <- renderUI({NULL})
  output$rawDataTable <- NULL
  
  # Reset rv$addAntigen and rv$addClass
  rv$addAntigen = NULL
  rv$addClass = NULL
  rv$df = NULL
  rv$originalDF = NULL
  rv$numDimensions = 1
  

  
  # Clear previous antigens
  
  # Refresh UI to catch recent changes, this is necessary since the user may 
  # have made different pre-import selections before pressing "Load"
  updateUIBeforeBtnPress()
  
  # Import the file with user settings and file extension to update rv$df
  # [lock_preImport$done1]
  fileToReactiveVar()
  
  # Get more info of file upload's columns for sample ID and class dropdown menus
  computeUniqueCols()
  
  # [lock_preImport$done2]
  renderDependentDropdownMenus()
  
  # Render initial datatable of file upload
  renderRawDataTable()
  
  
  # Update action log
  updateActionLog(paste0("File loaded.<br/>",
                         "File type: ", rv$filetype, "<br/>",
                         "Header Row Number: ", rv$headerRow, "<br/>"
  )
  )
  
}) # end updateDataSelect aka "Load" button



# ==============================================================================
#
#                   3) HELPER FUNCTIONS FOR FILE UPLOAD
#
# ==============================================================================
# ------------------------------------------------------------------------------
# 3a) Update Pre-Import Settings
# ------------------------------------------------------------------------------
updateUIBeforeBtnPress <- function(){
  req(isTruthy(input$fileUploadInput))
  
  if (rv$filetype == "xlsx"){
    # Get available sheets in the upload if excel
    rv$currSheets = excel_sheets(rv$fileUploadPath)
    # By default use first sheet unless observeEvent triggered
    sheetCount = 1
    
    # Avoid crashes only proceed when ready
    if (isTruthy(input$selectSheet)){
      # Update sheetcount if on first sheet
      if (input$selectSheet %in% rv$currSheets){
        sheetCount = which(input$selectSheet %in% rv$currSheets)
      } 
    }
    
    # Load a temp copy of the data to get number of rows in the selected sheet
    tempdf = suppressMessages(
      read_xlsx(rv$fileUploadPath, sheetCount)
    )
  } else if (rv$filetype == "csv"){
    tempdf = read.csv(rv$fileUploadPath)
    
  } 
  
  # Get metadata to help fill in some reactive inputs
  numRowsDF = nrow(tempdf)
  rui$numPossibleRowsDataStart = seq(1, numRowsDF)
  rui$numPossibleRowsDataStart = c("No Header", rui$numPossibleRowsDataStart)
  
  # Avoid crashes
  if (isTruthy(input$specifyHeaderRow)){
    # Update header row select tracker
    if (input$specifyHeaderRow == "No Header"){
      rv$prevHeaderRowSelect = 0
    } else {
      rv$prevHeaderRowSelect = as.numeric(input$specifyHeaderRow)
    }
  } 
  
  # render the UI for specifying header row number
  output$specifyHeaderRow <- renderUI({
    # suppressMessages fails, too many options under selectInput choices
    # Only show first 100 rows to prevent harmless warning 
    if (numRowsDF > 100){
      selectInput("specifyHeaderRow",
                  label = rui$selHeader,
                  choices = rui$numPossibleRowsDataStart[1:100], # Only show first 
                  selected = rv$prevHeaderRowSelect
      ) # end selectInput
    } else {
      selectInput("specifyHeaderRow",
                  label = rui$selHeader,
                  choices = rui$numPossibleRowsDataStart, # Only show first 
                  selected = rv$prevHeaderRowSelect
      ) # end selectInput
    }
  }) # end renderUI
}


# ------------------------------------------------------------------------------
# 3b) Import Data into reactive variable
# ------------------------------------------------------------------------------
fileToReactiveVar <- function(){
  # Block other actions that require latest rv$df
  lock_preImport$done1 = FALSE
  # adjust header location and rows to skip based on user input 
  headerRow = as.numeric(rv$headerRow)
  headerRowSkip = headerRow - 1
  
  if (headerRowSkip < 0){
    # Don't skip negative number of rows
    headerRowSkip = 0
  } 
  
  # Handle excel files
  if (rv$filetype == "xlsx"){
    
    # If no header is present (select 0 for header row)
    if (headerRow == 0){
      # Read excel sheet without column names (default to numeric names)
      df = suppressMessages(
        read_xlsx(rv$fileUploadPath, rv$sheetCount, skip = headerRowSkip,
                  col_names = FALSE, na = rv$na
        )
      )
    } else {
      df = read_xlsx(rv$fileUploadPath, rv$sheetCount, skip = headerRowSkip,
                     na = rv$na
      )
    }
    # Handle csv files
  } else if (rv$filetype == "csv"){
    # If no header, don't apply column names
    if (headerRow == 0){
      df = read_csv(rv$fileUploadPath, col_names = FALSE, na = rv$na)
      # If the first row is the header, use that row as column names
    } else if (headerRow == 1){
      df = read_csv(rv$fileUploadPath, col_names = TRUE, na = rv$na)
    } else {
      # Apply a skip if header row is not the first row
      df = read_csv(rv$fileUploadPath, col_names = TRUE, skip = headerRowSkip,
                    na = rv$na)
    }
  }
  
  
  # Save df reactively
  rv$originalDF = df
  
  # IMPORTANT - The following are for general updates to metadata inherent to 
  #             the file upload only. 
  #             ie) user-selected settings aren't included until "Submit" pressed
  # Get column names of data
  rv$dataColNames = colnames(df)
  # Get numeric columns
  rv$nums <- unlist(lapply(df, is.numeric), use.names = FALSE)
  # Get names of numeric columns
  rv$numDataColNames = colnames(df[, rv$nums])
  
  # Free lock to continue
  lock_preImport$done1 = TRUE
}

# ------------------------------------------------------------------------------
# 3c) Render Sample ID Available Columns
# ------------------------------------------------------------------------------
computeUniqueCols <- function(){
  req(lock_preImport$done1)
  lock_preImport$done2 = FALSE
  
  # Initialize data structures for unique columns
  colIsUnique = c()
  uniqueCols = c()
  # df = data.frame(df)
  # Save column names of import
  dataColNames = colnames(rv$originalDF)
  
  # Get total number of rows
  numTotalEntries = dim(rv$originalDF)[1]
  
  # i) Find columns that contain all unique entries ____________________________
  # Loop through all columns
  for (i in 1:length(dataColNames)){
    # Get current column
    currColName = dataColNames[i]
    currCol = rv$originalDF[, currColName]
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
  rv$uniqueCols = uniqueCols
  # all column names
  rv$dataColNames = dataColNames
  
  # Free lock
  lock_preImport$done2 = TRUE
  
}


# ------------------------------------------------------------------------------
# 3d) Render dropdown menus for class assignment
# ------------------------------------------------------------------------------
renderDependentDropdownMenus <- function(){
  req(lock_preImport$done2)
  
  # Highlight invalid columns that would not be suitable for a unique ID column
  concatStmt = colorInvalidColumns("#selectSampleIDCol",
                                   rv$dataColNames, rv$uniqueCols)
  
  
  # Render the dropdown menu to select a column for unique/sample ID
  output$selectSampleIDCol <- renderUI({
    div(tags$head(
      # Highlight options for selectInput
      tags$style(HTML(concatStmt))),
      selectInput("selectSampleIDCol",
                  rui$selectSampleID, 
                  choices = c(rui$noneSel, rv$dataColNames),
                  selected = rui$noneSel)
    )
  })
  # } # end conditional block
  
  # Render the dropdown menu for additional metadata columns to track
  output$selectMetadataCols <- renderUI({
    # Highlight options for selectInput
    # tags$style(HTML(concatStmt))),
    selectInput("selectMetadataCols",
                rui$selectMetadataCol, 
                choices = rv$dataColNames,
                multiple = TRUE
    )
  })
  
  # Render dropdown menu for column selection for class assignments
  output$selectClassCol <- renderUI({
    selectInput("selectClassCol",
                "Select column for class labels:",
                choices = c("None", rv$dataColNames),
                selected = "None")
  })
  
  # Render line to divide import from optional settings
  output$separatingLine <- renderUI({
    p("______________________________________________________________________________________________")
  })
  output$optionalSettingsTitle <- renderUI({
    strong("Optional Settings:")
  })
  
  # Render "Next" button to go to next tab after optional settings
  output$doneImportBtn <- renderUI({
    actionButton("doneImportBtn", "Next", width = "240px")
  })
  
  ## 112023 added to allow another file upload in same session
  rv$selectClassCol = rv$dataColNames[1]
  
  renderSelectclassDropdown()
}



renderFilteredDataTable <- function(){
  req(lock_preImport$done1 && lock_preImport$done2)
  
  # i) Identify numeric vs int columns _________________________________________
  df = rv$filteredDF
  ## 120523 added
  allAntigenCols = unlist(as.character(rv$addAntigen$colSelect))
  allMin = min(df[, allAntigenCols])
  
  allLgTransformed = logTransformation_2dArray_allMin(data.frame(df[, allAntigenCols]), allMin)
  colnames(allLgTransformed) = paste0("Log Transform ", colnames(allLgTransformed))
  df = cbind(df, allLgTransformed)
  
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
  output$filteredDataTable <- DT::renderDataTable({
    req(lock_preImport$done2)
    

    
    
    # For numeric columns (in df2), set sigfigs depending but only for double
    df[, df2] = df[, df2]  %>%
      mutate(across(everything(),
                    .fns = function(x) {format(round(x, 4), nsmall = 4)}))
    
    # Get column renames if they exist
    allAg = data.frame(agName = rv$addAntigen$colSelect,
                       agRename = rv$addAntigen$colRenamed)
    # Iterate through all antigens
    for (i in 1:length(allAg)){
      currRename = allAg$agRename[i]
      newName = allAg$agName[i]
      # If a rename exists
      if (isTruthy(currRename)){
        newName = currRename
        # Rename current column of data frame to new new
        colnames(df)[i] = currRename
      }
    }
    
    df
    
    # Other alternatives for sigfigs
    # df[, df4] = df[, df4]  %>%
    #   mutate(across(where(
    #     function(x) is.double(x) & 
    #       sapply(!sum(x%%1)==0, isFALSE)),
    #     .fns = function(x) {format(round(x, 4), nsmall = 4)}))
    #
    # df  %>%
    #   mutate(across(where(function(x) is.double(x) & !sum(x%%1)==0),
    #                 .fns = function(x) {format(round(x, 4), nsmall = 4)}))
    # 
    # mutate(across(where(function(x) is.numeric(x) & !is.integer(x)), 
    # sprintf, fmt = '%.2f'))
    # 
    # df = df %>%
    #   mutate(across(everything(), as.character))
    
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
  
  # Render the tabs separating different datatables 
  output$dataTableTabs <- renderDataTableTabs(3, session)
}

# ------------------------------------------------------------------------------
# 3e) Render raw datatable
# ------------------------------------------------------------------------------
renderRawDataTable <- function(){
  # rv$df must be done loading before proceeding
  req(lock_preImport$done1 && lock_preImport$done2)
  
  
  # i) Identify numeric vs int columns _________________________________________
  df = rv$originalDF
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
  output$rawDataTable <- DT::renderDataTable({
    req(lock_preImport$done2)
    # For numeric columns (in df2), set sigfigs depending but only for double
    df[, df2] = df[, df2]  %>%
      mutate(across(everything(),
                    .fns = function(x) {format(round(x, 4), nsmall = 4)}))
    
    df
    
    # Other alternatives for sigfigs
    # df[, df4] = df[, df4]  %>%
    #   mutate(across(where(
    #     function(x) is.double(x) & 
    #       sapply(!sum(x%%1)==0, isFALSE)),
    #     .fns = function(x) {format(round(x, 4), nsmall = 4)}))
    #
    # df  %>%
    #   mutate(across(where(function(x) is.double(x) & !sum(x%%1)==0),
    #                 .fns = function(x) {format(round(x, 4), nsmall = 4)}))
    # 
    # mutate(across(where(function(x) is.numeric(x) & !is.integer(x)), 
    # sprintf, fmt = '%.2f'))
    # 
    # df = df %>%
    #   mutate(across(everything(), as.character))
    
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
  output$dataTableTabs <- renderDataTableTabs(2, session)
  # Make proxy to clear row selections 
  rdt_proxy <<- dataTableProxy('rawDataTable')
}

