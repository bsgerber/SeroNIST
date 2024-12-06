# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                           Program Header Status  
#                             COMPLETED 091823
#                   Header: 🗸  Comments: 🗸   Refactored: 🗸         
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ==============================================================================
#
# TITLE: b_Server/testData_submitImport.R 
#
#                               DESCRIPTION
# This program contains code that responds to the "Submit" button to import data
# and apply filters and selections made by the user and save to the SQL database
# backend. Some initial plots of the selected data are also rendered.
#
#                             TABLE OF CONTENTS
#       1) SUBMIT BUTTON BEHAVIOR
#
#       2) DATA IMPORT & APPLY SETTINGS
#             2a) Apply row and column filters
#             2b) Get positive and negative class
#             2c) Get additional metadata
#
#       3) UPDATE ACTION LOG
#
#                                 NOTES
#
# ==============================================================================
# ==============================================================================
# 
#                        1) SUBMIT BUTTON BEHAVIOR
#
# ==============================================================================
# This observeEvent responds to the submit button and requires the user to first
# assign a sufficient number of antigens and classes for their uploaded data.
observeEvent(input$testData_submitImport, {
  req(isTruthy(rv$testData_addAntigen) &&  #isTruthy(rv$testData_addClass) && 
        dim(rv$testData_addAntigen)[1] >= 1) # && dim(rv$testData_addClass)[1] >= 0)
  
  # disable("testData_submitImport")
  output$testDataTrainingTabs = NULL
  
  # # Lock reactive variables
  lock_testData$done1 = FALSE
  # lock_testData$done2 = FALSE
  lock_testData$done3 = FALSE
  ## 031424 reset this
  lock_testData$done4 = FALSE
  lock_testData$done5 = FALSE
  
  # [lock_import$done1]
  testData_applyDataImportSettings()
  
  # # Get antigen and positive/negative class data and optional metadata
  # # [lock_import$done2] --> rv$positives and rv$negatives 
  # getAntigenPosNegClasses() 
  
  # # Get optional metadata
  # assignOptionalCols()
  
  testData_renderRawDataTable()
  
  testData_renderFilteredDataTable()
  
  plot_anyData(ID ="testData_firstGraph", 
               TabID = "testData_initialImportPlotTabs", 
               Tabs = 2,  Dim = rv$testData_numDimensions, 
               Transform = "Raw", Stage = "Test", 
               Bound = "No", Precision = "Guess", 
               Select = "Column", Disp = "Subset",
               Session = session)
  
  plot_anyData(ID ="testData_logGraph", 
               TabID = "testData_initialImportPlotTabs", 
               Tabs = 2,  Dim = rv$testData_numDimensions, 
               Transform = "Log", Stage = "Test", 
               Bound = "No", Precision = "Guess", 
               Select = "Column", Disp = "Subset",
               Session = session)
  
  # Update tab for data acquisition selection
  updateTabsetPanel(session = session, inputId = "testDataImportTabs",
                    selected = rui$testTabNamesImport[4])
  
  # release lock to continue
  lock_testData$done3 = TRUE
  
  # # update action log for this button press
  # dataImportSubmit_updateActionLog()
  
  # enable("testData_submitImport")
  
})



testData_renderFilteredDataTable <- function(){
  req(lock_testData$done1 && lock_testData$done2 && 
        isTruthy(rv$testData_filteredDF))
  # i) Get min for training data _________________________________________
  train_df = rv$filteredDF
  ## 120523 added
  train_allAntigenCols = unlist(as.character(rv$addAntigen$colSelect))
  allMin = min(train_df[, train_allAntigenCols])
  
  # i) Identify numeric vs int columns _________________________________________
  df = rv$testData_filteredDF
  allAntigenCols = unlist(as.character(rv$testData_addAntigen$colSelect))
  testMin = min(df[, allAntigenCols])
  
  if (!isTruthy(train_allAntigenCols)){
    allMin = testMin
  }
  
  if (rv$testData_numDimensions == 1){
    allLgTransformed = logTransformation_2dArray_testMin(
      data.frame(df[, allAntigenCols]), 
      allMin, testMin)
  } else {
    
    
    
    allLgTransformed = logTransformation_2dArray_testMin(df[, allAntigenCols], 
                                                         allMin, testMin)
  }
  
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
  output$testData_filteredDataTable <- DT::renderDataTable({
    req(lock_testData$done1)
    # For numeric columns (in df2), set sigfigs depending but only for double
    df[, df2] = df[, df2]  %>%
      mutate(across(everything(),
                    .fns = function(x) {format(round(x, 4), nsmall = 4)}))
    
    allAg = data.frame(agName = rv$testData_addAntigen$colSelect,
                       agRename = rv$testData_addAntigen$colRenamed)
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
  output$testDataTableTabs <- renderTestDataTableTabs(3, session)
  
}



# ==============================================================================
# 
#                       2) DATA IMPORT & APPLY SETTINGS
#
# ==============================================================================
# ------------------------------------------------------------------------------
# 2a) Apply row and column filters
# ------------------------------------------------------------------------------
# This is the first helper called in a series of functions that respond to the
# "Submit" button press. This function extracts the antigen columns selected by
# the user, converts any non-numeric antigen columns to numeric and NA removal,
# and applies row selections if Option B was selected
testData_applyDataImportSettings <- function(){
  # If non-numeric columns selected for antigens, resolve here 
  tdf = rv$testData_originalDF
  # names of the columns for the user's selected antigens
  allAntigenCols = unlist(as.character(rv$testData_addAntigen$colSelect))
  # datatype in the antigen columns selected
  allColClasses = sapply(tdf, class)
  # all column names
  allColNames = colnames(tdf)
  
  # Go through each column and assign as numeric if needed
  for (i in 1:ncol(tdf)){
    # Get info about column i
    currCol = tdf[, i] 
    currColName = allColNames[i]
    # currClass = sapply(currCol, class)
    currClass = allColClasses[i]
    
    # If the column is not numeric and is an antigen column
    if (currClass != "numeric" && currColName %in% allAntigenCols){
      # Convert this column to numeric and unsave Nans
      convertAntigen = as.numeric(tdf[[currColName]])
      # Overwrite column with numeric
      tdf[[currColName]] = convertAntigen
    }
  }
  
  # Save new rv$df only if NA's exist after processing above for loop
  udf = tdf[rowSums(is.na(tdf[, allAntigenCols]))==0, ]
  # Option A for test data
  if (isOptionA("test")){
    # If a class column is selected or classes were added
    if (isTruthy(rv$testData_addClass) && isTruthy(rv$testData_selectClassCol) 
        && rv$testData_selectClassCol != "None"){
      # Add a column for renamed classes
      udf$classColumn = udf[[rv$testData_selectClassCol]]
      # Call on helper function to handle combined classes
      udf = overwriteCombinedClasses(udf, "test")
      # This is where rv$testData_filteredDF is defined first
      rv$testData_filteredDF = udf
      rv$testData_selectClassCol = "classColumn"
      
      # MUST COME FIRST 
      # Filtered data (not by row) to be used in plot
      rv$testData_filteredDF_forAllPlot = rv$testData_filteredDF[
        # Subset rows
        ,
        # Col select
        c(unlist(as.character(rv$testData_addAntigen$colSelect)), 
          rv$testData_selectClassCol,
          # Subset additional columns
          rv$testData_selectSampleIDCol, rv$testData_selectMetadataCols
        )]
      
      # Filtered data to be used in analysis
      rv$testData_filteredDF = rv$testData_filteredDF[
        # Row select
        rv$testData_filteredDF[[rv$testData_selectClassCol]] %in% 
          unlist(rv$testData_addClass$colRenamed),
        # Col select
        c(unlist(as.character(rv$testData_addAntigen$colSelect)), 
          rv$testData_selectClassCol,
          # Subset additional columns
          rv$testData_selectSampleIDCol, rv$testData_selectMetadataCols
        )]  
      
    } else {
      # If no added classes, use all of the data 
      nrowudf = nrow(udf)
      udf$classColumn = rep("Other", nrowudf)
      
      udf = overwriteCombinedClasses(udf, "test")
      rv$testData_filteredDF = udf
      rv$testData_selectClassCol = "classColumn"
      
      # MUST COME FIRST 
      # Filtered data (not by row) to be used in plot
      rv$testData_filteredDF_forAllPlot = rv$testData_filteredDF[
        ,
        # Col select
        c(unlist(as.character(rv$testData_addAntigen$colSelect)),
          rv$testData_selectClassCol,
          # Subset additional columns
          rv$testData_selectSampleIDCol, rv$testData_selectMetadataCols
        )]
      
      # Filtered data to be used in analysis
      rv$testData_filteredDF = rv$testData_filteredDF[
        # Subset rows
        ,
        # Col select
        c(unlist(as.character(rv$testData_addAntigen$colSelect)), 
          rv$testData_selectClassCol,
          # Subset additional columns
          rv$testData_selectSampleIDCol, rv$testData_selectMetadataCols
        )]   
    } # end conditional
    
  } else {
    
    # Option B
    if (!isTruthy(rv$testData_selectClassCol) || 
        rv$testData_selectClassCol == "None" || 
        rv$testData_selectClassCol == "classColumn"){
      
      # If no added classes, use all of the data 
      nrowudf = nrow(udf)
      udf$classColumn = rep("Other", nrowudf)
    } else {
      udf$classColumn = udf[[rv$testData_selectClassCol]]
    }
    
    print(paste0("Option B about to overwrite combined classes"))
    ud1 <<- udf
    udf = overwriteCombinedClasses(udf, "test")
    print(paste0("Option B SUCCESS to overwrite combined classes"))
    
    ud2 <<- udf
    
    rv$testData_filteredDF = udf
    rv$testData_selectClassCol = "classColumn"
    classesToFilter = getClassesToFilter(rv$testData_addClass)
    
    # MUST COME FIRST 
    # Filtered data (not by row) to be used in plot
    rv$testData_filteredDF_forAllPlot = rv$testData_filteredDF[
      ,
      # Col select
      c(unlist(as.character(rv$testData_addAntigen$colSelect)),
        rv$testData_selectClassCol,
        # Subset additional columns
        rv$testData_selectSampleIDCol, rv$testData_selectMetadataCols
      )]
    
    abd1 <<- rv$testData_filteredDF_forAllPlot
    
    abd2 <<- rv$testData_filteredDF
    abd3 <<- rv$testData_selectSampleIDCol
    abd4 <<- rv$testData_selectMetadataCols
    
    # Filtered data to be used in analysis
    rv$testData_filteredDF = rv$testData_filteredDF[
      # Subset rows
      rv$testData_filteredDF[[rv$testData_selectClassCol]] %in% classesToFilter, 
      # Col select
      c(unlist(as.character(rv$testData_addAntigen$colSelect)), 
        rv$testData_selectClassCol,
        # Subset additional columns
        rv$testData_selectSampleIDCol, rv$testData_selectMetadataCols)]
    
    # # Get number of classes added by user
    # tnumclass = dim(rv$testData_addClass)[1]
    # # For each class assigned by user
    # for (i in 1:tnumclass){
    #   # Get the UI metadata for the current class
    #   currRVClass = rv$testData_addClass[i, ]
    #   # NOTE - Class names for Option B are encoded as row numbers
    #   # Example label of a class ie. "1:5, 10:12" for rows 1-5 and 10-12. 
    #   rowsToSubset = currRVClass$selectedClasses
    #   # The new class name, if provided
    #   colRenamed = currRVClass$colRenamed
    #   # Class number
    #   currClassNum = currRVClass$classNum
    #   
    #   # Check if column name valid, if not create one
    #   if (!isTruthy(colRenamed)){
    #     colRenamed = paste0("Class ", currClassNum)
    #   }
    #   # Get row numbers from the class 
    #   numList = interpretSeqList(rowsToSubset)
    #   # Filter the rows 
    #   cdf = udf[numList, ]
    #   
    #   ## 092223 Need to overwrite the cdf class column for this section
    #   optionBLabels = rep(colRenamed, length(numList))
    #   cdf = cbind(
    #     udf[numList, ],
    #     optionBLabels
    #   )
    #   
    #   rv$testData_selectClassCol = "optionBLabels"
    #   # Aggregate the results
    #   all_cdf = rbind(all_cdf, cdf)
    # }
    # 
    # rv$testData_filteredDF = all_cdf
    
  } # end isOptionA() 
  
  # Lock reactive variables
  lock_testData$done1 = TRUE
}
