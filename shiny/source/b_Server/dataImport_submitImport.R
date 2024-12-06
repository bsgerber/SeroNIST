# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                           Program Header Status  
#                             COMPLETED 091823
#                   Header: 🗸  Comments: 🗸   Refactored: 🗸         
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ==============================================================================
#
# TITLE: b_Server/dataImport_submitImport.R 
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
observeEvent(input$submitImport, {
  req(isTruthy(rv$addAntigen) && isTruthy(rv$addClass) && 
        # dim(rv$addAntigen)[1] > 1 && 
        dim(rv$addClass)[1] > 1)
  
  # Clear downstream outputs from previous analyses
  output$evalTabOptimBtn <- renderUI({NULL})
  output$dataTrainingTabs = NULL
  
  # Lock reactive variables
  lock_import$done1 = FALSE
  lock_import$done2 = FALSE
  lock_import$done3 = FALSE
  lock_analysis$done1 = FALSE
  lock_analysis$done2 = FALSE
  lock_analysis$done3 = FALSE
  lock_analysis$done4 = FALSE
  lock_analysis$done5 = FALSE
  lock_analysis$done6 = FALSE
  
  # Lock test data reactive variables
  lock_testData$done1 = FALSE
  lock_testData$done2 = FALSE
  lock_testData$done3 = FALSE
  lock_testData$done4 = FALSE
  lock_testData$done5 = FALSE
  
  # rv$currSelClassCol = rv$selectClassCol
  rv$selectClassCol =  rv$currSelClassCol 
  
  
  # Connect to a SQL database
  openDBConnection("Build")
  # Save the imported data to table named "rawData"
  dbWriteTable(con, "rawData", rv$originalDF, append = FALSE, overwrite = TRUE)
  
  # Update action log
  updateActionLog("Writing file upload to SQL database in table \"rawData\"")
  
  ## 090823 NO MORE MODIFICATIONS TO rv$df BEYOND THIS POINT
  # Remove NAs and apply row select (option B) to rv$df
  
  # [lock_import$done1]
  applyDataImportSettings()
  # Get antigen and positive/negative class data and optional metadata
  # [lock_import$done2] --> rv$positives and rv$negatives 
  getAntigenPosNegClasses() 
  # Get optional metadata
  assignOptionalCols()
  
  # Render new table showing filtered selections with tabs
  renderFilteredDataTable()
  
  # # Display tabs for plots to be rendered now
  # output$initialImportPlotTabs <- renderImportAndTrainingTabs(2, session)
  
  plot_anyData(ID ="firstGraph", 
               TabID = "initialImportPlotTabs", 
               Tabs = 2,  Dim = rv$numDimensions, 
               Transform = "Raw", Stage = "Train", 
               Bound = "No", Precision = "Guess", 
               Select = "Column", Disp = "Subset",
               Session = session)
  
  plot_anyData(ID ="logGraph", 
               TabID = "initialImportPlotTabs", 
               Tabs = 2,  Dim = rv$numDimensions, 
               Transform = "Log", Stage = "Train", 
               Bound = "No", Precision = "Guess", 
               Select = "Column", Disp = "Subset",
               Session = session)
  # output$firstGraph <- renderPlotly({
  #   # Render a plot for the raw data
  #   plot_rawData("dataImport", "init")
  # })
  
  
  # output$logGraph <- renderPlotly({
  #   # Render a plot for the log transformed data
  #   plot_logTransformData("dataImport", "init")
  # })
  
  
  rv$newAddClass = rv$addClass
  rv$addClass = rv$addClass
  # release lock to continue
  lock_import$done3 = TRUE
  
  # At this point we know the test data will have the same number of antigens
  updateSelectInput(session = session, 
                    inputId = "testData_numDimensions",
                    rui$selectedAntigens,
                    choices = rui$numPossibleAntigens,
                    selected = rv$numDimensions
  )
  
  disable("testData_numDimensions")
  # update action log for this button press
  dataImportSubmit_updateActionLog()
  
  # Go to final tab ## added 120123 after EValuate tab created
  tabSwitchNext()
  
  
  
})


# Render the dropdown menu to select a column for unique/sample ID
observeEvent(input$selectSampleIDCol, {
  
  if (isTruthy(input$selectSampleIDCol) && 
      input$selectSampleIDCol != rui$noneSel){
    rv$selectSampleIDCol = input$selectSampleIDCol
    
  } else {
    rv$selectSampleIDCol = NULL
    
  }
})

# Render the dropdown menu to select a column for unique/sample ID
observeEvent(input$testData_selectSampleIDCol, {
  
  if (isTruthy(input$testData_selectSampleIDCol) && 
      input$testData_selectSampleIDCol != rui$noneSel){
    rv$testData_selectSampleIDCol = input$testData_selectSampleIDCol
    
  } else {
    rv$testData_selectSampleIDCol = NULL
    
  }
})

# Render the dropdown menu for additional metadata columns to track
observeEvent(input$selectMetadataCols, {
  if (isTruthy(input$selectMetadataCols)){
    rv$selectMetadataCol = input$selectMetadataCols
  } else {
    rv$selectMetadataCol = NULL
  }
})

# Render the dropdown menu for additional metadata columns to track
observeEvent(input$testData_selectMetadataCols, {
  if (isTruthy(input$testData_selectMetadataCols)){
    rv$testData_selectMetadataCols = input$testData_selectMetadataCols
  } else {
    rv$testData_selectMetadataCols = NULL
  }
})



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
applyDataImportSettings <- function(){
  # If non-numeric columns selected for antigens, resolve here 
  tdf = rv$originalDF
  # tdf = rv$originalDF
  # names of the columns for the user's selected antigens
  allAntigenCols = unlist(as.character(rv$addAntigen$colSelect))
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
  
  
  ## 090723 apply row filter
  if (isOptionA("train")){
    # Only continue if a class column is selected
    # At least two classes are required for training data acquire to continue
    req(rv$selectClassCol != "None")
    ## 022224 adding class rename column
    udf$classColumn = udf[[rv$selectClassCol]]
    # Call on helper function to handle combined classes
    udf = overwriteCombinedClasses(udf, "train")
    # THIS IS WHERE rv$filteredDF IS DEFINED
    rv$filteredDF = udf
    rv$selectClassCol = "classColumn"
    
    # MUST COME FIRST
    # Filtered data to be used in plots
    rv$filteredDF_forAllPlot = rv$filteredDF[
      # # Subset rows
      ,
      # Subset columns
      c(unlist(as.character(rv$addAntigen$colSelect)), rv$selectClassCol,
        # Subset additional columns
        rv$selectSampleIDCol, rv$selectMetadataCol)
    ]
    
    # Filtered data to be used in analysis
    rv$filteredDF = rv$filteredDF[
      # Subset rows
      rv$filteredDF[[rv$selectClassCol]] %in% 
        # c(unlist(as.character(rv$addClass$selectedClasses))), 
        c(unlist(as.character(rv$addClass$colRenamed))), 
      # Subset columns
      c(unlist(as.character(rv$addAntigen$colSelect)), rv$selectClassCol,
        # Subset additional columns
        rv$selectSampleIDCol, rv$selectMetadataCol
      )]
    
    # # Filtered data to be used in analysis
    # rv$filteredDF =rv$filteredDF[
    #   # Subset rows
    #   rv$filteredDF[[rv$selectClassCol]] %in% rv$addClass$selectedClasses, 
    #   # Subset columns
    #   c(unlist(as.character(rv$addAntigen$colSelect)), rv$selectClassCol,
    #     # Subset additional columns
    #     rv$selectSampleIDCol, rv$selectMetadataCol
    #   )]
    
  } else {
    # Option B
    if (rv$selectClassCol == "None"){
      # If no class column selected, use blank class called "Other"
      nrowudf = nrow(udf)
      udf$classColumn = rep("Other", nrowudf)
    } else {
      # If a class column selected, initialize it here into its own column
      udf$classColumn = udf[[rv$selectClassCol]]
    }
    # Combine columns and apply renaming here
    udf = overwriteCombinedClasses(udf, "train")
    # First instance of rv$filteredDF for Training data
    rv$filteredDF = udf
    rv$selectClassCol = "classColumn"
    # Get updated classes to filter specifically for analysis
    classesToFilter = getClassesToFilter(rv$addClass)
    
    # Get a filtered version of data for plots (includes other classes)
    rv$filteredDF_forAllPlot = rv$filteredDF[
      ,
      # # Subset rows
      # rv$filteredDF[[rv$selectClassCol]] %in% classesToFilter,
      # Subset columns
      c(unlist(as.character(rv$addAntigen$colSelect)), rv$selectClassCol,
        # Subset additional columns
        rv$selectSampleIDCol, rv$selectMetadataCol)
    ]
    
    # Filtered data to be used in analysis
    rv$filteredDF =rv$filteredDF[
      # Subset rows
      rv$filteredDF[[rv$selectClassCol]] %in% classesToFilter, 
      # Subset columns
      c(unlist(as.character(rv$addAntigen$colSelect)), rv$selectClassCol,
        # Subset additional columns
        rv$selectSampleIDCol, rv$selectMetadataCol
      )]
  }
  
  # Lock reactive variables
  lock_import$done1 = TRUE
}


overwriteCombinedClasses <- function(udf, mode){
  
  if (mode == "train"){
    if (isOptionA("train")){
      
      for (i in 1:length(rv$addClass$selectedClasses)){
        currSelClass = rv$addClass$selectedClasses[i]
        
        # See if rename provided
        if (isTruthy(rv$addClass$colRenamed[i])){
          colRename = rv$addClass$colRenamed[i]
        } else {
          rv$addClass$colRenamed[i] = currSelClass
          colRename = currSelClass
        }
        
        if (isTruthy(currSelClass)){
          splitBy = unlist(strsplit(currSelClass, "; "))
          # If this is a combined class 
          for (ii in 1:length(splitBy)){
            thisClass = splitBy[ii]
            udf = data.frame(udf, check.names = FALSE)
            udf[udf[[rv$selectClassCol]] %in% thisClass, "classColumn"] = colRename
          }
        } # end if
      } # end i loop
    } else {
      # Option B
      for (i in 1:length(rv$addClass$selectedClasses)){
        currSelClass = rv$addClass$selectedClasses[i]
        
        # See if rename provided
        if (isTruthy(rv$addClass$colRenamed[i])){
          colRename = rv$addClass$colRenamed[i]
        } else {
          currSelClass = paste0("Class ", i)
          rv$addClass$colRenamed[i] = currSelClass
          colRename = currSelClass
        }
        
        if (isTruthy(currSelClass)){
          # Get metadata for i'th row of class assignments
          currRVClass = rv$addClass[i, ]
          rowsToSubset = currRVClass$selectedClasses
          numList = interpretSeqList(rowsToSubset)
          
          udf = data.frame(udf, check.names = FALSE)
          udf[numList, "classColumn"] = colRename
        } # end if
      } # end i loop
    }
  } else if (mode == "test"){
    # TEST DATA
    if (isOptionA("test")){
      
      for (i in 1:length(rv$testData_addClass$selectedClasses)){
        currSelClass = rv$testData_addClass$selectedClasses[i]
        
        # See if rename provided
        if (isTruthy(rv$testData_addClass$colRenamed[i])){
          colRename = rv$testData_addClass$colRenamed[i]
        } else {
          rv$testData_addClass$colRenamed[i] = currSelClass
          colRename = currSelClass
        }
        if (isTruthy(currSelClass)){
          splitBy = unlist(strsplit(currSelClass, "; "))
          # If this is a combined class 
          for (ii in 1:length(splitBy)){
            thisClass = splitBy[ii]
            udf = data.frame(udf, check.names = FALSE)
            udf[udf[[rv$testData_selectClassCol]] %in% thisClass, 
                "classColumn"] = colRename
          }
        } # end if
      } # end i loop
    } else {
      # Option B
      
      tt1 <<- rv$testData_addClass
      for (i in 1:length(rv$testData_addClass$selectedClasses)){
        currSelClass = rv$testData_addClass$selectedClasses[i]
        # See if rename provided
        if (isTruthy(rv$testData_addClass$colRenamed[i])){
          colRename = rv$testData_addClass$colRenamed[i]
        } else {
          currSelClass = paste0("Class ", i)
          rv$testData_addClass$colRenamed[i] = currSelClass
          colRename = currSelClass
        }
        
        print(paste0("i currSelClass [", i, "]: ", currSelClass))
        print(paste0("i colrename [", i, "]: ", colRename))
        
        if (isTruthy(currSelClass)){
          print(paste0("IS TRUTHYCURRSELCLASS AT i [", i, "]"))
          # Get metadata for i'th row of class assignments
          currRVClass = rv$testData_addClass[i, ]
          
          print(paste0("[", i, "]:", currRVClass))
          rowsToSubset = currRVClass$selectedClasses
          numList = interpretSeqList(rowsToSubset)
          udf = data.frame(udf, check.names = FALSE)
          udf[numList, "classColumn"] = colRename
          
          print(paste0("[", i, "] rowsToSubset:", rowsToSubset))
          print(paste0("[", i, "] numList:", numList))
          
          
        } # end if
      } # end i loop
    }
  } # end if test data
  return(udf)
}

getClassesToFilter <- function(addClass){
  classList2Filter = c()
  
  classesAdded = addClass$selectedClasses
  classesRenamed = addClass$colRenamed
  
  for (i in 1:length(classesAdded)){
    currSelClass = classesAdded[i]
    currRename = classesRenamed[i]
    if (isTruthy(currRename)){
      addThis = currRename
    } else {
      addThis = currSelClass
    }
    classList2Filter = c(classList2Filter, addThis)
  }
  return(classList2Filter)
}

# This helper function extracts the classes from the previously curated data. 
getAntigenPosNegClasses <- function(){
  req(lock_import$done1)
  
  # Reset currently stored data
  rv$positives = NULL
  rv$negatives = NULL
  
  # Get antigen data only
  allAntigenCols = unlist(as.character(rv$addAntigen$colSelect))
  # Get classes
  # classesToFilter = unlist(rv$addClass$selectedClasses)
  classesToFilter = getClassesToFilter(rv$addClass)
  
  # Get class assignments only
  classCol = rv$filteredDF[, rv$selectClassCol]
  fil_var = rv$selectClassCol
  # Select data for antigens selected and classes selected
  if (isOptionA("train")){
    # If option A, filter by user-selected column containing classes, 
    # and append antigen columns to class assignment column
    currData = as.data.frame(
      rv$filteredDF[rv$filteredDF[[rv$selectClassCol]] %in% classesToFilter,
                    c(allAntigenCols, rv$selectClassCol)])
  } else {
    # Option B, classes already filtered by row select 
    currData = as.data.frame(
      rv$filteredDF[, c(allAntigenCols, rv$selectClassCol)])
  }
  
  # Get colnames of antigens and classes (fil_var)
  x_var <- paste0(colnames(currData)[1])
  y_var <- paste0(colnames(currData)[2])
  # fil_var <- paste0(colnames(classCol))
  
  # Omit missing data
  currData_omitNA = na.omit(currData)
  # Redundant, but do this to prevent reuse of variable names
  currData_noNeg = currData_omitNA
  if (isOptionA("train")){
    # Option A, extract positive and negative classes
    positives = currData_noNeg[currData_noNeg[[fil_var]] == classesToFilter[1], 
                               allAntigenCols]
    negatives = currData_noNeg[currData_noNeg[[fil_var]] == classesToFilter[2],
                               allAntigenCols]
    
    
  } else {
    
    print(paste0("Option B attempt 1 START"))
    # Option B, classes were manually selected using row select
    # Get class assignments selected by user
    thisClassCol = currData[[rv$selectClassCol]]
    
    # Get user selected class info for positive class
    currRVClass = rv$addClass[1, ]
    # Get the "class" identity
    # in this case Option B saves class as comma delimited sequences 
    # ie) class 1 = "row:seq1, row:seq2"
    rowsToSubset = currRVClass$selectedClasses
    print(paste0("rowsToSubset POS:", rowsToSubset))
    # Take the above string and create numeric sequence from it
    posNumList = interpretSeqList(rowsToSubset)
    
    # Get the minimum from this list
    minPosNum = min(posNumList)
    # Correct index starting at 1
    if (minPosNum != 1){
      posNumList = posNumList - minPosNum + 1
    }
    
    maxPosNum = max(posNumList)
    
    # Repeat above steps for negative class or second row of rv$addClass
    ncurrRVClass = rv$addClass[2, ]
    # Get user selected class info from gui
    negRowsToSubset = ncurrRVClass$selectedClasses
    print(paste0("rowsToSubset NEG:", negRowsToSubset))
    
    # Take above string and create numeric sequence
    negNumList = interpretSeqList(negRowsToSubset)
    
    # Correct indices to follow after the pos class
    minNegNum = min(negNumList)
    ### 112823 try max(negNumList) 
    negNumList = negNumList - minNegNum + 1 + maxPosNum
    
    if (minNegNum != 1){
      negNumList = negNumList - minNegNum + 1
    }
    
    # Option B, get classes 
    positives = currData_noNeg[posNumList, allAntigenCols]
    negatives = currData_noNeg[negNumList, allAntigenCols]
    print(paste0("Option B attempt 1 END"))
    
  }
  
  # Put into reactive variable
  rv$positives = positives
  rv$negatives = negatives
  
  rvp <<- rv$positives
  rvn <<- rv$negatives
  # release lock
  lock_import$done2 = TRUE
}

# ------------------------------------------------------------------------------
# 2c) Get additional metadata
# ------------------------------------------------------------------------------
# This helper function applies user selections from dropdown menus to assign a 
# sample ID column and additional column(s) for other metadata
assignOptionalCols <- function(){
  # Only continue if this lock is freed
  # req(lock_import$done1)
  # 
  # checkCol = input$selectSampleIDCol
  # if (checkCol == rui$noneSel){
  #   # Set to default value if no optional column selected
  #   rv$sampleIDCol = NULL
  # } else {
  #   rv$sampleIDCol = as.character(input$selectSampleIDCol)
  # }
  
  # if (!isTruthy(input$selectMetadataCols)){
  #   # Set to default if no columns selected
  #   rv$metadataCols = NULL
  # } else {
  #   rv$metadataCols = input$selectMetadataCols
  #   currMetadataCols = rv$metadataCols
  # }
  
  # # Check work
  # print(paste0("Current SampleID Col:", rv$sampleIDCol))
  # print(paste0("Current Metadata Col(s):", rv$metadataCols))
  # print(paste0("Current antigens:", rv$addAntigen$colSelect))
  # print(paste0("Current classes:",  rv$addClass$selectedClasses))
  
  # # Get other columns to see if there's any overlap
  # currAntCols = rv$addAntigen$colSelect
  # currClassCols = rv$addClass$selectedClasses
  
  # # Exclude redundant columns 
  # if (isTruthy(currMetadataCols)){
  #   # Iterate through all metadata columns selected
  #   for (i in 1:length(rv$metadataCols)){
  #     
  #   }
  # }
}


# ==============================================================================
# 
#                           3) UPDATE ACTION LOG
#
# ==============================================================================
dataImportSubmit_updateActionLog <- function(){
  req(lock_import$done3)
  # Update action log for antigens added
  thisrvAddAnt = as.data.frame(rv$addAntigen)
  expandedAnt = paste0("Adding antigens using columns selected by user<br/>",
                       "Antigens Selected: ", as.character(rv$numDimensions),
                       "<br/>")
  nAnt = nrow(thisrvAddAnt)
  for (a in 1:nAnt){
    currAnt = thisrvAddAnt[a, ]
    if (a == nAnt){
      # don't add break
      tsep = ""
    } else {
      tsep = "<br/>"
    }
    
    if (isTruthy(currAnt$colRenamed)){
      expandedAnt = paste0(expandedAnt, 
                           paste0("   [", currAnt$antigenNum, "] ", 
                                  currAnt$colSelect, " as ", 
                                  currAnt$colRenamed, tsep))
    } else {
      expandedAnt = paste0(expandedAnt, 
                           paste0("   [", currAnt$antigenNum, "] ", 
                                  currAnt$colSelect, tsep))
    }
  }
  
  updateActionLog(paste0(expandedAnt))
  
  
  thisrvaddClass = as.data.frame(rv$addClass)
  if (isOptionA("train")){
    expandedClass = paste0("Adding classes using Option A by column \"", 
                           rv$selectClassCol, "\"<br/>")
    
  } else {
    expandedClass = "Adding classes using Option B, by row selections<br/>"
    
  }
  expandedClass = paste0(expandedClass, 
                         "Classes Selected: ", as.character(rv$numDimensions),
                         "<br/>")
  nClass = nrow(thisrvaddClass)
  for (a in 1:nClass){
    currClass = thisrvaddClass[a, ]
    if (a == nClass){
      # don't add break
      tsep = ""
    } else {
      tsep = "<br/>"
    }
    
    if (isTruthy(currClass$colRenamed)){
      expandedClass = paste0(expandedClass, paste0(
        "   [", currClass$classNum, "] ", currClass$selectedClasses, 
        " as ", currClass$colRenamed, " with ", currClass$numRows, " rows", tsep))
    } else {
      expandedClass = paste0(expandedClass, paste0(
        "   [", currClass$classNum, "] ", currClass$selectedClasses, 
        " with ", currClass$numRows, " rows", tsep))
    }
  }
  
  updateActionLog(paste0(expandedClass))
}

