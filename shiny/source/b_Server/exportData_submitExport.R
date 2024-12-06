
checkExportable <- function(dat){
  if (!isTruthy(dat)){
    return(data.frame(""))
  } else {
    return(data.frame(dat))
  }
}

observeEvent(input$fileNameOrDefault, {
  rv$exportFilenameConvention = input$fileNameOrDefault
  if (rv$exportFilenameConvention == rui$exportNameChoices[1]){
    # Use default naming behavior and disable text input
    disable("exportFileName")
    reset("exportFileName")
  } else {
    # Enable text output for choosing export file name
    enable("exportFileName")
  }
})

validateFilename <- function(filename){
  if (!isTruthy(filename)){
    return(FALSE)
  } else {
    for (i in 1:length(rui$forbiddenChars)){
      if (grepl(rui$forbiddenChars[i], filename, fixed = TRUE)){
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

determineFilename <- function(){
  defaultFilename = paste0("SeroNIST_Export_",  
                           format(Sys.time(), "_%Y-%m-%d_%H-%M-%S_%Z"),
                           ".xlsx")
  # If using default name
  if (rv$exportFilenameConvention == rui$exportNameChoices[2]){
    # Use textInput entry if it passes
    currFilename = input$exportFileName
    if (validateFilename(currFilename)){
      defaultFilename = currFilename
    }
  }
  return(paste0(defaultFilename, ".xlsx"))
}

observeEvent(input$finalSubmitExport, {
  # i) select directory_________________________________________________________
  
  currDir = input$exportDirChoose
  # If no directory given use default path
  defaultDir = "./Exports"
  if (isTruthy(currDir)){
    # Parse the path from the input (can try roots if problems)
    chosenPath = parseDirPath(volumes, input$exportDirChoose) 
    # Alternative written helper for parsing path
    # chosenPath = createExportPath(currDir$path, currDir$root)
    
    # Set the chosen directory to export
    chosenDir = chosenPath
  } else {
    chosenDir = defaultDir
  }
  
  # filename = "default.xlsx"
  filename = determineFilename()
  
  
  # ) Check all possible things to export
  sh1 = checkExportable(rv$filteredDF)
  # Assay Performance 
  sh2_a = checkExportable(rv$boundUncertaintyRhoMax)
  sh2_b = checkExportable(rv$evaluateResultsSumm)
  sh2_c = checkExportable(rv$optimizedEvalResultsSum)
  # Test Data
  # sh3 = checkExportable(rv$testData_originalDF)
  # sh3 = checkExportable(tst$classifiedDataTable)
  sh3 = checkExportable(rv$testData_toExport)
  
  # Raw Data
  sh4 = checkExportable(rv$originalDF)
  # Action Log 
  sh5 = checkExportable(actionLog$dispMessage)
  
  
  # Check antigen names for train and test before exporting
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
      colnames(sh3)[i] = currRename
    }
  }
  
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
      colnames(sh1)[i] = currRename
    }
  }
  
  # ii) Decide what to export __________________________________________________
  currWhat2Export = input$whatToExport
  # A selection MUST be made to export anything
  if (isTruthy(currWhat2Export)){
    if (currWhat2Export == rui$whatToExport[rui$wteLength]){
      # Export All of the Above
      write_xlsx(list(
        "Training Data" = sh1,
        "Assay Performance" = cbind(sh2_a, " ",
                                    sh2_b,  " ",
                                    sh2_c),
        "Test Data" = sh3,
        "Raw Data" = sh4,
        "Action Log" = sh5
      ), # end list

      # File location
      paste0(chosenDir, "/", filename),
      col_names = TRUE
      )
      
      
      
      
      
      # # i) Check prerequisites before continuing
    } else if (rui$whatToExport[1] %in% currWhat2Export){
      # Training Data
    }  else if (currWhat2Export == rui$whatToExport[2]){
      # Assay Performance
      
    } else if (currWhat2Export == rui$whatToExport[3]){
      # Test Data
      
    } else if (currWhat2Export == rui$whatToExport[4]){
      # Raw Data
      
    } else if (currWhat2Export == rui$whatToExport[5]){
      # Action Log
      
    }
    
  }
  
  
  
  # iv) Additional settings ____________________________________________________
  
  
  # _) Final export ____________________________________________________________
  
  # write_xlsx(list(
  #   "Training Data" = rv$filteredDF, 
  #   "Assay Performance" = cbind(rv$boundUncertaintyRhoMax, " ",
  #                               rv$evaluateResultsSumm,  " ",
  #                               rv$optimizedEvalResultsSum),
  #   "Test Data" = rv$testData_originalDF,
  #   "Raw Data" = rv$originalDF,
  #   "Action Log" = actionLog$dispMessage
  # ), # end list
  # 
  # # File location
  # paste0(chosenDir, "/", filename),
  # col_names = FALSE
  # )
  
  
})



createExportPath <- function(path, root){
  # Get position of root out of string ie) "(C:)" -> "C:"
  nstr = nchar(root)
  firstChar = 2
  lastChar = nstr - 1
  # Extract root path
  rootPath = substr(root, firstChar, lastChar)
  
  # Extract the path selected from path input
  numSubstr = length(path)
  overallPath = c(rootPath)
  for (i in 2:length(numSubstr)){
    
    thisSubstr = paste0(path[i], "/")
    overallPath = c(overallPath, thisSubstr)
  }
  
  return(overallPath)
  
}