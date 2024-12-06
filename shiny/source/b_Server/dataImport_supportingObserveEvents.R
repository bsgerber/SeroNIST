# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                           Program Header Status  
#                               IN PROGRESS
#                   Header: 🗸X  Comments: 🗸-  Refactored: 🗸        
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ==============================================================================
# 
# TITLE: dataImport_supportingObserveEvents.R
#
#                               DESCRIPTION
# This program contains short observeEvents that support the User interface
# relating to data import. 
#
#                             TABLE OF CONTENTS
#       1) DROPDOWN MENU STYLING 
#
#       2) ACTION LOG
#
#       3) PRE-IMPORT
#
#       4) SAMPLE ID COLUMN
#
#       5) OPTION A/B: ADDING CLASS BY ROW SELECTION
#
# ==============================================================================
# ==============================================================================
# 
#                       1) DROPDOWN MENU STYLING 
#
# ==============================================================================
# This function is used to highlight columns that may not be a best first pic
# for selection because they are either non-numeric or may not have all unique
# rows. The condition for determining which item is highlighted is determined
# before calling this function. 
colorInvalidColumns <- function(inputID, allColumns, inColumns){
  # Loop through all columns and create an HTML statement for color
  colorNonUniqueHTML = ""
  concatStmt = ""
  
  # Iterate through all columns
  for (i in 1:length(allColumns)){
    # If the current column i is not in the list of unique columns
    if (!(allColumns[i] %in% inColumns)){
      # Create custom color statement
      currStmt = paste0(inputID, " .option[data-value='", allColumns[i], "']{
          background: #f3b6b6 !important;
          color: black !important;
        width: 200px;
        font-size: 9pt;
      } ")
    } else {
      currStmt = paste0(inputID, " .option[data-value='", allColumns[i], "']{
        width: 200px;
        font-size: 9pt;
      } ")
    }
    # Add to end
    concatStmt = paste0(concatStmt, currStmt)
  }
  return(concatStmt)
}

# ==============================================================================
# 
#                           2) ACTION LOG
#
# ==============================================================================
# The action log tracks all major shiny app events in response to the user's
# interaction with the app. This program updates this log using reactive variables.
updateActionLog <- function(newMsg){
  
  # The separator used between lines
  tsep = "<br/><br/>"
  
  # If this is the first action being saved 
  if (actionLog$updateCount == 0){
    # Get session start time
    actionLog$dispMessage = paste0("Session Started: ", actionLog$sessionStart)
    # Update action count
    actionLog$updateCount = actionLog$updateCount + 1
  }
  
  # Get the existing log
  currMsg = actionLog$dispMessage
  
  # Only update if a valid message received
  if (isTruthy(newMsg)){
    # Update display message
    actionLog$dispMessage = c(currMsg, tsep,
                              actionLog$updateCount, ") ", newMsg)
    # Update action count
    actionLog$updateCount = actionLog$updateCount + 1
  }
  # print(paste0("NEW ACTION:", actionLog$dispMessage))
}

# This observeEvent simply renders the action log to display past messages
# A blank "" is passed in to account for viewing action log when no changes
# have been made. 
observeEvent(input$seeActionLog, {
  updateActionLog("")
  
  output$actionLog <- renderUI({
    HTML(actionLog$dispMessage)
  })
})

# ==============================================================================
# 
#                              3) PRE-IMPORT 
#
# ==============================================================================
# Save the user's row selection where the header is said to be located
observeEvent(input$specifyHeaderRow, {
  # Map "No Header" to row 0 for no header row specified
  if (input$specifyHeaderRow == "No Header"){
    rv$headerRow = 0
  } else {
    # Otherwise save the row number for the header
    rv$headerRow = input$specifyHeaderRow
  }
})

# This observeEvent updates the column range for data import from excel files
observeEvent(input$colRangeSelect, {
  INTERVAL = rui$colRangeMaxLim
  value = rv$minMaxCol
  newvalue = input$colRangeSelect
  
  # Update the column range to only allow an interval smaller than INTERVAL
  if(value[1] != newvalue[1] && newvalue[2] - newvalue[1] != INTERVAL)
    # Update slider accordingly
    updateSliderInput(session, "colRangeSelect", 
                      value = c(newvalue[1], newvalue[1] + INTERVAL))
  
  if(value[2] != newvalue[2] && newvalue[2] - newvalue[1] != INTERVAL)
    updateSliderInput(session, "colRangeSelect", 
                      value = c(newvalue[2] - INTERVAL, newvalue[2]))
  
  # Update to the latest range
  ### NOTE - this may not reflect the range used when submit was pressed
  rv$minMaxCol = input$colRangeSelect
}, ignoreInit = TRUE)


# ==============================================================================
# 
#                           4) SAMPLE ID COLUMN
#
# ==============================================================================






# ==============================================================================
# 
#                 5) OPTION A/B: ADDING CLASS BY ROW SELECTION
#
# ==============================================================================
# This observeEvent shows/hides certain UI based on if option A/B are selected
observeEvent(input$optionab, {
  # If Option A
  if (input$optionab == rui$optionAB[1]){
    # Option A selected
    show("uniqueClassesOutput")
    show("classAddnHeader")
    show("rowsOfClasses")
    show("selectClassCol")
    
    # Hide the following UI
    hide("classAddnHeader_B")
    hide("classRowsSelected")
    
  } else {
    # Option B selected
    hide("selectClassCol")
    hide("uniqueClassesOutput")
    hide("classAddnHeader")
    
    # Show the following UI
    show("classRowsSelected")
    show("classAddnHeader_B")
  }
  
  ### Warn user to clear all classes instead
  clearAddedClasses()
  
}, ignoreInit = TRUE)

# These outputs render the header above the class addition section
output$classAddnHeader <- renderUI({
  HTML(rui$classAddnHeader)
})

# This second header for class addition section is when Option B is selected
output$classAddnHeader_B <- renderUI({
  HTML(rui$classAddnHeader_B)
})


# observeEvent for responding to row selections in raw data table
observeEvent(input$rawDataTable_rows_selected, {
  
  rawInput = input$rawDataTable_rows_selected
  rv$rawNumRows = length(input$rawDataTable_rows_selected)
  # Use helper function to get the print statement
  rowSelectPrint = getCurrentRowSelect_toPrint(rawInput) 
  
  # Define the output when rows are selected or deselected
  output$classRowsSelected = renderText({
    as.character(rowSelectPrint)
  })
  
},
ignoreNULL = FALSE
)

# Helper function to determine the current row selection if any
getCurrentRowSelect_toPrint <- function(rawInput){
  # If valid rows were selected from the datatable
  if (isTruthy(rawInput)){
    # Process the string into a concise input
    rowSelectToPrint = splitNonConsecutives(rawInput)
  } else {
    rowSelectToPrint = "No Rows Selected"
  }
  
  return(rowSelectToPrint)
}

# This helper takes a string of random numbers and comma separates them
splitNonConsecutives <- function(lst){
  lst = sort(lst)
  splitList = split(lst, cumsum(c(0, diff(lst) > 1)))
  numSubLsts = length(splitList)
  outputStmt = ""
  
  for (i in 1:numSubLsts){
    lstToConcat = splitList[i]
    
    if (i == 1){
      outputStmt = paste0(outputStmt, as.character(lstToConcat))
    } else {
      outputStmt = paste0(outputStmt, ", ", as.character(lstToConcat))
      
    }
  }
  
  
  return(outputStmt)
}

# This helper takes a string of comma separated sequences/numbers and converts
# them to a numeric list of numbers
interpretSeqList <- function(str){
  numList = c()
  
  nums = strsplit(as.character(str), ", ")
  len = length(nums[[1]])
  
  # print(paste0("nums:", nums))
  # print(paste0("len:", len))
  if (len == 1){
    numList = getColSequence(nums)
    # print(paste0("numList:", numList))
  } else {
    for (i in 1:len){
      # print(paste0("ISL [", i, "]:"))
      numList = c(numList, getColSequence(nums[[1]][i]))
    }
  }
  # print(paste0("numList:" , numList))
  
  return(numList)
}

# This helper function breaks down a sequence into a list of numbers
getColSequence <- function (str){
  numList = c()
  colons = strsplit(as.character(str), ":")
  # print(paste0("colons:", colons))
  numList = as.vector(sapply(colons, as.integer))
  
  # print(paste0("NUMLIST!!! :", numList))
  if (length(numList) == 1){
    # print(paste0("ONLY ONE SELECTED"))
    ll <- ul <- numList[1]
  } else {
    
  
  ll = numList[1]
  ul = numList[2]
  }
  tseq = seq(ll,ul)
  return(tseq)
}

# This helper function simply checks which Class addition option is current
isOptionA <- function(mode){
  
  if (mode == "train"){
    currInput = input$optionab
  } else if (mode == "test"){
    currInput = input$testData_optionab
    
  }
  
  if (currInput == rui$optionAB[1]){
    return(TRUE)
  } else {
    return(FALSE)
  }
}



observeEvent(input$doneImportBtn, {
  tabSwitchNext()
})

observeEvent(input$updateAntigenSelect, { 
  tabSwitchNext()
  
})

# observeEvent(input$updateClassSelect, { 
#   tabSwitchNext()
#   
# })



tabSwitchNext <- function(){
  currIdx = which(rui$tabNamesImport == input$dataImportTabs)
  
  nextIdx = currIdx + 1
  if (nextIdx > rui$numTabsImport){
    nextIdx = rui$numTabsImport
  }
  
  
  updateTabsetPanel(session = session, inputId = "dataImportTabs",
                    selected = rui$tabNamesImport[nextIdx])
}

# TEST DATA ______________________________________________________________
observeEvent(input$testData_doneImportBtn, {
  testData_tabSwitchNext()
})

observeEvent(input$testData_updateAntigenSelect, { 
  testData_tabSwitchNext()
})



testData_tabSwitchNext <- function(){
  currIdx = which(rui$testTabNamesImport == input$testDataImportTabs)
  
  nextIdx = currIdx + 1
  if (nextIdx > rui$testData_numTabsImport){
    nextIdx = rui$testData_numTabsImport
  }
  
  updateTabsetPanel(session = session, inputId = "testDataImportTabs",
                    selected = rui$testTabNamesImport[nextIdx])
}

observeEvent(input$testData_specifyHeaderRow, {
  # Map "No Header" to row 0 for no header row specified
  if (input$testData_specifyHeaderRow == "No Header"){
    rv$testData_headerRow = 0
  } else {
    # Otherwise save the row number for the header
    rv$testData_headerRow = input$testData_specifyHeaderRow
  }
})


# observeEvent for responding to row selections in raw data table
observeEvent(input$testData_rawDataTable_rows_selected, {
  
  rawInput = input$testData_rawDataTable_rows_selected
  rv$testData_rawNumRows = length(input$testData_rawDataTable_rows_selected)
  # Use helper function to get the print statement
  rowSelectPrint = getCurrentRowSelect_toPrint(rawInput)
  
  # Define the output when rows are selected or deselected
  output$classRowsSelected = renderText({
    as.character(rowSelectPrint)
  })
  
},
ignoreNULL = FALSE
)



# TEST CLASSES 
# This observeEvent shows/hides certain UI based on if option A/B are selected
observeEvent(input$testData_optionab, {
  # If Option A
  if (input$testData_optionab == rui$optionAB[1]){
    # Option A selected
    show("testData_uniqueClassesOutput")
    show("testData_classAddnHeader")
    show("testData_rowsOfClasses")
    show("testData_selectClassCol")
    
    # Hide the following UI
    hide("testData_classAddnHeader_B")
    hide("testData_classRowsSelected")
    
  } else {
    # Option B selected
    hide("testData_selectClassCol")
    hide("testData_uniqueClassesOutput")
    hide("testData_classAddnHeader")
    
    # Show the following UI
    show("testData_classRowsSelected")
    show("testData_classAddnHeader_B")
  }
  
  ### Warn user to clear all classes instead
  testData_clearAddedClasses()
  
}, ignoreInit = TRUE)

# These outputs render the header above the class addition section
output$testData_classAddnHeader <- renderUI({
  HTML(rui$classAddnHeader)
})

# This second header for class addition section is when Option B is selected
output$testData_classAddnHeader_B <- renderUI({
  HTML(rui$classAddnHeader_B)
})


# observeEvent for responding to row selections in raw data table
observeEvent(input$testData_rawDataTable_rows_selected, {
  
  rawInput = input$testData_rawDataTable_rows_selected
  rv$testData_rawNumRows = length(input$testData_rawDataTable_rows_selected)
  # Use helper function to get the print statement
  rowSelectPrint = getCurrentRowSelect_toPrint(rawInput) 
  
  # Define the output when rows are selected or deselected
  output$testData_classRowsSelected = renderText({
    as.character(rowSelectPrint)
  })
  
},
ignoreNULL = FALSE
)



splitStringByDelim <- function(str, delim){
  
  res = strsplit(str, delim)
  res = unlist(res)
  
  return(res)
}


## 010424 

parameterizedOptim <- function(par, fun, positives, negatives, sigimin1, q){
  thisChoice = input$chooseBFGS
  # Perform the first optimization with the first sigval
  optim_out = optim(par = par, fn = fun, poss = positives,
                    negs = negatives, sigval = sigimin1, q = q,
                    method = thisChoice,
                    control = list(ndeps = rep(rv$ndeps, rv$numLen),
                                   reltol = rv$reltol,
                                   maxit = rv$maxit,
                                   abstol = rv$abstol,
                                   pgtol = rv$pgtol,
                                   factr = rv$factr)
  )
  
  # cl <- makeCluster(4)
  # setDefaultCluster(cl=cl)
  # optim_out = optimParallel(par = par, fn = fun, poss = positives,
  #                   negs = negatives, sigval = sigimin1, q = q,
  #                   method = thisChoice,
  #                   control = list(ndeps = rep(rv$ndeps, rv$numLen),
  #                                  reltol = rv$reltol,
  #                                  maxit = rv$maxit,
  #                                  abstol = rv$abstol,
  #                                  pgtol = rv$pgtol,
  #                                  factr = rv$factr)
  # )
  
  return(optim_out)
}

parameterizedOptimBU <- function(par, fun, positives, negatives, sigmin1, q,
                                 thisHP, thisFNR, thisFPR){
  thisChoice = input$chooseBFGS
  optim_out = optim(par = par, fn = fun,
                    poss = positives, negs = negatives, sigval = sigmin1,
                    q = q, hp = thisHP, fnr = thisFNR, fpr = thisFPR,
                    method = thisChoice,
                    control = list(ndeps = rep(rv$ndeps, rv$numLen),
                                   reltol = rv$reltol,
                                   maxit = rv$maxit,
                                   abstol = rv$abstol,
                                   pgtol = rv$pgtol,
                                   factr = rv$factr)
  )
  
  return(optim_out)
}



observeEvent(input$bu_hyperparameter, {
  currInp = input$bu_hyperparameter
  rv$bu = checkHyperparams(currInp, 
                           rui$bu_mmv[1], 
                           rui$bu_mmv[2], 
                           TRUE, FALSE, 
                           rui$bu_default)
})

observeEvent(input$reltol_hyperparam, {
  currInp = input$reltol_hyperparam
  rv$reltol = checkHyperparams(currInp, 
                               rui$reltol_mmv[1], 
                               rui$reltol_mmv[2], 
                               FALSE, FALSE, 
                               rui$reltol_default)
})
observeEvent(input$abstol_hyperparam, {
  currInp = input$abstol_hyperparam
  rv$abstol = checkHyperparams(currInp, 
                               rui$abstol_mmv[1], 
                               rui$abstol_mmv[2], 
                               FALSE, FALSE, 
                               rui$abstol_default)
})
observeEvent(input$ndeps_hyperparam, {
  currInp = input$ndeps_hyperparam
  rv$ndeps = checkHyperparams(currInp, 
                              rui$ndeps_mmv[1], 
                              rui$ndeps_mmv[2], 
                              FALSE, FALSE, 
                              rui$ndeps_default)
})
observeEvent(input$maxit_hyperparam, {
  currInp = input$maxit_hyperparam
  rv$maxit = checkHyperparams(currInp, 
                              rui$maxit_mmv[1], 
                              rui$maxit_mmv[2], 
                              TRUE, FALSE, 
                              rui$maxit_default)
})
observeEvent(input$pgtol_hyperparameter, {
  currInp = input$pgtol_hyperparameter
  rv$pgtol = checkHyperparams(currInp, 
                              rui$pgtol_mmv[1], 
                              rui$pgtol_mmv[2], 
                              FALSE, FALSE, 
                              rui$pgtol_default)
})
observeEvent(input$factr_hyperparameter, {
  currInp = input$factr_hyperparameter
  rv$factr = checkHyperparams(currInp, 
                              rui$factr_mmv[1], 
                              rui$factr_mmv[2], 
                              FALSE, FALSE, 
                              rui$factr_default)
})

checkHyperparams <- function(inp, min, max, minincl, maxincl, defaultVal){
  # Return default value if proposed param inp is invalid
  if (!isTruthy(inp) || !is.numeric(inp) || is.infinite(inp) ||
      # If lower limit of hyperparam is NOT inclusive (minincl == FALSE), 
      # then proposed param cannot equal the lower limit min
      (minincl == TRUE && inp < min) || (minincl == FALSE && inp == min) ||
      # If upper limit of hyperparam is NOT inclusive (maxincl == FALSE), then
      # proposed param cannot equal the upper limit
      (maxincl == TRUE && inp > max) || (maxincl == FALSE && inp == max)){
    return(defaultVal)
  } else {
    return(as.numeric(inp))
  }
}

observeEvent(input$hyperparam_restoreDefaults, {
  updateNumericInput(session = session,
                     "bu_hyperparameter", 
                     rui$bu_label,
                     min = rui$bu_mmv[1],
                     max = rui$bu_mmv[2],
                     value = rui$bu_mmv[3])
  updateNumericInput(session = session, 
                     "ndeps_hyperparam", 
                     rui$ndeps_label,
                     min = rui$ndeps_mmv[1],
                     max = rui$ndeps_mmv[2],
                     value = rui$ndeps_mmv[3])
  updateNumericInput(session = session, 
                     "reltol_hyperparam", 
                     rui$reltol_label,
                     min = rui$reltol_mmv[1],
                     max = rui$reltol_mmv[2],
                     value = rui$reltol_mmv[3])
  updateNumericInput(session = session, 
                     "abstol_hyperparam", 
                     rui$abstol_label,
                     min = rui$abstol_mmv[1],
                     max = rui$abstol_mmv[2],
                     value = rui$abstol_mmv[3])
  updateNumericInput(session = session, 
                     "pgtol_hyperparameter", 
                     rui$pgtol_label,
                     min = rui$pgtol_mmv[1],
                     max = rui$pgtol_mmv[2],
                     value = rui$pgtol_mmv[3])
  updateNumericInput(session = session, 
                     "maxit_hyperparam", 
                     rui$maxit_label,
                     min = rui$maxit_mmv[1],
                     max = rui$maxit_mmv[2],
                     value = rui$maxit_mmv[3])
  updateNumericInput(session = session, 
                     "factr_hyperparameter", 
                     rui$factr_label,
                     min = rui$factr_mmv[1],
                     max = rui$factr_mmv[2],
                     value = rui$factr_mmv[3]
  )
  
  rv$bu_default = as.numeric(rui$bu_default[3])
  rv$ndeps_default = as.numeric(rui$ndeps_default[3])
  rv$reltol_default = as.numeric(rui$reltol_default[3])
  rv$abstol_default = as.numeric(rui$abstol_default[3])
  rv$pgtol_default = as.numeric(rui$pgtol_default[3])
  rv$maxit_default = as.numeric(rui$maxit_default[3])
  rv$factr_default = as.numeric(rui$factr_default[3])
  
})


