# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                           Program Header Status  
#                             COMPLETED 092123
#                   Header: 🗸  Comments: 🗸   Refactored: 🗸X        
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ==============================================================================
# 
# TITLE: b_Server/testData_addClasses.R
#
#                               DESCRIPTION
# This program renders a variable number of rows of UI inputs that allow the 
# user to individually assign classes of data. Users may also combine
# classes as needed. 
# 
#                             TABLE OF CONTENTS
#       1) MAKE CLASS ASSIGNMENTS
#               1a) Required before proceeding
#               1b) Initialize variables
#               1c) Previous Data
#               1d) Define UI foreach row of a class
#                       i) One row per class
#                       ii) Add additional styling
#               1e) Render UI
#                       i) Autopopulate previous work
#                       ii) Render the outputs
#               1f) Store reactive variables
#
#       2) CLASS OBSERVEEVENTS
#               2a) Select column for class assignments
#               2b) Add class button
#               2c) Clear all classes
#
#       3) DYNAMIC CLASS OBSERVEEVENTS
#               3a) Dynamic Class txt
#               3b) Dynamic txt input Class rename
#               3c) Dynamic remove button
#
#       4) HELPER FUNCTIONS
#               4a) Define class input ID's
#               4b) Get rows for selected classes
#               4c) Clear Classes
#                       i) ObserveEvent for Clear Button Press
#                       ii) Helper function to clear classes 
#
#                                   NOTES
#       - CLASS in this program refers to class assignments chosen by the user
#         to indicate metadata associated with the uploaded data.
#
# ==============================================================================
# ==============================================================================
#
#                       1) MAKE CLASS ASSIGNMENTS
#
# ==============================================================================
# This function is called to make class assignments and receives a "mode" param
# that enables the function to observe different behavior depending on if the
# user is adding or removing a class assignment for their data.
testData_renderClassesAdded <- function(mode){
  # ----------------------------------------------------------------------------
  # 1a) Required before proceeding
  # ----------------------------------------------------------------------------
  # If using Option B and attempting to add a class
  if (!isOptionA("test") && mode == "add"){
    # There must be row selections from the rawDataTable
    req(rv$testData_rawNumRows > 0)
  }
  # If Option A is selected and attempt to add a class
  if (isOptionA("test") && mode == "add"){
    req(isTruthy(input$testData_uniqueClassesOutput))
  }
  
  # ----------------------------------------------------------------------------
  # 1b) Initialize variables
  # ----------------------------------------------------------------------------
  # Lock other parts of code until this function finishes using rv$testData_doneClass
  rv$testData_doneClass = FALSE
  # Initialize empty list to store outputs
  output_list = list()
  # Increment number of classes
  rv$testData_numClassCount = rv$testData_numClassCount + 1
  
  # Option A: use the provided input for the class name
  # Option B: use the rows selected for the "class name" now rows selected col
  if (isOptionA("test")){
    # Get the classes selected by user, separate by "; " if multiple
    rv$testData_classIndex = paste(rv$testData_uniqueClassesOutput, 
                                   collapse = "; ")
  } else {
    # If Option B, use helper function to get rows of data chosen by user
    rv$testData_classIndex = getCurrentRowSelect_toPrint(input$testData_rawDataTable_rows_selected) 
  }
  
  # ----------------------------------------------------------------------------
  # 1c) Previous Data
  # ----------------------------------------------------------------------------
  prevClassData = rv$testData_addClass
  # Clear any previously existing data
  rv$testData_addClass = NULL
  
  # If removing a class
  if (mode == "remove"){
    # Decrement class count
    prevNum = rv$testData_addedClasses
    currNum = rv$testData_addedClasses - 1
    # Don't allow (-) number of classes
    if (currNum < 0){
      currNum = 0
    }
    rv$testData_addedClasses = as.numeric(currNum)
    
  } else if (mode == "add"){
    # Increment counter
    prevNum = rv$testData_addedClasses
    rv$testData_addedClasses = rv$testData_addedClasses + 1
  }
  
  # ----------------------------------------------------------------------------
  # 1d) Define UI for each row of a class
  # ----------------------------------------------------------------------------
  # Only continue if more than 0 classes
  req(rv$testData_addedClasses > 0)
  
  # i) One row per class _______________________________________________________
  # For the total number of classes added
  for (i in 1:rv$testData_addedClasses){
    
    # Define dynamic number of input ID's    
    allClassIDs = testData_getClassInputIDs(i, rv$testData_numClassCount); 
    # Extract ID's from function call
    id = allClassIDs[1]; idplus_1 = allClassIDs[2]; idplus_2 = allClassIDs[3]; 
    idplus_3 = allClassIDs[4]
    
    # Save each row of input definitions in output_list at index i
    output_list[[i]] = 
      div(fluidRow(
        id = paste0("testData_optionA_pt3", rv$testData_numClassCount),
        # column for class number UI
        column(1, 
               div(
                 # number corresponds to i
                 i, 
                 style = ""
                 # style = "margin-top:0px; padding: 35px;"
               ) #end div
        ), # end column
        # column for selected classes
        column(4, 
               div(
                 textOutput(outputId = id),
                 style = ""
                 # style = "margin-top:25px; padding: 0px;"
               ) #end div
        ), # end column
        # column for number of rows for selected classes
        # column(1, 
        #        div(
        #          textOutput(outputId = idplus_1),
        #          style = ""
        #          # style = "margin-top: 20px; padding: 15px;"
        #        ) # end div
        # ), #end column
        # column for textInput to rename class
        column(5, 
               div(
                 uiOutput(outputId = idplus_2),
                 style = "margin-top: -40px;"
               ) # end div
        ), # end column
        # column for remove buttons
        column(1,
               div(
                 uiOutput(outputId = idplus_3),
                 style = "margin-top: -45px; padding: 20px;"
               ) # end div
        ) #end column
      ), #end fluidRow
      br()
      ) # end div
  } # end i loop
  
  
  # ii) Add additional styling _________________________________________________
  
  # Add scroll and sizing options depending on number of classes added
  output$testData_rowsOfClasses <- renderUI({
    # Add scroll if over 3 classes
    if (rv$testData_addedClasses > 3){
      div(output_list, 
          style = "padding: 45px; height:200px; overflow-y: scroll; 
          overflow-x: hidden;"
      ) # end div
    } else if (rv$testData_addedClasses > 2) {
      div(output_list, 
          style = "padding: 45px; height:200px;"
      ) # end div
    } else if (rv$testData_addedClasses > 1){
      div(output_list, 
          style = "padding: 45px; height:200px;"
      ) # end div
    } else if (rv$testData_addedClasses == 1){
      div(output_list, 
          style = "padding: 45px; height:100px;"
      ) # end div
    } else {
      # Return nothing if no classes
      NULL
    } # end conditional block
  }) # end renderUI
  
  # ----------------------------------------------------------------------------
  # 1e) Render UI
  # ----------------------------------------------------------------------------
  
  # Render individual inputs for each class
  for (i in 1:rv$testData_addedClasses){
    local({
      i <- i
      # Define dynamic number of input ID's    
      allClassIDs = testData_getClassInputIDs(i, rv$testData_numClassCount); 
      # Extract ID's from function call
      id = allClassIDs[1]; idplus_1 = allClassIDs[2]; idplus_2 = allClassIDs[3]; 
      idplus_3 = allClassIDs[4]
      
      
      ## OPTION A/B #################################
      if (mode != "remove"){
        # Find number of rows for current class
        if (isOptionA("test")){
          totalNumRows = testData_getNumClassRows()
        } else {
          totalNumRows = rv$testData_rawNumRows
        }
      } else {
        totalNumRows = 0 
      }
      ## OPTION A/B #################################
      
      
      # Use these variables for the rendered outputs
      txtOut_numRows = totalNumRows
      txtInp_entered = ""
      
      # i) Autopopulate previous work __________________________________________
      # Only continue if previous data was saved
      if (isTruthy(prevClassData)){
        # Use class number as index
        previdx = i
        # If any classes are staged to remove
        if (isTruthy(which(prevClassData$removed == 1))){
          # Only skip the current row if a class was removed
          if (which(prevClassData$removed == 1)[1] <= previdx){
            # Skip current value
            previdx = previdx + 1
          }
        }
        # Extract previous values using previdx 
        if (previdx <= prevNum){
          # Classes selected ie. PCR+; PCR-
          txtOut_classesAdded = prevClassData$selectedClasses[previdx]
          # Number of rows for the classes selected
          txtOut_numRows = prevClassData$numRows[previdx]
          # Txt input if provided by user to rename the class
          txtInp_entered = prevClassData$colRenamed[previdx]
        }
      }
      
      # ii) Render the outputs _________________________________________________
      # render class number 
      output[[id]] = renderText({
        paste0(txtOut_classesAdded, " (", txtOut_numRows, ")")
      })
      
      # # render Number of rows
      # output[[idplus_1]] = renderText({
      #   txtOut_numRows
      # })
      # 
      # render textInputs
      output[[idplus_2]] = renderUI({
        textInput(idplus_2, 
                  label = "", 
                  value = txtInp_entered)
      })
      
      # Remove buttons
      output[[idplus_3]] = renderUI({
        actionButton(idplus_3,
                     label = HTML("<span class='small'><i class=
                                       'glyphicon glyphicon-remove'
                                       ></i></span>"))
      })
    }) # End local
  } # end i local loop
  
  
  # ----------------------------------------------------------------------------
  # 1f) Store reactive variables
  # ----------------------------------------------------------------------------
  
  #### Initialize empty dataframe to store reactive variables
  addClassDF = data.frame(
    classNum = numeric(),
    classesTxtID = character(), selectedClasses = character(),
    numRowsTxtID = character(), numRows = numeric(),
    colRenameInput = character(), colRenamed = character(),
    removeClass = character(), removed = character()
  )
  
  # For each class, save the ID to reactive variable
  for (i in 1:rv$testData_addedClasses){
    # Define dynamic number of input ID's    
    allClassIDs = testData_getClassInputIDs(i, rv$testData_numClassCount)
    id = allClassIDs[1]
    idplus_1 = allClassIDs[2]
    idplus_2 = allClassIDs[3]
    idplus_3 = allClassIDs[4]
    
    ## OPTION A/B #################################
    # If adding data, get number of rows for corresponding class
    if (mode != "remove"){
      # IF option A
      if (isOptionA("test")){
        # Find number of rows for current class
        totalNumRows = testData_getNumClassRows()
      } else {
        totalNumRows = rv$testData_rawNumRows
      }
    } else {
      # If removing class
      totalNumRows = 0 
    }
    ## OPTION A/B #################################
    
    # Get previous values
    txtOut_classesAdded = rv$testData_classIndex
    txtOut_numRows = totalNumRows
    txtInp_entered = ""
    
    if (isTruthy(prevClassData)){
      previdx = i
      
      if (mode == "remove"){
        # If removing class, find index of the UI element to remove
        if (isTruthy(which(prevClassData$removed == 1))){
          # If the class is being removed, skip it by incrementing index
          if (which(prevClassData$removed == 1) <= previdx){
            previdx = previdx + 1
          }
        }
      }
      
      if (previdx <= prevNum){
        txtOut_classesAdded = prevClassData$selectedClasses[previdx]
        txtOut_numRows = prevClassData$numRows[previdx]
        txtInp_entered = prevClassData$colRenamed[previdx]
      }
    }
    
    # Add the row to the dataframe to save
    addClassDF[nrow(addClassDF)+1, ] = c(
      i,
      id, txtOut_classesAdded,
      idplus_1, txtOut_numRows,
      idplus_2, txtInp_entered,
      idplus_3, 0)
    
  } # end i loop
  
  # Save dataframe to reactive variable
  rv$testData_addClass = addClassDF
  rv$testData_doneClass = TRUE
  
}

# ==============================================================================
#
#                         2) CLASS OBSERVEEVENTS
#
# ==============================================================================
# ------------------------------------------------------------------------------
# 2a) Select column for class assignments
# ------------------------------------------------------------------------------
# This observeEvent responds to user's column selection that contains class
# assignments, and computes how many unique classes are present in selected col.

observeEvent(input$testData_selectClassCol, {
  ### This is very important needs to be replaced at some point?
  rv$testData_selectClassCol = input$testData_selectClassCol
  
  # When a column is selected to assign classes, update the dropdown menu
  # of available classe by calling this helper
  testData_renderSelectclassDropdown()
}) # end observeEvent


# This helper function renders the select class, multiple select dropdown menu
# for users to select their classes. This menu does not render if "None" is 
# chosen for the class assignment column, since a class label is not required
# for test data
testData_renderSelectclassDropdown <- function(){
  if (isTruthy(rv$testData_originalDF) && 
      isTruthy(input$testData_selectClassCol) &&
      input$testData_selectClassCol != "None"){
    
    # Compute unique classes in selected column
    newUniqueFromCol = unique(rv$testData_originalDF[, rv$testData_selectClassCol])
    numAvailClass = dim(newUniqueFromCol)[1]
    
    # Store in reactive variable for use elsewhere
    rv$testData_currUniqueClasses = newUniqueFromCol
    
    # Render multiple dropdown menu selectInput for unique classes
    
    output$testData_uniqueClassesOutput <- renderUI({
      
      if (numAvailClass > 100){
        classLim = 100
        numAvailClass2Print = "100+"
      } else {
        classLim = numAvailClass
        numAvailClass2Print = numAvailClass
      }
      
      # suppressMessages fails, too many options under selectInput choices
      # Only show first 100 rows to prevent harmless warning             
      selectInput("testData_uniqueClassesOutput", 
                  # Update input text based on number of available classes
                  paste0("Available classes (", numAvailClass2Print, 
                         " Total): "), 
                  choices = rv$testData_currUniqueClasses[1:classLim, ],
                  selected = "",
                  multiple = TRUE
      ) # end selectInput
    }) # end renderUI
  } else {
    output$testData_uniqueClassesOutput <- NULL
  }
}

# ------------------------------------------------------------------------------
# 2b) Add class button
# ------------------------------------------------------------------------------
# This observeEvent responds to the button to "add class" and dynamically 
# renders a row of UI inputs for class #, selected classes, number of rows per 
# class, textInput for overwriting class name, and a remove button for each
# added class. 

observeEvent(input$testData_addClassBtn, {
  # req(rv$addedClasses < 10 && isTruthy(input$uniqueClassesOutput))
  req(rv$testData_addedClasses < 10)
  rv$testData_uniqueClassesOutput = input$testData_uniqueClassesOutput
  # Re-render the dropdown menu for class selection
  testData_renderSelectclassDropdown()
  
  testData_renderClassesAdded("add")
  
  
})


# ------------------------------------------------------------------------------
# 2c) Clear all classes
# ------------------------------------------------------------------------------
# This observeEvent responds to the "clear all" button for class selection, but
# is also called when a new column is selected for class assignment.


# ==============================================================================
#
#                         3) DYNAMIC CLASS OBSERVEEVENTS
#
# ==============================================================================

# ------------------------------------------------------------------------------
# 3a) Dynamic Class txt
# ------------------------------------------------------------------------------
# # Dynamic number of selectInputs are observed here. User selects from each
# # dropdown menu the column they wish to stage for import. 
# observeEvent({sapply(rv$addClass$classTxtID, function(x){
#   req(isTruthy(rv$addClass) && rv$testData_doneClass)
#   # print(paste0("=============================================="))
#   # print(paste0("x input:", x))
#   # print(paste0("SELECTNUMERIC COL: ", input[[x]]))
#   
#   # Only update the value if a value is provided (ie. not at startup)
#   currInputX = input[[x]]
#   # Update reactive variable 
#   # Find index for current input
#   # print(paste0("%IN%:", which(rv$addClass$selectNumericCol == x)))
#   req(isTruthy(input[[x]]))
#   idx2update = which(rv$addClass$classTxtID == x)
#   rv$addClass$selectedClasses[idx2update] = currInputX
#   
#   # Print current values
#   # print(paste0("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"))
#   # print(paste0("NEW selectNumericCol:", rv$addClass$selectNumericCol))
#   # print(paste0("NEW colselect:", rv$addClass$colSelect))
#   # print(paste0("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"))
#   
#   input[[x]]} 
# )}, {
# }, ignoreInit = TRUE)

# ------------------------------------------------------------------------------
# 3b) Dynamic txt input Class rename
# ------------------------------------------------------------------------------
# Dynamic number of text inputs. If the user wishes to enter a specific name for
# the antigen it is done here. 
observeEvent({sapply(rv$testData_addClass$colRenameInput, function(x){
  req(isTruthy(rv$testData_addClass) && rv$testData_doneClass)
  # print(paste0("=============================================="))
  # print(paste0("x input:", x))
  # print(paste0("COL RENAME INPUT: ", input[[x]]))
  currInputX = input[[x]]
  
  if (!isTruthy(input[[x]])){
    currInputX = ""
  }
  
  idx2update = which(rv$testData_addClass$colRenameInput == x)
  rv$testData_addClass$colRenamed[idx2update] = currInputX
  
  input[[x]]} 
  
  
)}, {
}, ignoreInit = TRUE)


# ------------------------------------------------------------------------------
# 3c) Dynamic remove button
# ------------------------------------------------------------------------------
# Dynamic number of remove buttons. User presses a button to remove the antigen
# in the corresponding row. 
observeEvent({sapply(rv$testData_addClass$removeClass, function(x){
  req(isTruthy(rv$testData_addClass$removeClass) && rv$testData_doneClass)
  # Check work
  # print(paste0("=============================================="))
  # print(paste0("x input:", x))
  # print(paste0("REMOVE DIM: ", input[[x]]))
  # print(paste0("=============================================="))
  
  # Get the value of the input
  currInputX = input[[x]]
  # If no input present, use default value
  if(!isTruthy(input[[x]])){
    currInputX = 0
  }
  
  # Find index of the current input x
  idx2update = which(rv$testData_addClass$removeClass == x)
  # Update the saved current value of the obsEvent
  rv$testData_addClass$removed[idx2update] = currInputX
  
  # Only do this if the button is actually pressed
  if (currInputX > 0){
    
    testData_renderClassesAdded("remove")
  }
  
  # Return current input
  input[[x]]} 
)}, {
}, ignoreInit = TRUE)




# ==============================================================================
#
#                           4) HELPER FUNCTIONS
#
# ==============================================================================
# ------------------------------------------------------------------------------
# 4a) Define class input ID's
# ------------------------------------------------------------------------------
# This helper function defines the txt output, txt input and remove buttons
# used in adding classes
testData_getClassInputIDs <- function(i, uid){
  # i refers to current row, uid is number of times "add class" button pressed
  id = paste0("testData_chosenClassesTxt_", i, uid)
  idplus_1 = paste0("testData_numRowsForClass_", i, uid)
  idplus_2 = paste0("testData_renameSelectedClass_", i, uid)
  idplus_3 = paste0("testData_xRemoveClass_", i, uid)
  
  return(c(id, idplus_1, idplus_2, idplus_3))
}


# ------------------------------------------------------------------------------
# 4b) Get rows for selected classes
# ------------------------------------------------------------------------------
testData_getNumClassRows <- function(){
  # Get upload
  tempdf = rv$testData_originalDF
  # Extract current classes chosen by user
  allSelections = rv$testData_classIndex
  # Init row counter
  totalNumRows = 0
  # Get all classes chosen by the user
  sepClasses = unlist(strsplit(allSelections, "; "))
  
  # For every class added, count for each subclass
  for (ii in 1:length(sepClasses)){
    # Get the col chosen for class assignments
    colSelected = rv$testData_selectClassCol
    # Select rows of interest
    subsetdf = tempdf[, colSelected]
    # Get number of rows and add to counter
    tnumRows = length(which(subsetdf == sepClasses[[ii]]))
    totalNumRows = totalNumRows + tnumRows
  }
  return(totalNumRows)
}


# ------------------------------------------------------------------------------
# 4b) Clear Classes
# ------------------------------------------------------------------------------

# i) ObserveEvent for Clear Button Press _______________________________________
observeEvent(input$testData_clearClassBtn, {
  # Update the available classes dropdown menu
  testData_renderSelectclassDropdown()
  # Clear the classes using the helper function below
  testData_clearAddedClasses()
})

# ii) Helper function to clear classes _________________________________________
testData_clearAddedClasses <- function(){
  # Number of classes added
  numClasses = dim(rv$testData_addClass)[1]
  # If there are classes added
  if (!isTruthy(numClasses)){
    numClasses = rv$testData_addedClasses
  }
  # Iterate through selected classes
  for (i in 1:numClasses){
    local({
      i<-i
      # Find index of the current input x
      idx2update = which(rv$testData_addClass$removed == 0)[1]
      # Update the saved current value of the obsEvent
      rv$testData_addClass$removed[idx2update] = 1
      # Use function (1) to remove the class
      testData_renderClassesAdded("remove")
    })
  }
}
