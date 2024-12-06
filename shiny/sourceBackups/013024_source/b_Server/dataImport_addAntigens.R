# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                           Program Header Status  
#                             COMPLETED 092123
#                   Header: 🗸  Comments: 🗸   Refactored: 🗸         
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ==============================================================================
# 
# TITLE: b_Server/dataImport_addAntigens.R
#
#                               DESCRIPTION
# This program renders a variable number of rows of UI inputs that allow the 
# user to assign antigens to a column of data from the file upload.
# 
#                             TABLE OF CONTENTS
#       1) SELECT NUMBER OF ANTIGENS
#
#       2) DYNAMIC CLASS OBSERVEEVENTS
#               2a) Dynamic antigen column select
#               2b) Dynamic txt input to rename antigen
#               2c) Dynamic remove button
#
#       3) HELPER FUNCTIONS
#               3a) Define antigen input ID's
#               3b) Setup initial Antigen UI
#                       i) One row for each antigen
#                       ii) Add additional styling
#               3c) Render Antigen UI
#                       i) Autopopulate previous work
#                       ii) Render the outputs
#               3d) Store Antigen Metadata in Reactive variable
#
# ==============================================================================
# ==============================================================================
#
#                       1) SELECT NUMBER OF ANTIGENS
#
# ==============================================================================
# This observeEvent responds to the number of antigens "n" chosen from the 
# dropdown menu input$numDimensions, and dynamically renders n number of rows
# each containing a selectInput, textInput, and actionButton for each antigen.
observeEvent(ignoreInit = TRUE, list(
  # Responds to multiple inputs below
  input$numDimensions,
  input$showNumeric_inDataImport, 
  input$updateDataSelect),
  { 
    
    # Continue if "Load" button pressed, file upload present & 1 or more antigen
    req(isTruthy(input$fileUploadInput) && isTruthy(input$updateDataSelect) && 
          input$numDimensions > 0)
    
    # block other outputs until finished
    lock_preImport$done3 = FALSE
    # Use this variable to create a new set of inputs every time
    rv$numDimCount = rv$numDimCount + 1
    # Save previous values
    rv$prevAntigenData = rv$addAntigen
    # Clear previous antigens (*must come after save previous values)
    rv$addAntigen = NULL
    # Previous number of dimensions chosen
    rv$prevNumAnt = as.numeric(rv$numDimensions)
    # Current number of dimensions chosen
    rv$numDimensions = as.numeric(input$numDimensions)
    # Get difference to see if we added or removed
    rv$diffAnt = rv$numDimensions - rv$prevNumAnt
    
    # Define and render initial UI for each antigen
    setupAntigenUI()
    # Assign more specific antigen data to each UI element
    renderAntigenUI()
    # Store reactive variables
    updateReactiveAntigenData()
    # process complete
    lock_preImport$done3 = TRUE
  }) # end observeEvent(input$numDimensions, {...})

# ==============================================================================
#
#                         2) DYNAMIC CLASS OBSERVEEVENTS
#
# ==============================================================================
# ------------------------------------------------------------------------------
# 2a) Dynamic antigen column select
# ------------------------------------------------------------------------------
# Dynamic number of selectInputs are observed here. User selects from each
# dropdown menu the column they wish to stage for import. 
observeEvent({sapply(rv$addAntigen$selectNumericCol, function(x){
  req(isTruthy(rv$addAntigen) && lock_preImport$done3)
  # print(paste0("=============================================="))
  # print(paste0("x input:", x))
  # print(paste0("SELECTNUMERIC COL: ", input[[x]]))
  
  # Only update the value if a value is provided (ie. not at startup)
  currInputX = input[[x]]
  # Update reactive variable 
  # Find index for current input
  # print(paste0("%IN%:", which(rv$addAntigen$selectNumericCol == x)))
  req(isTruthy(input[[x]]))
  idx2update = which(rv$addAntigen$selectNumericCol == x)
  rv$addAntigen$colSelect[idx2update] = currInputX
  
  # Print current values
  # print(paste0("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"))
  # print(paste0("NEW selectNumericCol:", rv$addAntigen$selectNumericCol))
  # print(paste0("NEW colselect:", rv$addAntigen$colSelect))
  # print(paste0("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"))
  
  input[[x]]} 
)}, {
}, ignoreInit = TRUE)

# ------------------------------------------------------------------------------
# 2b) Dynamic txt input to rename antigen
# ------------------------------------------------------------------------------
# Dynamic number of text inputs. If the user wishes to enter a specific name for
# the antigen it is done here. 
observeEvent({sapply(rv$addAntigen$colRenameInput, function(x){
  req(isTruthy(rv$addAntigen) && lock_preImport$done3)
  # print(paste0("=============================================="))
  # print(paste0("x input:", x))
  # print(paste0("COL RENAME INPUT: ", input[[x]]))
  currInputX = input[[x]]
  
  if (!isTruthy(input[[x]])){
    currInputX = ""
  }
  
  idx2update = which(rv$addAntigen$colRenameInput == x)
  rv$addAntigen$colRenamed[idx2update] = currInputX
  
  input[[x]]} 
  
  
)}, {
}, ignoreInit = TRUE)

# ------------------------------------------------------------------------------
# 2c) Dynamic remove button
# ------------------------------------------------------------------------------
# Dynamic number of remove buttons. User presses a button to remove the antigen
# in the corresponding row. 
observeEvent({sapply(rv$addAntigen$removeDim, function(x){
  req(isTruthy(rv$addAntigen$removeDim) && lock_preImport$done3)
  # # Check work
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
  idx2update = which(rv$addAntigen$removeDim == x)
  # Update the saved current value of the obsEvent
  rv$addAntigen$removed[idx2update] = currInputX
  
  # Only do this if the button is actually pressed
  if (currInputX > 0){
    # Update the current number of antigens to be one less than the current
    currNumAnt = as.numeric(input$numDimensions)
    newNumAnt = currNumAnt - 1
    # If 0, prevent out of bounds error
    if (newNumAnt < 0){ newNumAnt = 0}
    # Update the dropdown menu to have one less antigen
    updateSelectInput(session = session, 
                      "numDimensions", rui$selectedAntigens,
                      choices = rui$numPossibleAntigens,
                      selected = newNumAnt)
  }
  # Return current input
  input[[x]]} 
)}, {
}, ignoreInit = TRUE)



# ==============================================================================
#
#                            3) HELPER FUNCTIONS
#
# ==============================================================================
# ------------------------------------------------------------------------------
# 3a) Define antigen input ID's
# ------------------------------------------------------------------------------
# This helper function defines the input ID's for the dropdown menus, txt input 
# and remove buttons used in adding an antigen
getAntigenInputIDs <- function(i, uid){
  # i refers to current row, uid is number of times "add class" button pressed
  id = paste0("selectNumericCol_", i, uid)
  idplus_1 = paste0("colRenameInput_", i, uid)
  idplus_2 = paste0("removeDim_", i, uid)
  
  return(c(id, idplus_1, idplus_2))
}

# ------------------------------------------------------------------------------
# 3b) Setup initial Antigen UI
# ------------------------------------------------------------------------------

setupAntigenUI <- function(){
  output_list = list()
  # i) One row for each antigen ________________________________________________
  # For number of antigens expected
  if (rv$numDimensions == 0){
    output$rowsOfData <- renderUI({
      NULL
    })
    
  } else {
    for (i in 1 : rv$numDimensions){
      # Define input IDs
      allID = getAntigenInputIDs(i, rv$numDimCount)
      id = allID[1]
      idplus_1 = allID[2]
      idplus_2 = allID[3]
      
      # If the first output ID, add extra margin-top:50px styling
      if (i == 1){
        output_list[[i]] = 
          div(fluidRow(
            column(1, 
                   div(i,
                       style = "margin-top:35px"
                   ) # end div
            ), # end column
            column(4, 
                   uiOutput(outputId = id)
            ), # end column
            column(4, 
                   uiOutput(outputId = idplus_1) 
            ), # end column
            column(1,
                   div(
                     uiOutput(outputId = idplus_2),
                     style = "margin-top: -30px; padding: 15px;"
                   ) # end div
            ) # end column
          ), # end fluidRow
          style = "margin-top:-50px"
          ) # End div
      } else {
        # If subsequent outputs, do without margin-top:-50px; 
        output_list[[i]] = 
          div(fluidRow(
            column(1, 
                   div(i,
                       style = "margin-top:35px"
                   ) # end div
            ), # end column
            column(4, 
                   uiOutput(outputId = id)
            ), # end column
            column(4, 
                   uiOutput(outputId = idplus_1) 
            ), # end column
            column(1,
                   div(
                     uiOutput(outputId = idplus_2),
                     style = "margin-top: -30px; padding: 15px;"
                   ) #end div 
            ) # end column
          ), # end fluidRow
          ) # End div
      } # end conditional block
    } # End i loop
    
    # ii) Add additional styling _________________________________________________
    # Render the dynamic number of rows of UI inputs defined above
    # with additional styling depending on number of added classes
    output$rowsOfData <- renderUI({
      if (rv$numDimensions > 3){
        div(output_list, 
            style = "padding: 45px; height:250px; 
          overflow-y: scroll; overflow-x: hidden;"
        )
      } else if (rv$numDimensions > 2) {
        div(output_list, 
            style = "padding: 45px; height:300px;
                      overflow-y: scroll; overflow-x: hidden;"
            
        )
      } else if (rv$numDimensions > 1){
        div(output_list, 
            style = "padding: 45px; height:200px;
                      overflow-y: scroll; overflow-x: hidden;"
            
        )
      } else if (rv$numDimensions == 1) {
        div(output_list, 
            style = "padding: 45px; height:100px;"
        )
      } else {
        NULL
      }
    })
  }
}


# ------------------------------------------------------------------------------
# 3c) Render Antigen UI
# ------------------------------------------------------------------------------
renderAntigenUI <- function(){
  # For each antigen, render the row of inputs  
  for (i in 1:rv$numDimensions){
    local({
      i<- i
      # Define input IDs
      allID = getAntigenInputIDs(i, rv$numDimCount)
      id = allID[1]
      idplus_1 = allID[2]
      idplus_2 = allID[3]
      
      # i) Autopopulate previous work __________________________________________
      # Init autopop value defaults
      numericCol_selected = rv$dataColNames[1]
      txtInp_entered = ""
      
      # Get any previous data
      if (isTruthy(rv$prevAntigenData)){
        previdx = i
        # Removing an antigen (-) diff
        if (rv$diffAnt < 0){
          # Check if this antigen was removed
          if (isTruthy(which(rv$prevAntigenData$removed == 1))){
            if (which(rv$prevAntigenData$removed == 1) <= previdx){
              previdx = previdx + 1
            }
          } 
        }
        # Only use previous values while previdx is less than existing number
        # of previous values (prevNum)
        if (previdx <= rv$prevNumAnt){
          numericCol_selected = rv$prevAntigenData$colSelect[previdx]
          txtInp_entered = rv$prevAntigenData$colRenamed[previdx]
        } 
      } # end conditional
      
      # ii) Render the outputs _________________________________________________
      # Determine which columns need marking as invalid column
      concatStmt = colorInvalidColumns(paste0("#", id),
                                       rv$dataColNames, 
                                       rv$numDataColNames)
      
      # Render the dropdown menu for each antigen column select
      if (input$showNumeric_inDataImport == FALSE){
        output[[id]] = renderUI({
          div(tags$head(
            tags$style(HTML(concatStmt))),
            style = "width: 150px; margin-top:5px",
            selectInput(id,
                        "",
                        choices = rv$dataColNames,
                        selected = numericCol_selected)
          )
        }) # end renderUI
        
      } else {
        output[[id]] = renderUI({
          div(tags$head(
            tags$style(HTML(concatStmt))),
            style = "width: 150px; margin-top:5px",
            selectInput(id,
                        "",
                        choices = rv$numDataColNames,
                        selected = rv$numDataColNames[1])
          )
        }) # end renderUI
      }
      
      # render textInput to give the antigen a new name
      output[[idplus_1]] = renderUI({
        div(
          textInput(idplus_1, "",
                    value = txtInp_entered),
          style = "width: 175px; margin-top:-10px; padding: 10px"
        ) # end div
      }) # end renderUI
      
      # render the remove button for each antigen
      output[[idplus_2]] = renderUI({
        div(
          actionButton(idplus_2,
                       label = HTML("<span class='small'><i class=
                                       'glyphicon glyphicon-remove'
                                       ></i></span>")),
          style = "margin-top:20px; padding: 15px"
        ) # end div
      }) # end renderUI
    }) # end local
  } # end i loop
}


# ------------------------------------------------------------------------------
# 3d) Store Antigen Metadata in Reactive variable
# ------------------------------------------------------------------------------
updateReactiveAntigenData <- function(){
  
  #### Initialize empty dataframe to store reactive variables
  addAntigenDF = data.frame(
    antigenNum = numeric(),
    selectNumericCol = character(), colSelect = character(),
    colRenameInput = character(), colRenamed = character(),
    removeDim = character(), removed = character()
  )
  
  # For each antigen, save the ID to reactive variable
  for (i in 1:rv$numDimensions){
    # Define input IDs
    allID = getAntigenInputIDs(i, rv$numDimCount)
    id = allID[1]
    idplus_1 = allID[2]
    idplus_2 = allID[3]
    
    # Check work
    # print(paste0("output[[ ]] i = ", i, ": ", output[[id]]))
    # Save fields to dataframe
    addAntigenDF[nrow(addAntigenDF)+1, ] = c(
      i, 
      id, rv$numDataColNames[1],
      idplus_1, "",
      idplus_2, 0)
  }
  
  # Save dataframe to reactive variable
  rv$addAntigen = addAntigenDF
}