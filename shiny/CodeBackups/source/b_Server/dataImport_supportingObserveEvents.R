# ==============================================================================
# 
# TITLE: dataImport_supportingObserveEvents.R
#
#                               DESCRIPTION
# This program contains short observeEvents that support the User interface
# relating to data import. 
#
#                             TABLE OF CONTENTS
#       1) FILE UPLOAD
#       2) SELECT EXCEL SHEET
#       3) SELECT SAMPLE ID COLUMN
#       3) CHEFCKBOX FOR SAMPLE ID FILTER
#
# ==============================================================================
# ==============================================================================
# 1) FILE UPLOAD
# ==============================================================================
# This observeEvent allows users to upload one or many files, and updates the
# selectSheet input (if an xlsx upload) to show available sheets to import

observeEvent(input$fileUploadInput, {
  # Get the datapath from the file upload
  datapath = paste0("Data/", input$fileUploadInput[1])
  # Write to reactive variable
  rv$fileUploadPath = datapath
  # Check file extension
  
  # Do the upload
  updateUIBeforeBtnPress()
  
  # Render UI for excel sheet dropdown menu select
  output$selectSheet <- renderUI({
    selectInput("selectSheet",
                "Select Excel sheet to upload",
                choices = rv$currSheets,
                selected = rv$currSheets[1])
  })
  
})

# ==============================================================================
# 2) SELECT EXCEL SHEET
# ==============================================================================
observeEvent(input$selectSheet, {
  rv$sheetCount = input$selectSheet
  
})

# ==============================================================================
# 3) SELECT SAMPLE ID COLUMN
# ==============================================================================
observeEvent(input$selectSampleIDCol, {
  rv$selectSampleIDCol = input$selectSampleIDCol
})


# ==============================================================================
# 4) CHEFCKBOX FOR SAMPLE ID FILTER
# ==============================================================================
# This observeEvent filters the available columns that can be used for sample ID
# If checked, this checKbox will only show columns where EVERY value is unique.
# If not checked, any column can be used as sample ID but extra processes will
# be required to make every value unique. 

observeEvent(input$addUniqueColAnyway, {
  rv$addUniqueColAnyway = input$addUniqueColAnyway
  
  if (rv$addUniqueColAnyway == FALSE){
    # When a sheet is selected, see the available columns
    output$selectSampleIDCol <- renderUI({
      selectInput("selectSampleIDCol",
                  rui$selectSampleID, 
                  choices = rv$uniqueCols,
                  selected = rv$uniqueCols[1])
    })
  } else {
    # When a sheet is selected, see the available columns
    output$selectSampleIDCol <- renderUI({
      selectInput("selectSampleIDCol",
                  rui$selectSampleID, 
                  choices = rv$dataColNames,
                  selected = rv$dataColNames[1])
    })
  }
})




# 07/11/23 _____________________________________________________________________
# 
# observeEvent(input$headerPresentCheck, {
#   # Ask user to check (YES) if the header is located in an abnormal location
#   #         Ex) A header that is not in the first row
#   # Ask user to uncheck (NO) if no header is present or the header is row 1
#   rv$headerPresent = input$headerPresentCheck
#   
# })

observeEvent(input$specifyHeaderRow, {
  
  rv$headerRow = input$specifyHeaderRow
})


observeEvent(input$selectSheet, {
  
  updateUIBeforeBtnPress()
  
})



updateUIBeforeBtnPress <- function(){
  
  # Get available sheets in the upload if excel
  rv$currSheets = excel_sheets(rv$fileUploadPath)
  # By default use first sheet unless observeEvent triggered
  sheetCount = 1
  
  # Loop through all available sheets
  for (a in 1:length(rv$currSheets)){
    currCurr = rv$currSheets[a]
    if (isTruthy(input$selectSheet)){
      
      
      # If the current sheet isn't the one currently selected
      if (input$selectSheet != currCurr){
        # increment counter
        sheetCount = sheetCount + 1
      } else {
        # If found currently selected sheet, exit with current sheet count
        break
      }
    }
  } # end a loop
  
  # Load a temp copy of the data to get number of rows in the selected sheet
  tempdf = read_xlsx(rv$fileUploadPath, sheetCount)
  numRowsDF = nrow(tempdf)
  rui$numPossibleRowsDataStart = seq(0, numRowsDF)
  
  if (isTruthy(input$specifyHeaderRow)){
    rv$prevHeaderRowSelect = as.numeric(input$specifyHeaderRow)
  } 
  
  # render the UI for specifying header row number
  output$specifyHeaderRow <- renderUI({
    selectInput("specifyHeaderRow", "Specify the row number for the header",
                choices = rui$numPossibleRowsDataStart,
                selected = rv$prevHeaderRowSelect)
  })
}









