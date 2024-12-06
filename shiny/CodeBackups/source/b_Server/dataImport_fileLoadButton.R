# ==============================================================================
# 
# TITLE: dataImport_fileLoadButton.R
#
#                               DESCRIPTION
# This button contains the large observeEvent that responds to the user pressing
# the "Load" button to begin data import. Users must first upload a file and 
# select an excel sheet before pressing. A table showing raw data is rendered, 
# as well as UI inputs to allow users to select and import data.
# 
#                             TABLE OF CONTENTS
#       1) IMPORT DATA
#               i) Check excel sheet upload  
#       2) DEFINE SAMPLE ID AVAILABLE COLUMNS
#               i) Find columns that contain all unique entries
#               ii) Render sample ID dropdown menu
#       3) DEFINE CLASS AVAILABLE COLUMNS
#       4) RENDER RAW DATATABLE
#               i) Set sigfigs for numeric columns
#               ii) Raw datatable output
#
# ==============================================================================


observeEvent(input$updateDataSelect, {
  # ============================================================================
  # 1) IMPORT DATA
  # ============================================================================
  # Only continue if file(s) uploaded 
  req(isTruthy(input$fileUploadInput))
  
  # Check file extension
  updateUIBeforeBtnPress()
  
  # adjust header location and rows to skip based on user input 
  headerRow = as.numeric(input$specifyHeaderRow)
  headerRowSkip = headerRow - 1
  if (headerRowSkip < 0){
    # Don't skip negative number of rows
    headerRowSkip = 0
  } 
  
  # If no header is present (select 0 for header row)
  if (headerRow == 0){
    # Read excel sheet without column names (default to numeric names)
    df = read_xlsx(rv$fileUploadPath, rv$sheetCount, skip = headerRowSkip,
                   col_names = FALSE)
  } else {
    df = read_xlsx(rv$fileUploadPath, rv$sheetCount, skip = headerRowSkip)
  }
  
  # Save df reactively
  rv$df = df
  
  # ============================================================================
  # 2) DEFINE SAMPLE ID AVAILABLE COLUMNS
  # ============================================================================
  # Initialize data structures for unique columns
  colIsUnique = c()
  uniqueCols = c()
  
  # Save column names of import
  dataColNames = colnames(df)
  # Get total number of rows
  numTotalEntries = dim(df)[1]
  
  # i) Find columns that contain all unique entries ____________________________
  # Loop through all columns
  for (i in 1:length(dataColNames)){
    # Get current column
    currColName = dataColNames[i]
    currCol = df[, currColName]
    # Get number of unique entries in current column i
    uniqueEntries = unique(currCol)
    numUniqueEntries = dim(uniqueEntries)[1]
    
    # See if the column is valid for a sample ID column (every value is)
    if (numUniqueEntries == numTotalEntries){
      colIsUnique = c(colIsUnique, "TRUE")
      # Store list of all entirely unique columns
      uniqueCols = c(uniqueCols, currColName)
    } else {
      colIsUnique = c(colIsUnique, "FALSE")
    }
  } # end i loop
  
  # Save to reactive variable
  # All columns that are entirely unique
  rv$uniqueCols = uniqueCols
  # all column names
  rv$dataColNames = dataColNames
  
  # ii) render sample ID dropdown menu _________________________________________
  # If filter is on
  if (rv$addUniqueColAnyway == FALSE){
    uniqueCols = c(uniqueCols)
    # When a sheet is selected, see the available columns
    output$selectSampleIDCol <- renderUI({
      # Dropdown menu for selecting sample ID column
      selectInput("selectSampleIDCol",
                  rui$selectSampleID, 
                  choices = uniqueCols,
                  selected = uniqueCols[1])
    })
  } else {

    
    concatStmt = colorInvalidColumns("#selectSampleIDCol",
                                     dataColNames, uniqueCols)
    
    # When a sheet is selected, see the available columns
    
    output$selectSampleIDCol <- renderUI({
      
      div(tags$head(
        # Highlight options for selectInput
        tags$style(HTML(concatStmt))),
        selectInput("selectSampleIDCol",
                    rui$selectSampleID, 
                    choices = dataColNames,
                    selected = dataColNames[1])
      )
    })
  } # end conditional block
  
  # ============================================================================
  # 3) DEFINE CLASS AVAILABLE COLUMNS
  # ============================================================================
  output$selectClassCol <- renderUI({
    selectInput("selectClassCol",
                "Select a column containing class assignments:",
                choices = dataColNames,
                selected = dataColNames[1])
  })
  
  # Find unique classes for the current column
  rv$currUniqueClasses = unique(df[, dataColNames[1]])
  
  # render dropdown menu for classes
  output$uniqueClassesOutput <- renderUI({
    selectInput("uniqueClassesOutput", "Available classes: ",
                choices = rv$currUniqueClasses,
                selected = ""
    )
  })
  
  # ============================================================================
  # 4) RENDER RAW DATATABLE
  # ============================================================================
  # i) Set sigfigs for numeric columns _________________________________________
  nums <- unlist(lapply(df, is.numeric), use.names = FALSE)
  numData_colnames = colnames(df[, nums])
  numsInt = unlist(lapply(df, is.integer), use.names = FALSE)
  
  df2 = sapply(df,is.numeric)
  ints2<-sapply(df[,df2],function(col){return((sum(col%%1)==0))})
  
  
  
  # ii) Raw datatable output ___________________________________________________
  output$rawDataTable <- DT::renderDataTable({
    # For numeric columns (in df2), set sigfigs depending on double vs. integer
    df[, df2] = df[, df2]  %>%
      mutate(across(where(
        function(x) is.double(x) 
        & !sapply(!sum(x%%1)==0, isFALSE)),
        .fns = function(x) {format(round(x, 4), nsmall = 4)}))
    
    # return full dataframe (all)
    df
    
    # Other alternatives for sigfigs
    # df  %>%
    #   mutate(across(where(function(x) is.double(x) & !sum(x%%1)==0),
    #                 .fns = function(x) {format(round(x, 4), nsmall = 4)}))
    # 
    # mutate(across(where(function(x) is.numeric(x) & !is.integer(x)), 
    # sprintf, fmt = '%.2f'))
  }, 
  filter = "top", # search box
  selection = "multiple", 
  extensions = c("FixedHeader", "FixedColumns"),
  server = FALSE, # disable this for numeric sort
  options = list(
    paging = FALSE,
    scrollX = TRUE,
    scrollY = "500px",
    fixedHeader = T,
    fixedColumns = list(leftColumns = 1))
  )
}) # end renderDataTable










## 07/11/23 - helper function to color invalid columns

colorInvalidColumns <- function(inputID, allColumns, inColumns){
  # Loop through all columns and create an HTML statement for color
  colorNonUniqueHTML = ""
  concatStmt = ""
  
  print(paste0("IN COLUMNS:", inColumns))
  print(paste0("!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#"))
  for (i in 1:length(allColumns)){
    
    print(paste0("INPUT ID:", inputID))
    print(paste0("CURRENTOCLUMN [", i, "]:", allColumns[i]))
    # If the current column i is not in the list of unique columns
    if (!(allColumns[i] %in% inColumns)){
      print(paste0("_____!$!_____"))
      
      currStmt = paste0(inputID, " .option[data-value='", allColumns[i], "']{
          background: red !important;
          color: white !important;} 
                        ")
      concatStmt = paste0(concatStmt, currStmt)
      
    }
    
  }
  
  print(paste0("!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#!@#"))
  
  print(paste0("CONCATSTMT:", concatStmt))
  return(concatStmt)
}

