#
#
# ==============================================================================
#                              !!!WARNING!!!
#                             AS OF 06/20/2023
#                       DO NOT AUTO-INDENT THIS FILE
# ==============================================================================
#
# ==============================================================================
# 
# TITLE: sst_GUI_Viewer.R
#
#                               DESCRIPTION
# This program defines the UI for the Database builder portion of the WDIT.
# Users can use this tab to select data to incorporate into the SQL database.
# 
#                             TABLE OF CONTENTS
#       1) Sidebar Panel
#       1A)     IMPORT
#                   i) Row for selecting excel sheet and loading
#                   ii) Row for specifying header select
#                   iii) Row for selecting a column for sample ID 
#                   iiv) Numeric data import row
#                   v) Row for displaying selected antigens
#                   vi) Class selection row
#                   vii) Row to stage individual classes for import
#                   viii) Row for displaying classes currently staged for import
#                   ix) Display the raw data that was uploaded in a table 
#       1B)     
#       1C)     
#
# ==============================================================================
# ==============================================================================
# 1) Sidebar Panel
# ==============================================================================

# Opening parens
sst_GUI_Viewer <- tabPanel(
  # Tab title
  "Classification Tool",
  br(), br(),
  
  sidebarPanel(shinyjs::useShinyjs(),
               id = "startingPanel", width = 12,
               tabsetPanel(id = "tabSwitch_startPanel",
               
# ==============================================================================
# 1A) IMPORT
# ==============================================================================

# Subtab 1 of 3
tabPanel("Data Import & Training", # subtab title
  br(),
  
  # Sample tooltip code
  # bsTooltip(id = "fileUploadInput", title = "This is an input", 
  #           placement = "left", trigger = "hover"),
  # addTooltip(session, id = "someInput", title = "This is an input.",
  #            placement = "left", trigger = "hover") # For server
  
  # File upload button
  fileInput("fileUploadInput", rui$upload),
  
  # i) Row for selecting excel sheet and loading________________________________
  fluidRow(
    column(4, 
           # Dropdown menu to select excel sheet, file must be uploaded 
           uiOutput("selectSheet"),
           bsTooltip(id = "selectSheet", title = "This is an input!!", 
                     placement = "left", trigger = "hover"),
    ), # end column
    column(4, 
           # Specify the row where the header begins (0 for no header)
           uiOutput("specifyHeaderRow")
    ), # end column
    column(4, br(),
           # Load button once file uploaded and sheet selected
           div(
             actionButton("updateDataSelect", "Load",
                          width = "240px"),
             align = "center"
           ) # end div
    ) # end column
  ), # end fluidRow
  br(),  
  
  # iii) Row for selecting a column for sample ID (all unique values)___________
  fluidRow(
    column(6,  
           # dropdown menu for column select
           uiOutput("selectSampleIDCol")
    ),
    column(6, br(),
           # checkbox to filter columns
           checkboxInput("addUniqueColAnyway", rui$displayOnlyCols)
    ), # end column
    p("______________________________________________________________________________________________________________________________________________________________________"),
    br(),
  ), # end fluidRow
  
  # iiv) Numeric data import row________________________________________________
  fluidRow(
    column(6, 
           # Dropdown menu to select number of antigens to add
           selectInput("numDimensions", rui$selectedAntigens, 
                       choices = rui$numPossibleAntigens,
                       selected = 0)
    ), # end column
    column(3, 
           # checkbox to filter columns
           checkboxInput("showNumeric_inDataImport",
                         rui$dispOnlyNum,
                         value = FALSE),
    ), # end column
    column(3, 
           # checkbox to autopopulate
           checkboxInput("autoPopulateNumeric",
                         rui$autopop,
                         value = FALSE)
    ) # end column
  ), # end fluidRow
  br(),
  
  # v) Row for displaying selected antigens ____________________________________
  fluidRow(

    # Raw HTML for column headers, see global.R for rui contents
    HTML(rui$dimAddnHeader),
    # Display a row for each antigen added
    div(
      uiOutput("rowsOfData")
      # style = "padding: 45px; height:100px;"
      # style = "padding: 45px; height:300px; 
      # overflow-y: scroll; overflow-x: hidden;",
    ), # end div
    p("______________________________________________________________________________________________________________________________________________________________________"),
    br(),
  ), # end fluidRow
  br(), 

  # v) Select option A/B
  fluidRow(id = "pickClassOption",
           column(12, 
                  radioButtons("optionab", label = "Choose how to define classes:",
                               choices = c("Option A: Add classes by column", 
                                           "Option B: Add classes by row (manually)"),
                               selected = c("Option A: Add classes by column")
                  )
           ), # end column
  ), #end fluidRow
  # vi) Class selection row ____________________________________________________
  fluidRow(id = "optionA",
    column(6, 
           # Dropdown menu to select a column to select classes from
           uiOutput("selectClassCol"),
    ), # end column
    column(3, 
           # checkbox to autopopulate using classes from selected column
           checkboxInput("useAllAvailClasses", 
                         "Auto populate using all classes",
                         value = FALSE)
    ), # end column
    column(3, 
           # checkbox for multiple upload support
           checkboxInput("useMultiFileClass",
                         "One class per file (multi-upload)",
                         value = FALSE
           )
    ) # end column
  ), # end fluidRow
  
  # vii) Row to stage individual classes for import ____________________________
  fluidRow(
    column(6, 
           # dropdown menu w/multi-select to select classes for import
           uiOutput("uniqueClassesOutput")
    ), # end column
    column(3, 
           div(
             # Button to add the selected class(es)
             actionButton("addClassBtn", "Add Class"),
             style = "margin-top: 20px;"
           ) #end div
    ), # end column
    column(3, 
           div(
             # clear current selections of classes button
             actionButton("clearClassBtn", "Clear all"),
             style = "margin-top: 20px;"
           ) # end div
    ) # end column
  ), # end fluidRow
  br(), 
  
  # viii) Row for displaying classes currently staged for import _______________
  fluidRow(
    # Raw HTML for class headers
    HTML(rui$classAddnHeader),
    
    div(
      # Display a row of inputs for each class staged for import
      uiOutput("rowsOfClasses"),
      # style = "height:300px;",
      # style = "height:300px; overflow-y: scroll; overflow-x: hidden;",
    ), # end div
    p("______________________________________________________________________________________________________________________________________________________________________"),
    br(),
    
  ), # end fluidRow
  br(),

  # Submit button
  actionButton("submitImport", "Submit"),
  br(),
  
  # ix) Display the raw data that was uploaded in a table ______________________
  DT::dataTableOutput("rawDataTable"), 
  # div(DT::dataTableOutput("rawDataTable"), 
  #     style = "height:500px; overflow-y: scroll;overflow-x: scroll;"),
  
), # end Import tabPanel

# ==============================================================================
# 
# ==============================================================================

tabPanel("Evaluate",
         
        
), # end Evaluate tabPanel

# ==============================================================================
# 
# ==============================================================================

tabPanel("Export",
        
) # end Export tabPanel

# ==============================================================================
# Closing parens 

    ) # end tabsetPanel
  ) # end sidebarPanel
) # end tabPanel
