# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                           Program Header Status  
#                             COMPLETED 01182024
#                   Header: 🗸  Comments: 🗸   Refactored: 🗸      
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ==============================================================================
# 
# TITLE: sst_GUI_TestData.R
#
#                               DESCRIPTION
# This program defines the UI for the Test Data tab. This tab is mostly 
# identical to the Data Import & Training tab with some slight modifications and
# a completely different set of input IDs. Helper functions for defining the UI
# are defined at the bottom. 
# 
#                             TABLE OF CONTENTS
#       1) MAIN UI
# 
#       2) DATA ACQUISITION TABS   
#               2A) PRE-IMPORT & OPTIONAL SETTINGS
#                     i) Pre-import
#                     ii) Optional Settings
#
#               2B) SELECT ANTIGENS
#
#               2C) ASSIGN CLASSES
#                     i) Option A vs. Option B
#                     ii) Choose Classes
#                     iii) Assigned Classes
#                     iv) Next Button
#
#               2D) ANALYZE
#                     i) Evaluate
#
#       3) DATATABLE TABS
#
#       4) PLOT TABS
#
#       5) TRAINING METRIC TABS
#
# ==============================================================================
# ==============================================================================
#
#                             1) MAIN LAYOUT
#
# ==============================================================================
# This creates the skeleton of the Test Data tab, it's relatively
# short because the uiOutputs are defined further down in helper functions that
# are only called in response to user activity. 
# Overall tabPanel defined in source/a_StartHere/appUI.R. 
Test_GUITAB <- tabPanel(
  "Test Data",
  # Start sidebarPanel
  sidebarPanel(
    shinyjs::useShinyjs(),
    id = "testDataPanel", width = 12,
    # File upload button
    fluidRow(
      column(6,
             fileInput("testData_fileUploadInput", rui$upload),
      ), # end column
    ), # end fluidRow
    br(), 
    fluidRow(
      # Data acquisition UI, top left
      column(6, 
             uiOutput("testDataAcquisitionTabs"),
             br(),
      ), # end column
      # Datatable UI, top right
      column(6, 
             uiOutput("testDataTableTabs")
      ) # end column
    ), # end fluidRow
    br(),
    fluidRow(
      # Plot UI, bottom left
      column(6,
             uiOutput("testData_initialImportPlotTabs"),
      ), # end column
      # Test Metrics, bottom right
      column(6, 
             uiOutput("testDataTrainingTabs")
      ) # end column
    ), # end fluidRow
    br(),
  ), # end sidebarPanel
  br(),
) # end tabPanel "Test Data"

# ==============================================================================
#
#                         2) DATA ACQUISITION TABS
#
# ==============================================================================
# This helper function defines the tabsetPanel for test data acquisition in four
# tabs for pre-import/optional, select antigens, assign classes and analyze
renderTestDataAcquisitionTabs <- function(){
  renderUI({
    # This subsection contains tools to specify header, excel sheet, and the
    # "Load" button that must be pressed before continuing.
    tabsetPanel(
      type = "tabs", id = "testDataImportTabs",
      # ------------------------------------------------------------------------
      # 2A) PRE-IMPORT & OPTIONAL SETTINGS
      # ------------------------------------------------------------------------
      # i) Pre-Import __________________________________________________________
      tabPanel(
        rui$testTabNamesImport[1], br(),
        div(
          style = "height:500px; overflow-y:auto; overflow-x: hidden",
          fluidRow(
            column(12, 
                   # Dropdown menu to select excel sheet, file must be uploaded 
                   uiOutput("testData_selectSheet"),
                   bsTooltip(id = "testData_selectSheet", 
                             title = rui$selectSheet, 
                             placement = "top", trigger = "hover"),
            ), # end column
          ), # end fluidRow
          fluidRow(
            column(6, 
                   # Specify the row where the header begins
                   uiOutput("testData_specifyHeaderRow"),
                   bsTooltip(id = "testData_specifyHeaderRow", 
                             title = rui$specifyHeaderRow, 
                             placement = "top", trigger = "hover"),
            ), # end column
            column(6, 
                   # Load button
                   actionButton("testData_updateDataSelect", "Load",
                                width = "240px"),
                   bsTooltip(id = "testData_updateDataSelect", 
                             title = rui$updateDataSelect, 
                             placement = "left", trigger = "hover"),
            ) # end column
          ), # end fluidRow
          br(), 
          # ii) Optional Settings ______________________________________________
          # Sample ID and addl. metadata column select
          uiOutput("testData_separatingLine"), 
          uiOutput("testData_optionalSettingsTitle"),
          br(),
          fluidRow(
            column(6,  
                   # dropdown menu for column select
                   uiOutput("testData_selectSampleIDCol"),
                   bsTooltip(id = "testData_selectSampleIDCol", 
                             title = paste0(rui$sampleIDOptionalLabel), 
                             placement = "top", trigger = "hover"),
            ),
            column(6, 
                   # Dropdown menu for additional metadata
                   uiOutput("testData_selectMetadataCols"),
                   bsTooltip(id = "testData_selectMetadataCols", 
                             title = paste0(rui$metadataOptionalLabel),
                             placement = "top", trigger = "hover"
                   )
            ), # end column
          ), # end fluidRow
          br(), br(),
          fluidRow(
            column(12, 
                   align = "right",
                   uiOutput("testData_doneImportBtn") 
            ) # end column
          ), # end fluidRow
        ) # end div
      ), # end tabPanel
      # ------------------------------------------------------------------------
      # 2B) SELECT ANTIGENS
      # ------------------------------------------------------------------------
      tabPanel(
        rui$testTabNamesImport[2],
        div(
          style = "height:500px; overflow-y:auto; overflow-x: hidden",
          br(),
          fluidRow(
            column(6,
                   # Dropdown menu to select number of antigens to add
                   selectInput("testData_numDimensions", 
                               rui$selectedAntigens,
                               choices = rui$numPossibleAntigens,
                               selected = 1),
                   bsTooltip(id = "testData_numDimensions",
                             title = paste0("Provide the number of antigens you wish to submit for training."),
                             placement = "top", trigger = "hover"),
            ), # end column
            column(6,
                   # checkbox to filter columns
                   checkboxInput("testData_showNumeric_inDataImport",
                                 rui$dispOnlyNum,
                                 value = FALSE),
                   bsTooltip(id = "testData_showNumeric_inDataImport",
                             title = paste0("Columns highlighted in Red contain some non-numeric data. Check this box to display the columns with only numeric data."),
                             placement = "left", trigger = "hover"),
            ) # end column
          ), # end fluidRow
          br(),
          fluidRow(
            # Raw HTML for column headers, see global.R for rui contents
            HTML(rui$dimAddnHeader),
            # Display a row for each antigen added
            uiOutput("testData_rowsOfData")
          ), # end fluidRow
          fluidRow(
            column(12,
                   div(
                     # Next button
                     actionButton("testData_updateAntigenSelect", "Next",
                                  width = "240px"),
                     bsTooltip(id = "testData_updateAntigenSelect",
                               title = rui$updateAntigenSelect,
                               placement = "left", trigger = "hover"),
                     align = "right"
                   )
            )
          ) # end fluidRow
        ) # end div
      ),
      # ------------------------------------------------------------------------
      # 2C) ASSIGN CLASSES
      # ------------------------------------------------------------------------
      # i) Option A vs. Option B _______________________________________________
      tabPanel(
        rui$testTabNamesImport[3],
        div(
          style = "height:500px; overflow-y:auto; overflow-x: hidden",
          br(),
          fluidRow(
            id = "testData_pickClassOption",
            column(6,
                   radioButtons("testData_optionab", label = "Choose how to define classes:",
                                choices = rui$optionAB,
                                selected = rui$optionAB[1]
                   ),
                   bsTooltip(id = "testData_optionab",
                             title = paste0("[Option A]: Select a column from your upload to assign classes.<br/>[Option B]: Manually select rows from the table below to assign classes."),
                             placement = "top", trigger = "hover"),
            ), # end column
            # ii) Option A _______________________________________________________
            column(6, 
                   uiOutput("testData_selectClassCol"),
                   bsTooltip(id = "testData_selectClassCol",
                             title = paste0("Select the column from your file upload to get class assignments for your data."),
                             placement = "top", trigger = "hover"),
            ) # end column
          ), #end fluidRow
          # fluidRow(
          #   id = "testData_optionA",
          #   column(6,
          #          # Dropdown menu to select a column to select classes from
          #          uiOutput("testData_selectClassCol"),
          #          bsTooltip(id = "testData_selectClassCol",
          #                    title = paste0("Select the column from your file upload to get class assignments for your data."),
          #                    placement = "top", trigger = "hover"),
          #   ), # end column
          #   # column(3,
          #   #        # checkbox to autopopulate using classes from selected column
          #   #        checkboxInput("useAllAvailClasses",
          #   #                      "Auto populate using all classes",
          #   #                      value = FALSE)
          #   # ), # end column
          #   column(3,
          #          # checkbox for multiple upload support
          #          # checkboxInput("useMultiFileClass",
          #          #               "One class per file (multi-upload)",
          #          #               value = FALSE
          #          # )
          #   ) # end column
          # ), # end fluidRow
          
          # ii) Select Classes _________________________________________________
          fluidRow(
            id = "testData_optionA_pt2",
            column(6,
                   # dropdown menu w/multi-select to select classes for import
                   uiOutput("testData_uniqueClassesOutput"),
                   bsTooltip(id = "testData_uniqueClassesOutput",
                             title = paste0("Select one or more unique classes and press the Add Class button."),
                             placement = "top", trigger = "hover"),
                   # Option B - show number of rows instead
                   hidden(uiOutput("testData_classRowsSelected")),
                   bsTooltip(id = "testData_classRowsSelected",
                             title = paste0("This will display the rows selected for the current class to be added."),
                             placement = "top", trigger = "hover"),
                   
            ), # end column
            column(3,
                   div(
                     # Button to add the selected class(es)
                     actionButton("testData_addClassBtn", "Add Class"),
                     style = "margin-top: 20px;"
                   ) #end div
            ), # end column
            column(3,
                   div(
                     # clear current selections of classes button
                     actionButton("testData_clearClassBtn", "Clear all"),
                     bsTooltip(id = "testData_clearClassBtn",
                               title = paste0("Clear the assigned classes"),
                               placement = "top", trigger = "hover"),
                     style = "margin-top: 20px;"
                   ) # end div
            ) # end column
          ), # end fluidRow
          
          # iii) Selected Classes ______________________________________________
          fluidRow(
            id = "testData_optionA_pt3",
            # Raw HTML for class headers
            uiOutput("testData_classAddnHeader"),
            hidden(uiOutput("testData_classAddnHeader_B")),
            # Display a row of inputs for each class staged for import
            uiOutput("testData_rowsOfClasses"),
          ), # end fluidRow
          br(),
          # iv) Next Button ____________________________________________________
          fluidRow(
            column(12,
                   div(
                     # Submit button
                     actionButton("testData_submitImport", "Submit"),
                     bsTooltip(id = "testData_submitImport",
                               title = paste0(rui$submitImportBSLabel),
                               placement = "top", trigger = "hover"),
                     align = "right"
                     
                   )# end div
            ) # end column 
          ) # end fluidRow
        ) # end div
      ), # end tabPanel
      # ------------------------------------------------------------------------
      # 2D) ANALYZE
      # ------------------------------------------------------------------------
      # i) Evaluate ____________________________________________________________
      tabPanel(
        rui$testTabNamesImport[4],
        div(
          style = "height:500px; overflow-y:auto; overflow-x: hidden",
          br(),
          fluidRow(
            column(6,
                   actionButton("testData_evalTabSubmitBtn", 
                                "Optimize Classifier for Test Data"
                   )
            ), # end column
          ), # end fluidRow
          br(), br(), br(), br(), br(), br(), br(), br(),
          fluidRow(
            column(12,
                   div(
                     uiOutput("testData_evalTabOptimBtn"),
                     align = "right"
                   ) # end div
            ) # end column
          ), # end fluidRow
        ) # end div
      ) # end tabPAnel
    ) # end tabsetPanel
  }) # end renderUI
}

# ==============================================================================
#
#                           3) DATATABLE TABS
#
# ==============================================================================
# Helper function to render datatable tabs for Training data
# Either Preview, Raw or Selected data tabs
renderTestDataTableTabs <- function(numTabs, session){
  if (numTabs == 1){
    r = renderUI({
      div(style = "margin-top: -100px", 
          tabsetPanel(type = "tabs", id = "testData_dataImport_dataTableTabs", 
                      tabPanel(rui$trainingDataTableTabNames[1],
                               div(
                                 DT::dataTableOutput("testData_previewDataTable"),
                                 style = "font-size:70%; margin-top: -100px"
                               )
                      ) # end tabPanel
          )
      )
    }) # end renderUI
  } else if (numTabs == 2){
    r = renderUI({
      div(style = "margin-top: -100px", 
          tabsetPanel(type = "tabs", id = "testData_dataImport_dataTableTabs", 
                      tabPanel(rui$trainingDataTableTabNames[1],
                               div(
                                 DT::dataTableOutput("testData_previewDataTable"),
                                 style = "font-size:70%; margin-top: -100px"
                               )
                               
                      ), # end tabPanel
                      tabPanel(rui$trainingDataTableTabNames[2],
                               div(
                                 DT::dataTableOutput("testData_rawDataTable"),
                                 style = "font-size:70%; margin-top: -100px"
                               )
                      )
          )
      )
    })
  } else if (numTabs == 3){
    r = renderUI({
      div(style = "margin-top: -100px", 
          tabsetPanel(type = "tabs", id = "testData_dataImport_dataTableTabs",
                      tabPanel(rui$trainingDataTableTabNames[1],
                               div(
                                 DT::dataTableOutput("testData_previewDataTable"),
                                 style = "font-size:70%; margin-top: -100px"
                               )
                               
                      ), # end tabPanel
                      tabPanel(rui$trainingDataTableTabNames[2],
                               div(
                                 DT::dataTableOutput("testData_rawDataTable"),
                                 style = "font-size:70%; margin-top: -100px"
                               )
                      ),
                      tabPanel(rui$trainingDataTableTabNames[3],
                               div(
                                 DT::dataTableOutput("testData_filteredDataTable"),
                                 style = "font-size:70%; margin-top: -100px"
                               )
                      )
          )
      )
    })
  } # end conditional
  # Update tabsetPanel to most recent tab rendered
  updateTabsetPanel(session = session, 
                    inputId = "testData_dataImport_dataTableTabs",
                    selected = rui$trainingDataTableTabNames[numTabs])
  return(r)
}

# ==============================================================================
#
#                              4) PLOT TABS
#
# ==============================================================================
# Renders a dynamic number of tabs for plots, either raw, log transformed, 
# or classified data
renderTestDataPlotTabs <- function(numTabs, session){
  # Only two tabs
  if (numTabs == 2){
    r = renderUI({
      tabsetPanel(type = "tabs", id = "testData_initialImportPlotTabs",
                  tabPanel(rui$testData_trainingTabNames[1], 
                           plotlyOutput("testData_firstGraph", height = "700px")
                  ), # end tabPanel
                  tabPanel(rui$testData_trainingTabNames[2],
                           plotlyOutput("testData_logGraph", height = "700px"),
                  ) # end tabPanel
      ) # end tabsetPanel
    }) # end renderUI
    # Three tabs
  } else if (numTabs == 3){
    r = renderUI({
      tabsetPanel(type = "tabs", id = "testData_initialImportPlotTabs",
                  tabPanel( 
                    rui$testData_trainingTabNames[1], 
                    plotlyOutput("testData_firstGraph", height = "700px")
                  ), # end tabPanel
                  tabPanel( 
                    rui$testData_trainingTabNames[2], 
                    plotlyOutput("testData_logGraph", height = "700px"),
                  ), # end tabPanel
                  tabPanel(
                    rui$testData_trainingTabNames[3], 
                    plotlyOutput("testData_plotDataWithOptimBoundary",
                                 height = "700px")
                  ) # end tabPanel
      ) # end tabsetPanel for plots ) # hidden
    }) # end renderUI
  } 
  # Update to the most recent tab if there are more than 2 tabs
  # Ignore if two tabs since current tab depends on raw vs. log selection
  if (numTabs != 2){
    updateTabsetPanel(session = session, 
                      inputId = "testData_initialImportPlotTabs",
                      selected = rui$testData_trainingTabNames[numTabs])
  } else {
    # Update tab in plot depending on Training tab's choice
    # Raw data
    if (rv$rawOrLog == rui$rawOrLog[1]){
      updateTabsetPanel(session = session, 
                        inputId = "testData_initialImportPlotTabs",
                        selected = rui$testData_trainingTabNames[1])
    } else {
      # Log Data
      updateTabsetPanel(session = session,
                        inputId = "testData_initialImportPlotTabs",
                        selected = rui$testData_trainingTabNames[2])
    }
  }
  return(r)
}

# ==============================================================================
#
#                         5) TRAINING METRIC TABS
#
# ==============================================================================
# This helper function renders two tabs, one for classifier metrics the other
# for the matrices and objective fn values associated with the homotopy method
testData_renderEvaluatorTables <- function(){
  renderUI({
    tabsetPanel(
      type = "tabs", id = "testData_evaluatorTabs",
      
      tabPanel("Classifier Metrics", 
               br(),
               p("Prevalence Calculation"),
               tableOutput("testData_optimizedEvalResultsSum"),
               br(),
               uiOutput("testData_boundUncertaintyRhoMax"),
               br(),
               # p("Initial Results"),
               # tableOutput("testData_evaluateResultsSumm"),
               # br(),
               p("Confusion Matrix"),
               tableOutput("testData_confusionMatrix"),
               br(),
               
      ), # end tabPanel
      tabPanel("Matrix associated with Homotopy Optimization", 
               # allIter
               fluidRow(
                 column(4, tableOutput("testData_plotly3dMatViewer_q1")),
                 column(4, tableOutput("testData_plotly3DMatrixM_q1")),
               ),
               br(),
               fluidRow(
                 column(4, tableOutput("testData_matrixAViewer_q1"))
               ),
               fluidRow(
                 column(6, tableOutput("testData_matrixStats_q1"))
               ),
               fluidRow(
                 column(4, actionButton("testData_q1_nextA", "Next Matrix")),
                 column(4, actionButton("testData_q1_resetA", "Start Over"))
               ),
               # q1
               fluidRow(
                 column(4, tableOutput("testData_plotly3dMatViewer_q2")),
                 column(4, tableOutput("testData_plotly3DMatrixM_q2")),
               ),
               br(),
               fluidRow(
                 column(4, tableOutput("testData_matrixAViewer_q2"))
               ),
               fluidRow(
                 column(6, tableOutput("testData_matrixStats_q2"))
               ),
               fluidRow(
                 column(4, actionButton("testData_q2_nextA", "Next Matrix")),
                 column(4, actionButton("testData_q2_resetA", "Start Over"))
               ),
               # q2
               fluidRow(
                 column(4, tableOutput("testData_plotly3dMatViewer_q3")),
                 column(4, tableOutput("testData_plotly3DMatrixM_q3")),
               ),
               br(),
               fluidRow(
                 column(4, tableOutput("testData_matrixAViewer_q3"))
               ),
               fluidRow(
                 column(6, tableOutput("testData_matrixStats_q3"))
               ),
               fluidRow(
                 column(4, actionButton("testData_q3_nextA", "Next Matrix")),
                 column(4, actionButton("testData_q3_resetA", "Start Over"))
               ),
      ) # end tabPanel
    ) # end tabsetPanel
  }) # end renderUI
} # end function

