# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                           Program Header Status  
#                             COMPLETED 01182024
#                   Header: 🗸  Comments:🗸   Refactored: 🗸          
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ==============================================================================
# 
# TITLE: sst_GUI_Viewer.R
#
#                               DESCRIPTION
# This program defines the UI for the Data Import & Training tab. The main UI is
# defined in section (1), which defines four main uiOutputs that are defined
# in response to the user's actions during the session. These actions call
# helper functions in sections (2-4) that render the remainder of the uiOutputs.
# 
#                             TABLE OF CONTENTS
#       1) MAIN LAYOUT
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
#                     ii) Choose classes
#                     iii) Assigned Classes
#                     iv) Next Button
#
#               2D) ANALYZE & OPTIMIZE
#                     i) Analyze/Evaluate
#                     ii) Optimization Parameters
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
# This creates the skeleton of the Data import and Training tab, it's relatively
# short because the uiOutputs are defined further down in helper functions that
# are only called in response to user activity. 
# Overall tabPanel defined in source/a_StartHere/appUI.R. 
Import_GUITAB <- tabPanel(
  "Data Import & Training", br(),
  # Start sidebarPanel
  sidebarPanel(
    shinyjs::useShinyjs(),
    id = "startingPanel", width = 12,
    # File upload button
    fluidRow(
      column(6,
             fileInput("fileUploadInput", rui$upload),
      ),
    ), # end fluidRow
    br(), 
    fluidRow(
      # Data acquisition UI, top left
      column(6, 
             # Contents of each tab rendered in renderDataAcquisitionTabs()
             uiOutput("dataAcquisitionTabs"),
             br(),
      ), 
      # Datatable UI, top right
      column(6, 
             uiOutput("dataTableTabs"),
      ),
    ), # end fluidRow
    br(),
    fluidRow(
      # Plot UI, bottom left
      column(6, 
             uiOutput("initialImportPlotTabs"),
             
      ), # end column
      # Training Metrics, bottom right
      column(6, 
             uiOutput("dataTrainingTabs"),
      ) # end column
    ), # end fluidRow
    br(),
  ), # end sidebarPanel
  br(),
) # end tabPanel "Data Import & Training" 

# ==============================================================================
#
#                         2) DATA ACQUISITION TABS
#
# ==============================================================================
# This helper function defines the tabsetPanel, four tabs to pre-import, select 
# antigens, assign classes, and analyze. 
renderDataAcquisitionTabs <- function(){
  renderUI({
    # This subsection contains tools to specify header, excel sheet, and the
    # "Load" button that must be pressed before continuing.
    tabsetPanel(
      type = "tabs", id = "dataImportTabs",
      # ------------------------------------------------------------------------
      # 2A) PRE-IMPORT & OPTIONAL SETTINGS
      # ------------------------------------------------------------------------
      # i) Pre-Import __________________________________________________________
      tabPanel(
        rui$tabNamesImport[1], br(),
        div(
          style = "height:500px; overflow-y:auto; overflow-x: hidden",
          fluidRow(
            column(12, 
                   # Dropdown menu to select excel sheet, file must be uploaded 
                   uiOutput("selectSheet"),
                   bsTooltip(id = "selectSheet", 
                             title = rui$selectSheet, 
                             placement = "top", trigger = "hover"),
            ) # end column
          ), # end fluidRow
          fluidRow(
            column(6, 
                   # Specify the row where the header begins
                   uiOutput("specifyHeaderRow"),
                   bsTooltip(id = "specifyHeaderRow", 
                             title = rui$specifyHeaderRow, 
                             placement = "top", trigger = "hover"),
            ), # end column
            column(6, 
                   # Load button
                   actionButton("updateDataSelect", "Load",
                                width = "240px"),
                   bsTooltip(id = "updateDataSelect", 
                             title = rui$updateDataSelect, 
                             placement = "left", trigger = "hover"),
            ) # end column
          ), # end fluidRow
          br(), 
          # ii) Optional Settings ______________________________________________
          # Sample ID and addl. metadata column select
          uiOutput("separatingLine"),
          uiOutput("optionalSettingsTitle"),
          br(),
          fluidRow(
            column(6,  
                   # dropdown menu for column select
                   uiOutput("selectSampleIDCol"),
                   bsTooltip(id = "selectSampleIDCol", 
                             title = paste0(rui$sampleIDOptionalLabel), 
                             placement = "top", trigger = "hover"),
            ),
            column(6, 
                   # Dropdown menu for additional metadata
                   uiOutput("selectMetadataCols"),
                   bsTooltip(id = "selectMetadataCols", 
                             title = paste0(rui$metadataOptionalLabel),
                             placement = "top", trigger = "hover"
                   )
            ), # end column
          ), # end fluidRow
          br(), br(),
          fluidRow(
            column(12, 
                   align = "right",
                   uiOutput("doneImportBtn")
            ) # end column
          ), # end fluidRow
        ) # end div
      ), # end tabPanel
      # ------------------------------------------------------------------------
      # 2B) SELECT ANTIGENS
      # ------------------------------------------------------------------------
      tabPanel(
        rui$tabNamesImport[2], 
        div(
          style = "height:500px; overflow-y:auto; overflow-x: hidden",
          br(),
          fluidRow(
            column(6, 
                   # Dropdown menu to select number of antigens to add
                   selectInput("numDimensions", 
                               rui$selectedAntigens, 
                               choices = rui$numPossibleAntigens,
                               selected = 1),
                   bsTooltip(id = "numDimensions", 
                             title = paste0("Provide the number of antigens you wish to submit for training."), 
                             placement = "top", trigger = "hover"),
            ), # end column
            column(6,
                   # checkbox to filter columns
                   checkboxInput("showNumeric_inDataImport",
                                 rui$dispOnlyNum,
                                 value = FALSE),
                   bsTooltip(id = "showNumeric_inDataImport", 
                             title = paste0("Columns highlighted in Red contain some non-numeric data. Check this box to display the columns with only numeric data."), 
                             placement = "left", trigger = "hover"),
            ) # end column
          ), # end fluidRow
          br(),
          fluidRow(
            # Raw HTML for column headers, see global.R for rui contents
            HTML(rui$dimAddnHeader),
            # Display a row for each antigen added
            uiOutput("rowsOfData")
          ), # end fluidRow
          fluidRow(
            column(12, 
                   div(
                     # Next button
                     actionButton("updateAntigenSelect", "Next",
                                  width = "240px"),
                     bsTooltip(id = "updateAntigenSelect", 
                               title = rui$updateAntigenSelect, 
                               placement = "left", trigger = "hover"),
                     align = "right"
                   ) # end div
            ) # end column
          ) # end fluidRow
        ) # end div
      ),
      # ------------------------------------------------------------------------
      # 2C) ASSIGN CLASSES
      # ------------------------------------------------------------------------
      # i) Option A vs. Option B _______________________________________________
      tabPanel(
        rui$tabNamesImport[3], 
        div(
          style = "height:500px; overflow-y:auto; overflow-x: hidden",
          br(),
          fluidRow(
            id = "pickClassOption",
            column(6, 
                   radioButtons("optionab", 
                                label = "Choose how to assign classes:",
                                choices = rui$optionAB[1],
                                selected = rui$optionAB[1]
                   ),
                   bsTooltip(id = "optionab", 
                             title = paste0("[Option A]: Select a column from your upload to assign classes.<br/>[Option B]: Manually select rows from the table below to assign classes."), 
                             placement = "top", trigger = "hover"),
            ), # end column
            column(6, 
                   # Dropdown menu to select a column to select classes from
                   uiOutput("selectClassCol"),
                   bsTooltip(id = "selectClassCol", 
                             title = paste0("Select the column from your file upload to get class assignments for your data."), 
                             placement = "top", trigger = "hover"),
            )# end column
          ), #end fluidRow          
          # ii) Choose Classes _________________________________________________
          fluidRow(
            id = "optionA_pt2",
            column(6, 
                   # dropdown menu w/multi-select to select classes for import
                   uiOutput("uniqueClassesOutput"),
                   bsTooltip(id = "uniqueClassesOutput", 
                             title = paste0(rui$classSelectBSLabel), 
                             placement = "top", trigger = "hover"),
                   # Option B - show number of rows instead
                   hidden(uiOutput("classRowsSelected")),
                   bsTooltip(id = "classRowsSelected", 
                             title = paste0(rui$classRowSelectedBSLabel), 
                             placement = "top", trigger = "hover"),
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
                     bsTooltip(id = "clearClassBtn", 
                               title = paste0("Clear the assigned classes"), 
                               placement = "top", trigger = "hover"),
                     style = "margin-top: 20px;"
                   ) # end div
            ) # end column
          ), # end 
          # iii) Assigned Classes ______________________________________________
          fluidRow(
            id = "optionA_pt3",
            # Raw HTML for class headers
            uiOutput("classAddnHeader"),
            hidden(uiOutput("classAddnHeader_B")),
            # Display a row of inputs for each class staged for import
            uiOutput("rowsOfClasses"),
          ), # end fluidRow
          br(),
          # iv) Next Button ____________________________________________________
          fluidRow(
            column(12, 
                   div(
                     # Submit button
                     actionButton("submitImport", "Submit"),
                     bsTooltip(id = "submitImport", 
                               title = paste0(rui$submitImportBSLabel), 
                               placement = "left", trigger = "hover"),
                     align = "right"
                   ) # end div
            ) # end column
          ) # end fluidRow
        ) # end div
      ), # end tabPanel
      # ------------------------------------------------------------------------
      # 2D) ANALYZE AND OPTIMIZE
      # ------------------------------------------------------------------------
      # i) Analyze/Evaluate ____________________________________________________
      tabPanel(
        rui$tabNamesImport[4],
        div(
          style = "height:500px; overflow-y:auto; overflow-x: hidden",
          br(),
          fluidRow(
            column(6,
                   radioButtons("rawOrLogData", 
                                label = "Choose data transformation:",
                                choices = rui$rawOrLog,
                                selected = rui$rawOrLog[1]
                   )
            ), # end column
            column(6, 
                   actionButton("evalTabSubmitBtn", "Initialize Optimization")
            )
          ), # end fluidRow
          fluidRow(
            column(6,
                   div(align = "center"),
                   uiOutput("evalTabOptimBtn")
            ) # end column 
          ), # end fluidRow
          br(), br(),
          # ii) Optimization Parameters ________________________________________
          p("______________________________________________________________________________________________"),
          strong("Optimization Parameters:"), br(),br(), 
          fluidRow(
            column(6, 
                   radioButtons("chooseBFGS", label = rui$bfgs_label,
                                choices = rui$bfgsChoices,
                                selected = rui$bfgsChoices[1],
                                inline = TRUE
                   ),
                   bsTooltip(id = "chooseBFGS",
                             title = paste0(rui$bfgs_bstt),
                             placement = "bottom", trigger = "hover")
            ), # end column
            column(6, 
                   div(
                     br(),
                     numericInput("bu_hyperparameter", 
                                  rui$bu_label,
                                  min = rui$bu_mmv[1],
                                  max = rui$bu_mmv[2], 
                                  value = rui$bu_mmv[3]),
                     style = "margin-top: -20px"
                   ), # end div
                   bsTooltip(id = "bu_hyperparameter",
                             title = paste0(rui$bu_bstt),
                             placement = "bottom", trigger = "hover")
            ) # end column
          ), # end fluidRow
          fluidRow(
            column(6, 
                   numericInput("reltol_hyperparam", 
                                rui$reltol_label,
                                min = rui$reltol_mmv[1], 
                                max = rui$reltol_mmv[2], 
                                value = rui$reltol_mmv[3]),
                   bsTooltip(id = "reltol_hyperparam",
                             title = paste0(rui$reltol_bstt),
                             placement = "bottom", trigger = "hover")
            ), # end column
            column(6, 
                   numericInput("abstol_hyperparam", 
                                rui$abstol_label,
                                min = rui$abstol_mmv[1], 
                                max = rui$abstol_mmv[2], 
                                value = rui$abstol_mmv[3]),
                   bsTooltip(id = "abstol_hyperparam",
                             title = paste0(rui$abstol_bstt),
                             placement = "bottom", trigger = "hover")
            ) # end column
          ), # end fluidRow
          fluidRow(
            column(6, 
                   numericInput("ndeps_hyperparam", 
                                rui$ndeps_label,
                                min = rui$ndeps_mmv[1], 
                                max = rui$ndeps_mmv[2],
                                value = rui$ndeps_mmv[3]),
                   bsTooltip(id = "ndeps_hyperparam",
                             title = paste0(rui$ndeps_bstt),
                             placement = "bottom", trigger = "hover")
            ), # end column
            column(6,
                   div(
                     numericInput("maxit_hyperparam", 
                                  rui$maxit_label,
                                  min = rui$maxit_mmv[1], 
                                  max = rui$maxit_mmv[2], 
                                  value = rui$maxit_mmv[3]),
                     style = "margin-top:20px"
                   ), # end div
                   bsTooltip(id = "maxit_hyperparam",
                             title = paste0(rui$maxit_bstt),
                             placement = "bottom", trigger = "hover")
            ) # end column
          ), # end fluidRow
          fluidRow(
            column(6, 
                   numericInput("pgtol_hyperparameter", 
                                rui$pgtol_label,
                                min = rui$pgtol_mmv[1], 
                                max = rui$pgtol_mmv[2], 
                                value = rui$pgtol_mmv[3]),
                   bsTooltip(id = "pgtol_hyperparameter",
                             title = paste0(rui$pgtol_bstt),
                             placement = "bottom", trigger = "hover")
            ), # end column
            column(6, 
                   numericInput("factr_hyperparameter",
                                rui$factr_label,
                                min = rui$factr_mmv[1],
                                max = rui$factr_mmv[2], 
                                value = rui$factr_mmv[3]),
                   bsTooltip(id = "factr_hyperparameter",
                             title = paste0(rui$factr_bstt),
                             placement = "bottom", trigger = "hover")
            ) # end column
          ), # end fluidRow
          fluidRow(
            actionButton("hyperparam_restoreDefaults",
                         label = "Restore Defaults"),
            bsTooltip(id = "hyperparam_restoreDefaults",
                      title = paste0(rui$hyperparamRestore_bstt),
                      placement = "bottom", trigger = "hover")
          ), # end fluidRow
          br(), br(), br(), br(), br(), br(), 
        ) # end div
      ) # end tabPanel
    ) # end tabsetPanel
  }) # end renderUI
} # end function

# ==============================================================================
#
#                           3) DATATABLE TABS
#
# ==============================================================================
# Helper function to render datatable tabs for Training data
# Either Preview, Raw or Selected data tabs
renderDataTableTabs <- function(numTabs, session){
  if (numTabs == 1){
    r = renderUI({
      div(style = "margin-top: -100px", 
          tabsetPanel(type = "tabs", id = "dataImport_dataTableTabs", 
                      tabPanel(rui$trainingDataTableTabNames[1],
                               div(
                                 DT::dataTableOutput("previewDataTable"),
                                 style = "font-size:70%; margin-top: -100px"
                               )
                      ) # end tabPanel
          ) # end tabsetPanel
      ) # end div
    }) # end renderUI
  } else if (numTabs == 2){
    r = renderUI({
      div(style = "margin-top: -100px", 
          tabsetPanel(type = "tabs", id = "dataImport_dataTableTabs", 
                      tabPanel(rui$trainingDataTableTabNames[1],
                               div(
                                 DT::dataTableOutput("previewDataTable"),
                                 style = "font-size:70%; margin-top: -100px"
                               )
                      ), # end tabPanel
                      tabPanel(rui$trainingDataTableTabNames[2],
                               div(
                                 DT::dataTableOutput("rawDataTable"),
                                 style = "font-size:70%; margin-top: -100px"
                               )
                      ) # end tabPanel
          ) # end tabsetPanel
      ) # end div
    }) # end renderUI
  } else if (numTabs == 3){
    r = renderUI({
      div(style = "margin-top: -100px", 
          tabsetPanel(type = "tabs", id = "dataImport_dataTableTabs", 
                      tabPanel(rui$trainingDataTableTabNames[1],
                               div(
                                 DT::dataTableOutput("previewDataTable"),
                                 style = "font-size:70%; margin-top: -100px"
                               )
                      ), # end tabPanel
                      tabPanel(rui$trainingDataTableTabNames[2],
                               div(
                                 DT::dataTableOutput("rawDataTable"),
                                 style = "font-size:70%; margin-top: -100px"
                               )
                               
                      ), # end tabPanel
                      tabPanel(rui$trainingDataTableTabNames[3], 
                               div(
                                 DT::dataTableOutput("filteredDataTable"),
                                 style = "font-size:70%; margin-top: -100px"
                               )
                      ) # end tabPanel
          ) # end tabsetPanel
      ) # end div
    }) # end renderUI
  } # end conditional
  # Update tabsetPanel to most recent tab rendered
  updateTabsetPanel(session = session, inputId = "dataImport_dataTableTabs",
                    selected = rui$trainingDataTableTabNames[numTabs])
  return(r)
}

# ==============================================================================
#
#                              4) PLOT TABS
#
# ==============================================================================
# This helper function renders a dynamic number of tabs that contain training 
# plots for raw, log transformed, initial guess and optimzied classifier
renderImportAndTrainingTabs <- function(numTabs, session){
  # Only two tabs
  if (numTabs == 2){
    r = renderUI({
      tabsetPanel(
        type = "tabs", id = "trainingTabs", 
        tabPanel(rui$trainingTabNames[1], 
                 plotlyOutput("firstGraph", height = "700px")
        ), # end tabPanel
        tabPanel(rui$trainingTabNames[2],
                 plotlyOutput("logGraph", height = "700px"),
        ) # end tabPanel
      ) # end tabsetPanel
    }) # end renderUI
    # Three tabs
  } else if (numTabs == 3){
    r = renderUI({
      tabsetPanel(
        type = "tabs", id = "trainingTabs", 
        tabPanel(rui$trainingTabNames[1], 
                 plotlyOutput("firstGraph", height = "700px")
        ), # end tabPanel
        tabPanel(rui$trainingTabNames[2],
                 plotlyOutput("logGraph", height = "700px"),
        ), # end tabPanel
        tabPanel(
          rui$trainingTabNames[3],
          plotlyOutput("plotDataWithBoundary", height = "700px")
        ) # end tabPanel
      ) # end tabsetPanel
    }) # end renderUI
    # Four tabs
  } else if (numTabs == 4){
    r = renderUI({
      tabsetPanel(
        type = "tabs", id = "trainingTabs", 
        tabPanel(rui$trainingTabNames[1], 
                 plotlyOutput("firstGraph", height = "700px")
        ), # end tabPanel
        tabPanel(rui$trainingTabNames[2],
                 plotlyOutput("logGraph", height = "700px"),
        ), # end tabPanel
        tabPanel(
          rui$trainingTabNames[3],
          plotlyOutput("plotDataWithBoundary", height = "700px")
        ), # end tabPanel
        tabPanel(
          rui$trainingTabNames[4], 
          plotlyOutput("plotDataWithOptimBoundary", height = "700px")
        ) # end tabPanel
      ) # end tabsetPanel
    }) # end renderUI
  }
  
  # Update to the most recent tab if there are more than 2 tabs
  # Ignore if two tabs since current tab depends on raw vs. log selection
  if (numTabs != 2){
    updateTabsetPanel(session = session, inputId = "trainingTabs",
                      selected = rui$trainingTabNames[numTabs])
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
renderEvaluatorTables <- function(){
  renderUI({
    tabsetPanel(
      type = "tabs", id = "evaluatorTabs",
      # First tab
      tabPanel("Classifier Metrics", 
               br(),
               p("Uniform Uncertainty Bounds"), 
               uiOutput("boundUncertaintyRhoMax"),
               # br(),
               # p("Initial Results"),
               # tableOutput("evaluateResultsSumm"),
               br(),
               p("Confusion Matrix"),
               uiOutput("optimizedEvalResultsSum"),
               br(),
      ), # end tabPanel
      # Second tab
      tabPanel("Matrix associated with Homotopy Optimization", 
               fluidRow(
                 column(4, tableOutput("plotly3dMatViewer")),
                 column(4, tableOutput("plotly3DMatrixM")),
               ),
               br(),
               fluidRow(
                 column(4, tableOutput("matrixAViewer"))
               ),
               fluidRow(
                 column(4, tableOutput("matrixStats"))
               ),
               fluidRow(
                 column(4, actionButton("nextA", "Matrix Viewer")),
                 column(4, actionButton("resetA", "Start Over"))
               )
      ) # end tabPanel
    ) # end tabsetPanel
  }) # end renderUI
} # end function
