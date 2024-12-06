# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                           Program Header Status  
#                             COMPLETED 01182024
#                   Header: 🗸  Comments:🗸   Refactored: 🗸          
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ==============================================================================
# 
# TITLE: sst_GUI_Export.R
#
#                               DESCRIPTION
# This program defines the UI for the Export tab. The main UI is
# defined in section (1)
# 
#                             TABLE OF CONTENTS
#       1) MAIN LAYOUT
#
#
# ==============================================================================
# ==============================================================================
#
#                             1) MAIN LAYOUT
#
# ==============================================================================
# Export tab UI elements are defined here. 
# Overall tabPanel defined in source/a_StartHere/appUI.R. 
Export_GUITAB <- tabPanel(
  "Export",
  sidebarPanel(
    shinyjs::useShinyjs(),
    id = "exportPanel", width = 12,
    # tabsetPanel(
    #   id = "exportOptions", 
    # tabPanel(rui$exportTabs[1], br(),
    fluidRow(
      
      column(6,
             strong("Select a folder for export"), br(), br(),
             shinyDirButton("exportDirChoose", "Browse", 
                            "Select a folder for export",
                            allowDirCreate = TRUE
             )
      ),
      column(6, 
             div(
               textOutput("chosenExportDir"),
               style = "margin-top:100px"
             ) # end div
      )
    ), # end fluidRow
    br(),
    fluidRow(
      column(6, 
             radioButtons("fileNameOrDefault", 
                          label = "Choose file name",
                          choices = rui$exportNameChoices,
                          selected = rui$exportNameChoices[1],
                          inline = TRUE
             )
      ), 
      column(6, 
             textInput("exportFileName", "Choose a file name", value = "", 
                       placeholder = "")
      ), # end column
    ), # end fluidRow
    br(),
    fluidRow(
      column(6, 
             selectInput("whatToExport", 
                         label = "Select data to export",
                         choices = rui$whatToExport,
                         selected = rui$whatToExport[rui$wteLength],
                         multiple = FALSE
                         # multiple = TRUE
             )
      ), # end column
      column(6, 
             selectInput("expFileType", 
                         label = "Select a file format",
                         choices = rui$expFileTypeChoices,
                         selected = rui$expFileTypeChoices[1],
                         multiple = FALSE)
      ) # end column
    ), # end fluidRow
    br(),
    
    # ) # end tabPanel
    # ), # end tabPanel
    #   tabPanel(rui$exportTabs[2], br(),
    #            fluidRow(
    #              
    #              column(6,
    #                     strong("Select a folder to save SQL database"), br(), br(),
    #                     shinyDirButton("sql_exportDirChoose", "Browse", 
    #                                    "Select a folder for SQL backup",
    #                                    allowDirCreate = TRUE
    #                     )
    #              ),
    #              column(6, 
    #                     textOutput("sql_chosenExportDir")
    #              )
    #            ), # end fluidRow
    #            fluidRow(
    #              column(6,
    #                     textInput("sqlNameEntry", rui$sqlPrompt),
    #                     bsTooltip(id = "sqlNameEntry", 
    #                               title = paste0(rui$sqlOptionalLabel), 
    #                               placement = "top", trigger = "hover"),
    #              ),
    #            ),
    #            br(),
    #            
    #   ), # end tabPanel
    #   tabPanel(rui$exportTabs[3], br(),
    #            fluidRow(
    #              
    #              column(6,
    #                     strong("Select a folder for figures"), br(), br(),
    #                     shinyDirButton("figs_exportDirChoose", "Browse", 
    #                                    "Select a folder for figures",
    #                                    allowDirCreate = TRUE
    #                     )
    #              ),
    #              column(6, 
    #                     textOutput("figs_chosenExportDir")
    #              )
    #            ), # end fluidRow
    #            
    #   )
    # ),
    br(),
    fluidRow(
      column(12, 
             align = "right",
             actionButton("finalSubmitExport", "Export", width = "240px"))
    ) # end fluidRow
  ) # end sidebarPanel
) # end tabPanel
