# ==============================================================================
#
# TITLE: appUI.R
#
#                               DESCRIPTION 
# This program defines the overall UI (user interface) of the shiny application.
# Sub-tabs of the navbar page are sourced, then called using shinyUI({}). 
#
#                                 NOTES
#         - The UI files called below can be found in the source/b_UI/ folder. 
#
# ==============================================================================

# Load GUI programs
source('source/b_UI/sst_GUI_Viewer.R')
source('source/b_UI/sst_GUI_TestData.R')
source('source/b_UI/sst_GUI_Export.R')
source('source/b_UI/sst_GUI_About.R')


# Package appUI 
appUI <- shinyUI({
  # Define image banner, style and title
  fixedPage(theme=shinytheme("flatly"),
            shinyjs::useShinyjs(),
            # (optional) load an image
            # list(tags$head(HTML('<link rel="icon", href="image_name.png",
            #                       type="image/png" />'))),
            div(style="width: '100%'", 
                titlePanel(
                  title="",windowTitle="SERONIST")),
            # Load navbarPage and tabs here
            navbarPage("SERONIST",
                       div(
                         # actionButton("seeActionLog", "View Action Log"),
                         # # style = "position: absolute; top: 5px; right: 5px; z-index:10000;"
                         # style = "position:absolute;right:1em;bottom: 5px;",
                         
                         bsTooltip(id = "seeActionLog", 
                                   title = paste0("View action history. Note that the Load and Submit buttons must be pressed before certain changes are logged."), 
                                   placement = "top", trigger = "hover"),
                         bsModal(id = "outmatPopup", title = "Action Log", trigger = "seeActionLog", size = "large",
                                 withSpinner(  div(
                                   htmlOutput("actionLog"), 
                                   br()))
                         )
                       ), # end div
                       Import_GUITAB, # Data import & training tab
                       Test_GUITAB, # Test Data tab
                       Export_GUITAB, # Export tab
                       About_GUITAB
            )
  )
})