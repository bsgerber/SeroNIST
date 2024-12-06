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
                  title="",windowTitle="Window Title")),
            # div(p(img(src="imagename.png",width="70%")),align="center"),
            # Load navbarPage and tabs here
            navbarPage("SERONIST",
                       sst_GUI_Viewer
            )
  )
})