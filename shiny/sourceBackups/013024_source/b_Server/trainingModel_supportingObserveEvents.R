# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                           Program Header Status  
#                             IN PROGRESS 092123
#                   Header: 🗸  Comments: 🗸X  Refactored: 🗸X        
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ==============================================================================
# 
# TITLE: trainingModel_supportingObserveEvents.R
#
#                               DESCRIPTION
# This program contains short observeEvents that support the User interface
# relating to data analysis and training the model.
#
#                             TABLE OF CONTENTS
#       1) MATRIX DISPLAY
#
# ==============================================================================
# ==============================================================================
# 
#                       1) MATRIX DISPLAY
#
# ==============================================================================


observeEvent(input$resetA, {
  rv$currA2IDX = 1
  
  output$matrixStats <- renderUI({
    currIdx = rv$currA2IDX
    allIterVals = rv$allIterVals
    currRow = allIterVals[which(allIterVals$idx == currIdx), 
                          c("L", "sigma")]
    
    HTML(paste0("Matrix: ", rv$currA2IDX, "<br/>",
                "Sigma: ", unique(currRow$sigma), "<br/>",
                "L: ", unique(currRow$L)))
  })
  
  
  output$matrixAViewer <- renderTable({
    currIdx = rv$currA2IDX
    
    # Get where current index is
    allIterVals = rv$allIterVals
    A2disp = allIterVals[which(allIterVals$idx == currIdx), "A"]
    
    
    format(round(matrix(A2disp, ncol = rv$numCol), 16), nsmall = 16)
    
  }, caption= paste0("Matrix ", rv$currA2IDX), caption.placement = "top")
  
})

observeEvent(input$nextA, {
  currIDX = rv$currA2IDX
  newidx = currIDX + 1
  
  if (newidx > rv$maxA2IDX){
    newidx = rv$maxA2IDX
  }
  
  rv$currA2IDX = newidx
  
  output$matrixStats <- renderUI({
    currIdx = rv$currA2IDX
    allIterVals = rv$allIterVals
    currRow = allIterVals[which(allIterVals$idx == currIdx), 
                          c("L", "sigma")]
    
    HTML(paste0("Matrix: ", rv$currA2IDX, "<br/>",
                "Sigma: ", unique(currRow$sigma), "<br/>",
                "L: ", unique(currRow$L)))
  })
  
  
  output$matrixAViewer <- renderTable({
    
    # Get where current index is
    currIdx = newidx
    allIterVals = rv$allIterVals
    A2disp = allIterVals[which(allIterVals$idx == currIdx), "A"]
    
    format(round(matrix(A2disp, ncol = rv$numCol), 16), nsmall = 16)
    
  }, caption= paste0("Matrix ", rv$currA2IDX), caption.placement = "top")
})


appendA2DF <- function(df, uID, idx, L, S, A, nreps){
  
  for (i in 1:nreps){
    df[nrow(df)+1, ] = c(uID, idx, L, S, A[i])
    
  }
  return(df)
  
}



observeEvent(input$rawOrLogData, {
  # lock_analysis$done2 = FALSE ### removed 010524
  rv$rawOrLog = input$rawOrLogData
  
  if (rv$rawOrLog == rui$rawOrLog[2]){
  updateTabsetPanel(session = session, inputId = "trainingTabs",
                    selected = rui$trainingTabNames[2])
  } else {
    # Switch to raw data tab
    updateTabsetPanel(session = session, inputId = "trainingTabs",
                      selected = rui$trainingTabNames[1])
  }
  
})




# +--------------------------------------------------------------------------------

observeEvent(input$testData_q1_resetA, {
  tst$currACounter_q1 = 1
  
  output$testData_matrixStats_q1 <- renderUI({
    currIdx = tst$currACounter_q1
    allIterVals = tst$allIterVals
    currRow = allIterVals[which(allIterVals$idx == currIdx), 
                          c("L", "sigma")]
    
    HTML(paste0("Matrix: ", tst$currACounter_q1, "<br/>",
                "Sigma: ", unique(currRow$sigma), "<br/>",
                "L: ", unique(currRow$L)))
  })
  
  
  output$testData_matrixAViewer_q1 <- renderTable({
    currIdx = tst$currACounter_q1
    
    # Get where current index is
    allIterVals = tst$allIterVals
    A2disp = allIterVals[which(allIterVals$idx == currIdx), "A"]
    
    
    format(round(matrix(A2disp, ncol = rv$numCol), 16), nsmall = 16)
    
  }, caption= paste0("Initial Matrix ", tst$currACounter_q1), caption.placement = "top")
  
})

observeEvent(input$testData_q1_nextA, {
  currIDX = tst$currACounter_q1
  newidx = currIDX + 1
  
  if (newidx > rv$maxA2IDX){
    newidx = rv$maxA2IDX
  }
  
  tst$currACounter_q1 = newidx
  
  output$testData_matrixStats_q1 <- renderUI({
    currIdx = tst$currACounter_q1
    allIterVals = tst$allIterVals
    currRow = allIterVals[which(allIterVals$idx == currIdx), 
                          c("L", "sigma")]
    
    HTML(paste0("Matrix: ", tst$currACounter_q1, "<br/>",
                "Sigma: ", unique(currRow$sigma), "<br/>",
                "L: ", unique(currRow$L)))
  })
  
  
  output$testData_matrixAViewer_q1 <- renderTable({
    
    # Get where current index is
    currIdx = newidx
    allIterVals = tst$allIterVals
    A2disp = allIterVals[which(allIterVals$idx == currIdx), "A"]
    
    format(round(matrix(A2disp, ncol = rv$numCol), 16), nsmall = 16)
    
  }, caption= paste0("Initial Matrix ", tst$currACounter_q1), caption.placement = "top")
})


# ______________________________________________________________________________

observeEvent(input$testData_q2_resetA, {
  tst$currACounter_q2 = 1
  
  output$testData_matrixStats_q2 <- renderUI({
    currIdx = tst$currACounter_q2
    allIterVals = tst$q1AllIterVals
    currRow = allIterVals[which(allIterVals$idx == currIdx), 
                          c("L", "sigma")]
    
    HTML(paste0("Matrix: ", tst$currACounter_q2, "<br/>",
                "Sigma: ", unique(currRow$sigma), "<br/>",
                "L: ", unique(currRow$L)))
  })
  
  
  output$testData_matrixAViewer_q2 <- renderTable({
    currIdx = tst$currACounter_q2
    
    # Get where current index is
    allIterVals = tst$q1AllIterVals
    A2disp = allIterVals[which(allIterVals$idx == currIdx), "A"]
    
    
    format(round(matrix(A2disp, ncol = rv$numCol), 16), nsmall = 16)
    
  }, caption= paste0("q1 Matrix ", tst$currACounter_q2), caption.placement = "top")
  
})

observeEvent(input$testData_q2_nextA, {
  currIDX = tst$currACounter_q2
  newidx = currIDX + 1
  
  if (newidx > rv$maxA2IDX){
    newidx = rv$maxA2IDX
  }
  
  tst$currACounter_q2 = newidx
  
  output$testData_matrixStats_q2 <- renderUI({
    currIdx = tst$currACounter_q2
    allIterVals = tst$q1AllIterVals
    currRow = allIterVals[which(allIterVals$idx == currIdx), 
                          c("L", "sigma")]
    
    HTML(paste0("Matrix: ", tst$currACounter_q2, "<br/>",
                "Sigma: ", unique(currRow$sigma), "<br/>",
                "L: ", unique(currRow$L)))
  })
  
  
  output$testData_matrixAViewer_q2 <- renderTable({
    
    # Get where current index is
    currIdx = newidx
    allIterVals = tst$q1AllIterVals
    A2disp = allIterVals[which(allIterVals$idx == currIdx), "A"]
    
    format(round(matrix(A2disp, ncol = rv$numCol), 16), nsmall = 16)
    
  }, caption= paste0("q1 Matrix ", tst$currACounter_q2), caption.placement = "top")
})


# ______________________________________________________________________________

observeEvent(input$testData_q3_resetA, {
  tst$currACounter_q3 = 1
  
  output$testData_matrixStats_q3 <- renderUI({
    currIdx = tst$currACounter_q3
    allIterVals = tst$q2AllIterVals
    currRow = allIterVals[which(allIterVals$idx == currIdx), 
                          c("L", "sigma")]
    
    HTML(paste0("Matrix: ", tst$currACounter_q3, "<br/>",
                "Sigma: ", unique(currRow$sigma), "<br/>",
                "L: ", unique(currRow$L)))
  })
  
  
  output$testData_matrixAViewer_q3 <- renderTable({
    currIdx = tst$currACounter_q3
    
    # Get where current index is
    allIterVals = tst$q2AllIterVals
    A2disp = allIterVals[which(allIterVals$idx == currIdx), "A"]
    
    
    format(round(matrix(A2disp, ncol = rv$numCol), 16), nsmall = 16)
    
  }, caption= paste0("q2 Matrix ", tst$currACounter_q3), caption.placement = "top")
  
})

observeEvent(input$testData_q3_nextA, {
  currIDX = tst$currACounter_q3
  newidx = currIDX + 1
  
  if (newidx > rv$maxA2IDX){
    newidx = rv$maxA2IDX
  }
  
  tst$currACounter_q3 = newidx
  
  output$testData_matrixStats_q3 <- renderUI({
    currIdx = tst$currACounter_q3
    allIterVals = tst$q2AllIterVals
    currRow = allIterVals[which(allIterVals$idx == currIdx), 
                          c("L", "sigma")]
    
    HTML(paste0("Matrix: ", tst$currACounter_q3, "<br/>",
                "Sigma: ", unique(currRow$sigma), "<br/>",
                "L: ", unique(currRow$L)))
  })
  
  
  output$testData_matrixAViewer_q3 <- renderTable({
    
    # Get where current index is
    currIdx = newidx
    allIterVals = tst$q2AllIterVals
    A2disp = allIterVals[which(allIterVals$idx == currIdx), "A"]
    
    format(round(matrix(A2disp, ncol = rv$numCol), 16), nsmall = 16)
    
  }, caption= paste0("q2 Matrix ", tst$currACounter_q3), caption.placement = "top")
})





