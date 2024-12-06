# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                           Program Header Status 
#                             COMPLETED 092123
#                   Header: 🗸   Comments: 🗸   Refactored: 🗸        
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ==============================================================================
# 
# TITLE: trainingModel_performOptimization.R
#
#                               DESCRIPTION
# This program contains functions that perform the optimization over the initial
# classification boundary to generate a new one. Helper functions are provided
# to make calling the optimization easier and a helper to display matrices.
# 
#                             TABLE OF CONTENTS
#       1)  PERFORM OPTIMIZATION
#               1a) Optimize initial conditions
#               1b) Optimize over sigvals
#               1c) Function Param for Optim
#
#       2) UPDATE MATRIX VIEWER
#
# ==============================================================================

observeEvent(input$evalTabOptimBtn, {
  req(isTruthy(rv$addAntigen) && isTruthy(rv$addClass) && 
        dim(rv$addAntigen)[1] >= 1 && dim(rv$addClass)[1] > 1 &&
        lock_import$done3 && lock_analysis$done1)
  disable("evalTabOptimBtn")
  rv$addClass = rv$newAddClass
  
  incprog = 1/4
  withProgress(message = "Performing Optimization:", value = 0, {
    
    # output$initialImportPlotTabs =  renderImportAndTrainingTabs(4, session)
    #  # perform optimization 
    # Gets rv$allIterVals to use for rv$outmat
    # [lock_analysis#done2]
    performOptimization()
    
    incProgress(incprog, detail = "Displaying optimized model")
    
    # Update matrix viewer
    # [lock_analysis$done4]
    updateMatrixViewer()
    
    req(lock_analysis$done4)
    incProgress(incprog, detail = "Plotting data with optimized boundary")
    
    if (rv$rawOrLog == rui$rawOrLog[1]){
      plot_anyData(ID ="plotDataWithOptimBoundary", 
                   TabID = "initialImportPlotTabs", 
                   Tabs = 4,  Dim = rv$numDimensions, 
                   Transform = "Raw", Stage = "Train", 
                   Bound = "Yes", Precision = "Optimized", 
                   Select = "Column", Disp = "Subset",
                   Session = session)
    } else {
      plot_anyData(ID ="plotDataWithOptimBoundary", 
                   TabID = "initialImportPlotTabs", 
                   Tabs = 4,  Dim = rv$numDimensions, 
                   Transform = "Log", Stage = "Train", 
                   Bound = "Yes", Precision = "Optimized", 
                   Select = "Column", Disp = "Subset",
                   Session = session)
    }
    # # Create plot
    #  plotInitialBoundary("train")
    
    incProgress(incprog, detail = "Displaying optimized confusion matrix")
    
    lock_analysis$done5 = TRUE
    
    # Get summary statistics for results
    getTableSummaryStats("train")
    
    
  })
  enable("evalTabOptimBtn")
  
})

# ==============================================================================
# 
#                         1) PERFORM OPTIMIZATION
#
# ==============================================================================
# This program collects the initial conditions to perform an optimization using
# the first sigval. The resulting parameters are then used as the starting point
# to repeat the optimization under all sigval conditions. 
performOptimization <- function(){
  req(lock_analysis$done1)
  # ----------------------------------------------------------------------------
  # 1a) Optimize initial conditions
  # ----------------------------------------------------------------------------
  # Get latest values
  outmat = op$outmat
  sigvals = op$sigvals
  
  ## 121423 added
  ### Double check all areas wehre rv$lgPositives is used vs rv$positives 
  if (input$rawOrLogData == rui$rawOrLog[2]){
    
    positives = rv$lgPositives
    negatives = rv$lgNegatives
  } else {
    # Extract latest data
    positives = rv$positives
    negatives = rv$negatives
    
  }
  # Perform optimization
  Aimin1 = as.vector(outmat)
  sigimin1 = sigvals[1]
  rv$numLen = length(as.vector(outmat))
  rv$numCol = sqrt(length(as.vector(outmat)))
  
  optim_out = parameterizedOptim(Aimin1, errorEstimate_forOptim,
                                 positives, negatives, sigimin1, -1)
  
  # # Perform the first optimization with the first sigval
  # optim_out = optim(par = Aimin1, fn = errorEstimate_forOptim, poss = positives,
  #                   negs = negatives, sigval = sigimin1, q = -1,
  #                   # method = "L-BFGS-B",
  #                   method = "BFGS",
  #                   
  #                   control = list(ndeps = rep(1e-8, rv$numLen),
  #                   # control = list(ndeps = rep(1.49e-8, rv$numLen),
  #                                  
  #                                  reltol = 1e-20, # 1e-10 -> 1e-12 
  #                                  maxit = 10000,
  #                                  abstol = 1e-20,
  #                                  pgtol = 1e-16,
  #                                  factr = 1e1)
  # )
  
  # optim_out = optimx(par = as.vector(Aimin1), fn = errorEstimate_forOptim, poss = positives,
  #                   negs = negatives, sigval = sigimin1, q = -1,
  #                   method = "L-BFGS-B",
  #                   control = list(ndeps = rep(1e-8, rv$numLen),
  #                                  reltol = 1e-16,
  #                                  maxit = 10000,
  #                                  abstol = 1e-16,  
  #                                  factr = 1e9)
  # )
  
  # # # Check work
  # print(paste0("================================================="))
  # print(paste0("OPTIM PAR:", optim_out$par))
  # print(paste0("OPTIM VALUE:", optim_out$value))
  # print(paste0("OPTIM COUNTS:", optim_out$counts))
  # print(paste0("OPTIM CONVERGENCE:", optim_out$convergence))
  # print(paste0("OPTIM MESSAGE:", optim_out$message))
  # print(paste0("================================================="))
  
  # ------------------------------------------------------------------------------
  # 1b) Optimize over sigvals
  # ------------------------------------------------------------------------------
  # Use a unique ID to track which search / session we are currently on
  uID = 1
  # Save initial Aimin1
  A = Aimin1
  thisAlen = length(Aimin1)
  # Save number of sigvals
  rv$maxA2IDX = length(sigvals)
  
  # Initialize dataframe to store optimization outputs for each sigval
  allIterVals = data.frame(
    uID = numeric(),
    idx = numeric(),
    L = numeric(),
    sigma = numeric(),
    A = numeric()
  )
  
  # For each sigval, perform the optimization
  for (i in 1:length(sigvals)){
    # for (i in 1:2){
    rv$numSigval = i
    
    currSig = sigvals[i]
    
    optim_out = parameterizedOptim(A, errorEstimate_forOptim, 
                                   positives, negatives, currSig, -1)
    # optim_out = optim(par = A, fn = errorEstimate_forOptim, poss = positives,
    #                   negs = negatives, sigval = currSig, q = -1,
    #                   # method = "L-BFGS-B",
    #                   method = "BFGS",
    #                   control = list(ndeps = rep(1e-8, rv$numLen),
    #                   # control = list(ndeps = rep(1.49e-8, rv$numLen),
    #                                  reltol = 1e-20,
    #                                  maxit = 10000,
    #                                  abstol = 1e-20,
    #                                  pgtol = 1e-16,
    #                                  factr = 1e1)
    # )
    # optim_out = optimx(par = as.vector(A), fn = errorEstimate_forOptim, poss = positives,
    #                   negs = negatives, sigval = currSig, q = -1,
    #                   method = "L-BFGS-B",
    #                   control = list(ndeps = rep(1e-8, rv$numLen),
    #                                  reltol = 1e-16,
    #                                  maxit = 10000,
    #                                  abstol = 1e-16,
    #                                  factr = 1e9)
    # )
    
    # # Check work
    # print(paste0("================================================="))
    # print(paste0("OPTIM PAR:", optim_out$par))
    # print(paste0("OPTIM VALUE:", optim_out$value))
    # print(paste0("OPTIM COUNTS:", optim_out$counts))
    # print(paste0("OPTIM CONVERGENCE:", optim_out$convergence))
    # print(paste0("OPTIM MESSAGE:", optim_out$message))
    # print(paste0("================================================="))
    
    # Update the parameters for the next iteration in the for loop
    Aimin1 = A
    A = optim_out$par
    objVal = optim_out$value
    
    #### Append the results using a helper function
    allIterVals = appendA2DF(allIterVals, uID, i,
                             objVal, currSig, Aimin1, thisAlen)
  }
  
  
  
  # Save to reactive value
  rv$allIterVals = allIterVals
  
  ## 101723
  thisAllOutmat = rv$allIterVals
  currIdx = length(sigvals)
  nCol = rv$numDimensions + 1
  rv$currOutmat = matrix(
    as.vector(thisAllOutmat[thisAllOutmat$idx == currIdx, "A"]), ncol = nCol)
  
  # Release lock to continue
  lock_analysis$done4 = TRUE
}


# ==============================================================================
# 
#                         2) UPDATE MATRIX VIEWER
#
# ==============================================================================
# This function waits for the optimization to end (lock_analysis) before 
# rendering the matrix viewer and matrix metadata table
updateMatrixViewer <- function(){
  req(lock_analysis$done4)
  # Get the results of each optimization over sigvals
  allIterVals = rv$allIterVals
  
  # Render a table to display matrix metadata
  output$matrixStats <- renderUI({
    currIdx = rv$currA2IDX
    allIterVals = rv$allIterVals
    # Get the current matrix's metadata
    currRow = allIterVals[which(allIterVals$idx == currIdx),
                          c("L", "sigma")]
    
    HTML(paste0("Matrix: ", rv$currA2IDX, "<br/>",
                "Sigma: ", unique(currRow$sigma), "<br/>",
                "L: ", unique(currRow$L)))
  })
  
  
  # Render the actual matrix viewer that shows the matrix
  output$matrixAViewer <- renderTable({
    # Get where current index is
    currIdx = rv$currA2IDX
    allIterVals = rv$allIterVals
    # Get the matrix to display at this index
    A2disp = allIterVals[which(allIterVals$idx == currIdx), "A"]
    # Convert to matrix
    format(round(matrix(A2disp, ncol = rv$numCol), 16), nsmall = 16)
  }, caption= paste0("Matrix ", rv$currA2IDX), caption.placement = "top")
  
  # Release lock
  ## removed 10/11/23
  # lock_analysis$done3 = TRUE
}


testData_a_updateMatrixViewer <- function(){
  req(lock_testData$done4)
  # Get the results of each optimization over sigvals
  allIterVals = tst$allIterVals
  
  # Render a table to display matrix metadata
  output$testData_matrixStats_q1 <- renderUI({
    currIdx = tst$currACounter_q1
    allIterVals = tst$allIterVals
    # Get the current matrix's metadata
    currRow = allIterVals[which(allIterVals$idx == currIdx),
                          c("L", "sigma")]
    
    HTML(paste0("Matrix: ", tst$currACounter_q1, "<br/>",
                "Sigma: ", unique(currRow$sigma), "<br/>",
                "L: ", unique(currRow$L)))
  })
  
  
  # Render the actual matrix viewer that shows the matrix
  output$testData_matrixAViewer_q1 <- renderTable({
    # Get where current index is
    currIdx = tst$currACounter_q1
    allIterVals = tst$allIterVals
    # Get the matrix to display at this index
    A2disp = allIterVals[which(allIterVals$idx == currIdx), "A"]
    # Convert to matrix
    format(round(matrix(A2disp, ncol = rv$numCol), 16), nsmall = 16)
  }, caption= paste0("Initial Matrix ", tst$currACounter_q1), caption.placement = "top")
  
  # Release lock
  ## removed 10/11/23
  # lock_analysis$done3 = TRUE
}



testData_b_updateMatrixViewer <- function(){
  req(lock_testData$done4)
  # Get the results of each optimization over sigvals
  allIterVals = tst$q1AllIterVals
  
  # Render a table to display matrix metadata
  output$testData_matrixStats_q2 <- renderUI({
    currIdx = tst$currACounter_q2
    allIterVals = tst$q1AllIterVals
    # Get the current matrix's metadata
    currRow = allIterVals[which(allIterVals$idx == currIdx),
                          c("L", "sigma")]
    
    HTML(paste0("Matrix: ", tst$currACounter_q2, "<br/>",
                "Sigma: ", unique(currRow$sigma), "<br/>",
                "L: ", unique(currRow$L)))
  })
  
  
  # Render the actual matrix viewer that shows the matrix
  output$testData_matrixAViewer_q2 <- renderTable({
    # Get where current index is
    currIdx = tst$currACounter_q2
    allIterVals =  tst$q1AllIterVals
    # Get the matrix to display at this index
    A2disp = allIterVals[which(allIterVals$idx == currIdx), "A"]
    # Convert to matrix
    format(round(matrix(A2disp, ncol = rv$numCol), 16), nsmall = 16)
  }, caption= paste0("q1 Matrix ", tst$currACounter_q2), caption.placement = "top")
  
  # Release lock
  ## removed 10/11/23
  # lock_analysis$done3 = TRUE
}





testData_c_updateMatrixViewer <- function(){
  req(lock_testData$done4)
  # Get the results of each optimization over sigvals
  allIterVals = tst$q2AllIterVals
  
  # Render a table to display matrix metadata
  output$testData_matrixStats_q3 <- renderUI({
    currIdx = tst$currACounter_q3
    allIterVals = tst$q2AllIterVals
    # Get the current matrix's metadata
    currRow = allIterVals[which(allIterVals$idx == currIdx),
                          c("L", "sigma")]
    
    HTML(paste0("Matrix: ", tst$currACounter_q3, "<br/>",
                "Sigma: ", unique(currRow$sigma), "<br/>",
                "L: ", unique(currRow$L)))
  })
  
  
  # Render the actual matrix viewer that shows the matrix
  output$testData_matrixAViewer_q3 <- renderTable({
    # Get where current index is
    currIdx = tst$currACounter_q3
    allIterVals =  tst$q2AllIterVals
    # Get the matrix to display at this index
    A2disp = allIterVals[which(allIterVals$idx == currIdx), "A"]
    # Convert to matrix
    format(round(matrix(A2disp, ncol = rv$numCol), 16), nsmall = 16)
  }, caption= paste0("q2 Matrix ", tst$currACounter_q3), caption.placement = "top")
  
  # Release lock
  ## removed 10/11/23
  # lock_analysis$done3 = TRUE
}



