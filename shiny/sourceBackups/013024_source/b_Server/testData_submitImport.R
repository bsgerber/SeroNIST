# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                           Program Header Status  
#                             COMPLETED 091823
#                   Header: 🗸  Comments: 🗸   Refactored: 🗸         
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ==============================================================================
#
# TITLE: b_Server/testData_submitImport.R 
#
#                               DESCRIPTION
# This program contains code that responds to the "Submit" button to import data
# and apply filters and selections made by the user and save to the SQL database
# backend. Some initial plots of the selected data are also rendered.
#
#                             TABLE OF CONTENTS
#       1) SUBMIT BUTTON BEHAVIOR
#
#       2) DATA IMPORT & APPLY SETTINGS
#             2a) Apply row and column filters
#             2b) Get positive and negative class
#             2c) Get additional metadata
#
#       3) UPDATE ACTION LOG
#
#                                 NOTES
#
# ==============================================================================
# ==============================================================================
# 
#                        1) SUBMIT BUTTON BEHAVIOR
#
# ==============================================================================
# This observeEvent responds to the submit button and requires the user to first
# assign a sufficient number of antigens and classes for their uploaded data.
observeEvent(input$testData_submitImport, {
  req(isTruthy(rv$testData_addAntigen) &&  #isTruthy(rv$testData_addClass) && 
        dim(rv$testData_addAntigen)[1] > 1) # && dim(rv$testData_addClass)[1] >= 0)
  # # Lock reactive variables
  # lock_testData$done1 = FALSE
  # # lock_testData$done2 = FALSE
  # lock_testData$done3 = FALSE
  # 
  # [lock_import$done1]
  testData_applyDataImportSettings()
  
  # # Get antigen and positive/negative class data and optional metadata
  # # [lock_import$done2] --> rv$positives and rv$negatives 
  # getAntigenPosNegClasses() 
  
  # # Get optional metadata
  # assignOptionalCols()
  testData_renderRawDataTable()
  
  testData_renderFilteredDataTable()
  
  # testData_renderDependentDropdownMenus()
  output$testDataTableTabs <- renderUI({
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
                    ), # end tabPanel
                    tabPanel(rui$trainingDataTableTabNames[3],
                             div(
                               DT::dataTableOutput("testData_filteredDataTable"),
                               style = "font-size:70%; margin-top: -100px"
                             )
                    ), # end tabPanel
        )
    )
  })
  
  
  # Display tabs for plots to be rendered now
  output$testData_initialImportPlotTabs <- renderTestDataPlotTabs(2, session)

  
  output$testData_firstGraph <- renderPlotly({
    # Plot raw data
    testData_plot_rawData("dataImport", "init")
  })
  
  # Populate initial plots on evaluate tab
  output$testData_logGraph <- renderPlotly({
    testData_plot_logTransformData("dataImport", "init")
    
  })
  
  # Update tab for data acquisition selection
  updateTabsetPanel(session = session, inputId = "testDataImportTabs",
                    selected = rui$testTabNamesImport[4])
  
  # release lock to continue
  lock_testData$done3 = TRUE
  
  # # update action log for this button press
  # dataImportSubmit_updateActionLog()
  
})



testData_renderFilteredDataTable <- function(){
  req(lock_testData$done1)
  
  # i) Get min for training data _________________________________________
  train_df = rv$filteredDF
  ## 120523 added
  train_allAntigenCols = unlist(as.character(rv$addAntigen$colSelect))
  
  allMin = min(train_df[, train_allAntigenCols])
  
  # i) Identify numeric vs int columns _________________________________________
  df = rv$testData_filteredDF
  ## 120523 added
  allAntigenCols = unlist(as.character(rv$testData_addAntigen$colSelect))
  
  testMin = min(df[, allAntigenCols])
  
  # If a file has been uploaded for test data and allMin is available
  if (lock_import$done3){
    ## 011724 added replacement for test data log transform
    allLgTransformed = logTransformation_2dArray_testMin(df[, allAntigenCols], 
                                                         allMin, testMin)
    
  } else {
    ## 011724 added replacement for test data log transform
    allLgTransformed = logTransformation_2dArray_testMin(df[, allAntigenCols], 
                                                         allMin, testMin)
    
  }
  
  colnames(allLgTransformed) = paste0("Log Transform ", colnames(allLgTransformed))
  df = cbind(df, allLgTransformed)
  
  # # Get numeric columns
  df2 = sapply(df, is.double)
  
  # Use this function to determine if a column contains only integers
  is_whole <- function(x){
    # Be prepared to handle non-numeric data (will crash)
    if (is.numeric(x)){
      all(floor(x) == x)
    } else {FALSE}
  }
  # Find which columns are integers (will return either NA or TRUE)
  df3 = sapply(df, is_whole)
  # Get the index of columns that contain only integers
  wholeidx = which(is.na(df3))
  trueidx = which(isTruthy(df3))
  # Set those columns to false so they are not adjusted for sigfigs
  df2[wholeidx] = FALSE
  df2[trueidx] = FALSE
  
  # ii) Raw datatable output ___________________________________________________
  output$testData_filteredDataTable <- DT::renderDataTable({
    req(lock_testData$done1)
    # For numeric columns (in df2), set sigfigs depending but only for double
    df[, df2] = df[, df2]  %>%
      mutate(across(everything(),
                    .fns = function(x) {format(round(x, 4), nsmall = 4)}))
    
    df
    
    # Other alternatives for sigfigs
    # df[, df4] = df[, df4]  %>%
    #   mutate(across(where(
    #     function(x) is.double(x) & 
    #       sapply(!sum(x%%1)==0, isFALSE)),
    #     .fns = function(x) {format(round(x, 4), nsmall = 4)}))
    #
    # df  %>%
    #   mutate(across(where(function(x) is.double(x) & !sum(x%%1)==0),
    #                 .fns = function(x) {format(round(x, 4), nsmall = 4)}))
    # 
    # mutate(across(where(function(x) is.numeric(x) & !is.integer(x)), 
    # sprintf, fmt = '%.2f'))
    # 
    # df = df %>%
    #   mutate(across(everything(), as.character))
    
  }, 
  escape = TRUE,
  filter = "top", # search box
  selection = "multiple", 
  # extensions = c("FixedHeader", "FixedColumns"), # Causes extra long scroll
  extensions = c("FixedColumns"),
  server = FALSE, # disable this for numeric sort
  options = list(
    paging = FALSE,
    scrollX = TRUE,
    scrollY = "500px",
    autoWidth = TRUE,
    fixedHeader = T,
    fixedColumns = list(leftColumns = 1),
    columnDefs = list(
      list(width = '100px', targets = 2:ncol(df) - 1)
    )
    
  ) # end option list
  ) # end renderDataTable
  
  # Render the tabs separating different datatables 
  output$testDataTableTabs <- renderTestDataTableTabs(3, session)
  
}



















observeEvent(input$testData_evalTabSubmitBtn, {
  req(lock_analysis$done4)
  
  # Render tabs for plots
  output$testData_initialImportPlotTabs <- renderTestDataPlotTabs(3, session)
  
  
  if (input$rawOrLogData == rui$rawOrLog[2]){
    positives = rv$lgPositives
    negatives = rv$lgNegatives
  } else {
    # Extract latest data
    positives = rv$positives
    negatives = rv$negatives
    
  }
  
  # Use a unique ID to track which search / session we are currently on
  uID = 1
  # Use initial guess as starting point
  A = op$outmat
  sigvals = op$sigvals
  thisAlen = length(A)
  
  # Initialize dataframe to store optimization outputs for each sigval
  allIterVals = data.frame(
    uID = numeric(),
    idx = numeric(),
    L = numeric(),
    sigma = numeric(),
    A = numeric()
  )
  
  withProgress({
    for (i in 1:length(sigvals)){
      
      currSig = sigvals[i]
      
      # optim_out = optim(par = A, fn = errorEstimate_forOptim, poss = positives,
      #                   negs = negatives, sigval = currSig, q = 0.5,
      #                   method = "L-BFGS-B",
      #                   # method = "BFGS"
      #                   control = list(ndeps = rep(1e-8, rv$numLen),
      #                                  reltol = 1e-16,
      #                                  maxit = 10000,
      #                                  abstol = 1e-16,  
      #                                  factr = 1e9
      #                   )
      # )
      
      optim_out = parameterizedOptim(A, errorEstimate_forOptim, positives, negatives,
                                     currSig, 0.5)
      # optim_out = optim(par = A, fn = errorEstimate_forOptim, poss = positives,
      #                   negs = negatives, sigval = currSig, q = 0.5,
      #                   # method = "L-BFGS-B",
      #                   method = "BFGS",
      #                   
      #                   control = list(ndeps = rep(1e-8, rv$numLen),
      #                                  # control = list(ndeps = rep(1.49e-8, rv$numLen),
      #                                  reltol = 1e-20,
      #                                  maxit = 10000,
      #                                  abstol = 1e-20,
      #                                  pgtol = 1e-16,
      #                                  factr = 1e1)
      # )
      
      # Update the parameters for the next iteration in the for loop
      Aimin1 = A
      A = optim_out$par
      objVal = optim_out$value
      
      #### Append the results using a helper function
      allIterVals = appendA2DF(allIterVals, uID, i,
                               objVal, currSig, Aimin1, thisAlen)
      
    } # end loop
    
  }) # end withProgress
  
  # Save all optimized models to reactive variable
  tst$allIterVals = allIterVals
  
  # View final model result for test data
  currIdx = 7
  finalA = allIterVals[which(allIterVals$idx == currIdx), "A"]
  
  # Similar to dataImport_submitImport.R/getAntigenPosNegClasses() function___________________
  
  tst$data = NULL
  
  # Get test data antigen and class selections 
  allAntigenCols_tst = unlist(as.character(rv$testData_addAntigen$colSelect))
  
  if (rv$testData_selectClassCol == "None"){
    classesToFilter = NULL
    classesToFilter_tst = NULL
    classCol_tst = NULL
    classColName_tst = NULL
    
    if (isOptionA("test")){
      # If option A, filter by user-selected column containing classes, 
      # and append antigen columns to class assignment column
      currData = as.data.frame(rv$testData_filteredDF[, allAntigenCols_tst])
    } else {
      # Option B, classes already filtered by row select 
      currData = as.data.frame(rv$testData_filteredDF[, allAntigenCols_tst])
    }
    
    fil_var <- NULL
    
    
  } else {
    # Get classes 
    classesToFilter = unlist(rv$testData_addClass$selectedClasses)
    classesToFilter_tst = splitStringByDelim(classesToFilter, "; ")
    
    # Get class assignments only
    classCol_tst = rv$testData_filteredDF[, rv$testData_selectClassCol]
    classColName_tst = rv$testData_selectClassCol
  
  
  if (isOptionA("test")){
    # If option A, filter by user-selected column containing classes, 
    # and append antigen columns to class assignment column
    currData = as.data.frame(
      rv$testData_filteredDF[
        rv$testData_filteredDF[[rv$testData_selectClassCol]] %in% 
          classesToFilter_tst, 
        c(allAntigenCols_tst, rv$testData_selectClassCol)])
  } else {
    # Option B, classes already filtered by row select 
    currData = as.data.frame(
      rv$testData_filteredDF[, c(allAntigenCols_tst, rv$testData_selectClassCol)])
  }
    
    fil_var <- paste0(colnames(classCol_tst))
    
    
  }
  
  # Get colnames of antigens and classes (fil_var)
  x_var <- paste0(colnames(currData)[1])
  y_var <- paste0(colnames(currData)[2])
  if (rv$testData_numDimensions == 3){
    z_var = paste0(colnames(currData)[3])
  } else {
    z_var = NULL
  }
  
  # Omit missing data
  currData_omitNA = na.omit(currData)
  # Redundant, but do this to prevent reuse of variable names
  currData_noNeg = currData_omitNA
  
  
  if (isOptionA("test")){
    
    if (rv$testData_selectClassCol == "None"){
      # Option A, extract positive and negative classes
      testData_filtered = currData_noNeg[, allAntigenCols_tst]
    } else {
    # Option A, extract positive and negative classes
    testData_filtered = currData_noNeg[
      currData_noNeg[[fil_var]] %in% classesToFilter_tst, 
      allAntigenCols_tst]
    }
  } else {
    
    # Option B, classes were manually selected using row select
    # Get class assignments selected by user
    thisClassCol = currData[[rv$testData_selectClassCol]]
    
    # Get user selected class info for positive class
    currRVClass = rv$testData_addClass[1, ]
    # Get the "class" identity
    # in this case Option B saves class as comma delimited sequences 
    # ie) class 1 = "row:seq1, row:seq2"
    rowsToSubset = currRVClass$selectedClasses
    # Take the above string and create numeric sequence from it
    posNumList = interpretSeqList(rowsToSubset)
    # Get the minimum from this list
    minPosNum = min(posNumList)
    # Correct index starting at 1
    if (minPosNum != 1){
      posNumList = posNumList - minPosNum + 1
    }
    
    maxPosNum = max(posNumList)
    
    # # Repeat above steps for negative class or second row of rv$addClass
    # ncurrRVClass = rv$addClass[2, ]
    # # Get user selected class info from gui
    # negRowsToSubset = ncurrRVClass$selectedClasses
    # # Take above string and create numeric sequence
    # negNumList = interpretSeqList(negRowsToSubset)
    # 
    # # Correct indices to follow after the pos class
    # minNegNum = min(negNumList)
    # negNumList = negNumList - minNegNum + 1 + maxPosNum
    
    # Option B, get classes 
    testData_filtered = currData_noNeg[posNumList, allAntigenCols_tst]
    # negatives = currData_noNeg[negNumList, allAntigenCols]
  }
  
  tst$data = testData_filtered
  
  # Project test data using model made from q=0.5 on training data ___________________________
  ### 083123 - change this to be the minimum  across both negatives and positives
  allMin = min(tst$data[, allAntigenCols_tst])
  # print(paste0("ALLMIN COMPUTE INIT:", allMin))
  # If log transform selected, perform transform here
  if (input$rawOrLogData == rui$rawOrLog[2]){
    tmp_tstData = tst$data
    
    
    # allMin = -0.0106
    ### Do log transformation over data
    # tstData_lgTransform = logTransformation_2dArray_allMin(tmp_tstData, allMin)
    ## 011724 added replacement for test data log transform
    tstData_lgTransform = logTransformation_2dArray_testMin(tmp_tstData, 
                                                         allMin, testMin)
    
    # Remove infinite values resulting from log transform
    tstData_lgTransform = tstData_lgTransform[
      !is.infinite(rowSums(tstData_lgTransform[, allAntigenCols_tst])), ]
    
    # Save to reactive value
    tst$lgData = tstData_lgTransform
    
    
    tstData = tst$lgData
  } else {
    tstData = tst$data
    
  }
  
  
  
  if (rv$testData_selectClassCol == "None"){
    allClasses = NULL
    computedClasses = computeClass(finalA, tstData, x_var, y_var, z_var, NULL)
    
  } else {
    
    allClasses = data.frame(className = rv$testData_addClass$selectedClasses,
                            classRename = rv$testData_addClass$colRenamed)
    computedClasses = computeClass(finalA, tstData, x_var, y_var, z_var, allClasses)
    
  }
 
  
  
  # Get pD and mD for positive and negative classes
  # Get column names for positive and negatives
  px_var = colnames(positives)[1]
  py_var = colnames(positives)[2]
  # Repeat for negatives
  nx_var = colnames(negatives)[1]
  ny_var = colnames(negatives)[2]
  
  if (rv$testData_numDimensions == 3){
    # If third dimension
    nz_var = colnames(negatives)[3]
    pz_var = colnames(positives)[3]
  } else {
    nz_var = NULL
    pz_var = NULL
  }
  
  
  if (rv$testData_selectClassCol == "None"){
    train_allClasses = NULL
    posClasses = computeClass(finalA, positives, 
                              px_var, py_var, pz_var, NULL)
    negClasses = computeClass(finalA, negatives, 
                              nx_var, ny_var, nz_var, NULL)
  } else {
    
  # Get all class information for positive and negative
  train_allClasses = data.frame(className = rv$addClass$selectedClasses,
                                classRename = rv$addClass$colRenamed)
  
  
  posClasses = computeClass(finalA, positives, 
                            px_var, py_var, pz_var, train_allClasses)
  negClasses = computeClass(finalA, negatives, 
                            nx_var, ny_var, nz_var, train_allClasses)
  
  }
  
  # Find Qd
  Qd = mapPredictedClass_toQD(computedClasses)
  Pd = mapPredictedClass_toQD(posClasses)
  Md = mapPredictedClass_toQD(negClasses)
  
  print(paste0("Qd/Pd/Md:", Qd, "/", Pd, "/", Md))
  q1 = (Qd - Md)/(Pd - Md)
  
  print(paste0("q1:", q1))
  
  # ____________________________________________________________________________
  # Use a unique ID to track which search / session we are currently on
  uID = 1
  # Use initial guess as starting point
  A = finalA
  sigvals = op$sigvals
  thisAlen = length(A)
  
  # Initialize dataframe to store optimization outputs for each sigval
  q1AllIterVals = data.frame(
    uID = numeric(),
    idx = numeric(),
    L = numeric(),
    sigma = numeric(),
    A = numeric()
  )
  
  withProgress({
    for (i in 1:length(sigvals)){
      
      currSig = sigvals[i]
      optim_out = parameterizedOptim(A, errorEstimate_forOptim, positives, negatives,
                                     currSig, q1)
      # Update the parameters for the next iteration in the for loop
      Aimin1 = A
      A = optim_out$par
      objVal = optim_out$value
      
      #### Append the results using a helper function
      q1AllIterVals = appendA2DF(q1AllIterVals, uID, i,
                                 objVal, currSig, Aimin1, thisAlen)
      
    } # end loop
  }) # end withProgress
  
  # Save all optimized models to reactive variable
  tst$q1AllIterVals = q1AllIterVals
  
  # View final model result for test data
  currIdx = 7
  q1A = q1AllIterVals[which(q1AllIterVals$idx == currIdx), "A"]
  print(paste0("A from q1:", q1A))
  # ____________________________________________________________________________
  # STEP 2)  Find final prevalence estimate q2
  
  q1ComputedClasses = computeClass(q1A, tstData, x_var, y_var, z_var, allClasses)
  
  # # Get pD and mD for positive and negative classes
  # # Get column names for positive and negatives
  # px_var = colnames(positives)[1]
  # py_var = colnames(positives)[2]
  # nx_var = colnames(negatives)[1]
  # ny_var = colnames(negatives)[2]
  
  # # Get all class information for positive and negative
  # train_allClasses = data.frame(className = rv$addClass$selectedClasses,
  #                               classRename = rv$addClass$colRenamed)
  
  
  ### THIS IS NOT WORKING WITH SUPPLEMENTAL DATASET AGAINST ITSELF Antigens AGM
  q1PosClasses = computeClass(q1A, positives, px_var, py_var, pz_var, train_allClasses)
  q1NegClasses = computeClass(q1A, negatives, nx_var, ny_var, nz_var, train_allClasses)
  
  # Find Qd
  q1_Qd = mapPredictedClass_toQD(q1ComputedClasses)
  q1_Pd = mapPredictedClass_toQD(q1PosClasses)
  q1_Md = mapPredictedClass_toQD(q1NegClasses)
  
  
  q2 = (q1_Qd - q1_Md)/(q1_Pd - q1_Md)
  
  print(paste0("q2:", q2))
  
  
  # ____________________________________________________________________________
  # STEP 3-4) Get A of q2 and use it to classify test data
  
  # Use a unique ID to track which search / session we are currently on
  uID = 1
  # Use initial guess as starting point
  A = q1A
  sigvals = op$sigvals
  thisAlen = length(A)
  
  # Initialize dataframe to store optimization outputs for each sigval
  q2AllIterVals = data.frame(
    uID = numeric(),
    idx = numeric(),
    L = numeric(),
    sigma = numeric(),
    A = numeric()
  )
  
  withProgress({
    for (i in 1:length(sigvals)){
      
      currSig = sigvals[i]
      
      optim_out = parameterizedOptim(A, errorEstimate_forOptim, 
                                     positives, negatives, currSig, q2)
      print(paste0("================================================="))
      print(paste0("Test data"))
      print(paste0("OPTIM PAR:", optim_out$par))
      print(paste0("OPTIM VALUE:", optim_out$value))
      print(paste0("OPTIM COUNTS:", optim_out$counts))
      print(paste0("OPTIM CONVERGENCE:", optim_out$convergence))
      print(paste0("OPTIM MESSAGE:", optim_out$message))
      print(paste0("================================================="))
      
      # Update the parameters for the next iteration in the for loop
      Aimin1 = A
      A = optim_out$par
      objVal = optim_out$value
      
      #### Append the results using a helper function
      q2AllIterVals = appendA2DF(q2AllIterVals, uID, i,
                                 objVal, currSig, Aimin1, thisAlen)
      
    } # end loop
    
    # Save all optimized models to reactive variable
    tst$q2AllIterVals = q2AllIterVals
    
    # View final model result for test data
    currIdx = 7
    q2A = q2AllIterVals[which(q2AllIterVals$idx == currIdx), "A"]
    tst$currOutmat = matrix(q2A, ncol = rv$testData_numDimensions + 1)
    
    q2ComputedClasses = computeClass(q2A, tstData, x_var, y_var, z_var, allClasses)
    
    # ____________________________________________________________________________
    lock_testData$done4 = TRUE
  })
  
  output$testData_optimizedEvalResultsSum <- renderTable({
    gq2AllPred <<- q2ComputedClasses
    lstOfGroups = unique(q2ComputedClasses$allPredClass)
    print(paste0("Lst of Groups testData_optimizedEvalREsultssum:", lstOfGroups))
    
    thisRes = setnames(data.frame(q2, (1-q2)), lstOfGroups)
    row.names(thisRes) = "Prevalence"
    # Save to reactive variable to export
    rv$evaluateResultsSumm = thisRes
    # Return what will be displayed
    thisRes
  }, rownames = TRUE)
  
  
  
  # Display boundaries resulting from evaluation over test data
  output$testDataTrainingTabs <- testData_renderEvaluatorTables()
  
  testData_a_updateMatrixViewer()
  testData_b_updateMatrixViewer()
  testData_c_updateMatrixViewer()
  
  
  output$testData_boundUncertaintyRhoMax <- renderUI({
    req(lock_analysis$done6)
    # thisFNR = allRes[2, "accuracy"]
    # thisFPR = allRes[1, "inaccuracy"]
    thisFNR = op$bu_fnr
    thisFPR = op$bu_fpr
    gthisFNRT <<- thisFNR
    gthisFPRT <<- thisFPR
    
    rhoMax = as.numeric(max(thisFNR, thisFPR))
    gRhoMax <<- rhoMax
    print(paste0("rhoMax in testData_boundUncertaintyRhoMax:", rhoMax))
    S = nrow(tst$data)
    gTST <<- tst$data
    val = sqrt(
      ((rhoMax*(1-rhoMax))/(S*(1-(2*rhoMax))^2)) + ((q2*(1-q2))/S))
    
    p(paste0("Prevalence Uncertainty <=", val))
  })
  
  output$testData_plotDataWithOptimBoundary <- renderPlotly({
    req(lock_testData$done4)
    
    toPlotDF = q2ComputedClasses
    toPlotDF = data.frame(toPlotDF, check.names = FALSE)
    x_var = colnames(q2ComputedClasses)[1]
    y_var = colnames(q2ComputedClasses)[2]
    fil_var = "allPredClass"
    compute_var = "allClassified"
    
    # Get boundary
    minX = min(toPlotDF[, x_var])
    maxX = max(toPlotDF[, x_var])
    minY = min(toPlotDF[, y_var])
    maxY = max(toPlotDF[, y_var])
    
    outmat = tst$currOutmat
    gcuroutmat <<- outmat
    cc <- emdbook::curve3d(
      matrix(c(x,y,1), ncol = 3)%*%tst$currOutmat%*%t(matrix(c(x,y,1), ncol = 3)),
      xlim=c(minX,maxX), ylim=c(minY,maxY), sys3d="none")
    
    # # OPTIM CONT
    dimnames(cc$z) <- list(cc$x, cc$y)
    mm <- reshape2::melt(cc$z)
    dfmm <- data.frame(mm, check.names = FALSE)
    contData = mm
    
    
    
    
    q <- ggplot(data = toPlotDF,
                aes(x = .data[[x_var]],
                    y = .data[[y_var]],
                    color = .data[[fil_var]],
                    # text = paste0(x_var, ": ", .data[[x_var]],
                    #               "\n", y_var, ": ", .data[[y_var]],
                    #               "\n", compute_var, ": ", .data[[compute_var]],
                    #               "\n", fil_var, ": ", .data[[fil_var]])
                )) + #xlim(minX, maxX) + ylim(minY,maxY) +
      geom_point() 
     
    q = q + geom_contour(data = dfmm,
                   aes(x = Var1,
                       y = Var2,
                       z = value
                   ),
                   breaks = 0,
                   colour = "black")
    
    q = ggplotly(q, tooltip = c("text")) %>% layout(
      legend = list(
        orientation = 'h', x = 0.3, y = -0.2, 
        title = list(text = 'Legend: '),
        itemdoubleclick = TRUE
      )
    )
    
    q
    
  }) # end renderPlotly
  
  
  
  output$testData_confusionMatrix <- renderTable({
    req(lock_testData$done4)
    
    ## 012324
    # A class column was in fact selected for test data
    classesToFilter = unlist(rv$addClass$selectedClasses)

    print(paste0("Classes2Filter testDAta_confusionMAtrix:", classesToFilter))

    toComputeDF_allCols = as.data.frame(rv$filteredDF[
      # Subset rows
      rv$filteredDF[[rv$currSelClassCol]] %in% classesToFilter,
      # Subset columns
      c(unlist(as.character(rv$addAntigen$colSelect)), rv$currSelClassCol,
        # Subset additional columns
        rv$selectSampleIDCol, rv$selectMetadataCol
      )])

    toComputeDF = as.data.frame(rv$filteredDF[
      # Subset rows
      rv$filteredDF[[rv$currSelClassCol]] %in% classesToFilter,
      # Subset columns
      c(unlist(as.character(rv$addAntigen$colSelect))
      )])

    grvcurselcol <<- rv$currSelClassCol
    
    colAssign = toComputeDF_allCols[, rv$currSelClassCol]

    gq2A <<- q2A
    gtcdf <<- toComputeDF
    gtcdf_all <<- toComputeDF_allCols
    gtd <<- tstData
    x_var = colnames(toComputeDF)[1]
    y_var = colnames(toComputeDF)[2]
    z_var = NULL
    grvadd <<- rv$addClass
    allClasses = data.frame(className = rv$addClass$selectedClasses,
                            classRename = rv$addClass$colRenamed)

    gac <<- allClasses
    q2ComputedClasses = computeClass(q2A, toComputeDF, x_var, y_var, z_var, allClasses)
    
    q2ComputedClasses$trueClass = colAssign
    q2ComputedClasses = q2ComputedClasses
    gq2CC <<- q2ComputedClasses 
    
    # Get table sum stats for q2
    lstOfGroups = unique(q2ComputedClasses$allPredClass)
    if (NA %in% lstOfGroups){
      idx2replace = which(is.na(lstOfGroups))
      lstOfGroups[idx2replace] = "NA"
    }
    # Replace NAs with "NA"'s
    q2ComputedClasses[is.na(q2ComputedClasses$allPredClass), "allPredClass"] = "NA"

    numGroups = length(lstOfGroups)
    tNumBoth = nrow(q2ComputedClasses)
    qRes = data.frame(
      classNum = numeric(),
      className = character(),
      accuracy = numeric(),
      inaccuracy = numeric()
    )

    print(paste0("Colnames:", colnames(q2ComputedClasses)))
    for (i in 1:numGroups){
      currClass = lstOfGroups[i]
      
      
      totalInCurrGroup = nrow(q2ComputedClasses[q2ComputedClasses$allPredClass %in% currClass, ])
      allWhereMatched = nrow(q2ComputedClasses[q2ComputedClasses$allPredClass == currClass & 
                                                 q2ComputedClasses$trueClass == q2ComputedClasses$allPredClass, ])

      
      
      
      print(paste0("Total currgroup:", totalInCurrGroup))
      print(paste0("all where matched:", allWhereMatched))
      res1 = allWhereMatched/totalInCurrGroup
      res2 = 1 - res1

      # Set sigfigs
      res1 = format(round(res1, 4), nsmall = 4)
      res2 = format(round(res2, 4), nsmall = 4)

      # res1 = tst$sumTanh[i]
      # res2 = 1-res1

      if (i == 1){
        qRes[nrow(qRes)+1,] = c(
          i, currClass, res1, res2
        )
      } else {
        qRes[nrow(qRes)+1,] = c(
          i, currClass, res2, res1
        )
      }
      qRes= qRes
    }
    
    # gtstSumTanh <<- tst$sumTanh
    # gallRes <<- qRes
    
    # thisRes = data.frame()
    thisRes = qRes[, c("accuracy", "inaccuracy")]
    thisRes = setnames(thisRes, lstOfGroups)
    row.names(thisRes) = lstOfGroups
    # Save to reactive variable to export
    rv$evaluateResultsSumm = thisRes
    # Return what will be displayed
    thisRes
  }, rownames = TRUE)
  
}) # end observeEvent


# ==============================================================================
# 
#                       2) DATA IMPORT & APPLY SETTINGS
#
# ==============================================================================
# ------------------------------------------------------------------------------
# 2a) Apply row and column filters
# ------------------------------------------------------------------------------
# This is the first helper called in a series of functions that respond to the
# "Submit" button press. This function extracts the antigen columns selected by
# the user, converts any non-numeric antigen columns to numeric and NA removal,
# and applies row selections if Option B was selected
testData_applyDataImportSettings <- function(){
  # If non-numeric columns selected for antigens, resolve here 
  tdf = rv$testData_originalDF
  # names of the columns for the user's selected antigens
  allAntigenCols = unlist(as.character(rv$testData_addAntigen$colSelect))
  # datatype in the antigen columns selected
  allColClasses = sapply(tdf, class)
  # all column names
  allColNames = colnames(tdf)
  
  # Make sure this works without any classes
  if (rv$testData_selectClassCol == "None"){
    tdf = tdf[, allAntigenCols]
  }
  
  
  # Go through each column and assign as numeric if needed
  for (i in 1:ncol(tdf)){
    # Get info about column i
    currCol = tdf[, i] 
    currColName = allColNames[i]
    # currClass = sapply(currCol, class)
    currClass = allColClasses[i]
    
    # If the column is not numeric and is an antigen column
    if (currClass != "numeric" && currColName %in% allAntigenCols){
      # Convert this column to numeric and unsave Nans
      convertAntigen = as.numeric(tdf[[currColName]])
      # Overwrite column with numeric
      tdf[[currColName]] = convertAntigen
    }
  }
  
  
  # Save new rv$df only if NA's exist after processing above for loop
  udf = tdf[rowSums(is.na(tdf[, allAntigenCols]))==0, ]
  
  rv$testData_filteredDF = udf
  
  all_cdf = data.frame()
  
  ## 090723 apply row filter
  if (!isOptionA("test")){
    print(paste0("OPTION B NOT TESTED FOR TEST DATA"))
    ### this should be disabled
    # colForClasses = rv$selectClassCol
    # Get number of classes added by user
    tnumclass = dim(rv$testData_addClass)[1]
    # For each class assigned by user
    for (i in 1:tnumclass){
      # Get the UI metadata for the current class
      currRVClass = rv$testData_addClass[i, ]
      # NOTE - Class names for Option B are encoded as row numbers
      # Example label of a class ie. "1:5, 10:12" for rows 1-5 and 10-12. 
      rowsToSubset = currRVClass$selectedClasses
      # The new class name, if provided
      colRenamed = currRVClass$colRenamed
      # Class number
      currClassNum = currRVClass$classNum
      
      # Check if column name valid, if not create one
      if (!isTruthy(colRenamed)){
        colRenamed = paste0("Class ", currClassNum)
      }
      # Get row numbers from the class 
      numList = interpretSeqList(rowsToSubset)
      # Filter the rows 
      cdf = udf[numList, ]
      
      ## 092223 Need to overwrite the cdf class column for this section
      optionBLabels = rep(colRenamed, length(numList))
      cdf = cbind(
        udf[numList, ],
        optionBLabels
      )
      
      rv$testData_selectClassCol = "optionBLabels"
      # Aggregate the results
      all_cdf = rbind(all_cdf, cdf)
    }
    
    ## 090823 MAY NEED NA CHECK SINCE SKIPPED ABOVE
    ### MAY NEED TO CHECK FILTER BOX EFFECT ON TABLE INDEX
    ## 092223 removed since adding extra column for new class name
    # setNames(all_cdf, colnames(udf))
    rv$testData_filteredDF = all_cdf
  }
  
  # Lock reactive variables
  lock_testData$done1 = TRUE
}
