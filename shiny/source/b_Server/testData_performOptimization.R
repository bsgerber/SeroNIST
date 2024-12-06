
observeEvent(input$testData_evalTabSubmitBtn, {
  req(lock_analysis$done4)
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
      optim_out = parameterizedOptim(A, errorEstimate_forOptim, positives, negatives,
                                     currSig, 0.5)
      # Update the parameters for the next iteration in the for loop
      Aimin1 = A
      A = optim_out$par
      objVal = optim_out$value
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
  
  # Get class assignments only
  classCol_tst = rv$testData_filteredDF[, rv$testData_selectClassCol]
  # Option B, classes already filtered by row select
  currData = as.data.frame(
    rv$testData_filteredDF[, c(allAntigenCols_tst, rv$testData_selectClassCol)])
  fil_var <- paste0(colnames(classCol_tst))
  
  x_var <- y_var <- z_var <- NULL
  # Get colnames of antigens and classes (fil_var)
  if (rv$testData_numDimensions == 1){
    x_var <- paste0(colnames(currData)[1])
    y_var <- NULL
    z_Var = NULL
  } else if (rv$testData_numDimensions == 2){
    x_var <- paste0(colnames(currData)[1])
    y_var <- paste0(colnames(currData)[2])
    z_var = NULL
  } else if (rv$testData_numDimensions == 3){
    x_var <- paste0(colnames(currData)[1])
    y_var <- paste0(colnames(currData)[2])
    z_var = paste0(colnames(currData)[3])
  }
  currData_noNeg = currData
  testData_filtered = currData_noNeg
  # # Omit missing data
  # currData_omitNA = na.omit(currData)
  # # Redundant, but do this to prevent reuse of variable names
  # currData_noNeg = currData_omitNA

    # if (isOptionA("test")){
  #   
  #   if (!isTruthy(rv$testData_selectClassCol)){
  #     print(paste0("030724__________________10______________________030724"))
  #     
  #     # Option A, extract positive and negative classes
  #     testData_filtered = currData_noNeg[, allAntigenCols_tst]
  #   } else {
  #   # Option A, extract positive and negative classes
  #   testData_filtered = currData_noNeg[
  #     currData_noNeg[[fil_var]] %in% classesToFilter_tst, 
  #     c(allAntigenCols_tst, rv$testData_selectClassCol)]
  #   }
  # } else {
  #   print(paste0("030724__________________11______________________030724"))
  #   
  #   # Option B, classes were manually selected using row select
  #   # Get class assignments selected by user
  #   thisClassCol = currData[[rv$testData_selectClassCol]]
  #   
  #   # Get user selected class info for positive class
  #   currRVClass = rv$testData_addClass[1, ]
  #   # Get the "class" identity
  #   # in this case Option B saves class as comma delimited sequences 
  #   # ie) class 1 = "row:seq1, row:seq2"
  #   rowsToSubset = currRVClass$selectedClasses
  #   # Take the above string and create numeric sequence from it
  #   posNumList = interpretSeqList(rowsToSubset)
  #   # Get the minimum from this list
  #   minPosNum = min(posNumList)
  #   # Correct index starting at 1
  #   if (minPosNum != 1){
  #     posNumList = posNumList - minPosNum + 1
  #   }
  #   
  #   maxPosNum = max(posNumList)
  #   
  #   # Option B, get classes 
  #   testData_filtered = currData_noNeg[posNumList, allAntigenCols_tst]
  # }
  
  tst$data = testData_filtered
  # Project test data using model made from q=0.5 on training data ___________________________
  
  # i) Get min for training data _________________________________________
  train_df = rv$filteredDF
  train_allAntigenCols = unlist(as.character(rv$addAntigen$colSelect))
  allMin = min(train_df[, train_allAntigenCols])
  # Repeat for test data
  df = tst$data
  allAntigenCols = unlist(as.character(rv$testData_addAntigen$colSelect))
  testMin = min(df[, allAntigenCols])
  
  # If log
  if (input$rawOrLogData == rui$rawOrLog[2]){
    tmp_tstData = tst$data
    # data frame issue for 1d resolved here
    if (rv$testData_numDimensions == 1){
      tstData_lgTransform = logTransformation_2dArray_testMin(
        data.frame(df[, allAntigenCols]), allMin, testMin)
      
      setnames(tstData_lgTransform, allAntigenCols)
    } else {
      tstData_lgTransform = logTransformation_2dArray_testMin(
        df[, allAntigenCols], allMin, testMin)
    }
    
    # Save to reactive value
    tst$lgData = tstData_lgTransform
    tstData = tst$lgData
  } else {
    tstData = tst$data
  }
  
  allClasses = data.frame(className = rv$addClass$selectedClasses,
                          classRename = rv$addClass$colRenamed)
  
  computedClasses = computeClass(finalA, tstData, x_var, y_var, z_var, allClasses)
  
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
  
  # Get all class information for positive and negative
  train_allClasses = data.frame(className = rv$addClass$selectedClasses,
                                classRename = rv$addClass$colRenamed)
  posClasses = computeClass(finalA, positives, 
                            px_var, py_var, pz_var, train_allClasses)
  negClasses = computeClass(finalA, negatives, 
                            nx_var, ny_var, nz_var, train_allClasses)
  
  # Find Qd
  Qd = mapPredictedClass_toQD(computedClasses)
  Pd = mapPredictedClass_toQD(posClasses)
  Md = mapPredictedClass_toQD(negClasses)
  q1 = (Qd - Md)/(Pd - Md)
  
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
      q1AllIterVals = appendA2DF(q1AllIterVals, uID, i,
                                 objVal, currSig, Aimin1, thisAlen)
      
    } # end loop
  }) # end withProgress
  # Save all optimized models to reactive variable
  tst$q1AllIterVals = q1AllIterVals
  
  # View final model result for test data
  currIdx = 7
  q1A = q1AllIterVals[which(q1AllIterVals$idx == currIdx), "A"]
  # ____________________________________________________________________________
  # STEP 2)  Find final prevalence estimate q2
  q1ComputedClasses = computeClass(q1A, tstData, x_var, y_var, z_var, allClasses)
  q1PosClasses = computeClass(q1A, positives, px_var, py_var, pz_var, train_allClasses)
  q1NegClasses = computeClass(q1A, negatives, nx_var, ny_var, nz_var, train_allClasses)
  # Find Qd
  q1_Qd = mapPredictedClass_toQD(q1ComputedClasses)
  q1_Pd = mapPredictedClass_toQD(q1PosClasses)
  q1_Md = mapPredictedClass_toQD(q1NegClasses)
  q2 = (q1_Qd - q1_Md)/(q1_Pd - q1_Md)
  # ____________________________________________________________________________
  # STEP 3-4) Get A of q2 and use it to classify test data
  uID = 1
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

      if (i == length(sigvals)){
        lock_testData$done5 = TRUE
      }
      currSig = sigvals[i]
      optim_out = parameterizedOptim(A, errorEstimate_forOptim, 
                                     positives, negatives, currSig, q2)
      # Update the parameters for the next iteration in the for loop
      Aimin1 = A
      A = optim_out$par
      objVal = optim_out$value
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
    tst$classifiedDataTable = q2ComputedClasses
    
    lock_testData$done4 = TRUE
  })
  
  output$testData_optimizedEvalResultsSum <- renderTable({
    lstOfGroups = unique(q2ComputedClasses$allPredClass)
    
    lstOfGroups_training = rv$addClass$selectedClasses
    
    if (lstOfGroups[1] != lstOfGroups_training[1]){
      temp1 = lstOfGroups[1]
      temp2 = lstOfGroups[2]
      
      lstOfGroups[1] = temp2
      lstOfGroups[2] = temp1
    }
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
    thisFNR = op$bu_fnr
    thisFPR = op$bu_fpr
    
    rhoMax = as.numeric(max(thisFNR, thisFPR))
    S = nrow(tst$data)
    val = sqrt(((rhoMax*(1-rhoMax))/(S*(1-(2*rhoMax))^2)) + ((q2*(1-q2))/S))
    
    HTML(paste0("Prevalence Uncertainty: ", val))
  })
  
  plot_anyData(ID ="testData_plotDataWithOptimBoundary", 
               TabID = "testData_initialImportPlotTabs", 
               Tabs = 3,  Dim = rv$testData_numDimensions, 
               Transform = "Raw", Stage = "Test", 
               Bound = "Yes", Precision = "Optimized", 
               Select = "Column", Disp = "Subset",
               Session = session)
  
  output$testData_confusionMatrix <- renderUI({
    output = div(
      fluidRow(
        div(
          p("True Class"), 
          style = "margin-left: 175px; width: 200px"
        ) # end div 
      ), 
      fluidRow(
        column(3, 
               div(
                 p("Assigned Class"),
                 # style = "transform: rotate(270deg);"
                 style = "transform: rotate(270deg); width: 200px; margin-left: -100px"
               )
        ), # end column
        column(9, 
               div(
                 tableOutput("testData_confusionMatrix_table"),
                 style = "margin-left: -120px"
               ) # end div
        )
      )
    )
  })
  
  output$testData_confusionMatrix_table <- renderTable({
    req(lock_testData$done4)
    
    
    classesToFilter = getClassesToFilter(rv$addClass)
    gggood <<- classesToFilter
    
    # rv$selectClassCol = "classColumn"
    # rv$currSelClassCol = "classColumn"
    # 
    # toComputeDF_allCols = as.data.frame(rv$filteredDF[
    #   # Subset rows
    #   rv$filteredDF[[rv$currSelClassCol]] %in% classesToFilter,
    #   # Subset columns
    #   c(unlist(as.character(rv$addAntigen$colSelect)), rv$currSelClassCol,
    #     # Subset additional columns
    #     rv$selectSampleIDCol, rv$selectMetadataCol
    #   )])
    # 
    # 
    toComputeDF = as.data.frame(rv$filteredDF[
      # Subset rows
      rv$filteredDF[[rv$currSelClassCol]] %in% classesToFilter,
      # Subset columns
      c(unlist(as.character(rv$addAntigen$colSelect))
      )])

    # Log transform
    # if (input$rawOrLogData == rui$rawOrLog[2]){
    #   allMin = min(toComputeDF)
    #   toComputeDF = logTransformation_2dArray_allMin(
    #     toComputeDF, allMin)
    #   # Save to reactive value
    #   tst$lgData = tstData_lgTransform
    #   tstData = tst$lgData
    # } else {
    #   tstData = tst$data
    # }
    # 
    # 
    # colAssign = toComputeDF_allCols[, rv$currSelClassCol]
    # 
    # 
    # if (rv$testData_numDimensions == 3){
    #   x_var = colnames(toComputeDF)[1]
    #   y_var = NULL
    #   z_var = NULL
    # } else if (rv$testData_numDimensions == 2){
    #   x_var = colnames(toComputeDF)[1]
    #   y_var = colnames(toComputeDF)[2]
    #   z_var = NULL
    # } else if (rv$testData_numDimensions == 1){
    #   x_var = colnames(toComputeDF)[1]
    #   y_var = colnames(toComputeDF)[2]
    #   z_var = colnames(toComputeDF)[3]
    # }
    # 
    # allClasses = data.frame(className = rv$addClass$selectedClasses,
    #                         classRename = rv$addClass$colRenamed)
    # 
    # q2ComputedClasses = computeClass(q2A, toComputeDF, x_var, y_var, z_var, allClasses)
    # q2ComputedClasses$trueClass = colAssign
    # q2ComputedClasses = q2ComputedClasses
    # 
    # # Get table sum stats for q2
    lstOfGroups = classesToFilter
    # if (NA %in% lstOfGroups){
    #   print(paste0("SOME NA's ARE BEING REPLACED -----!!!!!"))
    #   idx2replace = which(is.na(lstOfGroups))
    #   lstOfGroups[idx2replace] = "NA"
    # }
    # 
    # trueGroups = unique(q2ComputedClasses$trueClass)
    # if (length(lstOfGroups) != length(trueGroups)){
    #   lstOfGroups = trueGroups
    # }
    # 
    # # Replace NAs with "NA"'s
    # q2ComputedClasses[is.na(q2ComputedClasses$allPredClass), "allPredClass"] = "NA"
    # 
    numGroups = length(lstOfGroups)
    # tNumBoth = nrow(q2ComputedClasses)
    qRes = data.frame(
      classNum = numeric(),
      className = character(),
      accuracy = numeric(),
      inaccuracy = numeric()
    )
    # 
    # print(paste0("LSTOFGROUPS:", lstOfGroups))
    # print(paste0("trueGroups:", trueGroups))
    # n1 <<- numGroups
    # n2 <<- tNumBoth
    # n3 <<- q2ComputedClasses
    # n4 <<- lstOfGroups
    # 
    # 
    for (i in 1:numGroups){
      currClass = lstOfGroups[i]
      # totalInCurrGroup = nrow(q2ComputedClasses[q2ComputedClasses$allPredClass %in% currClass, ])
      # allWhereMatched = nrow(q2ComputedClasses[q2ComputedClasses$allPredClass == currClass & 
      #                                            q2ComputedClasses$trueClass == q2ComputedClasses$allPredClass, ])
      # 
      # print(paste0("TEST DATA VERIFY CLASS i [", i, "]: "))
      # 
      # print(paste0("ALLWHERE MATCHED [", i, "]:", allWhereMatched))
      # print(paste0("TOTALINCURRGROUP [", i, "]:", totalInCurrGroup))
      # print(paste0("CURRCLASS [", i, "]:", currClass))
      # 
      # res1 = allWhereMatched/totalInCurrGroup
      # res2 = 1 - res1
      # 
      # print(paste0("RES [", i, "]:", res1, ", res2:", res2))
      
      if (i == 1){
        res2 = tst$falseNeg
        
      } else {
        res2 = tst$falsePos
      }
      res1 = 1 - res2
      
      # Set sigfigs
      res1 = format(round(res1, 4), nsmall = 4)
      res2 = format(round(res2, 4), nsmall = 4)
      
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
    
    thisRes = qRes[, c("accuracy", "inaccuracy")]
    thisRes = setnames(thisRes, lstOfGroups)
    row.names(thisRes) = lstOfGroups
    # Save to reactive variable to export
    rv$evaluateResultsSumm = thisRes
    # Return what will be displayed
    t(thisRes)
    
  }, rownames = TRUE)
  
  
  output$testDataTableTabs <- renderTestDataTableTabs(4, session)
  
  output$testtest <- DT::renderDataTable({
    req(lock_testData$done4 && isTruthy(tst$classifiedDataTable))
    
    df = tst$classifiedDataTable
    df
    
  },
  escape = TRUE,
  filter = "top", # search box
  selection = "multiple",
  extensions = c("FixedColumns"),
  server = FALSE, # disable this for numeric sort
  options = list(
    paging = FALSE,
    scrollX = TRUE,
    scrollY = "500px",
    autoWidth = TRUE,
    fixedHeader = T,
    fixedColumns = list(leftColumns = 1)
    # columnDefs = list(
    #   list(width = '100px', targets = 2:ncol(df) - 1)
    # )
  ) # end option list
  )
  
}) # end observeEvent