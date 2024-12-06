# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                           Program Header Status  
#                             IN PROGRESS 092123
#                   Header: 🗸 Comments: 🗸-  Refactored: 🗸-      
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ==============================================================================
# 
# TITLE: trainingModel_setInitialConditions.R
#
#                               DESCRIPTION
# This program contains functions that are primarily called by the program 
# called dataImport_submitImport.R to set initial conditions used for training 
# the model over the imported data.  
# 
#                             TABLE OF CONTENTS
#
#       1) EVALUATE BUTTON BEHAVIOR 
#
#       2) COMPUTE INITIAL CONDITIONS
#               2a) Transform and Check Data
#               2b) Generate classification boundary
#               2c) Get error estimate
#
#       3) PLOT INITIAL BOUNDARY
#
#       4) SUMMARY STATS TABLE
#
# ==============================================================================

trainEval_initReqs <- function(){
  # antigens and classes must be present
  req(isTruthy(rv$addAntigen) && isTruthy(rv$addClass) && 
        # dim(rv$addAntigen)[1] > 1 &&
        # At least two classes must be present
        dim(rv$addClass)[1] > 1 &&
        # submitImport tasks must be complete
        lock_import$done3)
  
  # Clear reactive variables from previous file upload 
  # lock_testData$done1 = FALSE
  lock_testData$done3 = FALSE
  # rv$testDataFileType = NULL
  # rv$testDataFileUploadPath = NULL
  # rv$testData_numDimensions = 0
  # rv$testData_sheetCount = NULL
  # Clear outputs with every new file upload
  # output$testDataAcquisitionTabs = NULL
  # output$testDataInitialImportPlotTabs = NULL
  output$testDataTrainingTabs = NULL
  
  if (lock_testData$done1){
    output$testData_initialImportPlotTabs <- renderTestDataPlotTabs(2, session)
  } 
  
  # Clear outputs that render as a result of file upload
  # output$testData_separatingLine <- renderUI({NULL})
  # output$testData_optionalSettingsTitle <- renderUI({NULL})
  # output$testData_selectSampleIDCol <- renderUI({NULL})
  # output$testData_selectMetadataCols <- renderUI({NULL})
  # output$testData_doneImportBtn <- renderUI({NULL})
  # output$testData_evalTabOptimBtn <- renderUI({NULL})
  
  # # Clear previous class assignments
  # if (isTruthy(rv$testData_addClass)){
  #   testData_clearAddedClasses()
  # }
}

# ==============================================================================
# 
#                        1) EVALUATE BUTTON BEHAVIOR
#
# ==============================================================================
observeEvent(input$evalTabSubmitBtn, {
  
  rv$addClass = rv$newAddClass
  
  disable("evalTabSubmitBtn")
  # This function checks requirements and initializes variables 
  trainEval_initReqs()
  withProgress(message = "Training initial model: ", value = 0, {
    # [lock_analysis$done1] --> rv$outmat, op$outmat, op$L
    computeInitialConditions()
    
    incProgress(1/10, detail = "Plotting initial boundary")
    
    currChoice = rv$rawOrLog
    # Raw data
    if (currChoice == rui$rawOrLog[1]){
      plot_anyData(ID ="plotDataWithBoundary", 
                   TabID = "initialImportPlotTabs", 
                   Tabs = 3,  Dim = rv$numDimensions, 
                   Transform = "Raw", Stage = "Train", 
                   Bound = "Yes", Precision = "Guess", 
                   Select = "Column", Disp = "Subset",
                   Session = session)
    } else {
      # Log data
      plot_anyData(ID ="plotDataWithBoundary", 
                   TabID = "initialImportPlotTabs", 
                   Tabs = 3,  Dim = rv$numDimensions, 
                   Transform = "Log", Stage = "Train", 
                   Bound = "Yes", Precision = "Guess", 
                   Select = "Column", Disp = "Subset",
                   Session = session)
    }
    
    incProgress(1/10, detail = "Displaying confusion matrix")
    
    getTableSummaryStats("init")
    
    incProgress(1/10, detail = "Computing bound uncertainty")
    
    # Get confusion matrix for bound uncertainty
    computeBoundUncertainty()
    
    incProgress(1/2, detail = "Displaying bound confusion matrix")
    
    getTableSummaryStats("bu")
    
    incProgress(1/10, detail = "Rendering additional tools")
    
    output$dataTrainingTabs <- renderEvaluatorTables()
    
    output$evalTabOptimBtn <- renderUI({
      actionButton("evalTabOptimBtn",
                   "Optimize Classifier for Training Data",
                   width = "500px")
    })
  }) # end withProgress
  
  enable("evalTabSubmitBtn")
  
}) # end evaltabsubmitbttn

# ==============================================================================
# 
#                         2) COMPUTE INITIAL CONDITIONS
#
# ==============================================================================
# This function takes the positive and negative class and applies a log 
# transformation to compute an initial classification boundary & error estimate. 
computeInitialConditions <- function(){
  req(lock_import$done2)
  # ----------------------------------------------------------------------------
  # 2a) Transform and Check Data
  # ----------------------------------------------------------------------------
  # Extract latest data
  positives = rv$positives
  negatives = rv$negatives
  allAntigenCols = unlist(as.character(rv$addAntigen$colSelect))
  allMin = min(rbind(negatives, positives))
  
  # If log transform selected, perform transform here
  if (input$rawOrLogData == rui$rawOrLog[2]){
    if (rv$numDimensions == 1){
      positives = data.frame(positives)
      negatives = data.frame(negatives)
      setnames(positives, allAntigenCols[1])
      setnames(negatives, allAntigenCols[1])
    }
    
    ### Do log transformation over data
    positives = logTransformation_2dArray_allMin(positives, allMin)
    negatives = logTransformation_2dArray_allMin(negatives, allMin)
    
    
    # # Remove infinite values resulting from log transform
    if (rv$numDimensions == 1){
      setnames(positives, allAntigenCols[1])
      setnames(negatives, allAntigenCols[1])
    }
    # Save to reactive value
    rv$lgPositives = positives
    rv$lgNegatives = negatives
  } 
  # Proceed to another helper function to generate the model  
  generateClassificationModel(positives, negatives)
  
  lock_analysis$done1 = TRUE
}

computeBoundUncertainty <- function(){
  req(lock_analysis$done2 && lock_analysis$done6)
  # Get values from previous computations
  sigvals = op$bu_sigval
  # currSig = op$bu_sigval
  thisHP = op$bu_HP
  outmat = op$bu_outmat
  positives = op$bu_pos
  negatives = op$bu_neg
  thisQ = op$bu_q
  thisFNR = as.numeric(op$bu_fnr)
  thisFPR = as.numeric(op$bu_fpr)
  
  thisAlen = length(outmat)
  
  allIterVals = data.frame(
    uID = numeric(),
    idx = numeric(),
    L = numeric(),
    sigma = numeric(),
    A = numeric()
  )
  
  A = outmat
  
  rv$numLen = length(as.vector(outmat))
  
  for (i in 1:length(sigvals)){
    uID = 1
    currSig = sigvals[i]
    ## 120523 Get bound uncertainties in assay
    buOptim_out = parameterizedOptimBU(A, boundUncertainties_optim, 
                                       positives, negatives,
                                       currSig, thisQ, thisHP, thisFNR, thisFPR)
    # Update the parameters for the next iteration in the for loop
    Aimin1 = A
    A = buOptim_out$par
    objVal = buOptim_out$value
    
    #### Append the results using a helper function
    allIterVals = appendA2DF(allIterVals, uID, i,
                             objVal, currSig, Aimin1, thisAlen)
    
  }
  
  # Get this matrix
  thisA = buOptim_out$par
  rv$buOptimA = thisA
}

# ------------------------------------------------------------------------------
# 2b) Generate classification boundary
# ------------------------------------------------------------------------------
# This function takes the two classes to compute a boundary and then calls a
# second helper function to get an initial error estimate. 
generateClassificationModel <- function(positives, negatives){
  
  # Get number of columns (numAntigens)
  numdim = ncol(positives)
  # Get mean of positive and negative data over dimension '2' (columns)
  mp = apply(positives, 2, mean)
  mn = apply(negatives, 2, mean)
  # Get covariance matrix of both classes
  pcov = cov(positives)
  ncov = cov(negatives)
  # Get eigen computation
  peig = eigen(pcov)
  neig = eigen(ncov)
  # Get eigenvectors and values
  pvv = peig$vectors
  pdd = peig$values
  nvv = neig$vectors
  ndd = neig$values
  
  # Update 
  nu = mp - mn
  
  for (ii in 1:length(pdd)){
    if (pdd[ii] < 0){
      pdd[ii] = 0
    }
  }
  
  for (ii in 1:length(ndd)){
    if (ndd[ii] < 0){
      ndd[ii] = 0
    }
  }
  
  ndd = sqrt(ndd)
  pdd = sqrt(pdd)
  
  # Set initial weight values
  wx = 0
  wy = 0
  
  # About to calculate wx and wy 
  for (i in 1:numdim){
    wx = wx + abs(t(nu)%*%(nvv[, i]*ndd[i]))
    wy = wy + abs(t(nu)%*%(pvv[, i]*pdd[i]))
  }
  
  # Get rid of warnings
  wx = as.numeric(wx)
  wy = as.numeric(wy)
  nu = as.numeric(nu)
  mn = as.numeric(mn)
  mu = mn + nu*wx/(wx+wy)
  
  bvec = nu
  c = t(-bvec)%*%mu
  
  # Get sigval information
  nulen = sqrt(sum(nu^2))
  decades = 10^(-(0:6))
  sigvals = nulen*decades
  # sigvals = nulen*decades*100
  
  # Create a matrix of zeros
  tmat = matrix(0, numdim, numdim)
  
  outmat = cbind(
    rbind(
      tmat,
      bvec/2
    ), # end row bind
    c(t(bvec)/2, c)
  ) # end column bind
  
  
  # Update reactive variables
  rv$outmat = outmat
  rv$currOutmat = rv$outmat
  op$outmat = outmat
  op$sigvals = sigvals
  
  # Project the points using the new model
  pts = projectPoints(outmat, positives)
  npts = projectPoints(outmat, negatives)
  
  np = length(pts)
  nn = length(npts)
  q = np/(np+nn)
  
  # Get an initial error estimate associated with this model
  L = getErrorEstimate(pts, npts, -1, sigvals[1], outmat, positives, negatives)
  # Save to reactive value
  op$initialL = L
  
  currSig = sigvals[1]
  thisHP = input$bu_hyperparameter
  
  op$bu_sigval = sigvals
  op$bu_HP = thisHP
  op$bu_outmat = outmat
  op$bu_pos = positives
  op$bu_neg = negatives
  op$bu_q = 0.5
  
  op$bu_N = 2
  op$bu_S = np+nn
  op$bu_qj = q
  op$bu_qj2 = (nn/(np+nn))
  
  # free lock to continue
  lock_analysis$done2 = TRUE
  
}




# ------------------------------------------------------------------------------
# 1c) Function Param for Optim
# ------------------------------------------------------------------------------
# This outer shell function is called during the optimization and calls on the
# original getErrorEstimate() function that is actually being optimized. The 
# reason for this shell function is to project the points on the initial model.
errorEstimate_forOptim <- function(par, poss, negs, sigval, q){
  numCol = sqrt(length(as.vector(par)))
  outmat = matrix(par, ncol = numCol)
  
  pos_proj = projectPoints(outmat, poss)
  neg_proj = projectPoints(outmat, negs)
  
  L = getErrorEstimate(pos_proj, neg_proj, q, sigval, outmat, poss, negs)
  return(L)
}

# ------------------------------------------------------------------------------
# 2c) Get error estimate
# ------------------------------------------------------------------------------
# This helper function uses the outmat projected points 
# getErrorEstimate <- function(pts, npts, sigval, outmat, positives, negatives){
## added new param 11/3/23
getErrorEstimate <- function(pts, npts, q, sigval, 
                             outmat, positives, negatives){
  
  np = length(pts)
  nn = length(npts)
  if (q < 0){
    q = np/(np+nn)
  } 
  
  
  pos_tanh = 1- tanh_1dArray(pts, sigval)
  neg_tanh = tanh_1dArray(npts, sigval)
  
  sumPosTanh = sum(pos_tanh)
  sumNegTanh = sum(neg_tanh)
  
  # Compute Lr _________________________________________________________________
  outmat_vec = as.vector(outmat)
  outmat_ncol = sqrt(length(outmat_vec)) - 1
  Lr = 0
  for (i in 0:outmat_ncol){
    for (j in 0:outmat_ncol){
      t1 = outmat_vec[arrIDX2vecIDX(i,j,outmat_ncol + 1)]
      t2 = outmat_vec[arrIDX2vecIDX(j,i,outmat_ncol + 1)]
      diff = (t1-t2)
      Lr = Lr + diff^2
    }
  }
  # Lscale _____________________________________________________________________
  posregularize = sum((projectPoints(outmat, positives)^2)/np)
  negregularize = sum((projectPoints(outmat, negatives)^2)/nn)
  Lscale = (posregularize + negregularize - 1)^2
  
  if (lock_testData$done5){
    tst$qs = c(q, 1-q)
    tst$ns = c(np, nn)
    tst$sumTanh = c(sumPosTanh, sumNegTanh)
    tst$falseNeg = sumPosTanh/np
    tst$falsePos = sumNegTanh/nn
    
    print(paste0("tst$q:", q))
    print(paste0("tst$1-q:", 1-q))
    print(paste0("tst$np:", np))
    print(paste0("tst$nn:", nn))
    print(paste0("tst$sumPosTan:", sumPosTanh))
    print(paste0("tst$sumNegTan:", sumNegTanh))
    
    
    gt1 <<- q
    gt2 <<- (1-q)
    gt3 <<- np
    gt4 <<- nn 
    gt4_1 <<- tanh_1dArray(pts, sigval)
    gt4_1_1 <<- pos_tanh
    gt4_2 <<- neg_tanh
    gt5 <<- sumPosTanh
    gt6 <<- sumNegTanh
    
    
  }
  
  
  sumProd = ((q/np)*sumPosTanh) + (((1-q)/nn)*sumNegTanh) + Lscale + Lr
  
  return(sumProd)
}



# ==============================================================================
# ------------------------------------------------------------------------------
# 1c) BOUND UNCERTAINTIES IN ASSAY 120523
# ------------------------------------------------------------------------------


boundUncertainties_optim <- function(par, poss, negs, sigval, q, hp, fnr, fpr){
  numCol = sqrt(length(as.vector(par)))
  outmat = matrix(par, ncol = numCol)
  
  pos_proj = projectPoints(outmat, poss)
  neg_proj = projectPoints(outmat, negs)
  
  L = getErrorEstimate_boundUncertainty(pos_proj, neg_proj, q, sigval, 
                                        outmat, poss, negs, hp, fnr, fpr)
  return(L)
}



# ------------------------------------------------------------------------------
# 

getErrorEstimate_boundUncertainty <- function(pts, npts, q, sigval, outmat, 
                                              positives, negatives, hp,
                                              fnr, fpr){
  
  np = length(pts)
  nn = length(npts)
  if (q < 0){
    q = np/(np+nn)
  } 
  
  
  pos_tanh = 1- tanh_1dArray(pts, sigval)
  neg_tanh = tanh_1dArray(npts, sigval)
  
  sumPosTanh = sum(pos_tanh)
  sumNegTanh = sum(neg_tanh)
  
  outmat_vec = as.vector(outmat)
  outmat_ncol = sqrt(length(outmat_vec)) - 1
  Lr = 0
  for (i in 0:outmat_ncol){
    for (j in 0:outmat_ncol){
      t1 = outmat_vec[arrIDX2vecIDX(i,j,outmat_ncol + 1)]
      t2 = outmat_vec[arrIDX2vecIDX(j,i,outmat_ncol + 1)]
      diff = (t1-t2)
      Lr = Lr + diff^2
    }
  }
  
  # Lscale
  posregularize = sum((projectPoints(outmat, positives)^2)/np)
  negregularize = sum((projectPoints(outmat, negatives)^2)/nn)
  Lscale = (posregularize + negregularize - 1)^2
  
  # sumProd = ((q/np)*sumPosTanh) + (((1-q)/nn)*sumNegTanh) + #Lscale + Lr + 
  #   hp*((sumNegTanh/np) - sumPosTanh/nn)
  # sumProd = ((q/np)*sumPosTanh) + (((1-q)/nn)*sumNegTanh) + #Lscale + Lr + 
  #   hp*(fpr - fnr)
  
  sumProd = ((q/np)*sumPosTanh) + (((1-q)/nn)*sumNegTanh) + Lscale + Lr + 
    # hp*(fpr - fnr)
    hp*(sumNegTanh/nn - sumPosTanh/np)^2
  
  
  return(sumProd)
}


# ==============================================================================
# ==============================================================================
# 
#                          4) SUMMARY STATS TABLE
#
# ==============================================================================

getTableSummaryStats <- function(mode){
  ### NEED TO CHECK REQS FOR rv$selectClassCol ## 112723
  if (mode == "train"){
    req(lock_analysis$done4)
    req(lock_analysis$done5)
  } else if (mode == "bu"){
    req(lock_analysis$done2)
  }
  currChoice = rv$rawOrLog
  if (currChoice == rui$rawOrLog[1]){
    toTableDF = getRawData("raw")
    
  } else {
    toTableDF = getRawData("log")
  }
  
  # Get info about the columns for antigens and class assignments
  x_var <- paste0(colnames(toTableDF)[1])
  
  if (rv$numDimensions > 1){
    y_var <- paste0(colnames(toTableDF)[2])
    
  } else {
    y_var = NA
  }
  # Get class assignment column
  classCol = rv$filteredDF[, rv$selectClassCol]
  classColName = rv$selectClassCol
  # Get colname of class assignment 
  fil_var <- rv$selectClassCol
  
  # Get variables to pass into helper function
  allClasses = data.frame(className = rv$addClass$selectedClasses,
                          classRename = rv$addClass$colRenamed)
  if (mode == "bu"){
    outmat = rv$buOptimA
  } else {
    outmat = rv$currOutmat
    
  }
  
  if (rv$numDimensions == 3){
    z_var = paste0(colnames(toTableDF)[3])
    # Compute predicted class given outmat
    
    toTableDF_classified = computeClass(outmat, toTableDF, x_var, y_var, z_var,
                                        allClasses)
  } else if (rv$numDimensions == 2) {
    # Compute predicted class given outmat
    toTableDF_classified = computeClass(outmat, toTableDF, x_var, y_var, NULL,
                                        allClasses)
  } else if (rv$numDimensions == 1){
    # Compute predicted class given outmat
    toTableDF_classified = computeClass(outmat, toTableDF, x_var, NULL, NULL,
                                        allClasses)
  }
  
  lstOfGroups = unique(toTableDF_classified[, classColName])
  numGroups = length(lstOfGroups)
  tNumBoth = nrow(toTableDF_classified)
  allRes = data.frame(
    classNum = numeric(),
    className = character(),
    accuracy = numeric(),
    inaccuracy = numeric()
  )
  
  for (i in 1:numGroups){
    currClass = lstOfGroups[i]
    
    totalInCurrGroup = nrow(toTableDF_classified[
      toTableDF_classified[, classColName] %in% lstOfGroups[i], ])
    
    # Find number of rows where current class did match correctly to predicted
    allWhereMatched = nrow(toTableDF_classified[
      toTableDF_classified[, classColName] == lstOfGroups[i] &
        toTableDF_classified[, classColName] == toTableDF_classified$allPredClass, ])
    
    # Find number of rows where the current class did not match predicted
    whereNotMatched = nrow(toTableDF_classified[
      toTableDF_classified[, classColName] != lstOfGroups[i] &
        toTableDF_classified[, classColName] == toTableDF_classified$allPredClass, ])
    res1 = allWhereMatched/totalInCurrGroup
    res2 = 1 - res1
    
    # Set sigfigs
    res1 = format(round(res1, 4), nsmall = 4)
    res2 = format(round(res2, 4), nsmall = 4)
    
    if (i == 1){
      allRes[nrow(allRes)+1,] = c(
        i, currClass, res1, res2
      ) 
    } else {
      allRes[nrow(allRes)+1,] = c(
        i, currClass, res2, res1
      )  
    }
    
    allRes= allRes
  }
  
  if (mode == "init"){
    
    # 120723 Get bound uncertainty FPR FNR
    thisFNR = allRes[2, "accuracy"]
    thisFPR = allRes[1, "inaccuracy"]
    # thisFNR = format(round(thisFNR, 16), nsmall = 16)
    # thisFPR = format(round(thisFPR, 16), nsmall = 16)
    
    lock_analysis$done6 = TRUE
    
  } else if (mode == "train") {
    output$optimizedEvalResultsSum <- renderUI({
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
                   tableOutput("optimizedEvalResultsSum_table"),
                   style = "margin-left: -120px"
                 ) # end div
          )
        )
      )
    })
    
    # mode == "train"
    output$optimizedEvalResultsSum_table <- renderTable({
      thisRes = allRes[, c("accuracy", "inaccuracy")]
      thisRes = setnames(thisRes, lstOfGroups)
      row.names(thisRes) = lstOfGroups
      # Save to reactive variable to export
      rv$optimizedEvalResultsSum = thisRes
      # Return what will be displayed
      t(thisRes)
    }, rownames = TRUE)
    
  } else if (mode == "bu"){
    thisFNR = allRes[2, "accuracy"]
    thisFPR = allRes[1, "inaccuracy"]
    
    rhoMax = as.numeric(max(thisFNR, thisFPR))
    op$bu_fnr = thisFNR
    op$bu_fpr = thisFPR
    lock_analysis$done6 = TRUE
    
    N = as.numeric(op$bu_N)
    S = as.numeric(op$bu_S)
    qj = as.numeric(op$bu_qj)
    qj2 = as.numeric(op$bu_qj2)
    
    sigma = ((2*rhoMax - (N/(N-1))*(rhoMax^2))/(S*(1-rhoMax)^2)) + 
      (qj*(1-qj))/S + (qj2*(1-qj2))/S
    
    output$boundUncertaintyRhoMax <- renderUI({
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
                   tableOutput("boundUncertaintyRhoMax_table"),
                   style = "margin-left: -120px"
                 ) # end div
          )
        )
      )
    })
    output$boundUncertaintyRhoMax_table <- renderTable({
      thisRes = allRes[, c("accuracy", "inaccuracy")]
      thisRes = setnames(thisRes, lstOfGroups)
      row.names(thisRes) = lstOfGroups
      # Save to reactive variable to export
      rv$boundUncertaintyRhoMax = thisRes
      # Return what will be displayed
      t(thisRes)
      
    }, rownames = TRUE)
  }
}


getRawData <- function(mode){
  # classesToFilter = unlist(rv$addClass$selectedClasses)
  classesToFilter = getClassesToFilter(rv$addClass)
  
  toPlot = rv$filteredDF[
    rv$filteredDF[[rv$selectClassCol]] %in% classesToFilter,
    c(unlist(as.character(rv$addAntigen$colSelect)), rv$selectClassCol)]
  
  if (mode == "log"){
    # Get class assignments for each row
    classColumn = toPlot[, rv$selectClassCol]
    
    # Remove NA's
    toPlot = na.omit(toPlot)
    # Convert to data frame
    toPlotDF = as.data.frame(toPlot)
    
    # Get column metadata
    x_var <- paste0(colnames(toPlotDF)[1])
    y_var <- paste0(colnames(toPlotDF)[2])
    
    # Get metadata for class column
    fil_var = rv$selectClassCol
    # Remove NA's again?
    toPlotDF = na.omit(toPlotDF)
    
    # # If there are datapoints less than 0, set to 0 
    # toPlotDF[toPlotDF[[x_var]] < 0, x_var] = 0
    # toPlotDF[toPlotDF[[y_var]] < 0, y_var] = 0
    
    # If a 3D plot
    if (rv$numDimensions == 3){
      # Get third dimension information
      z_var = paste0(colnames(toPlotDF)[3])
      # Overwrite class column location
      fil_var = paste0(colnames(toPlotDF)[4])
      
      
      # Needed? 
      tPlotDF = toPlotDF
      
      # Take log transform of data
      negMin = min(tPlotDF[, 1])
      posMin = min(tPlotDF[, 2])
      zmin = min(tPlotDF[, 3])
      allMin = min(c(negMin, posMin, zmin))
      
      tlgPlotDF = logTransformation_2dArray_allMin(tPlotDF[, 1:3], allMin)
      lgPlotDF = cbind(tlgPlotDF, classColumn)
      
      toPlot = lgPlotDF
      
    } else if (rv$numDimensions == 2) {
      # Needed? 
      tPlotDF = toPlotDF
      
      # Take log transform of data
      negMin = min(tPlotDF[, 1])
      posMin = min(tPlotDF[, 2])
      allMin = min(c(negMin, posMin))
      
      tlgPlotDF = logTransformation_2dArray_allMin(tPlotDF[, 1:2], allMin)
      lgPlotDF = cbind(tlgPlotDF, classColumn)
      toPlot = lgPlotDF
      
    } else if (rv$numDimensions == 1){
      # Needed? 
      
      tPlotDF = toPlotDF
      tcolname = colnames(tPlotDF)[1]
      # Take log transform of data
      negMin = min(tPlotDF[, 1])
      allMin = negMin
      tlgPlotDF = logTransformation_2dArray_allMin(
        data.frame(tPlotDF[, 1]), allMin)
      setnames(tlgPlotDF, tcolname)
      lgPlotDF = cbind(tlgPlotDF, classColumn)
      
      toPlot = lgPlotDF
    } # end conditional block rv$numDimensions
  } # end mode == "log"
  
  
  # Convert to dataframe to manipulate below
  toPlotDF = as.data.frame(toPlot)
  
  return(toPlotDF)
}
