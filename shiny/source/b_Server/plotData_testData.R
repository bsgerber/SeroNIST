# ID: OutputId (String)
# TabID: OutputId (String)
# Tab: Numeric (#) 
# Dim: Numeric (#)
# Transform: Raw or Log
# Stage: Train or Test
# Bound: Yes or No
# Precision: Guess or Optimized
# Select: Column or Rows
# Disp: Subset or All
# ______________________________________________________________________________
plots_checkReqs <- function(ID){
  if (ID == "firstGraph" || ID == "logGraph"){
    req(isTruthy(rv$filteredDF_forAllPlot) && isTruthy(rv$addClass) 
        && isTruthy(rv$addAntigen) && lock_import$done2)
  } else if (ID == "plotDataWithBoundary"){
    req(isTruthy(rv$filteredDF_forAllPlot) && isTruthy(rv$addAntigen) &&
          isTruthy(rv$addClass) && dim(rv$addClass)[1] > 1 &&
          lock_import$done2 && lock_import$done3 && 
          lock_analysis$done2)
  } else if (ID == "plotDataWithOptimBoundary"){
    req(isTruthy(rv$addAntigen) && isTruthy(rv$addClass) && 
          dim(rv$addAntigen)[1] >= 1 && dim(rv$addClass)[1] > 1 &&
          lock_import$done3 && lock_analysis$done1 && lock_analysis$done4)
  } else if (ID == "testData_firstGraph" || ID == "testData_logGraph"){
    req(isTruthy(rv$testData_filteredDF_forAllPlot) && 
          isTruthy(rv$testData_addAntigen) && lock_testData$done2) 
    # && isTruthy(rv$testData_addClass) 
  } else if (ID == "testData_plotDataWithOptimBoundary"){
    req(lock_testData$done4)
  }
}
# ______________________________________________________________________________

plots_renderTabs<- function(TabID, Tabs, Stage, session){
  if (Stage == "Train"){
    output[[TabID]] <- renderImportAndTrainingTabs(Tabs, session)
  } else if (Stage == "Test"){
    output[[TabID]] <- renderTestDataPlotTabs(Tabs, session)
  }
}
# ______________________________________________________________________________

plots_getData <- function(Dim, Transform, Stage, Bound){
  if (Stage == "Train"){
      # Extract the user-selected data from the file upload
      toPlot = rv$filteredDF_forAllPlot[
        # Subset columns
        , c(unlist(as.character(rv$addAntigen$colSelect)),
            # rv$currSelClassCol,
            rv$selectClassCol,
            # Subset additional columns
            rv$selectSampleIDCol, rv$selectMetadataCol
        )]
  } else if (Stage == "Test"){
    if (Bound == "No"){
        # Class assignments are made to filter the data further
        toPlot = rv$testData_filteredDF_forAllPlot[
          # Col select
          , c(unlist(as.character(rv$testData_addAntigen$colSelect)), 
             rv$testData_selectClassCol,
             # Subset additional columns
             rv$testData_selectSampleIDCol, rv$testData_selectMetadataCols
          )]
      # }
    } else if (Bound == "Yes"){
      if (!isTruthy(rv$testData_selectClassCol) ||
          !isTruthy(rv$testData_addClass)){
        extraColumns = rv$testData_filteredDF_forAllPlot[, c(
          rv$testData_selectClassCol, rv$testData_selectSampleIDCol, 
          rv$testData_selectMetadataCols)]
        
        toPlot = cbind(tst$classifiedDataTable, extraColumns) 
      } else {
        classesToFilter = getClassesToFilter(rv$testData_addClass)
        extraColumns = rv$testData_filteredDF_forAllPlot[
          rv$testData_filteredDF_forAllPlot[[rv$testData_selectClassCol]] %in%
            classesToFilter,
          c(rv$testData_selectSampleIDCol, rv$testData_selectMetadataCols)]
        
        toPlot = cbind(tst$classifiedDataTable, extraColumns)
      }
      
      rv$testData_toExport = toPlot
    } # End Test vs. Train conditional
  }
  

  if (Transform == "Log"){
    toPlotDF = as.data.frame(toPlot)
    if (Stage == "Train"){
      optionBLabels = toPlot[, rv$selectClassCol]
      
      extraCols = toPlot[,  c(rv$selectSampleIDCol, rv$selectMetadataCol)]
    } else if (Stage == "Test" && isTruthy(rv$testData_selectClassCol)){
      optionBLabels = toPlot[, rv$testData_selectClassCol]
      extraCols = toPlot[,  c(rv$testData_selectSampleIDCol,
                              rv$testData_selectMetadataCols)]
      
    } else if (Stage == "Test" && !isTruthy(rv$testData_selectClassCol)){
      optionBLabels = NULL
      extraCols = toPlot[,  c(rv$testData_selectSampleIDCol,
                              rv$testData_selectMetadataCols)]
    }
    tPlotDF = toPlotDF
    
    negMin = min(tPlotDF[, 1])
    
    if (Dim == 1){
      allMin = negMin
      tcolname = colnames(tPlotDF)[1]
      # Do log transformation over data
      tlgPlotDF = logTransformation_2dArray_allMin(
        data.frame(tPlotDF[, 1]), allMin)
      
      setnames(tlgPlotDF, tcolname)
      # if plotting 2 antigens
    } else if (Dim == 2){
      posMin = min(tPlotDF[, 2])
      allMin = min(c(negMin, posMin))
      
      # Do log transformation over data
      tlgPlotDF = logTransformation_2dArray_allMin(tPlotDF[, 1:2], allMin)
    } else if (Dim == 3){
      posMin = min(tPlotDF[, 2])
      zmin = min(tPlotDF[, 3])
      allMin = min(c(negMin, posMin, zmin))
      # Do log transformation over data
      tlgPlotDF = logTransformation_2dArray_allMin(tPlotDF[, 1:3], allMin)
    }
    
    if (isTruthy(optionBLabels)){
      classColumn = optionBLabels
      lgPlotDF = cbind(tlgPlotDF, classColumn, extraCols)
    } else {
      lgPlotDF = cbind(tlgPlotDF, extraCols)
    }
    toPlot = lgPlotDF
  }
  return(toPlot)
}
# ______________________________________________________________________________

plots_getPlotVars <- function(Stage, toPlotDF, Dim, Bound){
  # Initialize return values
  x_var <- y_var <- z_var <- fil_var <- txt_var <- NA
  
  if (Stage == "Train"){
    allAg = data.frame(agName = rv$addAntigen$colSelect,
                       agRename = rv$addAntigen$colRenamed)
  } else if (Stage == "Test"){
    allAg = data.frame(agName = rv$testData_addAntigen$colSelect,
                       agRename = rv$testData_addAntigen$colRenamed)
  }
  
  # See if any antigen columns were renamed
  for (i in 1:length(allAg)){
    currRename = allAg$agRename[i]
    newName = allAg$agName[i]
    # If a rename exists
    if (isTruthy(currRename)){
      newName = currRename
      # Rename current column of data frame to new new
      colnames(toPlotDF)[i] = currRename
    }
  }
  
  # Hovertext will be generated separately, always put in this column
  txt_var = "Text"
  # There will always be two antigens, get those here
  x_var <- paste0(colnames(toPlotDF)[1])
  
  if (Dim >= 2){
    y_var <- paste0(colnames(toPlotDF)[2])
  }
  # Get z_var __________________________________________________________________
  if (Dim >= 3){
    z_var = paste0(colnames(toPlotDF)[3])
  } 
  
  # Get fil_var ________________________________________________________________
  if (Stage == "Train"){
    # fil_var <- rv$currSelClassCol
    fil_var = rv$selectClassCol
  } else if (Stage == "Test"){
    if (Bound == "No"){
      # Test data permits selection of no classes, determine fil_var below
      if (isTruthy(rv$testData_selectClassCol)){
        # Get column name for class assignments
        fil_var = rv$testData_selectClassCol
      } 
    } else if (Bound == "Yes"){
      fil_var = "allPredClass"
    }
  } # End Train vs. Test conditional
  # Group all values
  vars = c(x_var, y_var, z_var, fil_var, txt_var)
  return(list(vars, toPlotDF))
}

# ______________________________________________________________________________

getLLUL <- function(toPlotDF, Dim, colVars){
  # Initialize return values
  minX <- maxX <- minY <- maxY <- minZ <- maxZ <- NA
  
  if (Dim >= 1){
    # Get x, y, z vars
    x_var = colVars[1]
    # Compute the lower and upper limits for each antigen
    minX = min(toPlotDF[, x_var])
    maxX = max(toPlotDF[, x_var])
  }
  
  # If two antigens
  if (Dim >= 2){
    y_var = colVars[2]
    minY = min(toPlotDF[, y_var])
    maxY = max(toPlotDF[, y_var])
  }
  
  # If three antigens
  if (Dim >= 3){
    z_var = colVars[3]
    minZ = min(toPlotDF[, z_var])
    maxZ = max(toPlotDF[, z_var])
  }
  
  # Group return values
  allLimits = c(minX, maxX, minY, maxY, minZ, maxZ)
  return(allLimits)
}
# ______________________________________________________________________________

plots_getHoverText <- function(Dim, toPlotDF, colVars){
  # Helper function for lapply
  makePlotTxtA <- function(x){
    return(x)
  }
  
  # Helper function to help disseminate column data
  makePlotTxtB <- function(x){
    return(paste0(x, ": ", lapply(toPlotDF[[x]], makePlotTxtA)))
  }
  
  # Helper to split by new line
  Paste <- function(x){
    paste(x, collapse = "\n")
  } 
  
  # Generate all hovertext for this data frame
  df <- data.frame(do.call(cbind, lapply(colnames(toPlotDF), makePlotTxtB)))
  
  # Create a sequence for number of columns
  ncolseq = seq(1, ncol(df))
  # Separate metadata by new line
  dfText = apply(df[ncolseq], 1, Paste)
  # Add to dataframe
  toPlotDF$Text = dfText
  # Ensure copy (might not be necessary)
  toPlotDF = toPlotDF
  
  return(toPlotDF)
}

# ______________________________________________________________________________
generatePlotOutput <- function(Stage, Dim, Bound, toPlotDF, colVars, 
                               minMax, surf){
  if (Dim == 1){
    # 1D Plot
    floormin = floor(minMax[1])
    ceilmax = ceil(minMax[2])
    numBins = 50
    binWidth = (abs(floormin)+abs(ceilmax))/numBins
    
    if (Bound == "No"){
      q <- ggplot(data = toPlotDF, 
                  aes(
                    x = .data[[colVars[1]]], 
                      fill = .data[[colVars[4]]])) + 
        geom_histogram(alpha = 0.5,
                       position = "identity",
                       binwidth = binWidth,
                       
                       )
    } else if (Bound == "Yes"){
      if (length(surf) == 1){
        q <- ggplot(data = toPlotDF, 
                    aes(
                      x = .data[[colVars[1]]], 
                      fill = .data[[colVars[4]]])) + 
          geom_histogram(alpha = 0.5,
                         position = "identity",
                         binwidth = binWidth) + 
          geom_vline(xintercept = surf)
      } else {
        q <- ggplot(data = toPlotDF, 
                    aes(
                      x = .data[[colVars[1]]], 
                      fill = .data[[colVars[4]]])) + 
          geom_histogram(alpha = 0.5,
                         position = "identity",
                         binwidth = binWidth) + 
          geom_vline(xintercept = surf[[1]]) + 
          geom_vline(xintercept = surf[[2]])
      }
     
      
    }
    
  } else if (Dim == 2){
    # 2D Plot
    # Check if class column selected (only relevant for test)
    if (!isTruthy(colVars[4])){
      # No class selected 
      q <- ggplot(data = toPlotDF,
                  aes(x = .data[[colVars[1]]], y = .data[[colVars[2]]],
                      text = .data[[colVars[5]]])) + 
        geom_point()
    } else {
      if (Bound == "No"){
        q <- ggplot(data = toPlotDF,
                    aes(x = .data[[colVars[1]]], 
                        y = .data[[colVars[2]]],
                        color = .data[[colVars[4]]], 
                        text = .data[[colVars[5]]]
                    )) + 
          geom_point()
      } else {
        q <- ggplot(data = toPlotDF,
                    aes(x = .data[[colVars[1]]],
                        y = .data[[colVars[2]]]
                    )) +
          geom_point(aes_string(color = colVars[4],
                                label = colVars[5]
          ))
        q = q + geom_contour(data = surf,
                             aes(x = Var1,
                                 y = Var2,
                                 z = value
                             ),
                             breaks = 0,
                             colour = "black")
      }
    }
  } else if (Dim == 3){
    # 3D Plot
    if (!isTruthy(colVars[4])){
      # No class selected
      q <- plot_ly(data = toPlotDF,  
                   x = toPlotDF[[colVars[1]]],
                   y = toPlotDF[[colVars[2]]],
                   z = toPlotDF[[colVars[3]]],
                   type = 'scatter3d', 
                   mode = 'markers',
                   hovertemplate = toPlotDF[[colVars[5]]]
      )
    } else {
      if (Bound == "No"){
        q <- plot_ly(data = toPlotDF,  
                     x = toPlotDF[[colVars[1]]],
                     y = toPlotDF[[colVars[2]]],
                     z = toPlotDF[[colVars[3]]],
                     text = toPlotDF[[colVars[5]]],
                     type = 'scatter3d', 
                     mode = 'markers',
                     color = toPlotDF[[colVars[4]]]
        )
      } else if (Bound == "Yes"){
        # Suppress warnings since mode = "markers" not compatible with surface
        options(warn = -1)
        colours = c("Red", "Yellow", "Blue")
        nCol = length(colours)
        colourscale <- data.frame(
          y = seq(0, 1, length.out = nCol), 
          col = as.character(colours)
        )
        q <- plot_ly(data = toPlotDF,  
                     x = toPlotDF[[colVars[1]]],
                     y = toPlotDF[[colVars[2]]],
                     z = toPlotDF[[colVars[3]]],
                     text = toPlotDF[[colVars[5]]],
                     type = 'scatter3d', 
                     mode = 'markers',
                     color = toPlotDF[[colVars[4]]]
        ) %>% add_trace(data = surf,
                        x = surf$x,
                        y = surf$y,
                        z = surf$z,
                        type = "surface",
                        opacity = 0.1,
                        colorscale = colourscale
        ) %>% hide_colorbar()
        options(warn = 0)
      }
    }
    q <- q %>% layout(title = "", 
                      # autosize = F, width = 1000, height = 750,
                      scene = list(
                        xaxis = list(title = colVars[1]), 
                        yaxis = list(title = colVars[2]),
                        zaxis = list(title = colVars[3])
                      )
                      # scene = list(xaxis = list(dtick = 1))
    )
  }
  q = plotly_build(q)
  
  q = ggplotly(q, tooltip = c("text")) %>%
    layout(legend = list(
      orientation = 'h', x = 0.3, y = -0.2,
      title = list(text = 'Legend: '),
      itemdoubleclick = TRUE))
  # Return plot to render
  return(q)  
}
# ______________________________________________________________________________

plots_getBoundary <- function(Stage, Dim, Bound, Precision, toPlotDF, colVars, 
                              minMax){
  if (Bound == "Yes"){
    if (Stage == "Train"){
      if (Dim == 1){
        minX = minMax[1]
        maxX = minMax[2]
        evalPlotDF = toPlotDF
        
        
        if (Precision == "Guess"){
          rv$initOutmat = rv$outmat
          outmat = rv$initOutmat
         
          a12 = outmat[1,2]
          a21 = outmat[2,1]
          a22 = outmat[2,2]
          
          s1 = a22/(a12+a21)
          dfmm = s1
        } else if (Precision == "Optimized"){
          currIdx = as.numeric(max(rv$allIterVals$idx))
          currIdx = 7
          
          thisAllOutmat = rv$allIterVals
          
          rv$trainOutmat = matrix(
            as.vector(thisAllOutmat[thisAllOutmat$idx == 7, "A"]), ncol = 3)
          
          outmat = rv$trainOutmat
          
          a11 = outmat[1,1]
          a12 = outmat[1,2]
          a21 = outmat[2,1]
          a22 = outmat[2,2]
          
          s1 =  ((-(a12 + a21)) + sqrt(((a12 + a21)^2 - 4*(a11*a22))))/(2*a11)
          s2 =  ((-(a12 + a21)) - sqrt(((a12 + a21)^2 - 4*(a11*a22))))/(2*a11)
          
          dfmm = list(s1, s2)
        }
        
        
      } else if (Dim == 2){
        minX = minMax[1]
        maxX = minMax[2]
        minY = minMax[3]
        maxY = minMax[4]
        if (Precision == "Guess"){
          rv$initOutmat = rv$outmat
          outmat = rv$initOutmat
          
          cc <- emdbook::curve3d(
            matrix(c(x,y,1), ncol = 3)%*%rv$initOutmat%*%
              t(matrix(c(x,y,1), ncol = 3)),
            xlim=c(minX,maxX), ylim=c(minY,maxY), sys3d="none")
          
        } else if (Precision == "Optimized"){
          currIdx = as.numeric(max(rv$allIterVals$idx))
          currIdx = 7
          
          thisAllOutmat = rv$allIterVals
          
          rv$trainOutmat = matrix(
            as.vector(thisAllOutmat[thisAllOutmat$idx == 7, "A"]), ncol = 3)
          
          outmat = rv$trainOutmat
          cc <- emdbook::curve3d(
            matrix(c(x,y,1), ncol = 3)%*%rv$trainOutmat%*%
              t(matrix(c(x,y,1), ncol = 3)),
            xlim=c(minX,maxX), ylim=c(minY,maxY), sys3d="none")
        }
        
        dimnames(cc$z) <- list(cc$x, cc$y)
        mm <- reshape2::melt(cc$z)
        dfmm <- data.frame(mm, check.names = FALSE)
        contData = mm
        
        allClasses = data.frame(className = rv$addClass$selectedClasses,
                                classRename = rv$addClass$colRenamed)
        
        toPlotDF = computeClass(outmat, toPlotDF, colVars[1], colVars[2], NULL,
                                allClasses)
        toPlotDF = data.frame(toPlotDF, check.names = FALSE)
        evalPlotDF = data.frame(toPlotDF, check.names = FALSE)
        
        
      } else if (Dim == 3){
        minX = minMax[1]
        maxX = minMax[2]
        minY = minMax[3]
        maxY = minMax[4]
        minZ = minMax[5]
        maxZ = minMax[6]
        
        if (Precision == "Guess"){
          rv$currOutmat = rv$outmat
        } else if (Precision == "Optimized"){
          currIdx = as.numeric(max(rv$allIterVals$idx))
          thisAllOutmat = rv$allIterVals
          rv$currOutmat = matrix(
            as.vector(thisAllOutmat[thisAllOutmat$idx == currIdx, "A"]), ncol = 4)
        }
        
        tA2disp = rv$currOutmat
        
        Aa = tA2disp[1:2, 3]
        Ab = tA2disp[1:2, 4]
        Ac = tA2disp[3, 1:2]
        Ad = tA2disp[4, 1:2]
        Ae = tA2disp[3,3]
        Af = tA2disp[4,4]
        
        tA2disp[1:2, 3] = Ab
        tA2disp[1:2, 4] = Aa
        tA2disp[3, 1:2] = Ad
        tA2disp[4, 1:2] = Ac
        tA2disp[3,3] = Af
        tA2disp[4,4] = Ae
        
        rv$Q_beta = matrix(tA2disp[1:3, 4], ncol = 3)
        rv$Q_betaT = t(matrix(tA2disp[4, 1:3], ncol = 3))
        rv$Q_mzz = tA2disp[4,4]
        rv$Q_gamma = matrix(tA2disp[1:3, 1:3], ncol = 3)
        rv$moutmat = tA2disp
        
        if (Precision == "Guess"){
          cc = emdbook::curve3d(
            (-(matrix(c(x,y,1), ncol = 3))%*%rv$Q_gamma%*%
               (t(matrix(c(x,y,1), ncol = 3))))[1,1]/
              (2*(matrix(c(x,y,1),ncol=3))%*%rv$Q_betaT)[1,1],
            xlim = c(minX, maxX), ylim = c(minY,maxY), 
            sys3d="contour",levels =0)
          dd = NULL
          dfmm = cc
        } else if (Precision == "Optimized"){
          cc <- emdbook::curve3d(
            (-(rv$Q_beta%*%(t(matrix(c(y,x,1), ncol = 3))) +
                 (matrix(c(y,x,1), ncol = 3))%*%rv$Q_betaT)[1,1] +
               sqrt(
                 ((rv$Q_beta%*%(t(matrix(c(y,x,1), ncol = 3))) +
                     (matrix(c(y,x,1), ncol = 3))%*%rv$Q_betaT)[1,1])^2 -
                   (4*rv$Q_mzz*(
                     (matrix(c(y,x,1), ncol = 3))%*%rv$Q_gamma%*%
                       (t(matrix(c(y,x,1), ncol = 3)))))[1,1]
               )
            )/(2*rv$Q_mzz),
            xlim=c(minX,maxX), ylim=c(minY,maxY), n = c(200,200), 
            sys3d="contour", levels = 0)
          
          
          dd <- emdbook::curve3d(
            (-(rv$Q_beta%*%(t(matrix(c(y,x,1), ncol = 3))) +
                 (matrix(c(y,x,1), ncol = 3))%*%rv$Q_betaT)[1,1] -
               sqrt(
                 ((rv$Q_beta%*%(t(matrix(c(y,x,1), ncol = 3))) +
                     (matrix(c(y,x,1), ncol = 3))%*%rv$Q_betaT)[1,1])^2 -
                   (4*rv$Q_mzz*(
                     (matrix(c(y,x,1), ncol = 3))%*%rv$Q_gamma%*%
                       (t(matrix(c(y,x,1), ncol = 3)))))[1,1]
               )
            )/(2*rv$Q_mzz),
            xlim=c(minX,maxX), ylim=c(minY,maxY), n = c(200,200), 
            sys3d="contour", levels = 0)
          
          ccdd = veilTwoSurfaces(cc, dd)
          ccc = ccdd[[1]]
          ddd = ccdd[[2]]
          combinedSurfaces = list(
            x = append(ccc$x, ddd$x),
            y = append(ccc$y, ddd$y),
            z = rbind(
              ccc$z,
              ddd$z)
          )
          dfmm = combinedSurfaces
        }
        evalPlotDF = data.frame(toPlotDF, check.names = FALSE)
      } # end Dim == 3
    } else if (Stage == "Test"){ # End Train, Start Test
      if (Dim == 1){
        minX = minMax[1]
        maxX = minMax[2]
        evalPlotDF = toPlotDF
        
        
        if (Precision == "Guess"){
          
          outmat = tst$currOutmat
          
          a12 = outmat[1,2]
          a21 = outmat[2,1]
          a22 = outmat[2,2]
          
          s1 = a22/(a12+a21)
          dfmm = s1
        } else if (Precision == "Optimized"){
          # currIdx = as.numeric(max(rv$allIterVals$idx))
          # currIdx = 7
          
          # thisAllOutmat =  tst$currOutmat
          
          # rv$trainOutmat = matrix(
          #   as.vector(thisAllOutmat[thisAllOutmat$idx == 7, "A"]), ncol = 3)
          
          outmat = tst$currOutmat
          
          a11 = outmat[1,1]
          a12 = outmat[1,2]
          a21 = outmat[2,1]
          a22 = outmat[2,2]
          
          s1 =  ((-(a12 + a21)) + sqrt(((a12 + a21)^2 - 4*(a11*a22))))/(2*a11)
          s2 =  ((-(a12 + a21)) - sqrt(((a12 + a21)^2 - 4*(a11*a22))))/(2*a11)
          
          dfmm = list(s1, s2)
        }
        
        
      } else if (Dim == 2){
        minX = minMax[1]
        maxX = minMax[2]
        minY = minMax[3]
        maxY = minMax[4]
        
        outmat = tst$currOutmat
        cc <- emdbook::curve3d(
          matrix(c(x,y,1), ncol = 3)%*%tst$currOutmat%*%
            t(matrix(c(x,y,1), ncol = 3)),
          xlim=c(minX,maxX), ylim=c(minY,maxY), sys3d="none")
        
        # # OPTIM CONT
        dimnames(cc$z) <- list(cc$x, cc$y)
        mm <- reshape2::melt(cc$z)
        dfmm <- data.frame(mm, check.names = FALSE)
        contData = mm
        evalPlotDF = data.frame(toPlotDF, check.names = FALSE)
        
      } else if (Dim == 3){
        minX = minMax[1]
        maxX = minMax[2]
        minY = minMax[3]
        maxY = minMax[4]
        minZ = minMax[5]
        maxZ = minMax[6]
        
        tA2disp = tst$currOutmat
        Aa = tA2disp[1:2, 3]
        Ab = tA2disp[1:2, 4]
        Ac = tA2disp[3, 1:2]
        Ad = tA2disp[4, 1:2]
        Ae = tA2disp[3,3]
        Af = tA2disp[4,4]
        
        tA2disp[1:2, 3] = Ab
        tA2disp[1:2, 4] = Aa
        tA2disp[3, 1:2] = Ad
        tA2disp[4, 1:2] = Ac
        tA2disp[3,3] = Af
        tA2disp[4,4] = Ae
        
        
        rv$Q_beta = matrix(tA2disp[1:3, 4], ncol = 3)
        rv$Q_betaT = t(matrix(tA2disp[4, 1:3], ncol = 3))
        rv$Q_mzz = tA2disp[4,4]
        rv$Q_gamma = matrix(tA2disp[1:3, 1:3], ncol = 3)
        rv$moutmat = tA2disp
        
        cc <- emdbook::curve3d(
          (-(rv$Q_beta%*%(t(matrix(c(y,x,1), ncol = 3))) +
               (matrix(c(y,x,1), ncol = 3))%*%rv$Q_betaT)[1,1] +
             sqrt(
               ((rv$Q_beta%*%(t(matrix(c(y,x,1), ncol = 3))) +
                   (matrix(c(y,x,1), ncol = 3))%*%rv$Q_betaT)[1,1])^2 -
                 (4*rv$Q_mzz*(
                   (matrix(c(y,x,1), ncol = 3))%*%rv$Q_gamma%*%
                     (t(matrix(c(y,x,1), ncol = 3)))))[1,1]
             )
          )/(2*rv$Q_mzz),
          xlim=c(minX,maxX), ylim=c(minY,maxY),n = c(200,200), 
          sys3d="contour", levels = 0)
        
        
        dd <- emdbook::curve3d(
          (-(rv$Q_beta%*%(t(matrix(c(y,x,1), ncol = 3))) +
               (matrix(c(y,x,1), ncol = 3))%*%rv$Q_betaT)[1,1] -
             sqrt(
               ((rv$Q_beta%*%(t(matrix(c(y,x,1), ncol = 3))) +
                   (matrix(c(y,x,1), ncol = 3))%*%rv$Q_betaT)[1,1])^2 -
                 (4*rv$Q_mzz*(
                   (matrix(c(y,x,1), ncol = 3))%*%rv$Q_gamma%*%
                     (t(matrix(c(y,x,1), ncol = 3)))))[1,1]
             )
          )/(2*rv$Q_mzz),
          xlim=c(minX,maxX), 
          ylim=c(minY,maxY), n = c(200,200), sys3d="contour", levels = 0)
        
        ccdd = veilTwoSurfaces(cc, dd)
        
        ccc = ccdd[[1]]
        ddd = ccdd[[2]]
        
        combinedSurfaces = list(
          x = append(ccc$x, ddd$x),
          y = append(ccc$y, ddd$y),
          z = rbind(
            ccc$z,
            ddd$z)
        )
        dfmm = combinedSurfaces
        
        evalPlotDF = data.frame(toPlotDF, check.names = FALSE)
        
      }
    } # End Stage conditional
  } else { 
    
    evalPlotDF = toPlotDF
    dfmm = NULL
  } 
  return(list(evalPlotDF, dfmm))
}
# ______________________________________________________________________________


plot_anyData <- function(ID, TabID, Tabs, Dim, Transform, Stage, 
                         Bound, Precision, Select, Disp, Session){
  # Check requirements before continuing
  plots_checkReqs(ID)
  # Render dynamic number of tabs depending on the output
  plots_renderTabs(TabID, Tabs, Stage, session)
  # Retrieve data for this stage (Train or Test) and convert to dataframe
  toPlotDF = as.data.frame(plots_getData(Dim, Transform, Stage, Bound))
  
  if (Stage == "Test" && Bound == "Yes" && 
      length(unique(toPlotDF[, "allPredClass"])) == 1){
    Bound = "No"
  }
  # Get the column names for antigen, class, and hovertext data in toPlotDF
  datWithColVars = plots_getPlotVars(Stage, toPlotDF, Dim, Bound)
  colVars = datWithColVars[[1]]
  toPlotDF = datWithColVars[[2]]
  
  # Get lower and upper bounds for each antigen
  minMax = getLLUL(toPlotDF, Dim, colVars)
  
  datWithBound = plots_getBoundary(Stage, Dim, Bound, Precision, 
                                   toPlotDF,  colVars, minMax)
  
  toPlotDF = datWithBound[[1]]
  surf = datWithBound[[2]]
  
  # Get hovertext column here and incorporate into dataframe to plot
  toPlotDF = plots_getHoverText(Dim, toPlotDF, colVars)
  
  # Assimilate all components into the final plot structure
  q = generatePlotOutput(Stage, Dim, Bound, toPlotDF, colVars, minMax, surf)
  
  if (Stage == "Train"){
    classesToFilter = unlist(rv$addClass$selectedClasses)
    classesToRename = unlist(rv$addClass$colRenamed)
    q = legendOnly_classes(q, classesToFilter, classesToRename)
    
  } else if (Stage == "Test"){
    if (isTruthy(rv$testData_selectClassCol)){
      classesToFilter = unlist(rv$testData_addClass$selectedClasses)
      classesToRename = unlist(rv$testData_addClass$colRenamed)
      
      if (Bound == "Yes"){
      } else {
        if (!isTruthy(classesToFilter)){
          classesToFilter = "Other"
        }
        # Note this must be a ggplotly object to work
        q = legendOnly_classes(q, classesToFilter, classesToRename)
      }
    }
  }
  # Return figure object
  output[[ID]] = renderPlotly({q})
} 


