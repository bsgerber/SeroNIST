# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                           Program Header Status  
#                               IN PROGRESS
#                   Header: 🗸  Comments: 🗸-  Refactored: 🗸-        
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ==============================================================================
# 
# TITLE: trainingModel_initialPlots.R
#
#                               DESCRIPTION
# This program contains functions that compute and render initial plots of 
# the uploaded data, with conditional blocks to plot either 2 or 3 dimensions
#
#                             TABLE OF CONTENTS
#       1) PLOT RAW DATA
#
#       2) PLOT LOG TRANSFORMED DATA
#
#       3) 2D PLOTLY
#
#       4) 3D PLOTLY
#
#                                  NOTES
#       - #### mode = c("dataImport", "eval") for initial dataImport
#       - #### mode2 = c("init", "train") for data evaluation
#
# #
# # ==============================================================================

getSurroundingMean <- function(row, col, m, mm){
  maxn = ncol(m) + 1
  
  subrow = row - 1
  addrow = row + 1
  subcol = col - 1
  addcol = col + 1
  
  pts2Avg = c()
  newAvg = NA
  
  if (subrow > 0){
    if (!is.nan(m[subrow, col])){
      pts2Avg = c(pts2Avg, mm[subrow, col])
    }
  }
  
  if (addrow < maxn){
    if (!is.nan(m[addrow, col])){
      pts2Avg = c(pts2Avg, mm[addrow, col])
    }
  } 
  
  if (subcol > 0){
    if (!is.nan(m[row, subcol])){
      pts2Avg = c(pts2Avg, mm[row, subcol])
    }
  }
  
  if (addcol < maxn){
    if (!is.nan(m[row, addcol])){
      pts2Avg = c(pts2Avg, mm[row, addcol])
    }
  }
  
  if (subrow > 0 && subcol > 0){
    if (!is.nan(m[subrow, subcol])){
      pts2Avg = c(pts2Avg, mm[subrow, subcol])
    }
  }
  
  if (subrow > 0 && addcol < maxn){
    if (!is.nan(m[subrow, addcol])){
      pts2Avg = c(pts2Avg, mm[subrow, addcol])
    }
  } 
  
  if (addrow < maxn && subcol > 0){
    if (!is.nan(m[addrow, subcol])){
      pts2Avg = c(pts2Avg, mm[addrow, subcol])
    }
  }
  
  if (addrow < maxn && addcol < maxn){
    
    if (!is.nan(m[addrow, addcol])){
      pts2Avg = c(pts2Avg, mm[addrow, addcol])
    }
  }
  
  # print(paste0("Points being averaged:", pts2Avg))
  return(pts2Avg)
}

isAnEdge <- function(row, col, m){
  
  maxn = ncol(m) + 1
  
  subrow = row - 1
  addrow = row + 1
  subcol = col - 1
  addcol = col + 1
  
  isEdge = FALSE
  
  if (subrow > 0){
    if (is.nan(m[subrow, col])){
      isEdge = TRUE
    }
  }
  
  if (addrow < maxn){
    if (is.nan(m[addrow, col])){
      isEdge = TRUE
    }
  } 
  
  if (subcol > 0){
    if (is.nan(m[row, subcol])){
      isEdge = TRUE
    }
  }
  
  if (addcol < maxn){
    if (is.nan(m[row, addcol])){
      isEdge = TRUE
    }
  }
  
  if (subrow > 0 && subcol > 0){
    if (is.nan(m[subrow, subcol])){
      isEdge = TRUE
    }
  }
  
  if (subrow > 0 && addcol < maxn){
    if (is.nan(m[subrow, addcol])){
      isEdge = TRUE
    }
  } 
  
  if (addrow < maxn && subcol > 0){
    if (is.nan(m[addrow, subcol])){
      isEdge = TRUE
    }
  }
  
  if (addrow < maxn && addcol < maxn){
    
    if (is.nan(m[addrow, addcol])){
      isEdge = TRUE
    }
  }
  
  return(isEdge)
}

isNext2Edge <- function(row, col, m){
  
  maxn = ncol(m) + 1
  
  subrow = row - 1
  addrow = row + 1
  subcol = col - 1
  addcol = col + 1
  
  isEdge = FALSE
  
  if (subrow > 0){
    if (!is.nan(m[subrow, col])){
      isEdge = TRUE
    }
  }
  
  if (addrow < maxn){
    if (!is.nan(m[addrow, col])){
      isEdge = TRUE
    }
  } 
  
  if (subcol > 0){
    if (!is.nan(m[row, subcol])){
      isEdge = TRUE
    }
  }
  
  if (addcol < maxn){
    if (!is.nan(m[row, addcol])){
      isEdge = TRUE
    }
  }
  
  if (subrow > 0 && subcol > 0){
    if (!is.nan(m[subrow, subcol])){
      isEdge = TRUE
    }
  }
  
  if (subrow > 0 && addcol < maxn){
    if (!is.nan(m[subrow, addcol])){
      isEdge = TRUE
    }
  } 
  
  if (addrow < maxn && subcol > 0){
    if (!is.nan(m[addrow, subcol])){
      isEdge = TRUE
    }
  }
  
  if (addrow < maxn && addcol < maxn){
    
    if (!is.nan(m[addrow, addcol])){
      isEdge = TRUE
    }
  }
  
  return(isEdge)
}

veilTwoSurfaces <- function(cc, dd){
  cz = cc$z
  dz = dd$z
  
  cz_cpy = cz
  dz_cpy = dz
  
  ncol = ncol(cz)
  nrow = nrow(cz)
  
  newpts = data.frame(x = numeric(), y = numeric(), z = numeric())
  for (i in 1:nrow){
    for (ii in 1:ncol){
      
      cCurr = cz[i, ii]
      dCurr = dz[i, ii]
      
      if (is.nan(cCurr)){
        cByEdge = isNext2Edge(i,ii,cz)
      } else {
        cByEdge = FALSE
      }
      if (is.nan(dCurr)){
        dByEdge = isNext2Edge(i,ii,dz)
      } else {
        dByEdge = FALSE
      }
      
      if (cByEdge && dByEdge){
        # print(paste0("Point at [i/ii ", i, "/",ii, "] is by an edge"))
        czMean = mean(getSurroundingMean(i, ii, cz, dz))
        dzMean = mean(getSurroundingMean(i, ii, dz, cz))
        cdzMean = mean(c(czMean, dzMean))
        
        cz_cpy[i, ii] = cdzMean
        dz_cpy[i,ii] = cdzMean
      }
    }
  }
  
  cc$z = cz_cpy
  dd$z = dz_cpy
  
  return(list(cc, dd))
  
  
}

interpPoints <- function(ee, mode){
  if (mode == "mode1"){
    idxZnan = !is.nan(ee$z)
  } else {
    idxZnan = !is.na(ee$z)
  }
  interpZ = data.frame(x = numeric(), y = numeric(), z = numeric())
  
  for (i in 1:nrow(ee$z)){
    for (ii in 1:ncol(ee$z)){
      if (idxZnan[i, ii]){
        interpZ[nrow(interpZ)+1,] = c(ee$x[i], ee$y[ii], ee$z[i, ii])
      }
    }
  }
  
  interpZ = interpZ[order(interpZ$x), ]
  interpZ = interpZ[order(interpZ$y), ]
  
  # interpZ = interpZ[order(interpZ$z), ]
  
  if (mode == "mode2"){
    interpolatedPts <- interp(interpZ$x, interpZ$y, interpZ$z, 
                              xo=seq(gminx,gmaxx, by=0.1), 
                              yo=seq(gminy, gmaxy, by=0.1),
                              duplicate = "strip")
  } else {
    interpolatedPts = interp(interpZ$x, interpZ$y, interpZ$z, 
                             duplicate = "strip")
  }
  
  return(interpolatedPts)
}

#
# ==============================================================================
# interpPoints <- function(ee, mode){
#   if (mode == "mode1"){
#     idxZnan = !is.nan(ee$z)
#   } else {
#     idxZnan = !is.na(ee$z)
#   }
#   interpZ = data.frame(x = numeric(), y = numeric(), z = numeric())
#   
#   for (i in 1:nrow(ee$z)){
#     for (ii in 1:ncol(ee$z)){
#       if (idxZnan[i, ii]){
#         
#         interpZ[nrow(interpZ)+1,] = c(ee$x[i], ee$y[ii], ee$z[i, ii])
#       }
#     }
#   }
#   
#   # interpZ = interpZ[order(interpZ$z), ]
#   interpZ = interpZ[order(interpZ$x), ]
#   interpZ = interpZ[order(interpZ$y), ]
#   
#   interpolatedPts = pracma::interp(interpZ$x, interpZ$y, interpZ$z)
#   
#   return(interpolatedPts)
# }



# ==============================================================================
# 
#                             1) PLOT RAW DATA
#
# ==============================================================================
# Mode passed in can be either "dataImport" or "eval" 
# mode2 passed in can be either "init" or "Train"
plot_rawData <- function(mode, mode2){
  # Only continue if data present and classes and antigens added by user
  req(isTruthy(rv$filteredDF) && isTruthy(rv$addClass) 
      && isTruthy(rv$addAntigen) && lock_import$done2)
  
  # req(lock_analysis$done3)
  ### Need a lock to prevent updating the plots until submit button is pressed every time
  # Plot work __________________________________________________________________
  # Selected classes by user
  classesToFilter = unlist(rv$addClass$selectedClasses)
  
  # Filter different rows from the dataframe based on which plot is being made
  if (mode == "dataImport"){
    # Extract the user-selected data from the file upload
    toPlot = rv$filteredDF[
      # Subset columns
      , c(unlist(as.character(rv$addAntigen$colSelect)), rv$currSelClassCol,
          # Subset additional columns
          rv$selectSampleIDCol, rv$selectMetadataCol
      )]
  } else {
    # # Only continue if "Evaluate" button pressed
    # req(lock_analysis$done2)
    # mode == "eval", so filter additional rows for the selected classes
    toPlot = rv$filteredDF[
      # Subset rows
      rv$filteredDF[[rv$currSelClassCol]] %in% classesToFilter,
      # Subset columns
      c(unlist(as.character(rv$addAntigen$colSelect)), rv$currSelClassCol,
        # Subset additional columns
        rv$selectSampleIDCol, rv$selectMetadataCol
        )]
  }
  
  # Convert to dataframe to manipulate below
  toPlotDF = as.data.frame(toPlot)
  g0toPlotDF <<- toPlotDF
  # Get info about the columns for antigens and class assignments
  x_var <- paste0(colnames(toPlotDF)[1])
  y_var <- paste0(colnames(toPlotDF)[2])
  # Get class assignment column
  classCol = rv$filteredDF[, rv$currSelClassCol]
  # Get colname of class assignment 
  ## 092223 changed fil_var since it was returning NULL in option B
  # fil_var <- paste0(colnames(classCol))
  fil_var <- rv$currSelClassCol
  
  # If a 3D plot
  if (rv$numDimensions == 3){
    # Get third dimension information
    z_var = paste0(colnames(toPlotDF)[3])
    # Overwrite class column location
    fil_var = paste0(colnames(toPlotDF)[4])
    # Helper for 3d plot below
    q <- create_3dPlot_ly(mode, x_var, y_var, z_var, fil_var, toPlotDF, mode2)
    
  } else {
    # 2d plot helper function
    q <- create_2dPlot_ly(mode, x_var, y_var, fil_var, toPlotDF, mode2)
  }
  
  # If option A, plot the raw data but only display selected classes
  if (isOptionA("train")){
    # Note this must be a ggplotly object to work
    q = legendOnly_classes(q, classesToFilter)
  }
  
  # Return figure object
  q
}

# ==============================================================================
# 
#                         2) PLOT LOG TRANFORMED DATA
#
# ==============================================================================
plot_logTransformData <- function(mode, mode2){
  req(isTruthy(rv$filteredDF) && isTruthy(rv$addClass) 
      && isTruthy(rv$addAntigen) && lock_import$done2)
  
  # req(lock_analysis$done3)
  # Preliminary data processing step____________________________________________
  # allAntigenCols = rv$currSelClassCol
  # Get the user's selected classes to display
  classesToFilter = unlist(rv$addClass$selectedClasses)
  
  # Filter by the user selected classes
  if (mode == "dataImport"){
    # Extract the user-selected data from the file upload
    toPlot = rv$filteredDF[
      , c(unlist(as.character(rv$addAntigen$colSelect)), 
                               rv$currSelClassCol,
          # Subset additional columns
          rv$selectSampleIDCol, rv$selectMetadataCol
          )]
    # Get class assignments for each row
    ## 092223 renamed classCol to optionBLabels to resolve conflict
    optionBLabels = rv$filteredDF[, rv$currSelClassCol]
    extraCols = rv$filteredDF[,  c(rv$selectSampleIDCol, rv$selectMetadataCol)]
  } else {
    # # Only continue if "Evaluate" button pressed
    # req(lock_analysis$done2)
    
    # mode == "eval", so filter additional rows for the selected classes
    toPlot = rv$filteredDF[
      # Subset rows
      rv$filteredDF[[rv$currSelClassCol]] %in% classesToFilter,
      # Subset columns
      c(unlist(as.character(rv$addAntigen$colSelect)), rv$currSelClassCol,
        # Subset additional columns
        rv$selectSampleIDCol, rv$selectMetadataCol
        )]
    
    

    # Get class assignments for each row
    ## 092223 renamed classCol to optionBLabels to resolve conflict
    optionBLabels = toPlot[, rv$currSelClassCol]
    extraCols = rv$filteredDF[,  c(rv$selectSampleIDCol, rv$selectMetadataCol)]
    
  }
  
  ## 011224 removed
  # # Remove NA's
  # toPlot = na.omit(toPlot)
  # Convert to data frame
  toPlotDF = as.data.frame(toPlot)
  
  # Get column metadata
  x_var <- paste0(colnames(toPlotDF)[1])
  y_var <- paste0(colnames(toPlotDF)[2])
  
  # Get metadata for class column
  # fil_var <- paste0(colnames(classCol))
  fil_var = rv$currSelClassCol
  
  # # Remove NA's again? ## 120823
  # toPlotDF = na.omit(toPlotDF)
  
  # # If there are datapoints less than 0, set to 0 
  # toPlotDF[toPlotDF[[x_var]] < 0, x_var] = 0
  # toPlotDF[toPlotDF[[y_var]] < 0, y_var] = 0
  
  tPlotDF = toPlotDF
  
  # Take log transform of data
  # negMin = apply(lgPlotDF[, allAntigenCols],2,min)
  negMin = min(tPlotDF[, 1])
  posMin = min(tPlotDF[, 2])
  allMin = min(c(negMin, posMin))
  
  
  # if plotting 2 antigens
  if (rv$numDimensions == 2){
    gtPlotDF <<- tPlotDF
    # Do log transformation over data
    tlgPlotDF = logTransformation_2dArray_allMin(tPlotDF[, 1:2], allMin)
    gtlgPlotDF <<- tlgPlotDF
    lgPlotDF = cbind(tlgPlotDF, optionBLabels, extraCols)
    
    glgPlotDF <<- lgPlotDF
    q <- create_2dPlot_ly(mode, x_var, y_var, fil_var, lgPlotDF, mode2)
    
    ### TEMP 
    output$matrixAViewer <- renderTable({
      
      A2disp = rv$outmat
      
      format(round(matrix(as.numeric(A2disp), ncol = rv$numDimensions + 1), 16), nsmall = 16)
      
    })
    
    
    # If plotting 3 antigens
  } else if (rv$numDimensions == 3){
    zmin = min(tPlotDF[, 3])
    allMin = min(c(negMin, posMin, zmin))
    
    # Do log transformation over data
    tlgPlotDF = logTransformation_2dArray_allMin(tPlotDF[, 1:3], allMin)
    lgPlotDF = cbind(tlgPlotDF, optionBLabels)
    # Get additional metadata from 3rd antigen
    z_var = paste0(colnames(lgPlotDF)[3])
    # Can't take log of negative value
    ## removed 10/27
    # lgPlotDF[lgPlotDF[[z_var]] < 0, z_var] = 0
    # Perform log transformation over 3rd antigen
    # lgPlotDF[, z_var] = log(lgPlotDF[, z_var], base=2)
    # Helper function to create plotly object
    
    q <- create_3dPlot_ly(mode, x_var, y_var, z_var, fil_var, lgPlotDF, mode2)
    
  } 
  
  # If option A, display only user-chosen antigens
  if (isOptionA("train")){
    q = legendOnly_classes(q, classesToFilter)
  }
  
  # return plotly object
  
  q
  
} # end function


# ==============================================================================
# 
#                               3) 2D PLOTLY
#
# ==============================================================================
create_2dPlot_ly <- function(mode, x_var, y_var, fil_var, toPlotDF, mode2){
  minX = min(toPlotDF[, x_var])
  maxX = max(toPlotDF[, x_var])
  minY = min(toPlotDF[, y_var])
  maxY = max(toPlotDF[, y_var])
  
  if (mode == "eval"){
    
    if (mode2 == "init"){
      
      rv$initOutmat = rv$outmat
      outmat = rv$initOutmat
      
      cc <- emdbook::curve3d(
        matrix(c(x,y,1), ncol = 3)%*%rv$initOutmat%*%t(matrix(c(x,y,1), ncol = 3)),
        xlim=c(minX,maxX), ylim=c(minY,maxY), sys3d="none")
      
    } else {
      # mode2 == "train"
      currIdx = as.numeric(max(rv$allIterVals$idx))
      currIdx = 7
      
      thisAllOutmat = rv$allIterVals
      
      rv$trainOutmat = matrix(
        as.vector(thisAllOutmat[thisAllOutmat$idx == 7, "A"]), ncol = 3)
      # rv$currOutmat = rv$allIterVals[which(rv$allIterVals$idx == currIdx), "A"]
      
      outmat = rv$trainOutmat
      cc <- emdbook::curve3d(
        matrix(c(x,y,1), ncol = 3)%*%rv$trainOutmat%*%t(matrix(c(x,y,1), ncol = 3)),
        xlim=c(minX,maxX), ylim=c(minY,maxY), sys3d="none")
    }
    
    # OPTIM
    ## 120823 changed to outmat instead of reactive
    
    # cc <- emdbook::curve3d(
    #   matrix(c(x,y,1), ncol = 3)%*%rv$currOutmat%*%t(matrix(c(x,y,1), ncol = 3)),
    #   xlim=c(minX,maxX), ylim=c(minY,maxY), sys3d="none")
    ## removed 10/11
    # cc <- emdbook::curve3d(
    #   matrix(c(x,y,1), ncol = 3)%*%rv$outmat%*%t(matrix(c(x,y,1), ncol = 3)),
    #   xlim=c(minX,maxX), ylim=c(minY,maxY), sys3d="none")
    
    ### CONDITIONAL OPTIM BELOW 
    # thisAllOutmat = rv$allIterVals
    # rv$lastOutmat = as.vector(thisAllOutmat[thisAllOutmat$idx == 7, "A"])
    # 
    # req(isTruthy(rv$lastOutmat))
    # cc <- emdbook::curve3d(
    #   matrix(c(x,y,1), ncol = 3)%*%matrix(rv$lastOutmat,ncol = 3)%*%t(matrix(c(x,y,1), ncol = 3)),
    #   xlim=c(minX,maxX), ylim=c(minY,maxY), sys3d="none")
    
    
    # # OPTIM CONT
    dimnames(cc$z) <- list(cc$x, cc$y)
    mm <- reshape2::melt(cc$z)
    dfmm <- data.frame(mm, check.names = FALSE)
    contData = mm
    
    
    # GET STATISTICS
    ## 091123 plotting the classification boundary
    # This function does the same as projectPoints(...) but does the dot product
    # for each individual datapoint
    # outmat = rv$outmat
    # Extract just the class name and renaming info from user
    allClasses = data.frame(className = rv$addClass$selectedClasses,
                            classRename = rv$addClass$colRenamed)
    toPlotDF = computeClass(outmat, toPlotDF, x_var, y_var, NULL, allClasses)
    
    toPlotDF = data.frame(toPlotDF, check.names = FALSE)
    
    tCol = ncol(toPlotDF)
    colNameA = "Projected Value"
    colNameB = "Predicted Class"
    colnames(toPlotDF)[tCol - 1] = colNameA
    colnames(toPlotDF)[tCol] = colNameB
    
    toPlotDF$Metadata = paste0("\n", colNameA, ": ", toPlotDF[[colNameA]],
                               "\n", colNameB, ": ", toPlotDF[[colNameB]])
    
    evalPlotDF = data.frame(toPlotDF, check.names = FALSE)
    t_var = "text"
    
    # # POSSIBLE OPTIM
    q <- ggplot(data = evalPlotDF,
                aes(x = .data[[x_var]],
                    y = .data[[y_var]],
                    # color = .data[[fil_var]],
                    # tooltip = .data[[t_var]]
                    # text = paste0(x_var, ": ", gEval[[x_var]],
                    #               "\n", y_var, ": ", gEval[[y_var]],
                    #               "\n", fil_var, ": ", gEval[[fil_var]],
                    #               "\nComputed class: ", gEval[["allClassified"]])
                )) +
      geom_point(aes_string(color = fil_var,
                            Metadata = "Metadata"
      ))
    # OPTIM CONT
    q = q +
      geom_contour(data = dfmm,
                   aes(x = Var1,
                       y = Var2,
                       z = value
                   ),
                   breaks = 0,
                   colour = "black")
    # q = ggplotly(q, tooltip = c("tooltip"))
    
  } else {
    if (!isTruthy(fil_var)){
      # No class selected 
      q <- ggplot(data = toPlotDF,
                  aes(x = .data[[x_var]],
                      y = .data[[y_var]],
                      # color = .data[[fil_var]],
                      text = paste0(x_var, ": ", .data[[x_var]],
                                    "\n", y_var, ": ", .data[[y_var]])
                  )) + #xlim(minX, maxX) + ylim(minY,maxY) +
        geom_point()
    } else {
      makePlotTxtA <- function(x){return(x)}
      makePlotTxtB <- function(x){
        return(paste0(x, ": ", lapply(toPlotDF[[x]], makePlotTxtA)))
      }
      
      df <- data.frame(do.call(cbind, lapply(colnames(toPlotDF), makePlotTxtB)))
      ncolseq = seq(1,ncol(df))
      
      Paste <- function(x) paste(x, collapse = "\n")
      # dfText = data.frame(allTxt = apply(df[ncolseq], 1, Paste))
      dfText = allTxt = apply(df[ncolseq], 1, Paste)
      
      txt_var = "allTxt"
      toPlotDF$allTxt = dfText
      # toPlotDF = toPlotDF
      
      q <- ggplot(data = toPlotDF,
                  aes(x = .data[[x_var]],
                      y = .data[[y_var]],
                      color = .data[[fil_var]],
                      text = paste(.data[[txt_var[[1]]]])
                  )) + #xlim(minX, maxX) + ylim(minY,maxY) +
      
      # q <- ggplot(data = toPlotDF,
      #             aes(x = .data[[x_var]],
      #                 y = .data[[y_var]],
      #                 color = .data[[fil_var]],
      #                 text = paste0(x_var, ": ", .data[[x_var]],
      #                               "\n", y_var, ": ", .data[[y_var]],
      #                               "\n", fil_var, ": ", .data[[fil_var]]
      #                               
      #                               )
      #             )) + #xlim(minX, maxX) + ylim(minY,maxY) +
        geom_point()
    }
    q = ggplotly(q, tooltip = c("text")) %>% layout(
      legend = list(
        orientation = 'h', x = 0.3, y = -0.2, 
        title = list(text = 'Legend: '),
        itemdoubleclick = TRUE
      )
    )
  }
  
  return(q)
}

# ==============================================================================
# 
#                             4) 3D PLOTLY
#
# ==============================================================================
create_3dPlot_ly <- function(mode, x_var, y_var, z_var, fil_var, 
                             toPlotDF, mode2){
  suppressWarnings({
    scene = list(camera = list(eye = list(x = 0, y = 0, z = 2.1)))
    
    if (mode == "eval"){
      
      graph_res <- 0.1
      
      minX = min(toPlotDF[[x_var]])
      maxX = max(toPlotDF[[x_var]])
      minY = min(toPlotDF[[y_var]])
      maxY = max(toPlotDF[[y_var]])
      minZ = min(toPlotDF[[z_var]])
      maxZ = max(toPlotDF[[z_var]])
      
      minmin = min(minX, minY, minZ)
      maxmax = max(maxX, maxY, maxZ)
      
      #Setup Axis
      axis_x <- seq(minX, maxX, by = graph_res)
      axis_y <- seq(minY, maxY, by = graph_res)
      axis_z <- seq(minZ, maxZ, by = graph_res)
      
      
      
      if (mode2 == "init"){
        rv$currOutmat = rv$outmat
        
      } else {
        # mode2 == "train"
        currIdx = as.numeric(max(rv$allIterVals$idx))
        # currIdx = 7
        
        thisAllOutmat = rv$allIterVals
        rv$currOutmat = matrix(
          as.vector(thisAllOutmat[thisAllOutmat$idx == currIdx, "A"]), ncol = 4)
        # rv$currOutmat = rv$allIterVals[which(rv$allIterVals$idx == currIdx), "A"]
        
        
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
      
      
      gminx <<- minX
      gmaxx <<- maxX
      gminy <<- minY
      gmaxy <<- maxY
      gminz <<- minZ
      gmaxz <<- maxZ
      gqbeta <<- rv$Q_beta
      gqbetaT <<- rv$Q_betaT
      gqmzz <<- rv$Q_mzz
      gqgamma <<- rv$Q_gamma
      
      if (mode2 == "init"){
        
        ## 102323 - init condition 
        cc = emdbook::curve3d(
          (-(matrix(c(x,y,1), ncol = 3))%*%gqgamma%*%(t(matrix(c(x,y,1), ncol = 3))))[1,1]/
            (2*(matrix(c(x,y,1),ncol=3))%*%gqbetaT)[1,1],
          
          xlim = c(gminx, gmaxx), ylim = c(gminy,gmaxy), sys3d="contour",levels =0)
        
        
        dd = NULL
        
      } else {
        
        
        # 102323 removed to try on init 3d cond
        cc <- emdbook::curve3d(
          #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
          # (-((t(matrix(c(x,y,1), ncol = 3)))%*%rv$Q_beta +
          #      rv$Q_betaT%*%(matrix(c(x,y,1), ncol = 3))) +
          #    sqrt(
          #      ((t(matrix(c(x,y,1), ncol = 3)))%*%rv$Q_beta +
          #         rv$Q_betaT%*%(matrix(c(x,y,1), ncol = 3)))^2 -
          #        (4*rv$Q_mzz*(
          #          (matrix(c(x,y,1), ncol = 3))%*%rv$Q_gamma%*%(t(matrix(c(x,y,1), ncol = 3)))))[1,1]
          #    )
          # )/(2*rv$Q_mzz),
          
          
          # (-((t(matrix(c(x,y,1), ncol = 3)))%*%gqbeta +
          #      gqbetaT%*%(matrix(c(x,y,1), ncol = 3))) +
          #    sqrt(
          #      ((t(matrix(c(x,y,1), ncol = 3)))%*%gqbeta +
          #         gqbetaT%*%(matrix(c(x,y,1), ncol = 3)))^2 -
          #        (4*gqmzz*(
          #          (matrix(c(x,y,1), ncol = 3))%*%gqgamma%*%(t(matrix(c(x,y,1), ncol = 3)))))[1,1]
          #    )
          # )/(2*gqmzz),
          
          (-(gqbeta%*%(t(matrix(c(y,x,1), ncol = 3))) +
               (matrix(c(y,x,1), ncol = 3))%*%gqbetaT)[1,1] +
             sqrt(
               ((gqbeta%*%(t(matrix(c(y,x,1), ncol = 3))) +
                   (matrix(c(y,x,1), ncol = 3))%*%gqbetaT)[1,1])^2 -
                 (4*gqmzz*(
                   (matrix(c(y,x,1), ncol = 3))%*%gqgamma%*%(t(matrix(c(y,x,1), ncol = 3)))))[1,1]
             )
          )/(2*gqmzz),
          #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
          xlim=c(gminx,gmaxx), ylim=c(gminy,gmaxy),n = c(200,200), sys3d="contour", levels = 0)
        
        # xlim=c(minX,maxX), ylim=c(minY,maxY), sys3d="contour", levels = 0)
        
        
        dd <- emdbook::curve3d(
          #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
          # (-((t(matrix(c(x,y,1), ncol = 3)))%*%rv$Q_beta +
          #      rv$Q_betaT%*%(matrix(c(x,y,1), ncol = 3))) +
          #    sqrt(
          #      ((t(matrix(c(x,y,1), ncol = 3)))%*%rv$Q_beta +
          #         rv$Q_betaT%*%(matrix(c(x,y,1), ncol = 3)))^2 -
          #        (4*rv$Q_mzz*(
          #          (matrix(c(x,y,1), ncol = 3))%*%rv$Q_gamma%*%(t(matrix(c(x,y,1), ncol = 3)))))[1,1]
          #    )
          # )/(2*rv$Q_mzz),
          
          
          # (-((t(matrix(c(x,y,1), ncol = 3)))%*%gqbeta +
          #      gqbetaT%*%(matrix(c(x,y,1), ncol = 3))) +
          #    sqrt(
          #      ((t(matrix(c(x,y,1), ncol = 3)))%*%gqbeta +
          #         gqbetaT%*%(matrix(c(x,y,1), ncol = 3)))^2 -
          #        (4*gqmzz*(
          #          (matrix(c(x,y,1), ncol = 3))%*%gqgamma%*%(t(matrix(c(x,y,1), ncol = 3)))))[1,1]
          #    )
          # )/(2*gqmzz),
          
          (-(gqbeta%*%(t(matrix(c(y,x,1), ncol = 3))) +
               (matrix(c(y,x,1), ncol = 3))%*%gqbetaT)[1,1] -
             sqrt(
               ((gqbeta%*%(t(matrix(c(y,x,1), ncol = 3))) +
                   (matrix(c(y,x,1), ncol = 3))%*%gqbetaT)[1,1])^2 -
                 (4*gqmzz*(
                   (matrix(c(y,x,1), ncol = 3))%*%gqgamma%*%(t(matrix(c(y,x,1), ncol = 3)))))[1,1]
             )
          )/(2*gqmzz),
          #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
          ### try wireframe or rgl
          # xlim=c(minX,maxX), ylim=c(minY,maxY), sys3d="contour", levels = 0)
          xlim=c(gminx,gmaxx), 
          ylim=c(gminy,gmaxy), n = c(200,200), sys3d="contour", levels = 0)
        
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
        
      }
      withProgress({
        # colorscale = list(c(0, 1), c("tan", "blue"))
        colours = c("Red", "Yellow", "Blue")
        nCol = length(colours)
        colourscale <- data.frame(
          y = seq(0, 1, length.out = nCol), 
          col = as.character(colours)
          # col = ramp.col(c("blue", "yellow", "red")) 
        )
        
        if (mode2 == "init"){
          
          # Suppress warnings since mode = "markers" not compatible with surface
          options(warn = -1)
          q <- plot_ly(data = toPlotDF,  
                       x = toPlotDF[[x_var]],
                       y = toPlotDF[[y_var]],
                       z = toPlotDF[[z_var]],
                       text = toPlotDF[[fil_var]],
                       type = 'scatter3d', 
                       mode = 'markers',
                       color = toPlotDF[[fil_var]],
                       hovertemplate = paste0(x_var, ': %{y:.4f}',
                                              '\n', y_var, ': %{x:.4f}',
                                              '\n', z_var, ': %{z:.4f}',
                                              '\n', fil_var, ': %{text}')
          ) %>% add_trace(data = cc,
                          x = cc$x,
                          y = cc$y,
                          z=cc$z,
                          type = "surface",
                          opacity = 0.1,
                          colorscale = colourscale
          ) %>% hide_colorbar()
          
          options(warn = 0)
          
          
        } else {
          # Suppress warnings since mode = "markers" not compatible with surface
          options(warn = -1)
          
          # mode == "train"
          q <- plot_ly(data = toPlotDF,  
                       x = toPlotDF[[x_var]],
                       y = toPlotDF[[y_var]],
                       z = toPlotDF[[z_var]],
                       text = toPlotDF[[fil_var]],
                       type = 'scatter3d', mode = 'markers',
                       color = toPlotDF[[fil_var]],
                       hovertemplate = paste0(x_var, ': %{y:.4f}',
                                              '\n', y_var, ': %{x:.4f}',
                                              '\n', z_var, ': %{z:.4f}',
                                              '\n', fil_var, ': %{text}')
          ) %>% add_trace(data = combinedSurfaces,
                          x = combinedSurfaces$x,
                          y = combinedSurfaces$y,
                          z = combinedSurfaces$z,
                          type = "surface",
                          opacity = 0.1,
                          colorscale = colourscale
          ) %>% hide_colorbar()
          
          options(warn = 0)
          
        }
      })
      q = plotly_build(q)
      
      
      q = ggplotly(q, tooltip = c("text")) %>% layout(
        # scene = scene,
        legend = list(
          orientation = 'h', x = 0.3, y = -0.2, 
          title = list(text = 'Legend: '),
          itemdoubleclick = TRUE
        )
      )
      
    } else {
      if (!isTruthy(fil_var)){
        # No class selected
        q <- plot_ly(data = toPlotDF,  
                     x = toPlotDF[[x_var]],
                     y = toPlotDF[[y_var]],
                     z = toPlotDF[[z_var]],
                     type = 'scatter3d', 
                     mode = 'markers',
                     # color = toPlotDF[[fil_var]],
                     hovertemplate = paste0(x_var, ': %{y:.4f}',
                                            '\n', y_var, ': %{x:.4f}',
                                            '\n', z_var, ': %{z:.4f}')
        )
      } else {
        
        
        # scene = list(camera = list(eye = list(x = 0, y = 2.3, z = 0)))
        q <- plot_ly(data = toPlotDF,  
                     x = toPlotDF[[x_var]],
                     y = toPlotDF[[y_var]],
                     z = toPlotDF[[z_var]],
                     text = toPlotDF[[fil_var]],
                     type = 'scatter3d', 
                     mode = 'markers',
                     color = toPlotDF[[fil_var]],
                     hovertemplate = paste0(x_var, ': %{y:.4f}',
                                            '\n', y_var, ': %{x:.4f}',
                                            '\n', z_var, ': %{z:.4f}',
                                            '\n', fil_var, ': %{text}')
        )
        
      }
      
      q <- q %>% layout(title = "", 
                        # autosize = F, width = 1000, height = 750,
                        scene = list(
                          xaxis = list(title = x_var), 
                          yaxis = list(title = y_var),
                          zaxis = list(title = z_var)
                        )
                        # scene = list(xaxis = list(dtick = 1))
      )
      
      q = plotly_build(q)
      
      q = ggplotly(q, tooltip = c("text")) %>% layout(
        # scene = scene,
        legend = list(
          orientation = 'h', x = 0.3, y = -0.2, 
          title = list(text = 'Legend: '),
          itemdoubleclick = TRUE
        )
      )
    }
    
  })
  return(q)
}
