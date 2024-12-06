# ==============================================================================
# 
#                             1) PLOT RAW DATA
#
# ==============================================================================
# Mode passed in can be either "dataImport" or "eval" 
# mode2 passed in can be either "init" or "Train"
testData_plot_rawData <- function(mode, mode2){
  ### ##010223 ### This entire section was not converted for test data
  # Only continue if data present and classes and antigens added by user
  req(isTruthy(rv$testData_filteredDF) && #isTruthy(rv$testData_addClass) 
        isTruthy(rv$testData_addAntigen) && lock_testData$done2)
  
  
  if (!isTruthy(rv$testData_addClass) || rv$testData_selectClassCol == "None"){
    # Filter different rows from the dataframe based on which plot is being made
    if (mode == "dataImport"){
      # Extract the user-selected data from the file upload
      toPlot = rv$testData_filteredDF[, unlist(as.character(
        rv$testData_addAntigen$colSelect))]
    } else {
      # # Only continue if "Evaluate" button pressed
      # req(lock_analysis$done2)
      # mode == "eval", so filter additional rows for the selected classes
      toPlot = rv$testData_filteredDF[, unlist(as.character(
        rv$testData_addAntigen$colSelect))]
    }
    
    fil_var <- NULL
    
    
  } else {
    # A class column was in fact selected for test data
    classesToFilter = unlist(rv$testData_addClass$selectedClasses)
    
    
    # Filter different rows from the dataframe based on which plot is being made
    if (mode == "dataImport"){
      # Extract the user-selected data from the file upload
      toPlot = rv$testData_filteredDF[, c(unlist(as.character(rv$testData_addAntigen$colSelect)), 
                                          rv$testData_selectClassCol)]
    } else {
      # # Only continue if "Evaluate" button pressed
      # req(lock_analysis$done2)
      # mode == "eval", so filter additional rows for the selected classes
      toPlot = rv$testData_filteredDF[
        rv$testData_filteredDF[[rv$testData_selectClassCol]] %in% classesToFilter,
        c(unlist(as.character(rv$testData_addAntigen$colSelect)), rv$testData_selectClassCol)]
    }
    
    # Get class assignment column
    classCol = rv$testData_filteredDF[, rv$testData_selectClassCol]
    # Get colname of class assignment 
    ## 092223 changed fil_var since it was returning NULL in option B
    # fil_var <- paste0(colnames(classCol))
    fil_var <- rv$testData_selectClassCol
    
    
  } 
  
  # Convert to dataframe to manipulate below
  toPlotDF = as.data.frame(toPlot)
  
  # Get info about the columns for antigens and class assignments
  x_var <- paste0(colnames(toPlotDF)[1])
  y_var <- paste0(colnames(toPlotDF)[2])
  
  # If a 3D plot
  if (rv$testData_numDimensions == 3){
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
  
  # # If option A, plot the raw data but only display selected classes
  # if (isOptionA("train")){
  #   # Note this must be a ggplotly object to work
  #   q = legendOnly_classes(q, classesToFilter)
  # }
  
  # Return figure object
  q
}

# ==============================================================================
# 
#                         2) PLOT LOG TRANFORMED DATA
#
# ==============================================================================
testData_plot_logTransformData <- function(mode, mode2){
  req(isTruthy(rv$testData_filteredDF) && # isTruthy(rv$testData_addClass) 
        isTruthy(rv$testData_addAntigen) && lock_testData$done2)
  
  # req(lock_analysis$done3)
  
  optionBLabels = NULL
  
  if (!isTruthy(rv$testData_addClass) || rv$testData_selectClassCol == "None"){
    # Filter different rows from the dataframe based on which plot is being made
    if (mode == "dataImport"){
      # Extract the user-selected data from the file upload
      toPlot = rv$testData_filteredDF[, unlist(as.character(
        rv$testData_addAntigen$colSelect))]
    } else {
      # # Only continue if "Evaluate" button pressed
      # req(lock_analysis$done2)
      # mode == "eval", so filter additional rows for the selected classes
      toPlot = rv$testData_filteredDF[, unlist(as.character(
        rv$testData_addAntigen$colSelect))]
    }
    
    fil_var <- NULL
    
    
    
  } else {
    
    classesToFilter = unlist(rv$testData_addClass$selectedClasses)
    
    # Filter by the user selected classes
    if (mode == "dataImport"){
      # Extract the user-selected data from the file upload
      toPlot = rv$testData_filteredDF[, c(unlist(as.character(rv$testData_addAntigen$colSelect)), 
                                          rv$testData_selectClassCol)]
      # Get class assignments for each row
      ## 092223 renamed classCol to optionBLabels to resolve conflict
      optionBLabels = rv$testData_filteredDF[, rv$testData_selectClassCol]
      
    } else {
      # # Only continue if "Evaluate" button pressed
      # req(lock_analysis$done2)
      
      # mode == "eval", so filter additional rows for the selected classes
      toPlot = rv$testData_filteredDF[
        rv$testData_filteredDF[[rv$testData_selectClassCol]] %in% classesToFilter,
        c(unlist(as.character(rv$testData_addAntigen$colSelect)), rv$testData_selectClassCol)]
      
      # Get class assignments for each row
      ## 092223 renamed classCol to optionBLabels to resolve conflict
      optionBLabels = toPlot[, rv$testData_selectClassCol]
    }
      # Get metadata for class column
      # fil_var <- paste0(colnames(classCol))
      fil_var = rv$testData_selectClassCol
    
  }
  # Remove NA's
  toPlot = na.omit(toPlot)
  # Convert to data frame
  toPlotDF = as.data.frame(toPlot)
  
  # Get column metadata
  x_var <- paste0(colnames(toPlotDF)[1])
  y_var <- paste0(colnames(toPlotDF)[2])
  if (rv$testData_numDimensions == 3){
    z_var <- paste0(colnames(toPlotDF)[3])
  } else {
    z_var = NULL
  }
  # Remove NA's again?
  toPlotDF = na.omit(toPlotDF)
  tPlotDF = toPlotDF
  
  # Take log transform of data
  # negMin = apply(lgPlotDF[, allAntigenCols],2,min)
  negMin = min(tPlotDF[, 1])
  posMin = min(tPlotDF[, 2])
  allMin = min(c(negMin, posMin))
  
  
  # if plotting 2 antigens
  if (rv$testData_numDimensions == 2){
    # Do log transformation over data
    tlgPlotDF = logTransformation_2dArray_allMin(tPlotDF[, 1:2], allMin)
    if (isTruthy(optionBLabels)){
      lgPlotDF = cbind(tlgPlotDF, optionBLabels)
      
    } else {
      lgPlotDF = tlgPlotDF
    }
    
    
    q <- create_2dPlot_ly(mode, x_var, y_var, fil_var, lgPlotDF, mode2)
    
    # If plotting 3 antigens
  } else if (rv$testData_numDimensions == 3){
    zmin = min(tPlotDF[, 3])
    allMin = min(c(negMin, posMin, zmin))
    
    # Do log transformation over data
    tlgPlotDF = logTransformation_2dArray_allMin(tPlotDF[, 1:3], allMin)
    
    if (isTruthy(optionBLabels)){
      lgPlotDF = cbind(tlgPlotDF, optionBLabels)
      
    } else {
      lgPlotDF = tlgPlotDF
    }
    
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
  
  rv$testData_logDataPlottedDF = lgPlotDF
  
  # # If option A, display only user-chosen antigens
  # if (isOptionA("train")){
  #   q = legendOnly_classes(q, classesToFilter)
  # }
  
  # return plotly object
  
  q
  
} # end function


# ==============================================================================
# 
#                               3) 2D PLOTLY
#
# ==============================================================================
testData_create_2dPlot_ly <- function(mode, x_var, y_var, fil_var, toPlotDF, mode2){
  minX = min(toPlotDF[, x_var])
  maxX = max(toPlotDF[, x_var])
  minY = min(toPlotDF[, y_var])
  maxY = max(toPlotDF[, y_var])
  
  if (mode == "eval"){
    
    if (mode2 == "init"){
      rv$currOutmat = rv$outmat
    } else {
      # mode2 == "train"
      currIdx = as.numeric(max(rv$allIterVals$idx))
      currIdx = 7
      
      thisAllOutmat = rv$allIterVals
      rv$currOutmat = matrix(
        as.vector(thisAllOutmat[thisAllOutmat$idx == 7, "A"]), ncol = 3)
      # rv$currOutmat = rv$allIterVals[which(rv$allIterVals$idx == currIdx), "A"]
      
      
    }
    
    outmat = rv$currOutmat
    # OPTIM
    
    cc <- emdbook::curve3d(
      matrix(c(x,y,1), ncol = 3)%*%rv$currOutmat%*%t(matrix(c(x,y,1), ncol = 3)),
      xlim=c(minX,maxX), ylim=c(minY,maxY), sys3d="none")
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
    
    
    # neg_compClass = computeClass(outmat, toPlotDF[, y_var])
    
    # Display results
    
    # output$prelimResults <- DT::renderDataTable({
    #   toPlotDF
    # })
    # output$poss <- DT::renderDataTable({
    #   toPlotDF
    # })
    # 
    
    
    # evalPlotDF$text = paste0(x_var, ": ", evalPlotDF[[x_var]],
    #                          "\n", y_var, ": ", evalPlotDF[[y_var]],
    #                          "\n", fil_var, ": ", evalPlotDF[[fil_var]],
    #                          "\nComputed class: ", evalPlotDF[["allClassified"]])
    
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
                            
                            # Projected.value = "allClassified",
                            # Predicted.class = "text"
                            
                            # text = t_var
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
    
    q <- ggplot(data = toPlotDF,
                aes(x = .data[[x_var]],
                    y = .data[[y_var]],
                    color = .data[[fil_var]],
                    text = paste0(x_var, ": ", .data[[x_var]],
                                  "\n", y_var, ": ", .data[[y_var]],
                                  "\n", fil_var, ": ", .data[[fil_var]])
                )) + #xlim(minX, maxX) + ylim(minY,maxY) +
      geom_point()
    q = ggplotly(q, tooltip = c("text")) %>% layout(
      legend = list(
        orientation = 'h', x = 0.3, y = -0.2, 
        title = list(text = 'Legend: '),
        itemdoubleclick = TRUE
      )
    )
    
    
    
    q <- ggplot(data = toPlotDF,
                aes(x = .data[[x_var]],
                    y = .data[[y_var]],
                    color = .data[[fil_var]],
                    text = paste0(x_var, ": ", .data[[x_var]],
                                  "\n", y_var, ": ", .data[[y_var]],
                                  "\n", fil_var, ": ", .data[[fil_var]])
                )) + #xlim(minX, maxX) + ylim(minY,maxY) +
      geom_point()
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
testData_create_3dPlot_ly <- function(mode, x_var, y_var, z_var, fil_var, 
                                      toPlotDF, mode2){
  suppressWarnings({
    # scene = list(camera = list(eye = list(x = 1, y = 1, z = 1)))
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
      
      
      tst$Q_beta = matrix(tA2disp[1:3, 4], ncol = 3)
      tst$Q_betaT = t(matrix(tA2disp[4, 1:3], ncol = 3))
      tst$Q_mzz = tA2disp[4,4]
      tst$Q_gamma = matrix(tA2disp[1:3, 1:3], ncol = 3)
      tst$moutmat = tA2disp
      
      if (mode2 == "init"){
        
        ## 102323 - init condition 
        cc = emdbook::curve3d(
          (-(matrix(c(x,y,1), ncol = 3))%*%gqgamma%*%(t(matrix(c(x,y,1), ncol = 3))))[1,1]/
            (2*(matrix(c(x,y,1),ncol=3))%*%gqbetaT)[1,1],
          
          xlim = c(gminx, gmaxx), ylim = c(gminy,gmaxy), sys3d="contour",levels =0)
        
        
        dd = NULL
        
      } else {
        
        cc <- emdbook::curve3d(
          
          (-(gqbeta%*%(t(matrix(c(y,x,1), ncol = 3))) +
               (matrix(c(y,x,1), ncol = 3))%*%gqbetaT)[1,1] +
             sqrt(
               ((gqbeta%*%(t(matrix(c(y,x,1), ncol = 3))) +
                   (matrix(c(y,x,1), ncol = 3))%*%gqbetaT)[1,1])^2 -
                 (4*gqmzz*(
                   (matrix(c(y,x,1), ncol = 3))%*%gqgamma%*%(t(matrix(c(y,x,1), ncol = 3)))))[1,1]
             )
          )/(2*gqmzz),
          xlim=c(gminx,gmaxx), ylim=c(gminy,gmaxy),n = c(200,200), sys3d="contour", levels = 0)
        
        
        
        dd <- emdbook::curve3d(
          
          (-(gqbeta%*%(t(matrix(c(y,x,1), ncol = 3))) +
               (matrix(c(y,x,1), ncol = 3))%*%gqbetaT)[1,1] -
             sqrt(
               ((gqbeta%*%(t(matrix(c(y,x,1), ncol = 3))) +
                   (matrix(c(y,x,1), ncol = 3))%*%gqbetaT)[1,1])^2 -
                 (4*gqmzz*(
                   (matrix(c(y,x,1), ncol = 3))%*%gqgamma%*%(t(matrix(c(y,x,1), ncol = 3)))))[1,1]
             )
          )/(2*gqmzz),
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
          ) 
        } else {
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
      # q = ggplotly(q)
      
      # ============================================================================
    } else {
      
      # scene = list(camera = list(eye = list(x = 0, y = 2.3, z = 0)))
      
      
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
      )
      
      
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
