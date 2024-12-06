# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                           Program Header Status  
#                               IN PROGRESS
#                   Header: 🗸  Comments: X 🗸   Refactored: X 🗸         
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ==============================================================================
#
# TITLE: c_MathFunctions/allMathFunctions.R
#
#                               DESCRIPTION
# This program contains overflow functions that relate to simple math operations
#
#                           TABLE OF CONTENTS
#
#
#
#
# ==============================================================================

arrIDX2vecIDX <- function(i, j, m){
  # Converts array index 2D to index in 1D vector
  # Assume 0 index as start point
  rowN = i
  colM = j
  numCol = m
  
  
  return(((i*m)+j)+1)
}


computeClass <- function(outmat, df, x_var, y_var, z_var, allClasses){
  print(paste0("final outmat used:", outmat))
  
  if (!isTruthy(z_var)){
    # Extract antigen data 
    tdf = df[, c(x_var, y_var)]
  } else {
    tdf = df[, c(x_var, y_var, z_var)]
  }
  # Get number of columns for outmat
  numCol = sqrt(length(as.vector(outmat)))
  # Convert outmat to matrix
  outmat = matrix(outmat, ncol=numCol)
  
  if (!isTruthy(allClasses)){
    classA = "classA"
    classB = "classB"
  } else {
    # Extract class data
    if (!isTruthy(allClasses$classRename[1])){
      classA = allClasses$className[1]
    } else {
      classA = allClasses$classRename[1]
    }
    
    if (!isTruthy(allClasses$classRename[2])){
      classB = allClasses$className[2]
    } else {
      classB = allClasses$classRename[2]
    }
  }
 
  
  
  # Get number of rows depending on data structure
  if (isTruthy(nrow(tdf))){
    nrows = nrow(tdf)
  } else {
    nrows = length(tdf)
  }
  
  # Column bind rows of 1's
  tdf = cbind(tdf, rep(1.00, nrows))
  
  # Maintain matrix
  tdf = as.matrix(tdf)
  
  # Store new values in here
  allClassified = c()
  allPredClass = c()
  
  # For each row
  for (i in 1:nrows){
    # Get the current row of data and convert to matrix
    # currentPoint = matrix(tdf[i, ], ncol = 3) ### 10/26 removed
    currentPoint = matrix(tdf[i, ], ncol = numCol)
    
    # Project point with dot product
    thisResult = currentPoint%*%outmat%*%t(currentPoint)
    
    print(paste0("Point i [", i, "]:", currentPoint, "-> ", thisResult))
    
    if (thisResult > 0){
      thisPred = classA
    } else {
      thisPred = classB
    }
    # Check work 
    # print(paste0("dim CurrentPoint:", dim(currentPoint)))
    # print(paste0("Row at i [", i, "]: ", currentPoint))
    # print(paste0("Result:", thisResult))
    
    # Append to growing result set
    allClassified = c(allClassified, thisResult)
    allPredClass = c(allPredClass, thisPred)
  }
  
  # Add the results to their own column
  # tdf = cbind(tdf, allClassified)  
  rdf = cbind(df, allClassified, allPredClass)  
  
  
  return(rdf)
}


projectPoints <- function(outmat, tdf){
  numCol = sqrt(length(as.vector(outmat)))
  outmat = matrix(outmat, ncol=numCol)
  
  if (isTruthy(nrow(tdf))){
    nrows = nrow(tdf)
    
  } else {
    nrows = length(tdf)
  }
  
  tdf = cbind(tdf, rep(1.00, nrows))
  tdf = as.matrix(tdf)
  
  
  projectedPoints = diag(tdf%*%outmat%*%t(tdf))
  
    return(projectedPoints)
}



tanh_1dArray <- function(data, sigval){
  for (i in 1:length(data)){
    x = data[i]/sigval
    newval = 0.5*(1 +  tanh(x))
    data[i] = newval
  }
  return(data)
}

logTransformation_2dArray_allMin <- function(data, allMin){
  
  for (i in 1:ncol(data)){
    for (ii in 1:nrow(data)){
      thisVal = as.numeric(data[ii, i])
      allMin = as.numeric(allMin)
      val2Log = thisVal - allMin + 1e-2
      
      if (!isTruthy(val2Log)){
        print(paste0("ERRANT val2LOG AT i/ii [", i, ", ", ii, "]:", val2Log))
      }
      if (val2Log < 0){
        
        ### Double check negative infinity
        newVal = -Inf
      } else {
        newVal = log(val2Log)
        
      }
      # print(paste0("VAL2Log at i/ii [", i, "/", ii, "]: ", val2Log))
      data[ii,i] = newVal
      
    }
  }   
  
  
  
  return(data)
}

logTransformation_2dArray_testMin <- function(data, allMin, testMin){
  
  for (i in 1:ncol(data)){
    for (ii in 1:nrow(data)){
      thisVal = as.numeric(data[ii, i])
      allMin = as.numeric(allMin)
      
      ### ### ### 
      if (thisVal < allMin){
        val2Log = 1e-2
        
      } else {
        val2Log = thisVal - allMin + 1e-2
        
      }
      
      
      if (!isTruthy(val2Log)){
        print(paste0("ERRANT val2LOG AT i/ii [", i, ", ", ii, "]:", val2Log))
      }
      if (val2Log < 0){
        
        ### Double check negative infinity
        newVal = -Inf
      } else {
        newVal = log(val2Log)
        
      }
      # print(paste0("VAL2Log at i/ii [", i, "/", ii, "]: ", val2Log))
      data[ii,i] = newVal
      
    }
  }   
  
  
  
  return(data)
}



logTransformation_2dArray <- function(data, negMin){
  
  for (i in 1:ncol(data)){
    for (ii in 1:nrow(data)){
      thisVal = data[ii, i]
      val2Log = thisVal - negMin[i] + 1e-2
      if (val2Log < 0){
        newVal = -Inf
      } else {
        newVal = log(val2Log)
        
      }
      if (ii < 5){
        print(paste0("VAL2Log at i/ii [", i, "/", ii, "]: ", thisVal, "->", newVal))
      }
      
      data[ii,i] = newVal
      
    }
  }   
  
  return(data)
}




mapPredictedClass_toQD <- function(gcomp){
  
  possibleClasses = unique(gcomp$allPredClass)
  nratio = 1/nrow(gcomp)
  
  nZero = nrow(gcomp[gcomp$allClassified < 0, ])
  nOne = nrow(gcomp[gcomp$allClassified > 0, ])
  
  qD = nOne*nratio
  
  return(qD)
}
