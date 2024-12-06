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