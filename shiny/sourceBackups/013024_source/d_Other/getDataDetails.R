





getDataDetails <- function(data){
  nrows = nrow(data)
  ncols = ncol(data)
  dims = dim(data)
  lens = length(data)
  
  print(paste0("****____ DATA STRUCTURE DETAILS ____****"))
  if (isTruthy(nrows)){
    print(paste0("NROWS:", nrows))
  }
  if (isTruthy(ncols)){
    print(paste0("NCOLS:", ncols))
  }
  if (isTruthy(dims)){
    print(paste0("DIM:", dims))
  }
  if (isTruthy(lens)){
    print(paste0("LENGTH:", lens))
  }
  
  if (nrows > 15){
    print(paste0("DATAHEAD:", data[1:20, ]))
    
  } else {
    print(paste0("DATA: ", data))
  }
  print(paste0("****____ END DATASTRUCT DETAILS ____****"))
}
