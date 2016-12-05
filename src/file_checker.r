file_checker = function(firstday, currentday, filename, allreturns){
  if(file.exists(filename) && file.info(filename)$size > 100){
      print(paste0("file ",filename," already exists, loading"))
      C_mat = readRDS(filename)
  } else {
    print(paste(firstday, currentday))
    returns = allreturns[paste0(firstday, "::", currentday)]
    selector = !apply(returns, 2, function(column) any(is.na(column)))
    returns = returns[ , selector]
    
    # NA checker
    # lapply(returns , function(x) sum(is.na(x)))

    C_mat = cor(returns)
    colnames(C_mat) = colnames(returns)
    rownames(C_mat) = colnames(returns)
    saveRDS(C_mat, file = filename)
  }
  return (C_mat)
}
