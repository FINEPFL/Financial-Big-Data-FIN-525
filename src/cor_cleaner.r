"cor_cleaner" = function(C_mat, Tin){

  Nc = ncol(C_mat)
  Qc = Nc/Tin

  lambdaPlus  = 1 + Qc + 2 * sqrt(Qc)
  lambdaMinus = 1 + Qc - 2 * sqrt(Qc)

  eigns = eigen(C_mat)
  lambda = eigns$values

  lambdaFiltered = data.frame(lambda * (lambda > lambdaPlus))
  lambdaFiltered = lambdaFiltered[lambdaFiltered>0]

  tempVector = data.frame(eigns$vectors[, 1:length(lambdaFiltered)])
  C_filtered = (as.matrix(tempVector)) %*% diag(lambdaFiltered) %*% t(as.matrix(tempVector))

  diag(C_filtered) = 1
  colnames(C_filtered) = c(colnames(C_mat))
  rownames(C_filtered) = c(colnames(C_mat))
  return(C_filtered)
}
