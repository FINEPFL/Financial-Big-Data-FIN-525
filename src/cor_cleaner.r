cor_cleaner = function(C_mat, Tin){

  Nc = ncol(C_mat)
  Qc = Nc/Tin

  lambdaPlus  = 1 + Qc + 2 * sqrt(Qc)
  lambdaMinus = 1 + Qc - 2 * sqrt(Qc)

  eigns = eigen(C_mat)
  lambda = eigns$values

  lambdaFiltered = lambda * (lambda > lambdaPlus)
  C_filtered = t(eigns$vectors) %*% diag(lambdaFiltered) %*% eigns$vectors

  diag(C_filtered) = 1

  return(C_filtered)
}
