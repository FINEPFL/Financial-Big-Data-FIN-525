rm(list=ls())
setwd("/Users/mzhao/Desktop/FBD/Financial-Big-Data-FIN-525/src")
source("init.r")
source("file_checker.r")
source("cor_cleaner.r")
source("test_used_checker.r")
library(tawny)
pwd = init()

us_stock_data = "/Users/mzhao/Desktop/FBD/Financial-Big-Data-FIN-525/datasets/mid-results/us_stocks/us_stocks_returns_filtered.rds"

returns = readRDS(us_stock_data)
time_steps = as.character(index(returns))

Tin = 900
dirname = "/Users/mzhao/Desktop/FBD/Financial-Big-Data-FIN-525/datasets/mid-results/us_stocks/rolling_corr"

unfilteredList = list()
filteredList = list()

for(currentday in as.character(tail(time_steps, -Tin))){

  firstday = time_steps[which(time_steps %in% currentday) - Tin + 1]
  filename = paste0(dirname, "/corr_us_stocks_Tin", Tin, "_", currentday, ".rds")
  Crr_mat = file_checker(firstday, currentday, filename, returns)
  names_trimmer = colnames(Crr_mat)

  test_start_date = time_steps[which(time_steps %in% currentday) + 1]
  test_end_date = time_steps[which(time_steps %in% currentday) + Tin]

  if (!is.na(test_end_date)){
    test_filename = paste0(dirname, "/testset/test_corr_us_stocks_Tin", Tin, "_", currentday, ".rds")
    test_Crr_mat = test_used_checker(test_start_date, test_end_date, test_filename, returns, names_trimmer)
    filtered_test_Crr_mat = cor_cleaner(test_Crr_mat, Tin)

  myreturns = returns[paste0(firstday,"::",currentday)]
  stock_names = colnames(myreturns)

  # only considers stocks that have no NAs
  sel_not_NAs = !apply(myreturns,2,function(mycol) any(is.na(mycol)))
  myreturns = myreturns[,sel_not_NAs]

  mu = colMeans(myreturns)
  sigma_frame = data.frame(sqrt(colMeans((myreturns-mu)*(myreturns-mu))))

  colnames(sigma_frame) = "product_sigmas"
  sigma_map = sigma_frame$product_sigmas %*% t(sigma_frame$product_sigmas)

  colnames(sigma_map) = c(colnames(Crr_mat))
  rownames(sigma_map) = c(colnames(Crr_mat))

  test_sigma_map = sigma_map[c(colnames(test_Crr_mat)), c(colnames(test_Crr_mat))]

  Crr_filtered = cor_cleaner(Crr_mat, Tin)[c(colnames(test_Crr_mat)), c(colnames(test_Crr_mat))]
  Crr_mat = Crr_mat[c(colnames(test_Crr_mat)), c(colnames(test_Crr_mat))]

  cov_filtered = Crr_filtered * test_sigma_map
  cov_unfiltered = Crr_mat * test_sigma_map
  
  cov_filtered_shrinked = cov_shrink(cov_filtered)
  cov_unfiltered_shrinked = cov_shrink(cov_unfiltered)

  cov_filtered_testmat = filtered_test_Crr_mat * test_sigma_map
  # cov_unfiltered_testmat = test_Crr_mat * test_sigma_map
  
  cov_filtered_shrinked_testmat = cov_shrink(cov_filtered_testmat) * test_sigma_map
  # cov_unfiltered_shrinked_testmat = cov_shrink(cov_unfiltered_testmat) * test_sigma_map
  
  myreturns = myreturns[, c(colnames(filtered_test_Crr_mat))]

  filtered_portfolio_statis = portfolio.optim(myreturns, mean(myreturns), covmat=cov_filtered_shrinked)
  unfiltered_portfolio_statis = portfolio.optim(myreturns, mean(myreturns), covmat=cov_unfiltered_shrinked)
  
  filtered_tempweight = filtered_portfolio_statis$pw
  unfiltered_tempweight = unfiltered_portfolio_statis$pw

  port_risk = t(filtered_tempweight) %*% cov_filtered_shrinked_testmat %*% filtered_tempweight
  port_risk_unfiltered = t(unfiltered_tempweight) %*% cov_filtered_shrinked_testmat %*% unfiltered_tempweight

  filteredList[length(filteredList) + 1] = port_risk
  unfilteredList[length(unfilteredList) + 1] = port_risk_unfiltered
  }
}

# write.table(stdList, sep = ",")
