rm(list=ls())
setwd("/Users/mzhao/Desktop/FBD/Financial-Big-Data-FIN-525/src")
source("init.r")
source("file_checker.r")
source("cor_cleaner.r")
pwd = init()

us_stock_data = "/Users/mzhao/Desktop/FBD/Financial-Big-Data-FIN-525/datasets/mid-results/us_stocks/us_stocks_filtered.rds"

returns = readRDS(us_stock_data)
time_steps = as.character(index(returns))

Tin = 2000

dirname = "/Users/mzhao/Desktop/FBD/Financial-Big-Data-FIN-525/datasets/mid-results/us_stocks/rolling_corr"
for(currentday in as.character(tail(time_steps, -Tin))){

  firstday = time_steps[which(time_steps %in% currentday) - Tin + 1]
  filename = paste0(dirname, "/corr_us_stocks_Tin", Tin, "_", currentday, ".rds")
  C_mat = file_checker(firstday, currentday, filename, returns)

  C_filtered = cor_cleaner(C_mat, Tin)

  browser()

  myreturns = returns[paste0(firstday,"::",currentday)]
  stock_names = colnames(myreturns)

  portobj = portfolio.spec(stock_names)
  portobj = add.constraint(portfolio=portobj, type="weight_sum", min_sum=1, max_sum=1)

  #only considers stocks that have no NAs
  sel_not_NAs = !apply(myreturns,2,function(mycol) any(is.na(mycol)))
  myreturns = myreturns[,sel_not_NAs]

  mu = colMeans(myreturns)
  sigma = sqrt(colMeans((myreturns-mu)*(myreturns-mu)))

}
