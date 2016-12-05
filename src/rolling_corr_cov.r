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

stdList = list()
weighList = list()
counter = 0

for(currentday in as.character(tail(time_steps, -Tin))){
  counter = counter + 1
  if (counter > 1500) {
  firstday = time_steps[which(time_steps %in% currentday) - Tin + 1]
  filename = paste0(dirname, "/corr_us_stocks_Tin", Tin, "_", currentday, ".rds")
  Crr_mat = file_checker(firstday, currentday, filename, returns)

  Crr_filtered = cor_cleaner(Crr_mat, Tin)

  myreturns = returns[paste0(firstday,"::",currentday)]
  stock_names = colnames(myreturns)

  # only considers stocks that have no NAs
  sel_not_NAs = !apply(myreturns,2,function(mycol) any(is.na(mycol)))
  myreturns = myreturns[,sel_not_NAs]

  mu = colMeans(myreturns)
  sigma_frame = data.frame(sqrt(colMeans((myreturns-mu)*(myreturns-mu))))

  colnames(sigma_frame) = "product_sigmas"
  sigma_map = sigma_frame$product_sigmas %*% t(sigma_frame$product_sigmas)
  cov_mat = Crr_filtered * sigma_map

  # portObj = portfolio.spec(assets = names(myreturns))
  # portObj = add.constraint(portObj, "box", min = 0, max = 1)
  # optimize.portfolio(myreturns, portObj, optimize_method = "pso")
  portfolio_statis = portfolio.optim(myreturns, mean(myreturns), covmat=cov_mat)
  stdList[[length(stdList)+1]] = portfolio_statis$ps
  # tempweight = portfolio_statis$pw
  # weighList[length(weighList) + 1] = data.frame(tempweight)
  }
}

write.table(stdList, sep = ",")







#
