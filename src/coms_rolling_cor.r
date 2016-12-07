setwd("/Users/mzhao/Desktop/FBD/Financial-Big-Data-FIN-525/src")
source("init.r")
pwd = init()

full_data = na.omit(getComData())

all_returns = diff(log(full_data))[-1,]
time_steps = as.character(index(all_returns))

Tin = 20
dirname = "/Users/mzhao/Desktop/FBD/Financial-Big-Data-FIN-525/datasets/mid-results/commodities/rolling_corr"

stdList = list()
weighList = list()
counter = 0

for(currentday in as.character(tail(time_steps, -Tin))){

  counter = counter + 1

  if (counter > -1) {

    firstday = time_steps[which(time_steps %in% currentday) - Tin + 1]
    filename = paste0(dirname, "/corr_commodities_Tin", Tin, "_", currentday, ".rds")
    Crr_mat = file_checker(firstday, currentday, filename, all_returns)

    # test_start_date = time_steps[which(time_steps %in% currentday) + 1]
    # test_end_date = time_steps[which(time_steps %in% currentday) + Tin]
    #
    # if (!is.na(test_end_date)){
    #   test_filename = paste0(dirname, "/testset/test_corr_commodities_Tin", Tin, "_", currentday, ".rds")
    #   test_Crr_mat = file_checker(test_start_date, test_end_date, test_filename, all_returns)
    # }

  Crr_filtered = cor_cleaner(Crr_mat, Tin)

  myreturns = all_returns[paste0(firstday,"::",currentday)]
  stock_names = colnames(myreturns)

  # only considers stocks that have no NAs
  sel_not_NAs = !apply(myreturns,2,function(mycol) any(is.na(mycol)))
  myreturns = myreturns[,sel_not_NAs]

  mu = colMeans(myreturns)
  sigma_frame = data.frame(sqrt(colMeans((myreturns-mu)*(myreturns-mu))))

  colnames(sigma_frame) = "product_sigmas"
  sigma_map = sigma_frame$product_sigmas %*% t(sigma_frame$product_sigmas)

  cov_mat = Crr_filtered * sigma_map
  cov_unfiltered = Crr_mat * sigma_map

  # portObj = portfolio.spec(assets = names(myreturns))
  # portObj = add.constraint(portObj, "box", min = 0, max = 1)
  # optimize.portfolio(myreturns, portObj, optimize_method = "pso")
  portfolio_statis = portfolio.optim(myreturns, mean(myreturns), covmat=cov_mat)
  # stdList[[length(stdList)+1]] = portfolio_statis$ps
  tempweight = as.matrix(portfolio_statis$pw, ncol = length(tempweight))
  # weighList[length(weighList) + 1] = data.frame(tempweight)
  port_risk = t(tempweight) %*% cov_mat %*% tempweight
  port_risk_unfiltered = t(tempweight) %*% unfiltered %*% tempweight
  port_risk
  port_risk_unfiltered
  }
}





#
