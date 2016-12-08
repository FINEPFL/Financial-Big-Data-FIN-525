rm(list=ls())
setwd("/Users/mzhao/Desktop/FBD/Financial-Big-Data-FIN-525/src")
source("init.r")
source("file_checker.r")
source("cor_cleaner.r")
source("test_used_checker.r")
pwd = init()

us_stock_data = "../datasets/mid-results/us_stocks/us_stocks_returns_filtered.rds"

returns = readRDS(us_stock_data)
time_steps = as.character(index(returns))

Tin = 900
dirname = "../datasets/mid-results/us_stocks/rolling_corr"

filtered_risk_list = list()
unfiltered_risk_list = list()

in_filtered_mean_list = list()
in_filtered_sd_list = list()

in_sample_filtered_risk_list = list()
in_sample_unfiltered_risk_list = list()


counter = 0
skip_round = 0

for(currentday in as.character(tail(time_steps, -Tin))){
  counter = counter + 1
  if (counter > -1) {
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

      cov_filtered_testmat = filtered_test_Crr_mat * test_sigma_map
      # cov_unfiltered_testmat = test_Crr_mat * test_sigma_map

      myreturns = myreturns[, c(colnames(filtered_test_Crr_mat))]

      filtered_portfolio_statis = portfolio.optim(myreturns, mean(myreturns), covmat=cov_filtered)

      skip_round = tryCatch(
        {
          unfiltered_portfolio_statis = portfolio.optim(myreturns, mean(myreturns), covmat=cov_unfiltered)
        }, error = function(cond){
            message("The unfiltered testing matrix is not PSD...skipped..")
           })

      if(!is.null(skip_round)){

          in_filtered_mean_list[length(in_filtered_mean_list) + 1] = filtered_portfolio_statis$pm
          in_filtered_sd_list[length(in_filtered_sd_list) + 1] = filtered_portfolio_statis$ps

          filtered_tempweight = filtered_portfolio_statis$pw
          unfiltered_tempweight = unfiltered_portfolio_statis$pw

          in_sample_filtered_risk = t(filtered_tempweight) %*% cov_filtered %*% filtered_tempweight
          in_sample_unfiltered_risk = t(unfiltered_tempweight) %*% cov_filtered %*% unfiltered_tempweight

          in_sample_filtered_risk_list[length(in_sample_filtered_risk_list) + 1] = in_sample_filtered_risk
          in_sample_unfiltered_risk_list[length(in_sample_filtered_risk_list) + 1] = in_sample_unfiltered_risk

          port_risk = t(filtered_tempweight) %*% cov_filtered_testmat %*% filtered_tempweight
          port_risk_unfiltered = t(unfiltered_tempweight) %*% cov_filtered_testmat %*% unfiltered_tempweight

          filtered_risk_list[length(filtered_risk_list) + 1] = port_risk
          unfiltered_risk_list[length(unfiltered_risk_list) + 1] = port_risk_unfiltered

        }

   }
  }
}
