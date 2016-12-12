rm(list=ls())
setwd("/Users/mzhao/Desktop/FBD/Financial-Big-Data-FIN-525/src")
source("init.r")
pwd = init()

us_stock_data = "../datasets/mid-results/us_stocks/us_stocks_returns_filtered.rds"

returns = readRDS(us_stock_data)
time_steps = as.character(index(returns))

# normaly the window size should be 2~3 times of your number of stocks
Tin = 900

dirname = "../datasets/mid-results/us_stocks/rolling_corr"

filtered_risk_list = list()
unfiltered_risk_list = list()

in_filtered_mean_list = list()
in_filtered_sd_list = list()

in_sample_filtered_risk_list = list()
in_sample_unfiltered_risk_list = list()

filtered_weight_list = list()
unfiltered_weight_list = list()

filtered_exp_return_list = list()
return_900_days_list = list()

counter = 0
skip_round = 0

# start the rolling window
for(currentday in as.character(tail(time_steps, -Tin))){
  counter = counter + 1
  if (counter > -1) {

    firstday = time_steps[which(time_steps %in% currentday) - Tin + 1]
    filename = paste0(dirname, "/corr_us_stocks_Tin", Tin, "_", currentday, ".rds")

    Crr_mat = file_checker(firstday, currentday, filename, returns)

    # note that in the out-of-sample window, there mighty be NAs, but not true for
    # the in-sample window, thus we can trim the out-of-sample window via using the
    # name trimmer.
    names_trimmer = colnames(Crr_mat)
    test_start_date = time_steps[which(time_steps %in% currentday) + 1]
    test_end_date = time_steps[which(time_steps %in% currentday) + Tin]

    if (counter < 2)
      axis_test_start_date = test_start_date
      # get the firstday of Out-of-sample windows

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

      # sigma_map is applied to recover the covariance matrix from correlation matrix
      sigma_map = sigma_frame$product_sigmas %*% t(sigma_frame$product_sigmas)

      colnames(sigma_map) = c(colnames(Crr_mat))
      rownames(sigma_map) = c(colnames(Crr_mat))

      test_sigma_map = sigma_map[c(colnames(test_Crr_mat)), c(colnames(test_Crr_mat))]

      # call to filter the correlation matrices
      Crr_filtered = cor_cleaner(Crr_mat, Tin)[c(colnames(test_Crr_mat)), c(colnames(test_Crr_mat))]
      Crr_mat = Crr_mat[c(colnames(test_Crr_mat)), c(colnames(test_Crr_mat))]

      # recover to covariance matrices
      cov_filtered = Crr_filtered * test_sigma_map
      cov_unfiltered = Crr_mat * test_sigma_map

      # out-of-sample windows for testing, we choose the filtered one
      cov_filtered_testmat = filtered_test_Crr_mat * test_sigma_map
      cov_unfiltered_testmat = test_Crr_mat * test_sigma_map

      myreturns = myreturns[, c(colnames(filtered_test_Crr_mat))]

      # the unfiltered covariance matrices can be none positive (semi) definite
      # skipped if met
      skip_round = tryCatch(
        {
          unfiltered_portfolio_statis = portfolio.optim(myreturns, mean(myreturns), covmat=cov_unfiltered)
        }, error = function(cond){
            message("The unfiltered testing matrix is not PSD...skipped..")
           })

      if(!is.null(skip_round)){
          filtered_portfolio_statis = portfolio.optim(myreturns, mean(myreturns), covmat=cov_filtered)

          # get the end date of out-of-sample testing
          axis_test_end_date = test_end_date

          # in-sample portfolio performance
          in_filtered_mean_list[length(in_filtered_mean_list) + 1] = filtered_portfolio_statis$pm
          in_filtered_sd_list[length(in_filtered_sd_list) + 1] = filtered_portfolio_statis$ps

          filtered_tempweight = filtered_portfolio_statis$pw
          unfiltered_tempweight = unfiltered_portfolio_statis$pw

          # store weight vector is necessary -> slow down significantly
          # filtered_weight_list[length(filtered_weight_list) + 1] = list(filtered_tempweight)

          # average return of the stocks within the out-of-sample window
          filtered_exp_return = mean(myreturns %*% filtered_tempweight)
          filtered_exp_return_list[length(filtered_exp_return_list) + 1] = filtered_exp_return

          # for calculating sharp ratio
          return_900_days = (prod(myreturns %*% filtered_tempweight + 1))^(1/900)
          return_900_days_list[length(return_900_days_list) + 1] = return_900_days

          # in-sample performance -- multiplying the matrices/vectors
          in_sample_filtered_risk = t(filtered_tempweight) %*% cov_filtered %*% filtered_tempweight
          in_sample_unfiltered_risk = t(unfiltered_tempweight) %*% cov_filtered %*% unfiltered_tempweight

          # store if necessary
          in_sample_filtered_risk_list[length(in_sample_filtered_risk_list) + 1] = in_sample_filtered_risk
          in_sample_unfiltered_risk_list[length(in_sample_filtered_risk_list) + 1] = in_sample_unfiltered_risk

          port_risk = t(filtered_tempweight) %*% cov_filtered_testmat %*% filtered_tempweight
          port_risk_unfiltered = t(unfiltered_tempweight) %*% cov_filtered_testmat %*% unfiltered_tempweight

          # risk performance -- filtered and unfiltered
          filtered_risk_list[length(filtered_risk_list) + 1] = port_risk
          unfiltered_risk_list[length(unfiltered_risk_list) + 1] = port_risk_unfiltered
        }
   }
  }
}

write.table(filtered_risk_list, file="risk.csv", row.names=FALSE, col.names=FALSE, sep=",")
write.table(filtered_exp_return_list, file="mean.csv", row.names=FALSE, col.names=FALSE, sep=",")
write.table(return_900_days_list, file="return_900_days_list.csv", row.names=FALSE, col.names=FALSE, sep=",")

# plot(unlist(in_sample_filtered_risk_list), t = "l", col = "green", cex=13.5, xlab = "timesteps", ylab="portfolio risk" )
# lines(unlist(in_sample_unfiltered_risk_list), col = "red", cex=2.5)
# title(main="In-sample risk comparision")
# legend(0, 1.4e-05, legend= c("Filtered","Unfiltered"), col=c("green", "red"), lty=1:2, cex=1.2, bty = "n", lwd=2)
#
# plot(unlist(filtered_risk_list), t = "l", col = "green", cex=13.5, xlab = "timesteps", ylab="portfolio risk" )
# lines(unlist(unfiltered_risk_list), col = "red", cex=2.5)
# title(main="Out-of-sample risk comparision")
# legend(0, 2.74e-05, legend= c("Filtered","Unfiltered"), col=c("green", "red"), lty=1:2, cex=1.2, bty = "n", lwd=2)
#
# ratio_unfiltered = as.numeric(unlist(unfiltered_risk_list))/as.numeric(unlist(in_sample_unfiltered_risk_list))
# ratio_filtered = as.numeric(unlist(filtered_risk_list))/as.numeric(unlist(in_sample_filtered_risk_list))
#
# plot(unlist(ratio_filtered), t = "l", col = "green", cex=13.5, xlab = "timesteps", ylab="portfolio risk" )
# lines(unlist(ratio_unfiltered), col = "red", cex=2.5)
# title(main="Phi Plot")
# legend(0, 3.6, legend= c("Filtered","Unfiltered"), col=c("green", "red"), lty=1:2, cex=1.2, bty = "n", lwd=2)
