rm(list=ls())
setwd("/Users/mzhao/Desktop/FBD/Financial-Big-Data-FIN-525/src")
source("init.r")
pwd = init()

us_stock_data = "../datasets/mid-results/us_stocks/us_stocks_filtered.rds"
mtgov_path = "../datasets/mtgoxUSD.csv"

test = 0

if (test) {

    mtgov = fread(mtgov_path)
    setnames(mtgov, c("timestamp","price","volume"))
    mtgov$timestamp = as.POSIXct(mtgov$timestamp,origin = "1970-01-01")
    logreturn = diff(log(mtgov$price))
    r = diff(mtgov$price)
    logreturn
    plot(density(logreturn), log = "y")

  } else {

    dataset = as.xts(readRDS(us_stock_data))
    stock_names = names(dataset)
    time_steps = index(dataset)

    logreturn = diff(log(dataset$GE))
    logreturn = logreturn[2:length(logreturn)]
    r = diff(dataset$GE)
    r = r[2:length(r)]
    plot(density(logreturn), log = "y")

}
acf(r)
acf(logreturn)

# check cdf of logreturn
cdfval = ecdf(logreturn)
cdf_cons1 = sort(unique(logreturn))
plot(abs(cdf_cons1), 1-cdfval(abs(cdf_cons1)), log="xy", t="l")

sig = abs(logreturn)
acfval = acf(sig, plot = FALSE, log.max = 100)
plot(0:(length(acfval$acf)-1),acfval$acf,log='xy',t='l')
y1=tail(log(acfval$acf),-1)
lm(y1~cdf_cons1)

# check market performance
sp500 = fread("../datasets/sp500.csv")
sp500 = diff(sp500$Close)
plot(density(sp500))
theory = rnorm(10*length(sp500), mean = 0, sd=1)
plot(density(theory))

autostd_sp500 = acf(rollapplyr(sp500, 2, sd, fill = 0))
autostd_stock = acf(rollapplyr(logreturn, 5, sd, fill = 0))
