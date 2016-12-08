library(xts)
library(WGCNA)

raw_data = readRDS("../datasets/mid-results/us_stocks/us_stocks_filtered.rds")

returns = diff(log(raw_data))
paybackthreshold = 0.05
returns = apply(returns,2,function(mycol) pmin(mycol, paybackthreshold))

returns = xts(returns, index(diff(log(raw_data))))
returns = returns[-1,]

saveRDS(returns, file='../datasets/mid-results/us_stocks/us_stocks_returns_filtered.rds')
