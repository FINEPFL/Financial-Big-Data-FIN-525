setwd("/Users/mzhao/Desktop/FBD/Financial-Big-Data-FIN-525/src")
source("init.r")
pwd = init()
raw_data = as.xts(get(load("../datasets/us_stocks.rda")))

num_not_na = apply(raw_data, 1, function(line) {sum(!is.na(line))})
total_nas = apply(raw_data, 1, function(line) {sum(is.na(line))})

num_not_na = xts(num_not_na,index(raw_data))
total_nas = xts(total_nas, index(raw_data))

# only consider a small set of full the full us dataset
# as the starting year, ie, 1950 is roughly full of NAs
raw_data = raw_data['1995::']

# filter require parts
num_not_na = num_not_na[index(raw_data)]

weekends_selector = num_not_na < 100
weekends = index(raw_data)[weekends_selector]
weekdays = index(raw_data)[!weekends_selector]

wendsTick = lapply(weekends, function(tick)             {colnames(raw_data)[!is.na(raw_data[tick])]})
wendsTick = unique(unlist(wendsTick))

raw_data = raw_data[ ,!(colnames(raw_data) %in% wendsTick)]
raw_data = raw_data[index(weekdays)]

allNAdays = apply(raw_data, 1, function(row) all(is.na(row)))
raw_data = raw_data[!allNAdays]

num_stocks = length(names(raw_data))
num_dates = length(index(raw_data))

begin_date = index(raw_data)[1]
end_date = index(raw_data)[length(index(raw_data))]

saveRDS(raw_data,file='../datasets/mid-results/us_stocks/us_stocks_filtered.rds')

stats = list(num_stocks, num_dates, begin_date, end_date)
write.table(stats, file = "../stats/us_stock_stats", sep=",")
