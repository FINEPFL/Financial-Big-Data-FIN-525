setwd("/Users/mzhao/Desktop/FBD/Financial-Big-Data-FIN-525/src")
source("init.r")
source("commu_detector.r")
library(igraph)

cov_mat_us_stock = readRDS("/Users/mzhao/Desktop/FBD/Financial-Big-Data-FIN-525/datasets/mid-results/us_stocks/rolling_corr/corr_us_stocks_Tin900_2006-04-07.rds")
cov_mat_us_stock = cov_mat_us_stock[1:60, 1:60]
cov_mat_us_stock = 100 * sqrt((1-cov_mat_us_stock))
trees_n_communities =  community_detection(cov_mat_us_stock, 2)

plot(trees_n_communities$minimum_spanning_tree,
     vertex.color = trees_n_communities$community$membership,
     mark.groups = communities(trees_n_communities$community),
     vertex.size = 1.25,
     vertex.label.dist = 1.35,
     vertex.label.cex = 0.55)
