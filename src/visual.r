setwd("/Users/mzhao/Desktop/FBD/Financial-Big-Data-FIN-525/src")
source("init.r")
source("commu_detector.r")

cov_mat_us_stock = readRDS("/Users/mzhao/Desktop/FBD/Financial-Big-Data-FIN-525/datasets/mid-results/us_stocks/rolling_corr/corr_us_stocks_Tin2000_2008-04-28.rds")

cov_mat_us_stock = 50 * sqrt((1-cov_mat_us_stock))
trees_n_communities =  community_detection(cov_mat_us_stock, 2)

plot(trees_n_communities$minimum_spanning_tree,
     vertex.color = trees_n_communities$community$membership,
     mark.groups = communities(trees_n_communities$community),
     vertex.size = 0.25,
     vertex.label.dist = 0.35,
     vertex.label.cex = 0.35)
