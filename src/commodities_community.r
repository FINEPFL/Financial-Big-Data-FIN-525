setwd("/Users/mzhao/Desktop/FBD/Financial-Big-Data-FIN-525/src")
source("init.r")
source("commu_detector.r")

cov_mat_us_stock = readRDS("../datasets/mid-results/commodities/rolling_corr/corr_commodities_Tin20_2009-11-02.rds")

cov_mat_us_stock = sqrt((1-cov_mat_us_stock)) * 10
trees_n_communities =  community_detection(cov_mat_us_stock, 2)

plot(trees_n_communities$minimum_spanning_tree,
     vertex.color = trees_n_communities$community$membership,
     mark.groups = communities(trees_n_communities$community),
     vertex.size = 2.25,
     vertex.label.dist = 1.35,
     vertex.label.cex = 1.5)
