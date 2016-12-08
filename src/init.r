init = function(){
  rm(list = ls())
  out = tryCatch(
    {     library(zoo)
          library(xts)
          library(WGCNA)
          library(data.table)
          library(PortfolioAnalytics)
          library(tseries)
          library(igraph)
          setwd("/Users/mzhao/Desktop/FBD/Financial-Big-Data-FIN-525/src")
          source("commu_detector.r")
          source("file_checker.r")
          source("cor_cleaner.r")
          source("commodities_data_processing.r")

      }, error = function(cond){
              message(cond)
              message("Fail to load require packages and/or setwd...")
      }, warning = function(cond){
              message("Warning generated...")
      }, finally = {
              message("Successfully load packages and setwd!")
      }
  )
  return (out)
}
