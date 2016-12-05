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
