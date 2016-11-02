library(xts)
library(WGCNA)

setwd("/Users/mzhao/Desktop/FBD/fromProf/code-20161101")
allreturns = readRDS('../datasets/us_stocks_returns_filtered.rds')
alldates = as.character(index(allreturns))

# setting the window size. larger than 2~3 times of N(umber) of stocks
Tin = 2000

dirSaveCorr = "results/corr/"
dir.create(dirSaveCorr,recursive = TRUE)

for(mydate in as.character(tail(alldates,-Tin))){

  firstDate = alldates[which(alldates %in% mydate)-Tin+1]
  filename  = paste0(dirSaveCorr,"/corr_us_stocks_Tin",Tin,"_",mydate,".rds")

  if(file.exists(filename) && file.info(filename)$size > 100){
    print(paste0("file ",filename," already exists, loading"))
    C = readRDS(filename)
  } else {

    print(paste(firstDate,mydate))
    myreturns=allreturns[paste0(firstDate,"::",mydate)]

    #only considers stocks that have no NAs
    sel_not_NAs = !apply(myreturns,2,function(mycol) any(is.na(mycol)))
    myreturns = myreturns[,sel_not_NAs]

    C=cor(myreturns)
    colnames(C)=colnames(myreturns)
    rownames(C)=colnames(myreturns)
    saveRDS(C,file=filename)
  }
    # corr cleaning
    browser()

    Neff=ncol(C)
    myQ = Neff/Tin
    lambdaPlus = kjhjk

    myeigen=eigen(C)
    lambda=myeigen$values
    v=myeigen$vectors

    lambdaFiltered=lambda*(lambda>lambdaPlus)
    c_filter=v%*%diag(lambdaFiltered)%*%t(v)
    diag(c_filter) = 1

    ### ptf optim

    myreturns=allreturns[paste0(firstDate,"::",mydate)]

    #only considers stocks that have no NAs
    sel_not_NAs=!apply(myreturns,2,function(mycol) any(is.na(mycol)))
    myreturns=myreturns[,sel_not_NAs]

    mu=colMeans(myreturns)
    sigma=sqrt(colMeans((myreturns-mu)*(myreturns-mu)))

}

# alldates=index(myreturns)
# alldatesCalibration=tail(alldates,-Tin)
# for(mydate in alldatesCalibration{
#
# }
