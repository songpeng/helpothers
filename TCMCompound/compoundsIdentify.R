# Identify the compounds' sources from LiuWeiDiHuang.
# Songpeng Zu

#-- load package.
#source("http://bioconductor.org/biocLite.R")
#biocLite("fmcsR")
library(xlsx)
library(ChemmineR)
library(fmcsR)
library(Rcpp)
#library(parallel)
#-- load file
cidsNotSure <- read.xlsx("CompoundsIdentify_150528.xlsx",2,header=FALSE,
                         colIndex = 1)
cidsAll <- read.xlsx("CompoundsIdentify_150528.xlsx",1,header=FALSE,
                     colIndex = 1)
cidsAllothers <- setdiff(cidsAll[[1]],cidsNotSure[[1]])

#-- Calculate the maximum comman structure similarity.
compoundsNotSure <- getIds(cidsNotSure[[1]])
compoundsAllOthers <- getIds(cidsAllothers)

nnrow <- length(compoundsNotSure)
nncol <- length(compoundsAllOthers)

#MCSmat <- matrix(,nrow = nnrow, ncol = nncol)
MCScalcByEle <- function(rowindex,colindex){
    return(fmcs(compoundsNotSure[[rowindex]],compoundsAllOthers[[colindex]],
                fast=TRUE)[4])
}
MCScalcByRow <- function(rowindex){
    return(unlist(lapply(1:nncol,function(x) MCScalcByEle(rowindex,x))))
}

#fileConn <- file("outMCScalc.txt")
filename <- "outMCScalc.txt"
CatResult <- function(rowindex){
    cat(paste(MCScalcByRow(rowindex),collapse="\t"),
        file=filename,append=TRUE,sep="\n")
    #It seems to be slower than cat.
#    writeLines(paste(MCScalcByRow(rowindex),collapse="\t"),fileConn)
}
sourceCpp("MCScalc.cpp")
callFunction(10,CatResult)
#close(fileConn)

#sink("outMCScalc.txt",append=TRUE)
#lapply(1:10,function(x) CatResult(x))
#sink()
#use plotMCS to plot the matching results (fmcs without fast parameter).
#callFunction(10,CatResult)
#sink()
