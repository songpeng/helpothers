# Identify the compounds' sources from LiuWeiDiHuang.
# Songpeng Zu

#-- load package.
#source("http://bioconductor.org/biocLite.R")
#biocLite("fmcsR")
library(xlsx)
library(ChemmineR)
library(fmcsR)
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

MCSmat <- matrix(,nrow = nnrow, ncol = nncol)

MCScalcByRow <- function(rowindex){
    tmparray <- 1:nncol
    for(j in 1:nncol){
        tmp <- fmcs(compoundsNotSure[rowindex],compoundsAllOthers[j])
        tmparray[j] <- tmp@stats[4]
    }
    return(tmparray)
}

for(i in 1:nnrow){
    max(MCScalcByRow(i))
}
mc <- getOption("mc.cores",3)
test <- mclapply(1:nnrow,MCScalcByRow,mc.cores=mc)

MCScalcByEle <- function(rowindex,colindex){
    tmp <- fmcs(compoundsNotSure[rowindex],compoundsAllOthers[colindex])
    return(tmp@stats[4])
}
test <- outer(1:nnrow,1:nncol,FUN="MCScalcByEle")
test <- mapply(function(r,c) MCScalcByEle(r,c),1:nnrow,1:nncol)

col1 <- rep(1:nnrow,nncol)
col2 <- rep(1:nncol,nnrow)

totallen <- nnrow*nncol
array <- col1
for(i in 1:totallen){
    tmp <- fmcs(compoundsNotSure[col1[i]],compoundsAllOthers[col2[i]])
    array[i] <- tmp@stats[4]
}

MCScalcByArray <- function(numindex){
    tmp <- fmcs(compoundsNotSure[col1[1]],compoundsAllOthers[col2[i]])
    return(tmp@stats[4])
}

resultsBYarray <- lapply(1:totallen,function(x) MCScalcByArray(x))
