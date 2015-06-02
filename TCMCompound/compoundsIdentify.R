# Identify the compounds' sources from LiuWeiDiHuang.
# Songpeng Zu

#-- load package.
#source("http://bioconductor.org/biocLite.R")
#biocLite("fmcsR")
library(xlsx)
library(ChemmineR)
library(fmcsR)
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
    MCSmat[i,] <- MCScalcByRow(i)
}
