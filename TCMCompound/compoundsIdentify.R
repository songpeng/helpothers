# Identify the compounds' sources from LiuWeiDiHuang.
# Songpeng Zu

#-- load package.
#source("http://bioconductor.org/biocLite.R")
#biocLite("fmcR")
library(xlsx)
library(ChemmineR)
library(fmcR)
#-- load file
cidsNotSure <- read.xlsx("CompoundsIdentify_150528.xlsx",2,header=FALSE,
                         colIndex = 1)
cidsAll <- read.xlsx("CompoundsIdentify_150528.xlsx",1,header=FALSE,
                     colIndex = 1)
cidsAllothers <- setdiff(cidsAll[[1]],cidsNotSure[[1]])
#-- Calculate the maximum comman structure similarity.
compoundsNotSure <- getIds(cidsNotSure[[1]])
compoundsAllOthers <- getIds(cidsAllothers)
