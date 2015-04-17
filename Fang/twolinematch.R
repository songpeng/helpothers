# Match the every line in file A with every word in each line from file B.
# Songpeng Zu
# 2015-04-17

#-- Load Data
mouseGeneAnnote <- read.table(file="mouseGeneAnnotation.txt",header=FALSE,
                              sep="\t",colClasses="character",quote = "")
GO1246 <- read.table(file="GO1246.txt",header=FALSE,colClasses="character",
                     quote="",sep="\n")
GO1357 <-read.table(file="GO1357.txt",header=FALSE,colClasses="character",
                     quote="",sep="\n")
GOresponse <- read.table(file="GOresponse.txt",head=FALSE,
                         colClasses="character",quote="",sep="\n")
#-- Main Function
twolineMatch <- function(array,key2value,filenm){
    upperKey <- toupper(key2value[,1])
    oneitemMatch <- function(item){
        tmpindex <- which(upperKey==toupper(item))
        if(length(tmpindex)<1){
            tmpdata <- data.frame(x1 <- item, x2 <- "NA")
        }else{
            tmpdata <- data.frame(x1 <- rep(item,length(tmpindex)),
                                  x2 <- key2value[tmpindex,2])
        }
        write.table(tmpdata,file=filenm,append=TRUE,quote = FALSE,
                    sep="\t",row.names = FALSE,col.names = FALSE)
        }

    apply(array,1,function(x) oneitemMatch(x))
}

#-- Operation
twolineMatch(GO1357,mouseGeneAnnote,filenm="GO1357MatchResult.txt")
twolineMatch(GO1246,mouseGeneAnnote,filenm="GO1246MatchResult.txt")
twolineMatch(GOresponse,mouseGeneAnnote,filenm="GOresponseMatchResult.txt")
