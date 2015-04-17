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
    oneitemoneMatch <- function(item,key2valueline){
        if(grep(item,key2valueline[1],ignore.case=TRUE){
            cat(paste(c(item,key2valueline),collapse="\t"),file=filenm,
                sep="\n",append=TRUE)
        }
    }
    oneitemMatch <- function(item){
        apply(key2value,1,function(x) oneitemoneMatch(item,x))
    }
        apply(array,function(x) oneitemMatch(x))
}
