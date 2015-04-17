# Match the every line in file A with every word in each line from file B.
# Songpeng Zu
# 2015-04-17
# Note: this is a bad script cause it is too slow.
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

#    oneitemoneMatch <- function(item,key2valueline,t){
#        if(grepl(item,key2valueline[1],ignore.case=TRUE)){
#            cat(paste(c(item,key2valueline),collapse="\t"),file=filenm,
#                sep="\n",append=TRUE)
#        }
#        else if(t<1){
#            cat(item,file=filenm,sep="\n",append=TRUE)
#        }
#    }

    oneitemMatch <- function(item){
#       apply(key2value,1,function(x) oneitemoneMatch(item,x))
        rownum <- nrow(key2value)
        t <- 0
        for(i in 1:rownum){
            if(grepl(item,key2value[i,1],ignore.case = TRUE)){
                cat(paste(c(item,key2value[i,]),collapse = "\t"),file=filenm,
                    sep="\n",append=TRUE)
                t <- 1
            }
        }
        if(t < 1){
            cat(paste(c(item,"NA"),collapse = "\t"),file=filenm,
                sep="\n",append = TRUE)
        }
    }

    apply(array,1,function(x) oneitemMatch(x))
}

#-- Operation
twolineMatch(GO1357,mouseGeneAnnote,filenm="GO1357MatchResult.txt")
twolineMatch(GO1246,mouseGeneAnnote,filenm="GO1246MatchResult.txt")
twolineMatch(GOresponse,mouseGeneAnnote,filenm="GOresponseMatchResult.txt")
