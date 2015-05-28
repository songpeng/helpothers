# Regression on Fund Data from Songtao.
# Songpeng Zu
# 2015-04-08

#-- load package
library(xlsx) # Try install.packages("xlsx") if you do not have it.

#-- load data
# Get the first line to determine the column number.
firstl <- read.xlsx("datafromtao.xlsx",3,Encoding = "UTF-8",startRow = 1,
                    endRow = 1,header=FALSE, stringsAsFactors = FALSE)
setcolclass <- c("Date", rep("numeric",length(firstl)-1))
# Get the first column
firstc <- read.xlsx("datafromtao.xlsx",3,colClasses="Date", colIndex=1,
                    stringsAsFactors=FALSE)
# Read the data part of incomeweek.
incomedata <- read.xlsx("datafromtao.xlsx",3,startRow=4,header=FALSE,
                        colClasses=setcolclass)
incomematrix <- as.matrix(incomedata[,2:ncol(incomedata)])
rownames(incomematrix) <- as.character(firstc$X2010.01.08)
colnames(incomematrix) <- firstl[1:(ncol(incomedata)-1)]

# Read the data of noriskrate.
noriskratedata <- read.xlsx("datafromtao.xlsx",4,colClasses=c("Date","numeric"),
                            header=FALSE,startRow=4)
# Read the incomerate per week from comprehensive index.
incomeweekrate <- read.xlsx("datafromtao.xlsx",6,colClasses=c("Date","numeric",
                                                              "numeric"),
                            header=FALSE, startRow=4)
# Summary the data
rdelta <- incomeweekrate$X3 - noriskratedata$X2
#-- Multitype Regression.
model1 <- function(fundi){
    tmpdata <- data.frame(y=incomematrix[,fundi]-noriskratedata$X2,
                          x1=rdelta,x2=rdelta^2)
    fit <- lm(y~x1+x2,data = tmpdata)
}

model2 <- function(fundi){
    D <- (rdelta>0)*1
    tmpdata <- data.frame(y=incomematrix[,fundi]-noriskratedata$X2,
                          x1=rdelta,x2=rdelta*D)
    fit <- lm(y~x1+x2,data = tmpdata)
}

model3 <- function(fundi){
    tmpdata <- data.frame(y=incomematrix[,fundi]-noriskratedata$X2,
                          x1=pmin(0,rdelta),x2=pmax(0,rdelta))
    fit <- lm(y~x1+x2,data=tmpdata)
}

writeresult1 <- function(fundi){
    fit <- model1(fundi)
    filename <- "model1.txt"
    cat(paste(c(coefficients(fit),summary(fit)$adj.r.squared),
              collapse="\t"), file=filename,sep="\n",append=TRUE)

}
record <- lapply(1:ncol(incomematrix),function(x) writeresult1(x))

writeresult2 <- function(fundi){
    fit <- model2(fundi)
    filename <- "model2.txt"
    cat(paste(c(coefficients(fit),summary(fit)$adj.r.squared),
              collapse="\t"), file=filename,sep="\n",append=TRUE)

}
record <- lapply(1:ncol(incomematrix),function(x) writeresult2(x))

writeresult3 <- function(fundi){
    fit <- model3(fundi)
    filename <- "model3.txt"
    cat(paste(c(coefficients(fit),summary(fit)$adj.r.squared),
              collapse="\t"), file=filename,sep="\n",append=TRUE)

}
record <- lapply(1:ncol(incomematrix),function(x) writeresult3(x))
