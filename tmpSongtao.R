# Regression on Fund Data from Songtao.
# Songpeng Zu
# 2015-04-08

#-- load package
library(xlsx)

#-- load data
# Note: before reading, change the sheet names from Chinese to English.
# Get the first line to determine the column number.
firstl <- read.xlsx("datafromtao.xlsx",3,Encoding = "UTF-8",startRow = 1,
                    endRow = 1,header=FALSE, stringsAsFactors = FALSE)
setcolclass <- c("Date", rep("numeric",length(firstl)-1))
# Get the first column
firstc <- read.xlsx("datafromtao.xlsx",3,colClasses="Date", colIndex=1,
                    stringsAsFactors=FALSE)
# Read the data part.
incomedata <- read.xlsx("datafromtao.xlsx",3,startRow=4,header=FALSE,
                        colClasses=setcolclass)
