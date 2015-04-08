# Regression on Fund Data from Songtao.
# Songpeng Zu
# 2015-04-08

#-- load package
library(xlsx)

#-- load data
# Note: before reading, change the sheet names from Chinese to English.
tmp <- read.xlsx("datafromtao.xlsx",3,Encoding = "UTF-8",startRow = 1,
                 endRow = 1,header=FALSE)
