#script to download Trendlog data from Hiev (zip file) and extract relevant variables, 
#calcuate 1 minute averages and store data in a local database.
#'@details This function uses the \code{\link{masterChDF}} dataset, which is contained in this package.
#'@details \code{\link{masterRefDF}}

#TODO probably need to chunk data into monthly files so reduce filesize.

library(HIEv)
library(RODBC)
library(lubridate)
setToken(tokenfile="HIEv_token.txt")

#setwd("c:/repos/WTCautoscript")
source("R/getWTCtrendlog.R")

#-----
#- initialize the trendlog .RData files, if needed.
# d <- getWTCtrendlog(Date="2015-11-1",timestep=15)
# TrendlogChDF <- d[[1]]
# TrendlogRefDF <- d[[2]]
# save(TrendlogChDF,file="data/TrendlogChDF.RData")
# save(TrendlogRefDF,file="data/TrendlogRefDF.RData")
#-----


load("data/TrendlogChDF.RData")
load("data/TrendlogRefDF.RData")
LastChDate<-max(TrendlogChDF$DateTime) #check datafile to see where last datetime was
LastRefDate<-max(TrendlogRefDF$DateTime)

startdate<- as.Date(LastChDate+60) #start on date 1 min past last date NB may need to redownload whole day is part missing.
enddate<- as.Date(now())-1 #trendlogs are uploaded to HIEV in early morning for previous day so need to  take away one day.
dates<-seq(as.Date(startdate),as.Date(enddate),by="day")
#get relevant trendlog data for each day and append to Ref and Chamber data files
for (d in dates){
  d2<-as.Date(d,origin="1970-01-01")
  message("getting ",d2)
  dat<-getWTCtrendlog(d2,timestep=1)
  TrendlogChDF<-rbind(TrendlogChDF,dat[[1]])
  TrendlogRefDF<-rbind(TrendlogRefDF,dat[[2]])
}
TrendlogChDF<-TrendlogChDF[!duplicated(TrendlogChDF[1:2]),] #remove any duplicated data rows
TrendlogRefDF<-TrendlogRefDF[!duplicated(TrendlogRefDF[1]),] #remove any duplicated data rows
#visual check that all data present
tab<-with(TrendlogChDF,table(as.Date(DateTime),chamber))
tab[,13]<-with(TrendlogRefDF,table(as.Date(DateTime)))
tab
#resave datafiles
save(TrendlogChDF,file="data/TrendlogChDF.RData")
save(TrendlogRefDF,file="data/TrendlogRefDF.RData")



