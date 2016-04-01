#Script to generate automatic report of WTC logger data

library (HIEv)
require(ggplot)
setwd("c:/TempHIEv")

loadsoilvars<-function(chamber,sD,eD){
  filename<-paste("WTC_AUTO_C",chamber,"_SOILVARS_R_", sep="")
  dat<- downloadTOA5(filename=filename,endDate=eD,startDate=sD,topath="./tmpWTC/")
  dat$chamber<-paste("C",chamber,sep="")
  if (!(chamber %in% c("03","06","09","12"))){
    dat$VW_Avg.4.<-NaN
  }
    return(dat)
}

chambers<-c("01","02","03","04","05","06","07","08","09","10","11","12")

sD<-"2016-01-01"
eD<-"2016-01-28"

dd<-data.frame()
for (chamber in chambers){
  d<-loadsoilvars(chamber,sD,eD)
  dd<-rbind(dd,d)
}

qplot(DateTime,SoilTempProbe_Avg.6.,data=dd,color=chamber)

chtempprofile<-function(ch){
  dat<-subset(dd,chamber==paste(paste("C",chambers[ch],sep="")))
  ymax=max(dat$SoilTempProbe_Avg.1.,dat$SoilTempProbe_Avg.2.,dat$SoilTempProbe_Avg.3.,dat$SoilTempProbe_Avg.4.,dat$SoilTempProbe_Avg.5.,dat$SoilTempProbe_Avg.6.,na.rm=T)
  ymin=min(dat$SoilTempProbe_Avg.1.,dat$SoilTempProbe_Avg.2.,dat$SoilTempProbe_Avg.3.,dat$SoilTempProbe_Avg.4.,dat$SoilTempProbe_Avg.5.,dat$SoilTempProbe_Avg.6.,na.rm=T)
  with(dat,plot(SoilTempProbe_Avg.1.~DateTime,type="l",ylim=c(ymin,ymax)))
  with(dat,points(SoilTempProbe_Avg.2.~DateTime,type="l",ylim=c(ymin,ymax),col="red"))
  with(dat,points(SoilTempProbe_Avg.3.~DateTime,type="l",ylim=c(ymin,ymax),col="blue"))
  with(dat,points(SoilTempProbe_Avg.4.~DateTime,type="l",ylim=c(ymin,ymax),col="green"))
  with(dat,points(SoilTempProbe_Avg.5.~DateTime,type="l",ylim=c(ymin,ymax),col="yellow"))
  with(dat,points(SoilTempProbe_Avg.6.~DateTime,type="l",ylim=c(ymin,ymax),col="cyan"))

  
}