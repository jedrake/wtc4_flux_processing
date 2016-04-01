#download 15 min WTC data and K factor data from Hiev and use database trendlog values. Merge data and calculate 15min fluxes.

library(HIEv)
library(lubridate)
library(dplyr)
setwd("c:/repos/WTCautoscript")
load("data/TrendlogChDF.RData")
load("data/TrendlogRefDF.RData")

startdate<- as.Date("2016-02-28")
enddate<- as.Date("2016-03-08")

setwd("C:/Temphiev")
ch<-c("C01","C02","C03","C04","C05","C06","C07","C08","C09","C10","C11","C12")
WTCCentralRef<-downloadTOA5(filename="WTC_AUTO_ALL_REF15MIN_R",startDate=startdate,endDate=enddate)
WTCCentralCh<-downloadTOA5(filename="WTC_AUTO_ALL_CH15MIN_R",startDate=startdate,endDate=enddate)
WTCCentralRef$linktime<-nearestTimeStep(WTCCentralRef$DateTime,5,"floor")
WTCCentralCh$linktime<-nearestTimeStep((WTCCentralCh$DateTime+120),5,"floor")
WTCCentralCh$chamber<-ch[WTCCentralCh$chamber]
WTCCentral<-merge(WTCCentralCh,WTCCentralRef,by="linktime")
names(WTCCentral)[names(WTCCentral)=="DateTime.x"] <- "DateTime"

mergdat<-merge(WTCCentral,TrendlogRefDF,by="DateTime")
mergdat<-merge(mergdat,TrendlogChDF,by=c("DateTime","chamber"))
#cleanup
drops <- c("RECORD.x","RECORD.y","SOURCE.x","SOURCE.y","DATE.x","DATE.y")
mergdat<-mergdat[ , !(names(mergdat) %in% drops)]
#visual check for missing data rows
with(mergdat,table(as.Date(DateTime),chamber))

#get kfactors
kfactors<-read.csv("c:/repos/WTCautoscript/data/kfactors.txt",as.is=T)
kfactors$SD<-as.Date(kfactors$DateTime)
kfactors$ED<-as.Date(kfactors$DateTime)
kfact<-split(kfactors,kfactors$chamber)
for (i in 1:length(kfact)){
  for (j in 1:nrow(kfact[[i]])-1){
    kfact[[i]][j,"ED"]<-as.Date(kfact[[i]][j+1,"DateTime"])
  }
  kfact[[i]][nrow(kfact[[i]]),"ED"]<-as.Date(as.character(now()),tz="UTC",origin="1970-01-01")
}

getkfactor<-function(chamber,datet){
  #chamber<-x[1,1]
  #datet<-x[1,2]
  tdate<-as.Date(datet)
  ichamb<-grep(chamber,ch)
  for (i in nrow(kfact[[ichamb]]):1){
    if (tdate>=kfact[[ichamb]][i,"SD"] & tdate<kfact[[ichamb]][i,"ED"]){
      return(list(kfact[[ichamb]][i,"kfactor"],kfact[[ichamb]][i,"ChVolume"]))
    }
    
  }
}

mergdat$kfactor<-NA
mergdat$ChVolume<-NA
for (i in 1:nrow(mergdat)){
  x<-getkfactor(mergdat[i,"chamber"],mergdat[i,"DateTime"])
  mergdat[i,"kfactor"]<-x[1]
  mergdat[i,"ChVolume"]<-x[2]
  
  
}

#saturation vapor pressure (kPa) at Ta (C)
teten<-function(Ta){  
  teten <- 0.611 * exp(17.502 * Ta / (Ta + 240.97))
  return(teten)}

#calculate fluxes
#split data into chambers and sort by assending datetime
mergdat <- mergdat[order(mergdat$chamber, mergdat$DateTime),]
mergdat$HWTC<-mergdat$RH_al/100*teten(mergdat$Tair_al)*1000 #water content Pa
lmergdat<-split(mergdat,chamber)

mergdat$prevDateTime <- as.POSIXct(c(NA,mergdat[1:nrow(mergdat)-1, "DateTime"]),origin="1970-01-01 00:00:00",tz="UTC")
mergdat$prevHWTC <- c(NA,mergdat[1:nrow(mergdat)-1, "HWTC"])
mergdat$prevCWTC <- c(NA,mergdat[1:nrow(mergdat)-1, "CO2C"]) #CHECK CO2C is correct variable
mergdat$tcycle <- (as.numeric(mergdat$DateTime) - as.numeric(mergdat$prevDateTime)) #normally 900 seconds
mergdat$fCO2<-mergdat$CO2FLOW/60 #scc/sec
mergdat$Href<-mergdat$RHref_al / 100 * teten(mergdat$Taref_al) * 1000 #Pa
mergdat$ICO2<-(function(x)(ifelse (x$tcycle==0, 0, (x$fCO2/22.4)*x$AddTime/x$tcycle)))(mergdat) #average rate of CO2 addition during cycle (mmol s-1)
#density of fresh air (g/l)?
#mergdat$Dref = ((Patm*1000 - Href) / (R * (Tref + 273))) + (Href / (461.495 * (Tref + 273))) #kg/m3
R = 287 #specific gas constant for dry air (J K-1 kg-1)
mergdat$Dref <- ((mergdat$AIRPRESS*1000-mergdat$Href)/(R*(mergdat$Taref_al+273)))+(mergdat$Href/(461.495*(mergdat$Taref_al+273))) #kg/m3
#fresh air inflow (l s-1) standard ls-1
mergdat$Ain = (mergdat$kfactor * sqrt(mergdat$DIFFP * 1.2 / mergdat$Dref)) * (273.0 / (mergdat$Taref_al+273)) * (mergdat$AIRPRESS / 101.300)


#air flow leaving chamber (l s-1)
#the airflow leaving does not pass the iris so density of chamber air not relevant
#for mass balance the air leaving must equal that entering assuming no change in storage.
#change in moisture content of air will influence the amount leaving.

#Aout = (k * Sqr(DiffP * 1.2 / Dwtc)) * 273 / (Twtc + 273) * Patm / 101300 'don't think this is correct CVMB
mergdat$Aout = mergdat$Ain * (mergdat$AIRPRESS*1000 - mergdat$Href) / (mergdat$AIRPRESS*1000 - mergdat$HWTC) #water dilution effect NB same mass entering + water must leave chamber assuming no change in storage

#CO2 lost in outgoing air (mmol s-1)
mergdat$v = mergdat$Aout * mergdat$CO2CChamb / 22400
#CO2 gain by fresh air flow (mmol s-1)
mergdat$F = mergdat$Ain * mergdat$REFCO2 / 22400

#check mass balance for airflow
adiff = mergdat$Ain - mergdat$Aout
################################################################################################################
#change in storage of CO2 in chamber (mmol s-1)
mergdat$deltaS<-(function(x)(ifelse (x$tcycle>0,((x$CO2CChamb-x$prevCWTC)*x$ChVolume/22400)*(1/x$tcycle),0)))(mergdat)
#H2O entering in fresh air (mol s-1)
mergdat$Fwat = mergdat$Ain / 22.4 * (mergdat$Href / (mergdat$AIRPRESS*1000))
#H2O leaving in outgoing air air (mol s-1)
mergdat$Vwat = mergdat$Aout / 22.4 * (mergdat$HWTC / (mergdat$AIRPRESS*1000))
#change in storage of H2O in chamber (mol s-1)
mergdat$deltaH2O<-(function(x)(ifelse (x$tcycle>0,((x$HWTC-x$prevHWTC)/(mergdat$AIRPRESS*1000)*x$ChVolume/22.4)*(1/x$tcycle),0)))(mergdat)


mergdat$CONDH2O<-(function(x)(ifelse (x$tcycle>0,(x$CONDH2O/x$tcycle/18),NA)))(mergdat) #condensed water mol s-1
mergdat$FluxH2O<-(function(x)(ifelse (x$tcycle>0,(x$CONDH2O+x$Vwat-x$Fwat+x$deltaH2O),NA)))(mergdat) #flux of water mol s-1

mergdat$FluxCO2<-mergdat$F-mergdat$v+mergdat$ICO2-mergdat$deltaS


