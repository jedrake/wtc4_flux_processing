#charts to look at data.
library(ggplot2)
setwd("c:/repos/WTCautoscript/reports")

#calculate cumulative condensed water for each chamber each day
mergdat<-mergdat[with(mergdat,order(chamber,DateTime)),]

cumcond<-mergdat[0,]
for (p in unique(mergdat$chamber)){
  dsub<-subset(mergdat,chamber==p)
  miss <- is.na(dsub$CONDH2O)
  dsub$CONDH2O[miss] <- 0
  
  dsub$cumcond<-cumsum(dsub$CONDH2O)
  cumcond<-rbind(cumcond,dsub)
}

ans<-winDialog("yesno", "Do you want a pdf")
makepdf<-F
if(ans=="YES")makepdf<-T

if (makepdf)  pdf(paste("charts",format(now(),"%Y%m%d"),".pdf",sep=""),  bg = "white", paper="A4r",onefile=TRUE,height=600,width=800)

plt<-ggplot(data=mergdat,aes(x=DateTime,y=FluxCO2))
plt<-plt+ geom_point(aes(col=as.factor(chamber)))
#plt<-plt+ geom_line(data=mROSrain,aes(x=day,y=cumrain,col="mROS"))
#plt<-plt+ geom_line(data=mdaywsrain,aes(x=day,y=cumws,col="ROSWS"))
#plt<-plt+ theme(legend.title=element_text("Trtmnt"))
#plt<-plt+ scale_color_manual(values=colors,
#                             name="Trtmnt",
#                             labels=c("Control", "-50%", "+50%","periodic","seasonal","mROS\nRain","ROSWS\nRain")
#)
plt<-plt+ ggtitle("Flux CO2") + 
  theme(plot.title = element_text(lineheight=1.8, face="bold"))
#plt<-plt+ geom_vline(xintercept=as.numeric(vlines), linetype="dotted")
plt<-plt+ ylab("Flux CO2 (mmol s-1)")
plt

plt<-ggplot(data=cumcond,aes(x=DateTime,y=cumcond))
plt<-plt+ geom_point(aes(col=as.factor(chamber)))
#plt<-plt+ geom_line(data=mROSrain,aes(x=day,y=cumrain,col="mROS"))
#plt<-plt+ geom_line(data=mdaywsrain,aes(x=day,y=cumws,col="ROSWS"))
#plt<-plt+ theme(legend.title=element_text("Trtmnt"))
#plt<-plt+ scale_color_manual(values=colors,
#                             name="Trtmnt",
#                             labels=c("Control", "-50%", "+50%","periodic","seasonal","mROS\nRain","ROSWS\nRain")
#)
plt<-plt+ ggtitle("Cum condensate") + 
  theme(plot.title = element_text(lineheight=1.8, face="bold"))
#plt<-plt+ geom_vline(xintercept=as.numeric(vlines), linetype="dotted")
plt<-plt+ ylab("Cond H2O (mmol)")
plt

if(makepdf)dev.off()

#calculate cumulative condensed water for each chamber each day
mergdat<-mergdat[with(mergdat,order(chamber,DateTime)),]

cumcond<-mergdat[0,]
for (p in unique(mergdat$chamber)){
  dsub<-subset(mergdat,chamber==p)
  miss <- is.na(dsub$CONDH2O)
  dsub$CONDH2O[miss] <- 0
  
  dsub$cumcond<-cumsum(dsub$CONDH2O)
  cumcond<-rbind(cumcond,dsub)
}
