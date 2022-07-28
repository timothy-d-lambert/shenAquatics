library(readxl)
library(mapShen)
streamNames<-excel_sheets("C:/Users/echildress/Documents/waterQuality/data/SWAS_Discharge_Hourly_20200514.xlsx")

for(s in 1:length(streamNames)){
  q<-read_excel("C:/Users/echildress/Documents/waterQuality/data/SWAS_Discharge_Hourly_20200514.xlsx",sheet=s) %>%
    data.table() %>%
    setkey(datetime) %>%
    .[!is.na(cfs)]

  daily<-q[,.(cfs=mean(cfs)),.(date=as.Date(datetime))]

  medianHydro<-q[,.(medianCfs=median(cfs)),.(yday=as.numeric(floor(timeOfYear(datetime))))] %>%
    setkey(yday)

  tiff.par(paste0("figures/hydrograph",streamNames[s],".tif"),mfrow=c(2,1),
           width=8,height=8,mgp=c(2.5,0.5,0),mar=c(1.5,3.5,1,0))
    plot(NA,xlim=c(0,366),ylim=c(0,max(q$cfs,na.rm=T)),
         ylab="Discharge (cfs)",xaxt='n',xlab="",
         main=paste0(streamNames[s]," ",min(year(q$datetime)),"-",max(year(q$datetime))))
    axis(1,at=timeOfYear(seq.POSIXt(as.POSIXct("2018-01-01"),as.POSIXct("2018-12-01"),by="month")),
         labels=seq.POSIXt(as.POSIXct("2018-01-01"),as.POSIXct("2018-12-01"),by="month") %>%
           format("%b"))

    for(y in unique(year(q$datetime))){
      points(cfs~timeOfYear(datetime),data=q[year(datetime)==y],type='l',col=gray(0.5,0.5))
    }

    plot(NA,xlim=c(0,366),ylim=c(0,max(log(q$cfs+1),na.rm=T)),
         ylab="Discharge (cfs)",xaxt='n',xlab="",yaxt='n')
    axis(2,log(c(0,1,10,100,1000)+1),c(0,1,10,100,1000))
    axis(1,at=timeOfYear(seq.POSIXt(as.POSIXct("2018-01-01"),as.POSIXct("2018-12-01"),by="month")),
         labels=seq.POSIXt(as.POSIXct("2018-01-01"),as.POSIXct("2018-12-01"),by="month") %>%
           format("%b"))

    for(y in unique(year(q$datetime))){
      points(log(cfs+1)~timeOfYear(datetime),data=q[year(datetime)==y],type='l',col=gray(0.5,0.5))
    }
    points(log(medianCfs+1)~yday,data=medianHydro,type='l',lwd=2)
  dev.off()

}
