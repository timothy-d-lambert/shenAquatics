#'Estimates deep and shallow groundwater influence from paired air and water temperatures after Hare et al. 2020
#'
#'@return An amplitude ratio (water:air) and lag in days (positive means water is behind air)
#'
#'@param data A data.table that includes the variables airTemp, waterTemp, and date
#'@param plotFile An optional file for writing output plots. Plots are shown in the current device in this is left as null.
#'@param airThresh A threshold below which air temperatures could be excluded. Hare et al. 2020 says they excluded <1C, but their code suggests it was <0.5C. Adjusting this may strongly affect the amplitude ratio, but is not likely to influence lag much.
#'@references Hare, DK, AM Helton, ZC Johnson, JW Lane, and MA Briggs. 2021. Continental scale analysis of shallow and deep groundwater contributions to streams. Nature Communications 12: 1450.
#'
#'@export


gwEst<-function(data,plotFile=NULL,airThresh=0.5,
                water="waterTemp",air="airTemp"){
  if(water!="waterTemp"){
    data$waterTemp<-data[,get(water)]
  }

  if(air!="airTemp"){
    data$airTemp<-data[,get(air)]
  }

  data[,dateRadians:=as.numeric((date-as.Date(paste0(min(year(date)),"-01-01")))/365*2*pi)]
  fullDat<-data

  data[airTemp<airThresh]$airTemp<-NA
  airMod<-nls(airTemp~a*sin(dateRadians)+b*cos(dateRadians)+c,
              start=list(a= -10,b= -10,c=10),data=data)
  airAmp<-sqrt(summary(airMod)$coefficients[1]^2+
                 summary(airMod)$coefficients[2]^2)
  airLag<-atan(summary(airMod)$coefficients[2]/
                 summary(airMod)$coefficients[1])/2/pi*365

  waterMod<-nls(waterTemp~a*sin(dateRadians)+b*cos(dateRadians)+c,
                start=list(a= -10,b= -10,c=10),data=data)
  waterAmp<-sqrt(summary(waterMod)$coefficients[1]^2+
                   summary(waterMod)$coefficients[2]^2)
  waterLag<-atan(summary(waterMod)$coefficients[2]/
                   summary(waterMod)$coefficients[1])/2/pi*365

  par(mfrow=c(3,1),mar=c(2.5,2.5,1,1))
  if(!is.null(plotFile)){
    tiff.par(plotFile,mfrow=c(3,1),height=7)
  }
  plot(airTemp~dateRadians,pch=19,col=gray(0.5,0.5),
       data=fullDat)
  points(airTemp~dateRadians,pch=19,col='red',
         data=fullDat[airTemp<airThresh])
  points(predict(airMod)~data[!is.na(airTemp),dateRadians],
         type='l',lwd=2)

  plot(waterTemp~dateRadians,pch=19,col=gray(0.5,0.5),
       data=data)
  points(predict(waterMod)~data[!is.na(waterTemp),dateRadians],
         type='l',lwd=2,col='blue')

  plot(predict(airMod)~data[!is.na(airTemp),dateRadians],
       type='l',lwd=2)
  points(predict(waterMod)~data[!is.na(waterTemp),dateRadians],
         type='l',lwd=2,col='blue')
  if(!is.null(plotFile)){
    dev.off()
  }

  c(waterAmp/airAmp,airLag-waterLag)
}
