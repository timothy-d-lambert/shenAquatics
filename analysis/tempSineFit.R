# library(rnoaa)
library(dplyr)
library(data.table)
library(readxl)
library(mapShen)
#
# tData<-ghcnd("USC00441434") %>%
#   data.table()
#
# tmax<-tData[element=="TMAX"] %>%
#   .[,c("year","month",names(.)[grepl("VALUE",names(.))]),with=F] %>%
#   melt(id.vars=c("year","month")) %>%
#   data.table() %>%
#   .[,day:=substr(variable,6,8)] %>%
#   .[,date:=as.Date(paste(year,month,day,sep="-"))] %>%
#   .[!is.na(date)] %>%
#   .[,.(date,value)] %>%
#   setnames("value","tmax") %>%
#   setkey(date) %>%
#   .[,tmax:=tmax/10] %>%
#   .[,timeIndex:=as.numeric(date-min(date))/365*2*pi]

painAir<-fread("C:/Users/echildress/Documents/waterQuality/data/airTempAveragePainSWAS.csv",
               skip=2) %>%
  .[,.(V1,V2)] %>%
  setnames(c("datetime","airTemp")) %>%
  .[,.(airTemp=mean(airTemp)),.(date=as.Date(datetime))] %>%
  # .[airTemp<1,airTemp:=NA] %>%
  setkey(date)

painWater<-fread("C:/Users/echildress/Documents/waterQuality/data/waterTempAveragePainSWAS.csv",
               skip=2) %>%
  .[,.(V1,V2)] %>%
  setnames(c("datetime","waterTemp")) %>%
  .[,.(waterTemp=mean(waterTemp),waterMax=max(waterTemp)),.(date=as.Date(datetime))] %>%
  # .[waterTemp<1,waterTemp:=NA] %>%
  setkey(date)
pain<-merge(painAir,painWater)

painDaymet<-fread("data/daymet/3F123.csv") %>%
  setnames(c("year","yday","dayLength","prcp","srad","swe","tmax","tmin","vp")) %>%
  .[,daymetTemp:=(tmin+tmax)/2] %>%
  .[,date:=as.Date(paste0(year,"-01-01"))+yday-1] %>%
  .[,.(date,daymetTemp)] %>%
  setkey(date)

pain<-painDaymet[pain]

stanAir<-fread("C:/Users/echildress/Documents/waterQuality/data/airTempAverageStanSWAS.csv",
               skip=2) %>%
  .[,.(V1,V2)] %>%
  setnames(c("datetime","airTemp")) %>%
  .[,.(airTemp=mean(airTemp)),.(date=as.Date(datetime))] %>%
  # .[airTemp<1,airTemp:=NA] %>%
  setkey(date)

stanWater<-fread("C:/Users/echildress/Documents/waterQuality/data/waterTempAverageStanSWAS.csv",
                 skip=2) %>%
  .[,.(V1,V2)] %>%
  setnames(c("datetime","waterTemp")) %>%
  .[,.(waterTemp=mean(waterTemp),waterMax=max(waterTemp)),.(date=as.Date(datetime))] %>%
  # .[waterTemp<1,waterTemp:=NA] %>%
  setkey(date)
stan<-merge(stanAir,stanWater)

stanDaymet<-fread("data/daymet/2F072.csv") %>%
  setnames(c("year","yday","dayLength","prcp","srad","swe","tmax","tmin","vp")) %>%
  .[,daymetTemp:=(tmin+tmax)/2] %>%
  .[,date:=as.Date(paste0(year,"-01-01"))+yday-1] %>%
  .[,.(date,daymetTemp)] %>%
  setkey(date)

stan<-stanDaymet[stan]


pineAir<-fread("C:/Users/echildress/Documents/waterQuality/data/airTempAveragePineSWAS.csv",
               skip=2) %>%
  .[,.(V1,V2)] %>%
  setnames(c("datetime","airTemp")) %>%
  .[,.(airTemp=mean(airTemp)),.(date=as.Date(datetime))] %>%
  # .[airTemp<1,airTemp:=NA] %>%
  setkey(date)

pineWater<-fread("C:/Users/echildress/Documents/waterQuality/data/waterTempAveragePineSWAS.csv",
                 skip=2) %>%
  .[,.(V1,V2)] %>%
  setnames(c("datetime","waterTemp")) %>%
  .[,.(waterTemp=mean(waterTemp),waterMax=max(waterTemp)),.(date=as.Date(datetime))] %>%
  # .[waterTemp<1,waterTemp:=NA] %>%
  setkey(date)
pine<-merge(pineAir,pineWater)

pineDaymet<-fread("data/daymet/1F003.csv") %>%
  setnames(c("year","yday","dayLength","prcp","srad","swe","tmax","tmin","vp")) %>%
  .[,daymetTemp:=(tmin+tmax)/2] %>%
  .[,date:=as.Date(paste0(year,"-01-01"))+yday-1] %>%
  .[,.(date,daymetTemp)] %>%
  setkey(date)

pine<-pineDaymet[pine]




gwEst(pine)
gwEst(pine,air="daymetTemp")
gwEst(pain)
gwEst(pain,air="daymetTemp")
gwEst(stan)
gwEst(stan,air="daymetTemp")


pain[,":="(daymetDiff=daymetTemp-airTemp,
           yday=yday(date))]
pine[,":="(daymetDiff=daymetTemp-airTemp,
           yday=yday(date))]
stan[,":="(daymetDiff=daymetTemp-airTemp,
           yday=yday(date))]

painDay<-gam(daymetDiff~s(yday,bs="cc"),knots=knots,data=pain)
pineDay<-gam(daymetDiff~s(yday,bs="cc"),knots=knots,data=pine)
stanDay<-gam(daymetDiff~s(yday,bs="cc"),knots=knots,data=stan)


bmDaymet<-fread("data/daymet/bm.csv") %>%
  setnames(c("year","yday","dayLength","prcp","srad","swe","tmax","tmin","vp")) %>%
  .[,daymetTemp:=(tmin+tmax)/2] %>%
  .[,date:=as.Date(paste0(year,"-01-01"))+yday-1] %>%
  .[,.(date,daymetTemp,tmax,tmin)] %>%
  setkey(date)


plot(predict(painDay,newdata=data.frame(yday=1:365))~I(1:365),type='l',col="red",ylim=c(0,3))
points(predict(pineDay,newdata=data.frame(yday=1:365))~I(1:365),type='l')
points(predict(stanDay,newdata=data.frame(yday=1:365))~I(1:365),type='l',col='blue')
points(predict(bla,newdata=data.frame(yday=1:365))~I(1:365),type='l',lty=2)

