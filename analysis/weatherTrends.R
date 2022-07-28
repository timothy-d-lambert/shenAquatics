library(data.table)
library(dplyr)
library(plotHacks)

weather<-fread("data/lurayWeather.csv") %>%
  .[,DATE:=as.Date(DATE)] %>%
  .[,.(DATE,PRCP,TMAX,TMIN)] %>%
  setnames(c("date","prcp","tMax","tMin")) %>%
  setkey(date)

weather[,":="(year=year(date),month=month(date),yday=yday(date))]

monthly<-weather[year>1979,.(tMax=mean(tMax,na.rm=T),tMin=mean(tMin,na.rm=T),
                    prcpMed=log(median(prcp,na.rm=T)+1),
                    prcpMax=log(max(prcp,na.rm=T)+1),
                    prcp=sum(prcp),
                    sdTMax=sd(tMax,na.rm=T),sdTMin=sd(tMin,na.rm=T)),.(month,year)]
getTrend<-function(formula,return="rSquared"){
  a<-summary(lm(formula))
  out<-c(a$r.squared,
         a$coefficients[8],
         a$coefficients[2],
         a$coefficients[4])
  out[which(return==c("rSquared","pValue","slope","slope SE"))]

}

prcpMedTrends<-monthly[,.(rsq=getTrend(prcpMed~year),
                       p=getTrend(prcpMed~year,"pValue"),
                       slope=getTrend(prcpMed~year,"slope"),
                       slopeSE=getTrend(prcpMed~year,"slope SE")),month]

prcpMaxTrends<-monthly[,.(rsq=getTrend(prcpMax~year),
                          p=getTrend(prcpMax~year,"pValue"),
                          slope=getTrend(prcpMax~year,"slope"),
                          slopeSE=getTrend(prcpMax~year,"slope SE")),month]


maxTrends<-monthly[,.(rsq=getTrend(tMax~year),
                      p=getTrend(tMax~year,"pValue"),
                      slope=getTrend(tMax~year,"slope"),
                      slopeSE=getTrend(tMax~year,"slope SE")),month]

minTrends<-monthly[,.(rsq=getTrend(tMin~year),
                      p=getTrend(tMin~year,"pValue"),
                      slope=getTrend(tMin~year,"slope"),
                      slopeSE=getTrend(tMin~year,"slope SE")),month]

plot(slope~month,data=minTrends,pch=ifelse(p<0.05,19,1),ylim=c(-0.05,0.3))
with(minTrends,error.bar(month,slope,slopeSE*1.96))
abline(h=0,col='gray',lty=2)


plot(slope~month,data=maxTrends,pch=ifelse(p<0.05,19,1),ylim=c(-0.3,0.3))
with(maxTrends,error.bar(month,slope,slopeSE*1.96))
abline(h=0,col='gray',lty=2)

plot(slope~month,data=prcpMedTrends,pch=ifelse(p<0.05,19,1),ylim=c(-0.005,0.005))
with(prcpMedTrends,error.bar(month,slope,slopeSE*1.96))
abline(h=0,col='gray',lty=2)


plot(slope~month,data=prcpMaxTrends,pch=ifelse(p<0.05,19,1),ylim=c(-0.02,0.02))
with(prcpMaxTrends,error.bar(month,slope,slopeSE*1.96))
abline(h=0,col='gray',lty=2)

