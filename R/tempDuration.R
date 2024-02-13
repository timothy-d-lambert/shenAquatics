tempDuration<-function(data,datetimeCol="datetime",tempCol="temp",fun=mean,plot=T){
  if(max(data[,tempCol,with=F],na.rm=T)>50){
    data[,tempCol]<-(data[,tempCol,with=F]-32)*5/9
  }
  daily<-data[,.(temp=fun(get(tempCol))),as.Date(get(datetimeCol))]

  days<-1:63
  temp<-NULL

  for(d in days){
    temp[d]<-daily[,frollmean(temp,d,fill=0)] %>% max()
  }

  wehrlyTemps<-seq(20,28,0.1)
  if(fun(1:2)==1.5){
    wehrlyLimit<-3*10^10*exp(-0.9559*wehrlyTemps)} else{
      wehrlyLimit<-4*10^10*exp(-0.8856*wehrlyTemps)
    }

  plot(days~temp,type='l',
       xlim=c(min(c(temp,wehrlyTemps)),
              max(c(temp,wehrlyTemps))))
  points(wehrlyLimit~wehrlyTemps,type='l',lty=2)

}
