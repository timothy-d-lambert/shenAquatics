library(readxl)
library(data.table)
library(dplyr)
library(plotHacks)


tempDir<-"O:/Working/WATER/stream temperature loggers 2010/HOBO Temp Logger Data/Truncated Files for start and end times/Excel Formatted Data"
tempFiles<-list.files(tempDir)


for(f in tempFiles){
  tempData<-read_excel(paste0(tempDir,"/",f),skip=1) %>%
    .[,(2:3)] %>%
    data.table() %>%
    setnames(c("datetime","tempF")) %>%
    .[,temp:=(tempF-32)*5/9] %>%
    .[datetime<=as.POSIXct("2010-09-15")]

  streamName<-strsplit(tolower(f),"_trunk")[[1]][1] %>%
    strsplit("_") %>%
    .[[1]] %>%
    .[-1] %>%
    paste(collapse=" ")

  tempVar<-tempData[,.(sd=sd(temp)),.(date=as.Date(datetime))]

  tiff.par(paste0("figures/temps2010",streamName,".tif"),
           mfrow=c(2,1),width=4,height=8,mar=c(2,2.5,1,0))

  plot(temp~datetime,data=tempData,type='l',ylim=c(10,30),
       xlab="",ylab="Temp (C)",main=streamName)
  plot(sd~date,data=tempVar,type='l',ylim=c(0,2),
       ylab="Daily SD of Temp",xlab="")
  abline(h=median(tempVar$sd))

  dev.off()
}
