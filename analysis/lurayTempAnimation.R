
library(dplyr)
library(tidyr)
library(ggplot2)
library(animation)
library(data.table)

weather<-fread("data/lurayWeather.csv") %>%
  .[,DATE:=as.Date(DATE)] %>%
  .[,.(DATE,PRCP,TMAX,TMIN)] %>%
  setnames(c("date","prcp","tMax","tMin")) %>%
  setkey(date)

weather[,":="(year=year(date),month=month(date),yday=yday(date))]

monthly<-weather[,.(tMax=mean(tMax,na.rm=T),tMin=mean(tMin,na.rm=T),
                    prcp=sum(prcp),
                    sdTMax=sd(tMax,na.rm=T),sdTMin=sd(tMin,na.rm=T)),.(month,year)]

mo <- months(seq(as.Date("1910/1/1"), as.Date("1911/1/1"), "months"))
mo <- gsub("(^...).*", "\\1", mo)

saveGIF({

  for(i in 1941:2019){
    print(ggplot(monthly %>% filter(year <= i),
                 aes(x=month, y=tMin, color=year, group=year)) +
            geom_line(lwd=1) +
            ylim(0,70) +
            scale_color_gradient(low="blue", high="yellow", limits=c(1941, 2019), guide="none") +
            geom_hline(yintercept=50, color="black", lty=2) +
            geom_hline(yintercept=90, color="black", lty=2) +
            coord_polar() +
            annotate(x=1, y=-1.5, geom="text", label=i) +
            annotate(x=1, y=30, geom="label", label="50F", fill="white", label.size=0) +
            annotate(x=1, y=70, geom="label", label="100F", fill="white", label.size=0) +
            ggtitle("Luray Temperature Change 1941-2019") +
            scale_x_continuous(labels=mo, breaks=1:13) +
            ylab("") + xlab("")

    )}
}, interval=0.25)
