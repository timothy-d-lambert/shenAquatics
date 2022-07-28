library(mapShen)
library(plotHacks)
bear<-fread("C:/Users/echildress/Desktop/bearHarvest.txt")
setnames(bear,c("county","h2019","h2020","h2021"))
bear[,prop:=h2021/h2019] %>%
  .[prop==Inf,prop:=NA] %>%
  .[h2019<10,prop:=NA] %>%
  .[,logProp:=round(log(prop),1)] %>%
  setkey(logProp)

# bear[logProp<log(0.25),color:=""]

colors<-data.table(prop=round(seq(-1.8,0.7,0.1),1),
                   color=colorRampPalette(c(
                     "red","red","pink","white","blue"))(
                     length(seq(-1.8,0.7,0.1))
                   )) %>%
  setkey(prop)

bear<-colors[bear]


# bear<-melt(bear,id.vars="county") %>%
#   data.table() %>%
#   .[,year:=as.numeric(substr(variable,2,5))] %>%
#   .[,.(county,year,value)] %>%
#   setnames("value","harvest")

va<-readOGR(dsn="C:/Users/echildress/Documents/mapShen/data/gis",
                  layer="VirginiaCounty",verbose=F)

for(co in bear$county){
  va[va$NAME==co,c("h2019","h2021","prop","color")]<-
    bear[county==co,c("h2019","h2021","prop","color"),with=F]
}

tiff.par("C:/Users/echildress/Desktop/bearHarvestChange.tif",
         mar=c(0,0.5,1.5,0),cex.main=1.5)
plot(va,col=va$color,main="Bear Harvest Change (2019 to 2021)")
plot(va[is.na(va$color),],col="black",add=T)
legend('topleft',legend=c("+85%","","","","stable",
                          "","","","-85%"),
       fill=colorRampPalette(c('blue','white','red'))(9),
       border=NA,y.intersp=0.45)
dev.off()
