# source("analysis/grassSetup.R")
library(rgrass7)
initGRASS(gisBase='C:/Program Files/GRASS GIS 7.8',
          gisDbase='C:/Users/echildress/Documents/grassdata',
          location="shen",mapset="PERMANENT", override = TRUE)

library(rgdal)
library(mapShen)
library(raster)

use_sp()

sites<-aqData("sites") %>%
  .[!is.na(FISH_SiteType)] %>%
  setkey(SiteID)

wsAttr<-fread("C:/Users/echildress/Documents/mapShen/data/wsArea.csv") %>%
  setkey(SiteID)

sites<-wsAttr[sites]


dem <- raster::raster("data/gis/dem/w001001.adf")

streams <- shapefile("data/gis/streams.shp")
boundary<-readOGR(dsn="C:/Users/echildress/Documents/mapShen/data/gis",layer="PkBndryPly",verbose=F)
bedrock<-readOGR(dsn="C:/Users/echildress/Documents/mapShen/data/gis",layer="Bdrck_2009",verbose=F)

streams<-raster::intersect(streams,boundary)

nPoints<-1000
streamPoints<-sf::st_sample(st_as_sf(streams),nPoints) %>%
  .[!sf::st_is_empty(.),,drop=F] %>%
  st_cast(to="POINT") %>%
  as_Spatial() %>%
  as("SpatialPointsDataFrame")
streamPoints@data<-data.frame(id=1:1000)
streamPoints$elev<-extract(dem,streamPoints)
streamPoints$wsArea<-as.numeric(NA)
coords<-coordinates(streamPoints)
for(i in 23:nPoints){
    execGRASS("r.water.outlet",flags="overwrite",
              input="fdir",output="catchTemp",
              coordinates=c(coords[i,1],coords[i,2]))
    execGRASS('r.to.vect', flags='overwrite',input="catchTemp",
              output='vecTemp', type="area")
    catchTemp<-readVECT("vecTemp")
    streamPoints[i,"wsArea"]<-sum(area(catchTemp))/1000000

    clippedBedrock<-suppressWarnings(raster::intersect(bedrock,catchTemp))
    bedrockProps<-data.table(geo=clippedBedrock$MapUnit,area=area(clippedBedrock)) %>%
      .[,sum(area),.(geo=substr(geo,1,1))] %>%
      .[,.(pct=V1/sum(V1),geo)]

    for(b in bedrockProps$geo[bedrockProps$geo %in% c("Z","Y","C")]){
      streamPoints[i,c("pctSili","pctBasa","pctGran")[which(b==c("C","Z","Y"))]]<-bedrockProps[geo==b,pct]
    }
    cat(i,"\n")
}

streamPoints[is.na(streamPoints$pctBasa),"pctBasa"]<-0
streamPoints[is.na(streamPoints$pctGran),"pctGran"]<-0
streamPoints[is.na(streamPoints$pctSili),"pctSili"]<-0

tiff.par("figures/siteCharacteristicsVsBackground.tif",width=9)
layout(mat=matrix(c(1:15),nrow=3,ncol=5,byrow = T),
       widths=c(1,4,4,4,4),heights=c(1,6,6))
par(mar=c(0,0,0,0))
plot(NA,ylim=c(1,1),xlim=c(1,1),axes=F,xlab="",ylab="")
for(g in c("combined","pctBasa","pctGran","pctSili")){
  plot(NA,ylim=c(1,1),xlim=c(1,1),axes=F,xlab="",ylab="")
  text(1,1,bquote(bold(.(c("Combined","Basaltic","Granitic","Siliciclastic")[
    which(g==c("combined","pctBasa","pctGran","pctSili"))]))))
}

for(s in c("FISH_SiteType","AQIN_SiteType")){
  plot(NA,ylim=c(1,1),xlim=c(1,1),axes=F,xlab="",ylab="")
  text(1,1.1,bquote(bold(.(c("Fish","Macro")[
    which(s==c("FISH_SiteType","AQIN_SiteType"))]))))
  par(mar=c(2.5,3.5,0,0))


  plot(elev~log(wsArea),data=streamPoints,pch=19,col='gray',
       ylab="",xlab=bquote(ln(Watershed~Area~"in"~km^2)))
  title(ylab="Elevation (m)",line=2.5)

  points(Elev_m~log(wsArea),
         data=sites[get(s) %in% c("Secondary")],
         pch=19,col="blue",cex=1)
  points(Elev_m~log(wsArea),
         data=sites[get(s) %in% c("Primary")],
         pch=19,col="red",cex=1)

  for(g in c("pctBasa","pctGran","pctSili")){
    plot(elev~log(wsArea),data=streamPoints,pch=NA,
       ylab="",xlab=bquote(ln(Watershed~Area~"in"~km^2)))
    title(ylab="Elevation (m)",line=2.5)

    if(s=="FISH_SiteType"&g=="pctSili"){
      legend(1,1000,c("Random","Primary","Secondary"),col=c("gray","red","blue"),
             pch=19,bty='n')
    }

    points(elev~log(wsArea),data=streamPoints[streamPoints@data[,g]>0.8,],col='gray',pch=19)
    points(Elev_m~log(wsArea),
           data=sites[get(s) %in% c("Secondary")&get(g)>0.8],
           pch=19,col="blue",cex=1)
    points(Elev_m~log(wsArea),
           data=sites[get(s) %in% c("Primary")&get(g)>0.8],
           pch=19,col="red",cex=1)
  }
  par(mar=c(0,0,0,0))
}
dev.off()

points(elev~log(wsArea),data=streamPoints[streamPoints$pctBasa>0.8,],col='gray',pch=19)
points(Elev_m~log(wsArea),
       data=sites[FISH_SiteType %in% c("Secondary")&pctBasa>0.8],
       pch=19,col="blue",cex=2)
points(Elev_m~log(wsArea),
       data=sites[FISH_SiteType %in% c("Primary")&pctBasa>0.8],
       pch=19,col="red",cex=2)

saveRDS(streamPoints,"data/randomStreamPoints.rds")
