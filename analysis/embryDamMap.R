library(ggmap)
library(data.table)
library(mapShen)
library(plotHacks)

register_google("AIzaSyAMPy_HnX2UjZ5CWD163m4yswi9Fg2yX0g")

sites<-aqData("sites") %>%
  setkey(SiteID)

sites<-sites[Site_Name %in% c("Rapidan River","Hughes River","Staunton River","Rose River","Thornton River, N. Fork","Piney River")]



boundary<-readOGR(dsn="C:/Users/echildress/Documents/mapShen/data/gis",layer="PkBndryPly",verbose=F) %>%
  spTransform(CRS("+proj=longlat +datum=WGS84"))

embry<- data.table(lat=38.322720,lon= -77.490857)


  tiff.par("C:/Users/echildress/Documents/presentations/embreyDam.tif",width=4,height=4)

  map<-ggmap(get_googlemap(center=c(lon= -77.930991,
                                    lat= 38.467413),
                           zoom=9),extent="device") +
    geom_polygon(data=fortify(boundary),aes(x=long,y=lat,group=group),fill="darkgreen",color="darkgreen",alpha=0.4) +
    geom_point(data=embry, aes(x=lon,y=lat),color='red',size=5) +
    geom_point(data=sites, aes(x=Lon_n83,y=Lat_n83),color='blue',size=2)

  print(map)

  dev.off()

  tiff.par("C:/Users/echildress/Documents/presentations/embreyDam2.tif",width=4,height=4)

  map<-ggmap(get_googlemap(center=c(lon= -77.930991,
                                    lat= 38.467413),
                           zoom=9),extent="device") +
    geom_polygon(data=fortify(boundary),aes(x=long,y=lat,group=group),fill="darkgreen",color="darkgreen",alpha=0.4) +
    geom_point(data=embry, aes(x=lon,y=lat),color='red',size=5)

  print(map)

  dev.off()
