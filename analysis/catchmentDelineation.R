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
  .[!is.na(UTMX_E)&UTMX_E!=0]
sites[,":="(snap_x=as.numeric(NA),snap_y=as.numeric(NA))]
sites[SiteID=="3F101",":="(UTMX_E=UTMX_E-20,UTMY_N=UTMY_N-20)]
sites[SiteID=="2F040",":="(UTMX_E=UTMX_E-5,UTMY_N=UTMY_N-4.45)]
sitesSp<-sp::SpatialPointsDataFrame(cbind(sites$UTMX_E,sites$UTMY_N),sites,
                                            proj4string=CRS("+proj=utm +zone=17 +datum=NAD83 +units=m
                             +no_defs +ellps=GRS80 +towgs84=0,0,0"))

# dem <- raster::raster("data/gis/dem/w001001.adf")

#You need to do this and manually set up the mapset projection by this command, and then create a new location with the name of newLocation, use the reference from our dem data

# system('"C:/Program Files/GRASS GIS 7.8/grass78.bat"')

#Initiate the GRASS GIS within R studio


# setwd("C:/Users/ira syarif/Documents/grassdata/raster")

writeVECT(sitesSp,"sites",v.in.ogr_flags="overwrite")

#read raster image
dem <- raster::raster("data/gis/dem/w001001.adf")
#plot(r)
# r
#
# utmX<-sites[SiteID=="1F030",UTMX_E]
# utmY<-sites[SiteID=="1F030",UTMY_N]
#
# #create spatial polygons to crop the raster
# buffer<-10000
#
# ext<- raster::extent(utmX-buffer,utmX+buffer,utmY-buffer,utmY+buffer)
# pol <- as(ext, "SpatialPolygons")
# plot(pol,add=F)
# polCrop<-raster::crop(dem, pol)
# ext2 <- raster::extent(polCrop)
# r2 <- raster(ext2,nrow=nrow(polCrop)/2, ncol=ncol(polCrop)/2)

#resample the cell size if necessary
# new_c <- resample(polCrop, r2, method="ngb")
rast <- as(dem, "SpatialGridDataFrame")
# projection(rast)<-projection(dem)
writeRAST(rast, "rast_img", flags = c("overwrite"))
execGRASS("r.info", map = "rast_img")

#set the region based on the mapset
execGRASS("g.region", raster = "rast_img")
# out_raster <- readRAST("rast_img")
# str(out_raster)
# plot(out_raster)

#watershed delineation
execGRASS("r.watershed", flags="overwrite",
          parameters=list(elevation="rast_img", threshold=10000,
                          drainage= "fdir",stream="upstream", basin="rbasin"))
execGRASS('r.thin', flags='overwrite',
          parameters =  list(input='upstream',
                             output='riv_thin'))
execGRASS("r.to.vect", flags='overwrite',
          parameters = list(input="riv_thin",
                            output="streams", type="line"))
# execGRASS('r.to.vect', flags='overwrite',
#           parameters =  list(input='rbasin',
#                              output='catchments', type="area"))
execGRASS('v.out.ogr',flags=c('overwrite'),parameters=list(input='streams',
                                                           output="data/gis/streams.shp",type="line",format="ESRI_Shapefile"))



# Snap points to nearest stream vector created with the flow accumulation
# execGRASS("v.db.addcolumn",map="sites", columns="snap_x DOUBLE,snap_y DOUBLE")
execGRASS("v.distance",from="sites",to="streams",output="connectors",
          upload="to_x,to_y", column="snap_x,snap_y",flags="overwrite")
snappedSites<-readVECT("sites") %>% data.frame()
snappedSites<-sp::SpatialPointsDataFrame(cbind(snappedSites$snap_x,snappedSites$snap_y),snappedSites,
                            proj4string=CRS("+proj=utm +zone=17 +datum=NAD83 +units=m
                             +no_defs +ellps=GRS80 +towgs84=0,0,0"))
for(s in which(snappedSites$SiteID %in% c("2F040","3F101"))){
  execGRASS("r.water.outlet",flags="overwrite",
            input="fdir",output=paste0("catch_",snappedSites$SiteID[s]),
            coordinates=c(snappedSites$snap_x[s],snappedSites$snap_y[s]))
  execGRASS('r.to.vect', flags='overwrite',input=paste0('catch_',snappedSites$SiteID[s]),
                               output=paste0('vec_',snappedSites$SiteID[s]), type="area")
  cat(s,"\n")
}

catchments<-do.call(rbind,
                    lapply(snappedSites$SiteID,function(x){
                      a<-readVECT(paste0("vec_",x))
                      a$SiteID<-x
                      return(a)}))

wsArea<-data.table(SiteID=catchments$SiteID,wsArea=area(catchments)) %>%
  .[,.(wsArea=sum(wsArea)/1000000),SiteID] %>%
  setkey(SiteID)

# for(s in c("1F219","1F234","1F231","1F233","1F236","2F221","2F222","2F242",
#   "2F244","2F247","2F255","3F206","3F259","3F261")){
#
#   newExtent<-extent(catchments[catchments$SiteID==s,])
#   # newExtent<-extent(sites[SiteID==sTemp,UTMX_E]-100,sites[SiteID==sTemp,UTMX_E]+100,
#   #                   sites[SiteID==sTemp,UTMY_N]-100,sites[SiteID==sTemp,UTMY_N]+100)
#   newExtent[c(1,3)]<-newExtent[c(1,3)]-50
#   newExtent[c(2,4)]<-newExtent[c(2,4)]+50
#   croppedRast<-crop(dem,newExtent) %>%
#     as("SpatialGridDataFrame")
#   plot(croppedRast,col=colorRampPalette(c("black","white"))(100),main=s)
#   plot(catchments[catchments$SiteID==s,],add=T,border='red')
#   # plot(a, add=T,border='red',lwd=2)
#   plot(streams, add=T, col="blue")
#   plot(snappedSites[snappedSites$SiteID==s,],pch=19,col='yellow',add=T)
#
# }

# execGRASS('v.out.ogr', flags=c('overwrite'),
#           parameters=list(input='catch',
#                           output="area.shp",type="area",
#                           format="ESRI_Shapefile"))

#
#
# shapefile("streams.shp") -> streams
# shapefile("area.shp") -> a
#
# sTemp<-"2F040"
# newExtent<-extent(catchments[catchments$SiteID==sTemp,])
# # newExtent<-extent(sites[SiteID==sTemp,UTMX_E]-100,sites[SiteID==sTemp,UTMX_E]+100,
# #                   sites[SiteID==sTemp,UTMY_N]-100,sites[SiteID==sTemp,UTMY_N]+100)
# newExtent[c(1,3)]<-newExtent[c(1,3)]-50
# newExtent[c(2,4)]<-newExtent[c(2,4)]+50
# croppedRast<-crop(dem,newExtent) %>%
#   as("SpatialGridDataFrame")
# plot(croppedRast,col=colorRampPalette(c("black","white"))(100))
# plot(catchments[catchments$SiteID==sTemp,],add=T,border='red')
# # plot(a, add=T,border='red',lwd=2)
# plot(s, add=T, col="blue")
# points(snappedSites[snappedSites$SiteID==sTemp,"UTMX_E"],
#        snappedSites[snappedSites$SiteID==sTemp,"UTMY_N"],col='yellow',pch=1,cex=1)
# points(snappedSites[snappedSites$SiteID==sTemp,"snap_x"],
#        snappedSites[snappedSites$SiteID==sTemp,"snap_y"],col='yellow',pch=19,cex=1)
#
# bla<-rbind(sites[SiteID=="2F040",.(UTMX_E,UTMY_N)],
#            sites[SiteID=="2F040",.(UTMX_E,UTMY_N)],
#            sites[SiteID=="2F040",.(UTMX_E,UTMY_N)])
# bla[1,":="(UTMX_E=UTMX_E-5,UTMY_N=UTMY_N-4.45)]
# bla[2,":="(UTMX_E=UTMX_E+10,UTMY_N=UTMY_N-10)]
# bla<-sp::SpatialPointsDataFrame(cbind(bla$UTMX_E,bla$UTMY_N),bla,
#                                   proj4string=CRS("+proj=utm +zone=17 +datum=NAD83 +units=m
#                              +no_defs +ellps=GRS80 +towgs84=0,0,0"))
# plot(bla,add=F,pch=19,col='red')
# plot(streams, add=T, col="blue")

wsArea[,":="(pctSili=0,pctBasa=0,pctGran=0)]


bedrock<-readOGR(dsn="C:/Users/echildress/Documents/mapShen/data/gis",layer="Bdrck_2009",verbose=F)
for(s in wsArea$SiteID){
  clippedBedrock<-suppressWarnings(raster::intersect(bedrock,catchments[catchments$SiteID==s,]))
  bedrockProps<-data.table(geo=clippedBedrock$MapUnit,area=area(clippedBedrock)) %>%
    .[,sum(area),.(geo=substr(geo,1,1))] %>%
    .[,.(pct=V1/sum(V1),geo)]

  for(b in bedrockProps$geo[bedrockProps$geo %in% c("Z","Y","C")]){
    wsArea[SiteID==s,c("pctSili","pctBasa","pctGran")[which(b==c("C","Z","Y"))]:=bedrockProps[geo==b,pct]]
  }
}


wsArea[,majGeol:=c("basaltic","granitic","siliciclastic")[which.max(c(pctBasa,pctGran,pctSili))],SiteID]

setkey(sites,SiteID)

sites<-wsArea[sites]

write.csv(wsArea,"data/wsArea.csv",row.names=F)
