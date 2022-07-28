library(rgrass7)
initGRASS(gisBase='C:/Program Files/GRASS GIS 7.8',
          gisDbase='C:/Users/echildress/Documents/grassdata',
          location="shen",mapset="PERMANENT", override = TRUE)

library(rgdal)
library(mapShen)
library(raster)

use_sp()

sites<-aqData("sites") %>%
  .[AQIN_SiteType %in% c("Primary","Secondary")]
# 
# snappedSites<-readVECT("sites") %>% data.frame()
# snappedSites<-sp::SpatialPointsDataFrame(cbind(snappedSites$snap_x,snappedSites$snap_y),snappedSites,
#                                          proj4string=CRS("+proj=utm +zone=17 +datum=NAD83 +units=m
#                              +no_defs +ellps=GRS80 +towgs84=0,0,0"))


catchments<-do.call(rbind,
                    lapply(sites$SiteID,function(x){
                      a<-readVECT(paste0("vec_",x))
                      a$SiteID<-x
                      return(a)}))
