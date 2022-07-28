#'Adds a catchment identifier to the sites table
#'
#'@return A data.table with site information including catchment id
#'
#'@value Currently returns the sites table without rows for which Lat/Long are NA
#'
#'@export

addCatchment<-function(data){
  ws<-readOGR(dsn="C:/Users/echildress/Documents/mapShen/data/gis","CVIWatershed",verbose=F)

  sites<-aqData("sites") %>%
    .[!is.na(Lon_n83)&!is.na(Lat_n83)]

  point<-sp::SpatialPointsDataFrame(cbind(sites$Lon_n83,sites$Lat_n83),sites,
                                    proj4string=CRS("+proj=longlat +datum=NAD83 +no_defs"))

  out<-over(point,ws)
  out$SiteID <- point$SiteID
  out <- data.table(out) %>%
    setkey(SiteID)

  setkey(sites,SiteID)
  setkey(data,SiteID)

  sites[,catchment:=out$HUC_CVI]

  data<-sites[,.(SiteID,catchment)][data]

  return(data)
}
