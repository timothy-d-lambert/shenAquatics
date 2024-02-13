#'Get site data from database and make it a sf object for plotting
#'
#'@details Simply a quicker way to get site data for plotting
#'
#'@export


getSpatialSites<-function(){

  dat<-aqData("sites") %>%
  data.table() %>%
  setkey(SiteID) %>%
  .[!is.na(Lon_n83)&!is.na(Lat_n83)]

  dat<-st_as_sf(dat[,.(SiteID,Site_Name,STREAM,FISH_SiteType,AQIN_SiteType,
                    Elev_m,PCT_SILI,PCT_GRAN,PCT_BASA,MAJ_GEOL,
                    Lat_n83,Lon_n83,UTMX_E,UTMY_N)],coords=c("UTMX_E","UTMY_N"))

  st_crs(dat)<-st_crs(st_read("C:/Users/echildress/OneDrive - DOI/Documents/mapShen/data/gis/PkBndryPly_ESA_assesment_2021_dissolve.shp"))

  return(dat)
}
