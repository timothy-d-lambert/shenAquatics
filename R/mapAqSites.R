#'Create a map of aquatic monitoring sites within SHEN
#'@return A map with desired sites
#'@param stream A single value or vector containing the quoted names of streams to be mapped. NULL returns all sites that meet other criteria.
#'@param watershed A single value or vector containing the quoted names of watersheds to be mapped. NULL returns all sites that meet other criteria.
#'@param siteId A single value or vector containing the quoted names of siteIDs to be mapped. NULL returns all sites that meet other criteria.
#'@param fishType A vector of site types for fish for which sites should be returned.
#'@param aqinType A vector of site types for aquatic invertebrates for which sites should be returned.
#'@param text Logical indicating whether site names should be added next to points.
#'@param add Logical indicating whether to add to an existing map (TRUE) or create a new map (FALSE).
#'
#'@details This function plots the location of sampling sites for the SHEN aquatic resource monitoring program.
#'
#'@export


mapAqSites<-function(stream=NULL,watershed=NULL,siteId=NULL,fishType=NULL,
                     aqinType=NULL,text=F,add=T,pch=19,interactive=T,...){

  #should move these to the package level

  aqConnector()

  gisDir<-"C:/Users/echildress/OneDrive - DOI/Documents/mapShen/data/gis/"
  boundary<-st_read(paste0(gisDir,"PkBndryPly_ESA_assesment_2021_dissolve.shp"))
  boundary<-boundary[,c()]

  streams<-st_read(paste0(gisDir,"Stream.shp")) %>%
    .[,c("STREAM_NAM")]

  sites<-sqlQuery(con,"select * from R_zdd_Sites") %>%
    data.table() %>%
    setkey(SiteID)


  if(!is.null(stream)){
    sites<-sites[tolower(STREAM) %in% tolower(stream)]
    streams<-streams[tolower(streams$STREAM_NAM) %in% tolower(stream),]
  }

  if(!is.null(watershed)){
    sites<-sites[tolower(Watershed) %in% tolower(watershed)]
  }

  if(!is.null(siteId)){
    sites<-sites[tolower(SiteID) %in% tolower(siteId)]
  }

  if(!is.null(fishType)|!is.null(aqinType)){
    sites<-sites[tolower(FISH_SiteType) %in% tolower(fishType)|
                 tolower(AQIN_SiteType) %in% tolower(aqinType)]
  }

  noCoord<-sites[is.na(Lon_n83)|is.na(Lat_n83),SiteID]

  if(length(noCoord)>0){
    warning(cat("Site(s): ",paste(as.character(noCoord),collapse=", "),
                " skipped due to lacking coordinates in the database"))
  }

  sites<-sites[!is.na(Lon_n83)&!is.na(Lat_n83)]
  point<-st_as_sf(sites[,.(SiteID,Site_Name,STREAM,FISH_SiteType,AQIN_SiteType,
                           Elev_m,PCT_SILI,PCT_GRAN,PCT_BASA,MAJ_GEOL,
                           Lat_n83,Lon_n83,UTMX_E,UTMY_N)],coords=c("UTMX_E","UTMY_N"))
  st_crs(point)<-st_crs(boundary)



  # point<-sp::SpatialPointsDataFrame(cbind(sites$Lon_n83,sites$Lat_n83),sites,
  #                                   proj4string=CRS("+proj=longlat +datum=NAD83"))
  #
  # point<-sp::spTransform(point,
  #                        CRS("+proj=utm +zone=17 +datum=NAD83 +units=m
  #                            +no_defs +ellps=GRS80 +towgs84=0,0,0"))

  if(interactive){
    tmap_mode("view")
    if(text){
      tm_shape(point)+tm_text("SiteID")+tm_shape(streams)+tm_lines(col='blue')
    } else{
      tm_shape(point)+tm_dots(col='red')+tm_shape(streams)+tm_lines(col='blue')
    }} else{
      if(text){
        tm_shape(point)+tm_text("SiteID")+tm_shape(streams)+tm_lines(col='blue')
      } else{
        tm_shape(point)+tm_points(col='red')+tm_shape(streams)+tm_lines(col='blue')
    }}




  # if(bbox(point)[1,1]==bbox(point)[1,2]&add==F){
  #   e<-raster::extent(bbox(point))
  #   e[c(1,3)]<-e[c(1,3)]-200
  #   e[c(2,4)]<-e[c(2,4)]+200
  #   r<-raster::raster(e)
  #   dim(r)<-c(1,1)
  #   raster::projection(r)<-CRS(proj4string(point))
  #   g<-as(r,"SpatialPolygonsDataFrame")
  #   sp::plot(g,border=NA,col=NA)
  #   sp::plot(point,pch=pch,add=T,...)
  #
  # } else {
  #   sp::plot(point,pch=pch,add=add,...)
  # }
  #
  # if(text){
  #   text(point$UTMX_E+70,point$UTMY_N,labels=point$SiteID)
  # }

}
