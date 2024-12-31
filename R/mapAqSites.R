#'Create a map of aquatic monitoring sites within SHEN
#'@return A map with desired sites
#'@param stream A single value or vector containing the quoted names of streams to be mapped. NULL returns all sites that meet other criteria.
#'@param watershed A single value or vector containing the quoted names of watersheds to be mapped. NULL returns all sites that meet other criteria.
#'@param siteId A single value or vector containing the quoted names of siteIDs to be mapped. NULL returns all sites that meet other criteria.
#'@param fishType A vector of site types for fish for which sites should be returned.
#'@param aqinType A vector of site types for aquatic invertebrates for which sites should be returned.
#'@param text Logical indicating whether site names should be added next to points.
#'@param add Logical indicating whether to add to an existing map (TRUE) or create a new map (FALSE).
#'#'@param existing_map If add is TRUE, the existing map to which this plot is added
#'
#'@details This function plots the location of sampling sites for the SHEN aquatic resource monitoring program.
#'
#'@export
mapAqSites <- function(stream = NULL, watershed = NULL, siteId = NULL,
                       fishType = NULL, aqinType = NULL, text = FALSE,
                       add = TRUE, existing_map = NULL,
                       pch = 19, interactive = TRUE, ...) {

  # should move these to the package level

  con <- aqConnector() # establish a database connection
  on.exit({
    odbcClose(con) # ensure the connection is closed when the function exits
    rm(con, envir = .GlobalEnv) # remove con from global environment
  })

  root <- defineRoot() # define project root
  gisDir <- file.path(root, "data/gis/")

  boundary <- st_read(dsn = file.path(root, "data/gis"),
                      layer = "PkBndryPly_ESA_assesment_2021_dissolve") %>%
    select(geometry)
  # boundary <- st_read(paste0(gisDir, "PkBndryPly_ESA_assesment_2021_dissolve.shp")) %>%
  #   select(geometry)

  streams <- st_read(dsn = file.path(root, "data/gis"),
                     layer = "Stream") %>%
    select(STREAM_NAM)
  # streams <- st_read(paste0(gisDir, "Stream.shp")) %>%
  #   select(STREAM_NAM)

  # sites <- dbGetQuery(con, "select * from R_zdd_Sites") %>%
  #   as.data.table() %>%
  #   setkey(SiteID)
  sites <- sqlQuery(con,"select * from R_zdd_Sites") %>%
        data.table() %>%
        setkey(SiteID)

  if (!is.null(stream)) {
    sites <- sites[tolower(STREAM) %in% tolower(stream)]
    streams <- streams[tolower(streams$STREAM_NAM) %in% tolower(stream), ]
  }

  if (!is.null(watershed)) {
    sites <- sites[tolower(Watershed) %in% tolower(watershed)]
  }

  if (!is.null(siteId)) {
    sites <- sites[tolower(SiteID) %in% tolower(siteId)]
  }

  if (!is.null(fishType) | !is.null(aqinType)) {
    sites <- sites[tolower(FISH_SiteType) %in% tolower(fishType) |
                     tolower(AQIN_SiteType) %in% tolower(aqinType)]
  }

  noCoord <- sites[is.na(Lon_n83) | is.na(Lat_n83), SiteID]

  if (length(noCoord) > 0) {
    warning(paste0("Site(s): ", paste(as.character(noCoord), collapse = ", "),
                " skipped due to lacking coordinates in the database"))
  }

  sites <- sites[!is.na(Lon_n83) & !is.na(Lat_n83)]
  point <- st_as_sf(sites[, .(SiteID, Site_Name, STREAM, FISH_SiteType, AQIN_SiteType,
                              Elev_m, PCT_SILI, PCT_GRAN, PCT_BASA, MAJ_GEOL,
                              Lat_n83, Lon_n83, UTMX_E, UTMY_N)], coords = c("UTMX_E", "UTMY_N"))
  st_crs(point) <- st_crs(boundary)

  # col.sites = "MAJ_GEOL" # c("red", "Elev_m")[1] # color for the sites, which may either be a color name (e.g., "red") or a column name of the sites table that defines a numeric or factor variable along which colors should be defined
  # title.col.sites = "Geology" # "Elevation (m)"
  # size.sites = 0.4 # the size to be used for sites, as called by tm_dots
  # col.streams = "blue" # the color to be used for streams
  # lwd.streams = 1 # the lwd to be used for streams
  # col.boundary = "black" # the color to be used for the park boundary
  # lwd.boundary = 2 # the lwd to be used for the park boundary
  # size.text = 0.5 # the size to use for text labels, if text = TRUE
  #
  # map <- existing_map +
  #   tm_shape(boundary) +
  #   tm_borders(col = col.boundary, lwd = lwd.boundary) +
  #   tm_shape(streams) +
  #   tm_lines(col = col.streams, lwd = lwd.streams) +
  #   tm_shape(point) +
  #   tm_dots(col = col.sites, # "Elev_m",
  #           title = title.col.sites, #"Elevation (m)",
  #           size = size.sites) +
  #   tm_layout(legend.position = c("left", "top"))
  # if(text) {
  #   map <- map + tm_shape(point) +
  #     tm_text("SiteID", size = size.text)
  # }
  # print(map)

  tmap_args_list = list(
    tm_dots = list(col = c("red", "Elev_m", "MAJ_GEOL")[1],
                   size = 0.4,
                   title = c("", "Elevation (m)", "Geology")[1]),
    tm_borders = list(col = "black",
                      lwd = 2),
    tm_lines = list(col = "blue",
                    lwd = 1),
    tm_text = list(size = 0.5),
    tm_layout = list(legend.position = c("left", "top"))
  )

  map <- existing_map +
    tm_shape(boundary) + # park boundary
    do.call(tm_borders, tmap_args_list$tm_borders) +
    tm_shape(streams) + # stream lines
    do.call(tm_lines, tmap_args_list$tm_lines) +
    tm_shape(point) + # sites
    do.call(tm_dots, tmap_args_list$tm_dots) +
    tm_layout(legend.position = c("left", "top"))
  if(text) {
    map <- map + tm_shape(point) +
      tm_text("SiteID", size = size.text)
  }
  print(map)

  return(map)

  # odbcClose(con) # explicitly close the connection

}

# Example usage:
# mapAqSites(stream = c("Stream1", "Stream2"))




#' #### BACKUP OF EVAN'S SCRIPT (12/20/2024) ####
#' #'Create a map of aquatic monitoring sites within SHEN
#' #'@return A map with desired sites
#' #'@param stream A single value or vector containing the quoted names of streams to be mapped. NULL returns all sites that meet other criteria.
#' #'@param watershed A single value or vector containing the quoted names of watersheds to be mapped. NULL returns all sites that meet other criteria.
#' #'@param siteId A single value or vector containing the quoted names of siteIDs to be mapped. NULL returns all sites that meet other criteria.
#' #'@param fishType A vector of site types for fish for which sites should be returned.
#' #'@param aqinType A vector of site types for aquatic invertebrates for which sites should be returned.
#' #'@param text Logical indicating whether site names should be added next to points.
#' #'@param add Logical indicating whether to add to an existing map (TRUE) or create a new map (FALSE).
#' #'
#' #'@details This function plots the location of sampling sites for the SHEN aquatic resource monitoring program.
#' #'
#' #'@export
#'
#'
#' mapAqSites<-function(stream=NULL,watershed=NULL,siteId=NULL,fishType=NULL,
#'                      aqinType=NULL,text=F,add=T,pch=19,interactive=T,...){
#'
#'   #should move these to the package level
#'
#'   aqConnector()
#'
#'   gisDir<-"C:/Users/echildress/OneDrive - DOI/Documents/mapShen/data/gis/"
#'   boundary<-st_read(paste0(gisDir,"PkBndryPly_ESA_assesment_2021_dissolve.shp"))
#'   boundary<-boundary[,c()]
#'
#'   streams<-st_read(paste0(gisDir,"Stream.shp")) %>%
#'     .[,c("STREAM_NAM")]
#'
#'   sites<-sqlQuery(con,"select * from R_zdd_Sites") %>%
#'     data.table() %>%
#'     setkey(SiteID)
#'
#'
#'   if(!is.null(stream)){
#'     sites<-sites[tolower(STREAM) %in% tolower(stream)]
#'     streams<-streams[tolower(streams$STREAM_NAM) %in% tolower(stream),]
#'   }
#'
#'   if(!is.null(watershed)){
#'     sites<-sites[tolower(Watershed) %in% tolower(watershed)]
#'   }
#'
#'   if(!is.null(siteId)){
#'     sites<-sites[tolower(SiteID) %in% tolower(siteId)]
#'   }
#'
#'   if(!is.null(fishType)|!is.null(aqinType)){
#'     sites<-sites[tolower(FISH_SiteType) %in% tolower(fishType)|
#'                  tolower(AQIN_SiteType) %in% tolower(aqinType)]
#'   }
#'
#'   noCoord<-sites[is.na(Lon_n83)|is.na(Lat_n83),SiteID]
#'
#'   if(length(noCoord)>0){
#'     warning(cat("Site(s): ",paste(as.character(noCoord),collapse=", "),
#'                 " skipped due to lacking coordinates in the database"))
#'   }
#'
#'   sites<-sites[!is.na(Lon_n83)&!is.na(Lat_n83)]
#'   point<-st_as_sf(sites[,.(SiteID,Site_Name,STREAM,FISH_SiteType,AQIN_SiteType,
#'                            Elev_m,PCT_SILI,PCT_GRAN,PCT_BASA,MAJ_GEOL,
#'                            Lat_n83,Lon_n83,UTMX_E,UTMY_N)],coords=c("UTMX_E","UTMY_N"))
#'   st_crs(point)<-st_crs(boundary)
#'
#'   if(interactive){
#'     tmap_mode("view")
#'     if(text){
#'       tm_shape(point)+tm_text("SiteID")+tm_shape(streams)+tm_lines(col='blue')
#'     } else{
#'       tm_shape(point)+tm_dots(col='red')+tm_shape(streams)+tm_lines(col='blue')
#'     }} else{
#'       if(text){
#'         tm_shape(point)+tm_text("SiteID")+tm_shape(streams)+tm_lines(col='blue')
#'       } else{
#'         tm_shape(point)+tm_points(col='red')+tm_shape(streams)+tm_lines(col='blue')
#'     }}
#'
#' }
