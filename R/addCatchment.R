#'Adds a catchment identifier to the sites table
#'
#'@return A data.table with site information including catchment id
#'
#'@value Currently returns the sites table without rows for which Lat/Long are NA
#'
#'@export

addCatchment <- function(data){

  # Define the root as the directory of the current package (shenAquatics)
  root <- defineRoot()

  # ws <- readOGR(dsn="C:/Users/echildress/OneDrive - DOI/Documents/mapShen/data/gis","CVIWatershed",verbose=F)
  ws <- st_read(file.path(root, "data", "gis"), layer = "CVIwatershed") %>%
    st_transform(4326) %>%
    st_as_sf() %>%
    st_set_crs(4326)


  sites <- aqData("sites") %>%
    .[!is.na(Lon_n83)&!is.na(Lat_n83)]

  # point <- sp::SpatialPointsDataFrame(cbind(sites$Lon_n83,sites$Lat_n83),sites,
  #                                   proj4string=CRS("+proj=longlat +datum=NAD83 +no_defs"))
  point <- st_as_sf(sites, coords = c("Lon_n83", "Lat_n83"),
                    crs = 4326)

  # out <- over(point,ws)
  out <- st_join(point, ws)

  out$SiteID <- point$SiteID
  out <- data.table(out) %>%
    setkey(SiteID)

  setkey(sites, SiteID)
  setkey(data, SiteID)

  sites[,catchment:=out$HUC_CVI]

  data <- sites[,.(SiteID,catchment)][data]

  return(data)
}
