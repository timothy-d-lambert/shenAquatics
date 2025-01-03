#'Create a map of aquatic monitoring sites within SHEN
#'@return A map with desired sites
#'@param stream A single value or vector containing the quoted names of streams to be mapped. NULL returns all sites that meet other criteria.
#'@param watershed A single value or vector containing the quoted names of watersheds to be mapped. NULL returns all sites that meet other criteria.
#'@param siteId A single value or vector containing the quoted names of siteIDs to be mapped. NULL returns all sites that meet other criteria.
#'@param fishType A vector of site types for fish for which sites should be returned.
#'@param aqinType A vector of site types for aquatic invertebrates for which sites should be returned.
#'@param text Logical indicating whether site names should be added next to points.
#'@param base_map A base map on which this map should be overlaid; the default value of NULL creates a new map (i.e., there is no base map)
#'@param tmap_args_list A list of argument lists to be passed on to tm_dots (for sites), tm_borders (for park boundaries), tm_lines (for stream lines), tm_text (for site labels), and tm_layout (for legend). See details for examples.
#'
#'@details This function plots the location of sampling sites for the SHEN aquatic resource monitoring program.
#'
#' For the tmap_args_list contains named lists to be passed along to functions of the same name. The tm_dots arguments may include colors as a name (e.g., "red"), or as a column name of the sites table that defines a numeric or factor variable (e.g., "Elev_m" or "MAJ_GEOL"). See default arguments for arguments that may be useful to modify.
#'
#'@export
mapAqSites <- function(stream = NULL, watershed = NULL, siteId = NULL,
                       fishType = NULL, aqinType = NULL, text = FALSE,
                       base_map = NULL,
                       ## UNDER CONSTRUCTION: add color_key
                       # color_key = NA,
                       # color_key = data.frame(siteId = )
                       xlim_bbox = c(0,1), ylim_bbox = c(0,1),
                       tmap_args_list = vector(mode = "list", length = 0)
                       # list(
                       #   tm_dots = list(col = c("red", "Elev_m", "MAJ_GEOL")[1],
                       #                  size = 0.4,
                       #                  title = c("", "Elevation (m)", "Geology")[1]),
                       #   tm_borders = list(col = "black",
                       #                     lwd = 2),
                       #   tm_lines = list(col = "cornflowerblue",
                       #                   lwd = 1),
                       #   tm_text = list(size = 0.5),
                       #   tm_layout = list(legend.position = c("left", "top"))
                       # )
) {

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

  streams <- st_read(dsn = file.path(root, "data/gis"),
                     layer = "Stream") %>%
    select(STREAM_NAM)

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

  # UNDER CONSTRUCTION: add color variable, if provided


  point <- st_as_sf(sites[, .(SiteID, Site_Name, STREAM, FISH_SiteType, AQIN_SiteType,
                              Elev_m, PCT_SILI, PCT_GRAN, PCT_BASA, MAJ_GEOL,
                              Lat_n83, Lon_n83, UTMX_E, UTMY_N)], coords = c("UTMX_E", "UTMY_N"))
  st_crs(point) <- st_crs(boundary)


  ## Define plotting arguments
  # Define default plotting arguments
  default_tm_dots <- list(
    col = c("red3", "Elev_m", "MAJ_GEOL")[1],
    size = 0.3,
    title = c("", "Elevation (m)", "Geology")[1]
  )
  default_tm_borders <- list(
    col = "black",
    lwd = 2
  )
  default_tm_lines <- list(
    col = "cornflowerblue",
    lwd = 1
  )
  default_tm_text <- list(
    size = 0.5
  )
  default_tm_layout <- list(
    legend.position = c("left", "top"),
    legend.width = 0.6
  )

  # Combine user-defined lists with default lists, retaining default values
  `%||%` <- function(a, b) if (!is.null(a)) a else b # define a custom `%||%` operator to handle NULL values
  tmap_args_list$tm_dots <- modifyList(
    default_tm_dots, tmap_args_list$tm_dots %||% list())
  tmap_args_list$tm_borders <- modifyList(
    default_tm_borders, tmap_args_list$tm_borders %||% list())
  tmap_args_list$tm_lines <- modifyList(
    default_tm_lines, tmap_args_list$tm_lines %||% list())
  tmap_args_list$tm_text <- modifyList(
    default_tm_text, tmap_args_list$tm_text %||% list())
  tmap_args_list$tm_layout <- modifyList(
    default_tm_layout, tmap_args_list$tm_layout %||% list())


  # Extend bbox limits (if necessary)
  bbox_new <- st_bbox(streams) # current bounding box
  if(!all(xlim_bbox == c(0,1),
          ylim_bbox == c(0,1))) {
    xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
    yrange <- bbox_new$ymax - bbox_new$ymin # range of y values
    bbox_new["xmin"] <- bbox_new["xmin"] + (xlim_bbox[1] * xrange)
    bbox_new["xmax"] <- bbox_new["xmax"] + ((xlim_bbox[2]-1) * xrange)
    bbox_new["ymin"] <- bbox_new["ymin"] + (ylim_bbox[1] * yrange)
    bbox_new["ymax"] <- bbox_new["ymax"] + ((ylim_bbox[2]-1) * yrange)
  }
  bbox_new <- bbox_new %>%  # take the bounding box ...
    st_as_sfc() # ... and make it a sf polygon

  # Make the map
  map <- base_map +
    tm_shape(streams, bbox = bbox_new) + # stream lines
    do.call(tm_lines, tmap_args_list$tm_lines) +
    tm_shape(boundary) + # park boundary
    do.call(tm_borders, tmap_args_list$tm_borders) +
    tm_shape(point) + # sites
    do.call(tm_dots, tmap_args_list$tm_dots) +
    do.call(tm_layout, tmap_args_list$tm_layout)

  # add text labels for sites
  if(text) {
    map <- map + tm_shape(point) +
      tm_text("SiteID", size = size.text)
  }
  # print(map)

  return(map)

}

# Example usage:
# mapAqSites(stream = c("Stream1", "Stream2"))
