#'Create a map of streams within and surrounding SHEN
#'@return A map with desired streams and sites
#'@param stream A single value or vector containing the quoted names of streams to be mapped.
#'@param border.lwd line width for the Shenandoah National Park boundary
#'@param wholePark Logical indicating whether to map the whole park even if a subset of streams is selected. If wholePark is FALSE, the extent of the map will be trimmed to the area immediately surrounding selected streams.
#'@param base_map A base map on which this map should be overlaid; the default value of NULL creates a new map (i.e., there is no base map)
#'@param stream.color color for the streams
#'@param border.color color for the park boundary
#'@param bg.color color for the background
#'@param newBox A bbox that defines the area to be plotted.
#'
#'@details This function is a wrapper that creates maps of streams within and surrounding SHEN.
#'
#'@export
mapShenStreams <- function(stream = NULL, border.lwd = 2,
                           wholePark = FALSE,
                           base_map = NULL,
                           stream.color = "blue",
                           border.color = "forestgreen",
                           bg.color = NA,
                           newBox = NULL) {

  # Read the streams shapefiles using sf
  root <- defineRoot() # define project root
  streams <- st_read(dsn = file.path(root, "data/gis"),
                     layer = "Stream", quiet = TRUE)

  if (!is.null(stream)) {
    if (!all(stream %in% streams$STREAM_NAM)) {
      stop(cat(stream[!stream %in% streams$STREAM_NAM],
               " not in data. Stream must be in:\n",
               paste(unique(streams$STREAM_NAM)[
                 order(unique(streams$STREAM_NAM))],
                 collapse = ", ")))
    }
    streams <- streams[streams$STREAM_NAM %in% stream, ]
  }

  # Read the boundary shapefile using sf
  boundary <- st_read(dsn = file.path(root, "data/gis"),
                      layer = "PkBndryPly", quiet = TRUE)

  if (!is.null(newBox)) {
    st_bbox(streams) <- newBox
  }

  if (wholePark) {
    st_bbox(streams) <- st_bbox(boundary)
  }

  # Creating the map
  map <- base_map +
    tm_shape(streams) +
    tm_lines(col = stream.color, lwd = 1) +  # Adjust line width as needed
    tm_shape(boundary) +
    tm_borders(col = border.color, lwd = border.lwd) +
    if(!is.na(bg.color)) { tm_layout(bg.color = bg.color) } else {NULL}
  return(map)

}

# Example usage:
# mapShenStreams(stream = c("Stream1", "Stream2"))
