library(sf)

mapShenStreams <- function(stream = NULL, lwdBoundary = 2,
                           wholePark = FALSE, add = FALSE,
                           streamCol = "blue", border = "forestgreen",
                           newBox = NULL, ...) {

  # Read the shapefiles using sf
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

  plot(st_geometry(streams), col = streamCol, add = add, ...)
  plot(st_geometry(boundary), col = NA, border = border, add = TRUE, lwd = lwdBoundary, ...)

}

# Example usage:
# mapShenStreams(stream = c("Stream1", "Stream2"))



#' #### EVAN'S OLDER VERSION (uses defunct readOGR()) ####
#' #'Create a map of streams within and surrounding SHEN
#' #'@return A map with desired streams and sites
#' #'@param stream A single value or vector containing the quoted names of streams to be mapped.
#' #'@param lwdBoundary line width for Shen boundary
#' #'@param wholePark Logical indicating whether to map the whole park even if a subset of streams is selected. If wholePark is FALSE, the extent of the map will be trimmed to the area immediately surrounding selected streams.
#' #'@param add Logical indicating whether this should be added to an existing map (TRUE) or if a new map should be created (FALSE)
#' #'@param border color for the SHEN border.
#' #'@param newBox A bbox that defines the area to be plotted.
#' #'
#' #'@details This function is a wrapper that creates maps of streams within and surrounding SHEN.
#' #'
#' #'@export
#' mapShenStreams<-function(stream=NULL,lwdBoundary=2,
#'                          wholePark=F,add=F,streamCol="blue",
#'                          border="forestgreen",newBox=NULL,...){
#'
#'   streams<-readOGR(dsn="C:/Users/echildress/OneDrive - DOI/Documents/mapShen/data/gis","Stream",verbose=F)
#'
#'
#'   if(!is.null(stream)){
#'
#'     if(!all(stream %in% streams$STREAM_NAM)) {stop(cat(stream[!stream %in% streams$STREAM_NAM],
#'                                                        " not in data. Stream must be in:\n",
#'                                                       paste(unique(as.character(streams$STREAM_NAM))[
#'                                                         order(unique(as.character(streams$STREAM_NAM)))],
#'                                                             collpase=", ")))}
#'
#'     streams<-streams[streams$STREAM_NAM %in% stream,]
#'   }
#'
#'   # boundary<-readOGR(dsn="M:/GISData/basedata/boundary/gis",layer="PkBndryPly")
#'   boundary<-readOGR(dsn="C:/Users/echildress/OneDrive - DOI/Documents/mapShen/data/gis",layer="PkBndryPly",verbose=F)
#'
#'   if(!is.null(newBox)){
#'     streams@bbox<-newBox
#'   }
#'
#'   if(wholePark){
#'     streams@bbox<-boundary@bbox
#'   }
#'
#'   sp::plot(streams,col=streamCol,add=add,...)
#'
#'   sp::plot(boundary,col=NULL,border=border,add=T,lwd=lwdBoundary,...)
#'
#' }
