#'Indentifies the water year for dates
#'
#'Water year is typically defined as starting on October 1, but the origin can be defined.
#'@return returns a numeric value that is the water year for a given date
#'
#'@param times A vector of times or dates to be converted
#'@param origin The starting date for the water year in the format mm-dd. Defaults to 10-01
#'
#'@export

waterYear<-function(times,origin="10-01"){
  y<-year(times)+1
  if(origin!="01-01"){
    y[yday(times)<yday(as.POSIXct(paste0(y,"-",origin)))]<-
      y[yday(times)<yday(as.POSIXct(paste0(y,"-",origin)))]-1
  }
  return(y)
}
