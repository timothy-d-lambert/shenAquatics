#'similar to yday but converts POSIXct objects as well to a day (or other unit) of the year with portions of the day (or unit) as decimals.
#'
#'Creates a link to the SHEN MS Access database for import into R.
#'@return returns numeric values representing progress through the year (i.e. 0-366 days or 0-1 years or 0-8784 hours)
#'
#'@param times A vector of times or dates to be converted
#'@param unit Defaults to "days" but can be any other unit accepted by \code{difftime}
#'@param origin The starting date for the time of year in the format mm-dd. Defaults to 01-01
#'
#'@export

timeOfYear<-function(times,unit="days",origin="01-01",numeric=T){
  if(!any(class(times)=="POSIXct")){
    times<-as.POSIXct(times)
  }

  y<-year(times)
  if(origin!="01-01"){
    y[yday(times)<yday(as.POSIXct(paste0(y,"-",origin)))]<-
      y[yday(times)<yday(as.POSIXct(paste0(y,"-",origin)))]-1
  }

  origin<-as.POSIXct(paste0(y,"-",origin))
  toy<-difftime(times,origin,units=unit)
  if(numeric) toy<-as.numeric(toy)
  return(toy)
}
