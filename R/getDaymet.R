#'Downloads daymet data for fish and bug sites
#'
#'@return Saves daymet temperature and precipitation data to the specified path
#'
#'@param siteId A site ID in the sites table from the database that has lat/long data associated with it
#'@param file A file location to save the downloaded data
#'@param startDate character date in the format YYYY-MM-DD defining the start of the time period to download
#'@param endDate character date in the format YYYY-MM-DD defining the end of the time period to download
#'
#'@export

getDaymet<-function(siteId,file=paste0("data/daymet/",siteId,".csv"),startDate="1980-01-01",endDate=Sys.Date()){

  if(!exists("sites")){sites<-aqData("sites")}

  lat<-sites[SiteID==siteId,Lat_n83]
  lon<-sites[SiteID==siteId,Lon_n83]

  url<-paste0("https://daymet.ornl.gov/single-pixel/api/data?",
              "lat=",lat,
              "&lon=",lon,
              "&vars=dayl,prcp,srad,swe,tmax,tmin,vp&",
              "start=",startDate,
              "&end=",endDate)

  download.file(url,destfile=file)
}
