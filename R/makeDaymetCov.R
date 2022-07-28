#'Creates covariates after Kanno et al using daymet data downloaded with downloadDaymet
#'
#'@return A data.table of summarized temp and precip data
#'
#'@param siteId A site ID in the sites table from the database that has lat/long data associated with it
#'@param file A file location to save the downloaded data
#'
#'@export

makeDaymetCov<-function(siteId,file=paste0("C:/Users/echildress/Documents/mapShen/data/daymet/",siteId,".csv")){
  d<-fread(file,skip=7)
  setnames(d,c("year","yday","dayl","prcp","srad","swe","tmax","tmin","vp"))

  d[,date:=as.Date(paste0(year-1,"-12-31"))+yday]
  d[month(date) %in% 9:11,season:="fall"]
  d[month(date) %in% c(1,2,12),season:="winter"]
  d[month(date) %in% 3:5,season:="spring"]
  d[month(date) %in% 6:8,season:="summer"]

  d[,troutYear:=year(date)]
  d[month(date) %in% c(6:12),troutYear:=troutYear+1]

  seasonal<-d[,.(meanFallMaxTemp=mean(tmax[season=="fall"]),
                 meanWinterMaxTemp=mean(tmax[season=="winter"]),
                 meanSpringMaxTemp=mean(tmax[season=="spring"]),
                 meanSummerMaxTemp=mean(tmax[season=="summer"]),
                 meanFallMinTemp=mean(tmin[season=="fall"]),
                 meanWinterMinTemp=mean(tmin[season=="winter"]),
                 meanSpringMinTemp=mean(tmin[season=="spring"]),
                 meanSummerMinTemp=mean(tmin[season=="summer"]),
                 fallPrecip=sum(prcp[season=="fall"]),
                 winterPrecip=sum(prcp[season=="winter"]),
                 springPrecip=sum(prcp[season=="spring"]),
                 summerPrecip=sum(prcp[season=="summer"])),
              troutYear]
  return(seasonal)
}
