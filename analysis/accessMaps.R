library(ggmap)
library(data.table)
library(mapShen)
library(plotHacks)

register_google("AIzaSyAMPy_HnX2UjZ5CWD163m4yswi9Fg2yX0g")

access<-fread("O:/Working/AQUATIC/Season Planning/Landowner and Stream Info/sitesWithUncertainAccess.csv") %>%
  setkey(SiteID)

sites<-aqData("sites") %>%
  setkey(SiteID)

access<- sites[,.(SiteID,Lat_n83,Lon_n83,County)] %>%
  .[access]

# access[,Side:=ifelse(County %in% c("Rappahannock","Madison","Greene","Albemarle"),"East","West")]

access<-access[!SiteID %in% c("1F214","3F021","3F023","3F101")]

write.csv(access, file="O:/Working/AQUATIC/Season Planning/Landowner and Stream Info/site maps/siteList.csv",
          row.names=F)

for(s in unique(access$SiteID)){
  if(access[SiteID==s,is.na(Lat_n83)]){next}
  tiff.par(paste0("O:/Working/AQUATIC/Season Planning/Landowner and Stream Info/site maps/",s,".tif"))

  map<-ggmap(get_googlemap(center=c(lon=access[SiteID==s,Lon_n83],
                                    lat=access[SiteID==s,Lat_n83]),
                           zoom=15)) +
    geom_point(data=sites[SiteID==s], aes(x=Lon_n83,y=Lat_n83),color='red',size=3,alpha=0.5) +
    ggtitle(paste0(access[SiteID==s,Stream],", ",
                   access[SiteID==s,District]," District, ",
                   access[SiteID==s,County]," County"))
  
  print(map)

  dev.off()
}
