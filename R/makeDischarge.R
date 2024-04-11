#'Returns calculated discharge by site and date
#'
#'@return Discharge in m^3/s
#'
#'
#'@export

makeDischarge<-function(excludeSticksBuckets=T, conTemp=NULL){

  qInfo<-aqData("dischargeInfo", conTemp=conTemp)

  sticksBuckets<-qInfo[grepl("bucket",tolower(Method))|
          grepl("stick",tolower(Method)),.(SiteVisit_ID)]

  q<-aqData("discharge", conTemp) %>%
    .[!SiteVisit_ID %in% sticksBuckets$SiteVisit_ID] %>% # exclude stick/bucket spls
    .[SiteVisit_ID %in% qInfo[grepl("pygmy",tolower(Method)),SiteVisit_ID] & Vel > 0,
      ":="(Vel=(0.9604*Vel/60+0.0312)*0.3048,
            Depth_m=Depth_m*0.3048,
            Dist_m=Dist_m*0.3048)] %>%
    .[!SiteVisit_ID %in% c("1997-07-16_F_2F055","1995-05-23_A_NPI02")] %>%
    .[CXSegment!="z"] %>%
    .[,.(q=discharge(Dist_m,Depth_m,Vel)),.(SiteID,sDate,substr(CXSegment,1,1))] %>%
    .[,.(q=sum(q)),.(SiteID,sDate)] %>%
    .[q<0,q:=NA] %>%
    setkey(SiteID)

  return(q)
}
