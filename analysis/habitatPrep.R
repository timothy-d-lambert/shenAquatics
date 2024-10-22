library(shenAquatics)

sites <- aqData("sites") %>%
  setkey(SiteID)

fishVisits <- aqData("siteVisits") %>%
  # .[FishSampleType %in% c("QUANT","QUAL")] %>%
  .[,.(SiteVisit_ID,sDate,SiteID)] %>%
  setkey(SiteVisit_ID)

q <- aqData("discharge") %>%
  setkey(SiteVisit_ID) %>%
  .[SiteVisit_ID %in% fishVisits$SiteVisit_ID] %>%
  .[SiteVisit_ID != "1997-07-16_F_2F055"] %>%
  .[,.(q=discharge(Dist_m,Depth_m,Vel)),SiteVisit_ID] %>%
  .[q<0,q:=NA] #these need to be checked against the source data.



habitat <- aqData("habitat") %>%
  # .[SiteVisit_ID %in% fishVisits$SiteVisit_ID] %>%
  setkey(SiteVisit_ID) %>%
  .[,.(pools=mean(Pools,na.rm=T),
       width=mean(Width_m,na.rm=T),
       slope=mean(Grad_percent,na.rm=T)),
    SiteVisit_ID]

substrate <- aqData("habitatSubstrate") %>%
  setkey(SiteVisit_ID) %>%
  .[SiteVisit_ID!="2000-07-18_F_2F040"|Depth_m<1] %>%
  # .[SiteVisit_ID %in% fishVisits$SiteVisit_ID] %>%
  .[,.(depth=mean(Depth_m,na.rm=T)),SiteVisit_ID]

wq <- aqData("wq") %>%
  # .[SiteVisit_ID %in% fishVisits$SiteVisit_ID] %>%
  setkey(SiteVisit_ID) %>%
  .[,.(temp=mean(Temp,na.rm=T),
       do=mean(DO,na.rm=T),
       cond=mean(Cond,na.rm=T),
       pH=mean(pH,na.rm=T),
       waterLevel=unique(na.omit(WaterLevel)),
       waterColor=unique(na.omit(WaterColor))),
    SiteVisit_ID] %>%
  .[temp==0,temp:=NA] %>%
  .[do==0,do:=NA] %>%
  .[pH==0,pH:=NA] %>%
  .[cond==0,cond:=NA]

hab <- merge(wq,habitat,all=T) %>%
  merge(q,all=T) %>%
  merge(substrate,all=T)

hab <- fishVisits[hab]
hab[year(sDate)==1994,pH:=NA]
hab[q>1,q:=NA]
hab[,SiteVisit_ID:=NULL]
setkey(hab,SiteID,sDate)



