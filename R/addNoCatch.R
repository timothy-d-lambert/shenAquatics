#'Pads out count data by run to include passes where zero individuals of a species were caught
#'
#'@return A data.table expanded to include site visit/pass/species combos where no individuals were caught
#'
#'@param data data.table to be expanded. Must include SiteVisit_ID, SiteID, sDate, SPECIES, and RUN as columns
#'@param spBySite Logical. If TRUE, expand only to species that were observed at least once at a site within the supplied data. Otherwise, each species will be fully expanded at each site.
#'@param byAge Logical. If TRUE, pad out with an AGE==0 and AGE==1 row for each combination of other values.
#'
#'@export

addNoCatch<-function(data,spBySite=T,byAge=F){
  setkey(data,SiteVisit_ID,SiteID,sDate,SPECIES,RUN)

  maxRuns<-aqData("game",visitCols="FishSampleType") %>%
    .[,.(SiteVisit_ID,FishSampleType,SiteID,sDate,RUN,SPECIES)] %>%
    rbind(aqData("nongame",visitCols="FishSampleType")[,.(SiteVisit_ID,FishSampleType,SiteID,sDate,RUN,SPECIES)]) %>%
    .[SiteID %in% data$SiteID & FishSampleType %in% c("QUANT","QUAL")] %>%
    .[RUN<=4,.(maxRun=max(RUN)),.(SiteVisit_ID,SiteID,sDate,FishSampleType)]

  allRuns<-maxRuns[,.(RUN=1:maxRun),.(SiteVisit_ID,SiteID,sDate)] %>%
    setkey(SiteVisit_ID,SiteID,sDate,RUN)

  if(spBySite){
    spSite<-unique(data[,.(SiteID,SPECIES)])
  } else{
    spSite<-crossing(unique(data$SiteID),unique(as.character(data$SPECIES))) %>%
      data.table()
  }

  if(byAge){
    spSite<-crossing(spSite,data.frame(AGE=c(0,1))) %>%
      data.table()
  }

  spSiteRun<-allRuns[spSite,on="SiteID",allow.cartesian=T]

  data<-data[spSiteRun,on=names(spSiteRun)]
  suppressWarnings(data[,FishSampleType:=NULL])
  setkey(data,SiteVisit_ID)

  sampleType<-sqlQuery(con,paste("select SiteVisit_ID, FishSampleType from R_SiteVisits"),stringsAsFactors=F) %>%
    data.table() %>%
    setkey(SiteVisit_ID)

  data<-sampleType[data]
  data[is.na(N)&FishSampleType=="QUANT",N:=0]
  data[is.na(N)&FishSampleType=="QUAL"&SPECIES %in% c("BKT","BRT","SMB","ROB","RBT"),N:=0]

  return(data)
}
