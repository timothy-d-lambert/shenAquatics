#'Create a dataset with N and detection/non-detection for all species for all site visits in the SHEN fish database
#'
#'Creates a data.table with detection and number caught for each site visit in the fish database
#'@return returns the data.table
#'
#'@export

makeFish<-function(nRuns=4){
  aqConnector()

  sites<-aqData("sites") %>%
    setkey(SiteID)

  siteVisits<-aqData("siteVisits") %>%
    setkey(SiteVisit_ID)

  game<-aqData("game") %>%
    setkey(SiteVisit_ID)

  game<-siteVisits[game,.(sDate,SiteID,SPECIES,WT,RUN,FishSampleType)] %>%
    .[FishSampleType %in% c("QUANT","QUAL")]

  game<-game[,.(.N,meanWt=mean(WT,na.rm=T)),.(sDate,SPECIES,SiteID,RUN,FishSampleType)]

  nongame<-aqData("nongame") %>%
    setkey(SiteVisit_ID)

  nongame<-siteVisits[nongame,.(sDate,SiteID,SPECIES,NUMBER,meanWt=WEIGHT/NUMBER,RUN,FishSampleType)] %>%
    .[FishSampleType %in% c("QUANT","QUAL")] %>%
    setnames("NUMBER","N") %>%
    .[,.(sDate,SiteID,SPECIES,RUN,N,meanWt,FishSampleType)]

  fish<-rbind(game,nongame) %>%
    .[SiteID %in% sites[InPark==1,SiteID]]

  runs<-fish[,.(nRuns=max(RUN)),.(sDate,SiteID)] %>%
    .[nRuns<=4] %>%
    .[,.(run=1:nRuns),.(sDate,SiteID)] %>%
    setkey(SiteID,sDate,run)


  fish<-fish[!SPECIES %in% c("NogFISH","NOgFISH","NOFISH","UNK")&!is.na(SPECIES)] %>%
  .[N==0,N:=NA] %>%
  .[,pres:=1] %>%
  .[,SPECIES:=tolower(as.character(SPECIES))] %>%
  .[RUN<=4] %>%
  setkey(sDate,SiteID,SPECIES,RUN)

  allSp<-data.table(sp=unique(fish$SPECIES))

  setkey(siteVisits,sDate,SiteID)

  runSp<-crossing(runs,allSp) %>%
    data.table() %>%
    setkey(sDate,SiteID)

  runSp<-siteVisits[,.(sDate,SiteID,FishSampleType,SiteVisit_ID)] %>% .[runSp] %>%
    setkey(sDate,SiteID,sp,run)

  fish<-fish[,FishSampleType:=NULL] %>%
    .[runSp] %>%
    .[is.na(N)&FishSampleType=="QUANT",N:=0] %>%
    .[,SiteID:=as.character(SiteID)] %>%
    .[,pres:=any(pres==1),.(sDate,SPECIES,SiteID)] %>%
    .[is.na(pres),pres:=0] %>%
    .[RUN<=nRuns] %>%
    .[,year:=year(sDate)] %>%
    setkey(SiteID,year)

  return(fish)
}
