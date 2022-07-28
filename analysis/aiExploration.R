library(mapShen)

ai<-aqData("aiData") %>%
  setkey(TAXON_CODE)

aiTaxa<-aqData("aiTaxa") %>%
  setkey(TAXON_CODE)

aiEcoTaxa<-aqData("aiEcoTaxa")

siteVisits<-aqData("siteVisits") %>%
  setkey(SiteVisit_ID)

sampleInfo<-aqData("aiSampleInfo") %>%
  setkey(SiteVisit_ID,REPLICATE)


ai<-aiTaxa[,.(TAXON_CODE,Taxa_Name,Rank_aw)] %>%
  .[ai] %>%
  setkey(SiteVisit_ID,REPLICATE)

ai<-sampleInfo[,.(SiteVisit_ID,REPLICATE,SAMPLE_MET)] %>%
  .[ai] %>%
  setkey(SiteVisit_ID)

ai<-siteVisits[,.(SiteVisit_ID,sDate,SITEID)] %>%
  .[ai] %>%
  setnames(c("REPLICATE","INSECT_COU"),c("rep","N"))

byRank<-ai[!is.na(sDate),.N,.(year(sDate),Rank_aw)] %>%
  .[,prop:=N/sum(N),year] %>%
  .[,Rank_aw:=factor(as.character(Rank_aw),
                     levels=c("Species","Genus","SubFamily","Family","Order","Class","Phylum",NA),
                     ordered=T)] %>%
  .[,rankNum:=as.numeric(Rank_aw)] %>%
  setkey(year,rankNum) %>%
  .[,cumProp:=cumsum(prop),year]

plot(NA,xlim=range(byRank$year),ylim=c(0,1))
for(i in unique(byRank$rankNum)){
  points(cumProp~year,data=byRank[rankNum==i],type='l')
}
abline(v=2013.5,lwd=2)


for(i in byRank[!is.na(Rank_aw),unique(rankNum)]){
  plot(prop~year,data=byRank[rankNum==i],type='l',main=unique(byRank$Rank_aw)[i])
  points(prop~year,data=byRank[rankNum==i],pch=19)
  abline(v=2013.5,lwd=2)
}

plot(prop~year,data=byRank[is.na(rankNum)],type='l',main="NA")
points(prop~year,data=byRank[is.na(rankNum)],pch=19)
abline(v=2013.5,lwd=2)
