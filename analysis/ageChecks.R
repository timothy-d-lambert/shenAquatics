library(mapShen)
library(plotHacks)
aqData("sites")
con2<-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                       DBQ=O:/Working/AQUATIC/Database/DataEntry/Fish_dataentry_xp_2nd_Pass_A.mdb")
data<-sqlQuery(con2,"select * from W_FI_Gamefish") %>%
  data.table() %>%
  rbind(sqlQuery(con,"select * from W_FI_Gamefish")) %>%
  .[SPECIES=="BKT"] %>%
  .[,date:=as.Date(substr(SiteVisit_ID,1,10))] %>%
  .[,site:=tstrsplit(SiteVisit_ID,"_")[[3]]] %>%
  .[TL==19,TL:=90] %>%
  .[TL==24,TL:=104] %>%
  setkey(site)

age<-sqlQuery(con2,"select * from W_FI_Gamefish_age_assignments") %>%
  data.table() %>%
  .[SPECIES=="BKT"] %>%
  .[,site:=tstrsplit(SiteVisit_ID,"_")[[3]]] %>%
  setkey(site)

data<-age[,.(site,TL_Age0)] %>%
  .[data] %>%
  .[year(date)==2020&!is.na(TL_Age0),AGE:=as.numeric(TL>TL_Age0)] %>%
  setorder(-date)


hist(data$TL,breaks=25:350)


for(s in unique(data[year(date)==2020,site])){
  svg.par(paste0("figures/ageChecks/",s,".svg"),
          mfrow=c(length(unique(data[site==s,date])),1),
          height=length(unique(data[site==s,date]))*3,
          width=6,mar=c(2.5,2.5,1,0))
  for(d in unique(data[site==s,as.character(date)])){
    hist(data[site==s&date==d,TL],
         breaks=25:350,
         main=paste(s,d))
    abline(v=age[site==s,TL_Age0],lty=2,col="blue",lwd=2)
    if(nrow(data[year(date)==2020&site==s&AGE==1])>0){
    abline(v=data[year(date)==2020&site==s&AGE==1,min(TL,na.rm=T)],lty=2,lwd=2,col='red')}
  }
  dev.off()
}
