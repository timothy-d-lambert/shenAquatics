library(mapShen)

sites<-sqlQuery(con,"select * from R_zdd_Sites") %>%
  data.table() %>%
  .[,.(SiteID,FISH_SiteType,AQIN_SiteType,STREAM,Watershed)] %>%
  setkey(SiteID)

siteVisits<-sqlQuery(con,"select * from R_SiteVisits") %>%
  data.table() %>%
  .[,.(SiteVisit_ID,sDate,SITEID,Project,FishSampleType)] %>%
  setkey(SITEID)

siteVisits<-sites[siteVisits]

visitSummary<-siteVisits[,.(first=min(sDate),last=max(sDate),n=.N),.(SiteID,Project,FishSampleType)] %>%
  setkey(n,SiteID)

plotTimeline<-function(fish=NULL,dat=visitSummary){
  plot(NA,xlim=c(dat[FishSampleType==fish,min(first)],
                 dat[FishSampleType==fish,max(last)]),
       ylim=c(0,nrow(dat[FishSampleType==fish])),
       xlab="",ylab="",xaxt="n",yaxt="n")

  axis(1,seq.POSIXt(as.POSIXct("1983-01-01"),as.POSIXct("2019-01-01"),"year"),
       1983:2019)
  axis(2,1:nrow(dat[FishSampleType==fish]),
       dat[FishSampleType==fish,SiteID])
  axis(4,1:nrow(dat[FishSampleType==fish]),
       dat[FishSampleType==fish,n],tick=F)

  segments(dat[FishSampleType==fish,first],
           1:nrow(dat[FishSampleType==fish]),
           dat[FishSampleType==fish,last],
           1:nrow(dat[FishSampleType==fish]),
           lwd=dat[FishSampleType==fish,n^0.5]*3,
           lend=2)
  vs<-siteVisits[Project=="FISH"&FishSampleType==fish&SiteID %in% dat[FishSampleType==fish,SiteID]]

  vs[,n:=.N,.(SiteID,FishSampleType)] %>%
    setkey(n,SiteID) %>%
    .[,siteNum:=as.numeric(factor(as.character(SiteID),levels=unique(as.character(SiteID))))]

  points(siteNum~sDate,data=vs,pch=19,col='blue')
}


tiff.par("quantTimeline.tif",mar=c(2.5,8,0.5,2.5),
         height=15)
  plotTimeline("QUANT",dat=visitSummary[n>2])
dev.off()

tiff.par("quantTimeline2.tif",mar=c(2.5,8,0.5,2.5),
         height=15)
plotTimeline("QUANT",dat=visitSummary[n<=2])
dev.off()

tiff.par("removalTimeline.tif",mar=c(2.5,8,0.5,2.5))
plotTimeline("REMOVAL")
dev.off()

tiff.par("qualTimeline.tif",mar=c(2.5,8,0.5,2.5),height=15)
plotTimeline("QUAL",dat=visitSummary[n>1])
dev.off()

tiff.par("qualTimeline.tif",mar=c(2.5,8,0.5,2.5))
plotTimeline("QUAL",dat=visitSummary[n==1])
dev.off()

tiff.par("otherTimeline.tif",mar=c(2.5,8,0.5,2.5))
plotTimeline("OTHER",dat=visitSummary[n>0])
dev.off()


