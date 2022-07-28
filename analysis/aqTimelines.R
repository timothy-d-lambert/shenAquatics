library(mapShen)

sites<-aqData("sites") %>%
  .[,.(SiteID,FISH_SiteType,AQIN_SiteType,STREAM,Elev_m,MAJ_GEOL)] %>%
  .[SiteID !="PAINE"] %>%
  setkey(SiteID)

siteVisits<-aqData("siteVisits") %>%
  .[,.(SiteVisit_ID,sDate,SiteID,Project,FishSampleType)] %>%
  setkey(SiteID)

siteVisits<-sites[siteVisits]

visitSummary<-siteVisits[,.(first=min(sDate),last=max(sDate),
                            qual=sum(FishSampleType=="QUAL",na.rm=T),
                            quant=sum(FishSampleType=="QUANT",na.rm=T),
                            removal=sum(FishSampleType=="REMOVAL",na.rm=T),
                            aqin=sum(Project=="AQIN",na.rm=T),
                            wq=sum(Project=="WATERQ",na.rm=T),
                            total=.N),
                         .(SiteID)] %>%
  setkey(SiteID)

visitSummary<-sites[visitSummary] %>%
  setkey(aqin,SiteID)

visitSummary[is.na(FISH_SiteType),FISH_SiteType:="none"]
visitSummary[is.na(AQIN_SiteType),AQIN_SiteType:="none"]
visitSummary[,status:=as.numeric(FISH_SiteType=="Primary")+as.numeric(AQIN_SiteType=="Primary")+
               0.5*(as.numeric(FISH_SiteType=="Secondary")+as.numeric(AQIN_SiteType=="Secondary")) +
               0.1*(as.numeric(FISH_SiteType=="Inactive")+as.numeric(AQIN_SiteType=="Inactive"))]

setorder(visitSummary,-status)
visitSummary[FISH_SiteType=="Primary"|AQIN_SiteType=="Primary"]

par(mfrow=c(1,3))
boxplot(Elev_m~MAJ_GEOL,data=visitSummary[FISH_SiteType=="Primary"],main="Fish",ylim=c(300,950))
boxplot(Elev_m~MAJ_GEOL,data=visitSummary[AQIN_SiteType=="Primary"],main="AqIn",ylim=c(300,950))
boxplot(Elev_m~MAJ_GEOL,data=visitSummary[AQIN_SiteType=="Primary"&
                                            FISH_SiteType=="Primary"],main="Primary Both",ylim=c(300,950))
par(mfrow=c(1,4))
boxplot(Elev_m~MAJ_GEOL,data=visitSummary[AQIN_SiteType=="Primary"&
                                            FISH_SiteType=="Secondary"],main="Fish-S, AqIn-P",ylim=c(300,950))
boxplot(Elev_m~MAJ_GEOL,data=visitSummary[AQIN_SiteType=="Secondary"&
                                            FISH_SiteType=="Primary"],main="Fish-P, AqIn-S",ylim=c(300,950))
boxplot(Elev_m~MAJ_GEOL,data=visitSummary[AQIN_SiteType=="Primary"&
                                            (!FISH_SiteType %in% c("Primary","Secondary")|
                                               is.na(FISH_SiteType))],main="Fish-None, AqIn-P",ylim=c(300,950))
boxplot(Elev_m~MAJ_GEOL,data=visitSummary[FISH_SiteType=="Primary"&
                                            (!AQIN_SiteType %in% c("Primary","Secondary")|
                                               is.na(AQIN_SiteType))],main="Fish-P, AqIn-None",ylim=c(300,950))


plotTimeline<-function(dat){
  plot(NA,xlim=c(dat[,min(first)],
                 dat[,max(last)]),
       ylim=c(0,nrow(dat)),
       xlab="",ylab="",xaxt="n",yaxt="n")



  axis(1,seq.POSIXt(as.POSIXct("1980-01-01"),as.POSIXct("2019-01-01"),"year"),
       1980:2019)
  axis(2,1:nrow(dat),
       dat$SiteID)
  # axis(4,1:nrow(dat),
  #      dat$n,tick=F)

  segments(dat$first,
           1:nrow(dat),
           dat$last,
           1:nrow(dat),
           lwd=15,
           lend=2,col='gray')

  vs<-siteVisits[SiteID %in% dat$SiteID]

  vs[,n:=sum(Project=="AQIN",na.rm=T),.(SiteID)] %>%
    setkey(n,SiteID) %>%
    .[,siteNum:=as.numeric(factor(as.character(SiteID),levels=unique(as.character(SiteID))))]

  alpha<-0.7

  vs[,sampleType:=FishSampleType] %>%
    .[is.na(sampleType),sampleType:=Project] %>%
    .[FishSampleType=="QUANT",":="(pch=19,col=rgb(0,0,1,alpha))] %>%
    .[FishSampleType=="QUAL",":="(pch=1,col=rgb(0,0,1,alpha))] %>%
    .[Project=="AQIN",":="(pch=15,col=rgb(1,1,0,alpha))] %>%
    .[Project=="WATERQ",":="(pch=17,col=rgb(34/255,139/255,34/255,alpha))]

  points(siteNum~sDate,data=vs,pch=pch,col=col)


  mtext(dat$qual,4,0,at=1:nrow(dat),
    col=ifelse(dat$FISH_SiteType=="Primary"&!is.na(dat$FISH_SiteType),"red",
               ifelse(dat$FISH_SiteType!="Secondary"|is.na(dat$FISH_SiteType),
                      "gray","blue")))
  mtext(dat$quant,4,2,at=1:nrow(dat),
        col=ifelse(dat$FISH_SiteType=="Primary"&!is.na(dat$FISH_SiteType),"red",
                   ifelse(dat$FISH_SiteType!="Secondary"|is.na(dat$FISH_SiteType),
                          "gray","blue")),outer=F)
  mtext(dat$aqin,4,4,at=1:nrow(dat),
        col=ifelse(dat$AQIN_SiteType=="Primary"&!is.na(dat$AQIN_SiteType),"red",
                   ifelse(dat$AQIN_SiteType!="Secondary"|is.na(dat$AQIN_SiteType),
                          "gray","blue")),
        outer=F)

  # mtext(dat$wq,4,6,at=1:nrow(dat))
}


tiff.par("figures/primaryTimeline.tif",mar=c(2.5,8,0.5,10),
         height=10,width=15)
  plotTimeline(visitSummary[FISH_SiteType=="Primary"|AQIN_SiteType=="Primary"])
dev.off()

tiff.par("secondaryTimeline.tif",mar=c(2.5,8,0.5,10),
         height=12,width=15)
plotTimeline(visitSummary[FISH_SiteType!="Primary"&AQIN_SiteType!="Primary"&
                            (FISH_SiteType=="Seconary"|AQIN_SiteType=="Secondary")])
dev.off()

tiff.par("inactiveTimeline.tif",mar=c(2.5,8,0.5,10),
         height=12,width=15)
plotTimeline(visitSummary[!FISH_SiteType %in% c("Primary","Secondary")&
                          !AQIN_SiteType %in% c("Primary","Secondary")&
                          (FISH_SiteType=="Inactive"|AQIN_SiteType=="Inactive")])
dev.off()

