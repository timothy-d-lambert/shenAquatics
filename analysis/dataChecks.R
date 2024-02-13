library(mapShen)
library(plotHacks)
library(readxl)
library(ggplot2)
library(lme4)

###newDb is the database from which to add new data
newDb<-"C:/Users/echildress/OneDrive - DOI/Documents/mapShen/data/AQUATIC_Data_Entry_01_be_xp.accdb"
oldDb<-"C:/Users/echildress/OneDrive - DOI/Documents/mapShen/data/All_Stream_data_2020.accdb"

conNew<-odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                        DBQ=",newDb))
conOld<-odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                        DBQ=",oldDb))

############################
#####Non Game###############
###########################


nongame<-rbind(aqData("nongame",conTemp=conNew) %>%
  .[,type:="new"],
  aqData("nongame") %>%
    .[,-"Counter"] %>%
    .[,type:="old"]) %>%
  setnames(c("MIN LENGTH","MAX LENGTH"),c("min","max")) %>%
  .[NUMBER==1,length:=mean(c(min,max),na.rm=T),.(SiteVisit_ID,SPECIES,RUN)] %>%
  .[,SPECIES:=tolower(SPECIES)]

meanWeight<-nongame[,.(meanWeight=WEIGHT/NUMBER),.(SiteID,sDate,SPECIES,RUN,type)]
setkey(meanWeight,SPECIES,meanWeight)

lw20<-read_excel("O:/Working/AQUATIC/Database/Non-game Length to Weight TEMP.xlsx") %>%
  data.table() %>%
  setnames(c("site","run","species","length",'weight',"notes")) %>%
  .[,species:=tolower(species)] %>%
  .[,weight:=round(as.numeric(weight),1)] %>%
  .[,length:=as.numeric(length)] %>%
  .[,-"notes"]

sp<-unique(nongame$SPECIES)
sp<-sp[!grepl("un",sp)]
sp<-sp[!sp %in% c("nofish","notshockd","bkt","rob","brt","rbt","smb","pks","rdb","cap","lmb","ped","rys","abl","sws","ame")]

minBiggerMax<-weightOutliers<-mortBiggerCount<-NULL

colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

for(s in sp){

  mortBiggerCount<-rbind(mortBiggerCount,nongame[MORTALITY>NUMBER])


  tempDat<-rbind(nongame[SPECIES==s&NUMBER==1&!is.na(WEIGHT),.(SiteID,sDate,SPECIES,RUN,length,WEIGHT,type)] %>%
                   setnames("WEIGHT","weight"),
                 lw20[species==s&weight!=0&length>10,] %>%
                   .[,sDate:=as.POSIXct(NA)] %>%
                   setnames(c("SiteID","RUN","SPECIES","length","weight","sDate")) %>%
                   .[,.(SiteID,sDate,SPECIES,RUN,length,weight)] %>%
                   .[,type:="lw20"])



  xBreaks<-seq(nongame[SPECIES==s,min(min,na.rm=T)],
                nongame[SPECIES==s,max(min,na.rm=T)],
                1)
  p1<-ggplot(nongame[SPECIES==s],aes(x=min,fill=type))+
    geom_histogram(binwidth=1)+
    theme_classic()+
    ggtitle(toupper(s))

  p2<-ggplot(nongame[SPECIES==s],aes(x=max,fill=type))+
    geom_histogram(binwidth=1)+
    theme_classic()

  tempDat<-tempDat[!is.na(length)&!is.na(weight)]

  if(nrow(nongame[SPECIES==s&NUMBER==1])>0){

    p3<-ggplot(data=tempDat)+
      geom_point(aes(length,weight,color=type),size=3,alpha=ifelse(tempDat$type=="new",1,0.25))+
      scale_color_brewer(palette="Dark2")+
      theme_classic()

    if(nrow(tempDat)>2){
      lwMod<-rlm(log(weight)~log(length),data=tempDat)
      tempDat<-cbind(tempDat,exp(predict(lwMod,interval="prediction",level=0.995)))
      setkey(tempDat,fit)

      p3<-p3+
        geom_line(data=tempDat,aes(length,fit))+
        geom_line(data=tempDat,aes(length,upr),linetype=2)+
        geom_line(data=tempDat,aes(length,lwr),linetype=2)

      weightOutliers<-rbind(weightOutliers,tempDat[weight>upr|weight<lwr])
    }
  }else{
    p3<-ggplot()
   }


  p4<-ggplot(meanWeight[SPECIES==s],aes(x=meanWeight,fill=type))+
    geom_histogram(binwidth=ifelse(meanWeight[SPECIES==s,max(meanWeight,na.rm=T)>20],
                                              1,0.1))+
    theme_classic()

  pGrid<-grid.arrange(p1,p2,p3,p4,nrow=4)
  ggsave(file=paste0("O:/Working/AQUATIC/Database/Data checking/nongameSize/",s,"Figures.png"),
         pGrid,height=15,width=8)

  if(nrow(nongame[SPECIES==s&min>max])>0){
    minBiggerMax<-rbind(minBiggerMax,nongame[SPECIES==s&min>max])
  }

  if(nrow(nongame[SPECIES==s&max%%1!=0])>0){
    write.csv(nongame[SPECIES==s&max%%1!=0],paste0("O:/Working/AQUATIC/Database/Data checking/nongameSize/",s,"DecimalMax.csv"))
  }

}


write.csv(minBiggerMax,paste0("O:/Working/AQUATIC/Database/Data checking/nongameSize/minBiggerMax.csv"),row.names=F)
write.csv(mortBiggerCount,paste0("O:/Working/AQUATIC/Database/Data checking/nongameSize/mortBiggerCount.csv"),row.names=F)

setorder(weightOutliers,-sDate)
write.csv(weightOutliers,paste0("O:/Working/AQUATIC/Database/Data checking/nongameSize/weightOutliers.csv"),row.names=F)

#is max length less than min length
#is mean weight outside the expected range
#is min outside expected range
#is max outside expected range
#is mort greater than catch
#are singletons within the expected range using a length-weight relationship




#########################################
#######Game Fish ########################
###########################################

game<-rbind(aqData("game",conTemp=conNew) %>%
  .[,type:="new"],
  aqData("game") %>%
    .[,type:="old"])

getOutliers<-function(dat,level=0.9999){
  lw<-lm(log(WT)~log(TL),data=dat)
  prediction<-predict(lw,interval="prediction",level=level)
  dat[!is.na(log(WT))&!is.na(log(TL)),":="(lwr=exp(prediction[,2]),
                                           upr=exp(prediction[,3]))]
  o<-dat[WT<lwr|WT>upr]
  plot(log(WT)~log(TL),data=dat,main=unique(dat$SPECIES))
  points(log(WT)~log(TL),data=o,pch=19,col='gray')
  points(log(WT)~log(TL),data=o[type=="new"],pch=19,col='blue')

  return(o)
}

# svg.par("O:/Working/AQUATIC/Database/Data checking/gameSize/lengthWeight.svg",
#         mfrow=c(length(unique(data[type=="new",SPECIES])),1),
#         height=length(unique(data[type=="new",SPECIES]))*4,
#         width=6,mar=c(3.5,3.5,1,0))
outliers<-NULL
for(s in unique(game[type=="new",SPECIES])){
  if(s=="NOgFISH") next
  if(s=="BKT"){
    outliers<-rbind(outliers,getOutliers(game[SPECIES==s&TL<=90],level=0.9999))
    outliers<-rbind(outliers,getOutliers(game[SPECIES==s&TL>90],level=0.9999))

  } else{
    outliers<-rbind(outliers,getOutliers(game[SPECIES==s],level=0.999))
  }
}
# dev.off()

svg.par("O:/Working/AQUATIC/Database/Data checking/gameSize/weightHists.svg",
        mfrow=c(sum(outliers$type=="new"),1),height=sum(outliers$type=="new")*3,
        width=6,mar=c(3.5,3.5,1,0))
for(i in which(outliers$type=="new")){

  hist(data[SPECIES==outliers$SPECIES[i]&TL==outliers$TL[i],WT],
       main=outliers[i,paste0(SPECIES,", TL = ",TL,", WT = ",round(WT,2),", ",SiteID,", ",sDate,", Entry: ",EntryOrder)],
       breaks=seq(0,ceiling(data[SPECIES==outliers$SPECIES[i]&TL==outliers$TL[i],max(WT,na.rm=T)]),0.1),
       xlab="Weight (g)",ylab="Frequency of Fish with Same TL")
  abline(v=outliers[i,WT],col='red',lwd=3)
  abline(v=outliers[i,c(lwr,upr)],lty=2)
}
dev.off()

write.csv(outliers[type=="new"],"O:/Working/AQUATIC/Database/Data checking/gameSize/gameSizeOutliers.csv")

####################################
########Habitat######################
#######################################

sites<-aqData("sites") %>%
  setkey(SiteID)

hab<-aqData("habitat",conTemp=conNew)

wq<-rbind(aqData("wq",conTemp=conNew) %>%
            .[,type:="new"] %>%
            setnames("flag","Flag"),
          aqData("wq") %>%
            .[,type:="old"] %>%
            .[,-"SampleNum"]) %>%
  .[DO==0,DO:=NA] %>%
  .[Temp==0,Temp:=NA] %>%
  .[Cond==0,Cond:=NA] %>%
  .[TDS==0,TDS:=NA] %>%
  .[pH==0,pH:=NA]

setkey(wq,SiteID)

wq<-sites[,.(SiteID,Elev_m)][wq]
wq[,doSat:=DO.saturation(DO,Temp,Elev_m)]

wqOutlier<-function(p,level=2.576){
  m<-mean(p,na.rm=T)
  lwr<-m-level*sd(p,na.rm=T)
  upr<-m+level*sd(p,na.rm=T)
  p<lwr|p>upr
  }


doOutlier<-wq[wqOutlier(doSat)==T|wqOutlier(DO)==T]

wq[,condOutlier:=wqOutlier(log(Cond)),SiteID]
condOutlier<-wq[condOutlier==T]

wq[,phOutlier:=wqOutlier(pH),SiteID]
phOutlier<-wq[phOutlier==T]

wq[,tdsOutlier:=wqOutlier(log(TDS,+0.001)),SiteID]


wq[yday(sDate) %in% 50:275,yday:=as.numeric(scale(yday(sDate)))]
wq[,yday2:=yday^2]
tempMod<-lmer(Temp~yday+yday2+(1|SiteID),data=wq)
tempPred<-predictInterval(tempMod,which="all",level=0.95) %>%
  .[.$effect=="combined",2:4]
wq[,tempPred:=NULL]

wq[!is.na(Temp)&yday(sDate) %in% 50:275,":="(tempPred=tempPred[,1],
                                             tempLwr=tempPred[,3],
                                             tempUpr=tempPred[,2])]

wq[,tempOutlier:=Temp<tempLwr|Temp>tempUpr]
