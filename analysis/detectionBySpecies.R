library(mapShen)
library(RODBC)
library(data.table)
library(dplyr)
library(reshape2)
library(tidyr)
library(jagsUI)
library(plotHacks)

#connect to database
aqConnector() #produces object "con" to be used to pull data

# tables<-sqlQuery(con,'SELECT * FROM MSysObjects WHERE Type=1 AND Flags=0') #throws error because no read access on system tables

sites<-sqlQuery(con,"select * from R_zdd_Sites") %>%
  data.table() %>%
  setkey(SiteID)

siteVisits<-sqlQuery(con,"select * from R_SiteVisits") %>%
  data.table() %>%
  setkey(SiteVisit_ID)

game<-sqlQuery(con,"select * from W_FI_Gamefish") %>%
  data.table() %>%
  setkey(SiteVisit_ID)

game<-siteVisits[game,.(sDate,SITEID,SPECIES,RUN,FishSampleType)] %>%
  .[FishSampleType %in% c("QUANT")] %>%
  .[,FishSampleType:=NULL]

game<-game[,.N,.(sDate,SPECIES,SITEID,RUN)] %>%
  data.table()

nongame<-sqlQuery(con,"select * from W_FI_Non_Gamefish") %>%
  data.table() %>%
  setkey(SiteVisit_ID)

nongame<-siteVisits[nongame,.(sDate,SITEID,SPECIES,NUMBER,RUN,FishSampleType)] %>%
  .[FishSampleType %in% c("QUANT")] %>%
  .[,FishSampleType:=NULL] %>%
  setnames("NUMBER","N") %>%
  .[,.(sDate,SITEID,SPECIES,RUN,N)]

fish<-rbind(game,nongame) %>%
  .[SITEID %in% sites[InPark==1,SiteID]]

runs<-fish[,.(nRuns=max(RUN)),.(sDate,SITEID)] %>%
  .[nRuns<=4] %>%
  .[,.(run=1:nRuns),.(sDate,SITEID)] %>%
  setkey(SITEID,sDate,run)

fish<-fish[!SPECIES %in% c("NogFISH","NOgFISH","NOFISH","UNK")&N!=0]
fish[,SPECIES:=tolower(as.character(SPECIES))] %>%
  .[RUN<=4]
setkey(fish,sDate,SITEID,SPECIES,RUN)

allSp<-data.table(sp=unique(fish$SPECIES))
runSp<-crossing(runs,allSp) %>%
  data.table() %>%
  setkey(sDate,SITEID,sp,run)

fish<-fish[runSp] %>%
  .[is.na(N),N:=0] %>%
  .[,SITEID:=as.character(SITEID)] %>%
  .[,pres:=as.numeric(any(N>0)),.(sDate,SITEID,SPECIES)] %>%
  .[pres==0,pres:=NA] %>%
  .[,speciesNum:=as.numeric(as.factor(SPECIES))] %>%
  .[RUN<=3]

occ<-melt(fish,measure.vars="N") %>% acast(sDate+SITEID~SPECIES~RUN)

pres<-apply(occ,c(1,2),function(x){as.numeric(any(x>=1))})

p<-fish[pres==T,.(p=sum(N>0)/.N,
                  .N),
        .(SPECIES,RUN,speciesNum)] %>%
  .[,logitP:=log(p/(1-p))]


jagsData<-list(nRows=nrow(fish[pres==1]),
               species=fish[pres==1,speciesNum],
               nSpecies=max(fish[pres==1,speciesNum]),
               run=fish[pres==1,RUN],
               # z=fish[pres==1,pres],
               obs=as.numeric(fish[pres==1,N]>0))

nb=10000
ni=20000
nt=2
nc=3

#this model assumes a linear decrease in detection probability across runs on the logit scale, which doesn't quite match the data
parsToSave<-c("beta0","beta1","beta0Mean","beta0Eps","beta1Mean","beta1Eps","p")
modelFile="analysis/speciesDetectionModel.R"

out<-jags(data = jagsData,parameters.to.save=parsToSave,
          model.file=modelFile,n.chains=nc,n.iter=ni,n.burnin=nb,n.thin=nt,
          parallel=T)

parsToSave2<-c("p1Mean","p1Eps","p2Beta0","p2Beta1","p2Eps","p3Beta0","p3Beta1","p3Eps","p")
modelFile2="analysis/speciesDetectionModel2.R"

out2<-jags(data = jagsData,parameters.to.save=parsToSave2,
          model.file=modelFile2,n.chains=nc,n.iter=ni,n.burnin=nb,n.thin=nt,
          parallel=T)

par(mfrow=c(1,1))
for(s in 1:length(unique(fish$SPECIES))){
  plot(p~RUN,data=p[SPECIES==unique(fish$SPECIES)[s]],type='l',
       xlim=c(1,3),ylim=c(0,1),
       main=paste(unique(fish$SPECIES)[s],paste(N,collapse=", ")))
  points(p~RUN,data=p[SPECIES==unique(fish$SPECIES)[s]],pch=19)

  points(out$mean$p[s,]~I(1:3),type='l',col='gray')
  points(out$mean$p[s,]~I(1:3),pch=19,col='gray')
  error.bar(1:3,out2$mean$p[s,],
            out$q97.5$p[s,],out$q2.5$p[s,],
            col='gray',interval.type="other")

  points(out2$mean$p[s,]~I(1:3),type='l',col='blue')
  points(out2$mean$p[s,]~I(1:3),pch=19,col='blue')
  error.bar(1:3,out2$mean$p[s,],
            out2$q97.5$p[s,],out2$q2.5$p[s,],
            col='blue',interval.type="other")
}

plot(NA,xlim=c(1,3),ylim=c(0,1))
for(s in 1:max(fish$speciesNum)){
  points(out2$mean$p[s,]~I(1:3),type='l')
}

par(mfrow=c(1,3))
plot(NA,xlim=c(0,1),ylim=c(0,100))
for(s in 1:dim(out$mean$p)[1]){
  points(density(apply(1-out2$sims.list$p[,s,],1,prod)),type='l',lwd=2,col=gray(0.5,0.5))
}

plot(NA,xlim=c(0,1),ylim=c(0,100))
for(s in 1:dim(out$mean$p)[1]){
  points(density(apply(1-out2$sims.list$p[,s,1:2],1,prod)),type='l',lwd=2,col=gray(0.5,0.5))
}


plot(NA,xlim=c(0,1),ylim=c(0,100))
for(s in 1:dim(out$mean$p)[1]){
  points(density(1-out2$sims.list$p[,s,1]),type='l',lwd=2,col=gray(0.5,0.5))
}


parsToSave3<-c("p1Mean","p1Eps","p2Beta0","p2Beta1","p2Eps","p3Beta0","p3Beta1","p3Eps","p")
modelFile3="analysis/speciesDetectionModel3.R"

out3<-jags(data = jagsData,parameters.to.save=parsToSave3,
           model.file=modelFile3,n.chains=nc,n.iter=ni,n.burnin=nb,n.thin=nt,
           parallel=T)

