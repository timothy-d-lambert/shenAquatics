library(shenAquatics)
library(RODBC)
library(data.table)
library(dplyr)
library(reshape2)
library(tidyr)
library(jagsUI)
library(plotHacks)
library(here)

#connect to database
aqConnector() #produces object "con" to be used to pull data

# tables <- sqlQuery(con,'SELECT * FROM MSysObjects WHERE Type=1 AND Flags=0') #throws error because no read access on system tables

source(here("analysis", "habitatPrep.R"))

sites <- aqData("sites") %>%
  setkey(SiteID) %>%
  .[InPark==1] %>%
  .[,.(SiteID,Aspect_deg,Slope_deg,Elev_m,MAJ_GEOL)]

siteVisits <- aqData("siteVisits") %>%
  setkey(SiteVisit_ID)

game <- aqData("game") %>%
  setkey(SiteVisit_ID)

game <- siteVisits[game,.(sDate,SiteID,SPECIES,RUN,AGE,FishSampleType,TL)] %>%
  .[FishSampleType %in% c("QUANT","QUAL")]

nongame <- aqData("nongame") %>%
  setkey(SiteVisit_ID) %>%
  setnames(c("MIN LENGTH","MAX LENGTH","WEIGHT","NUMBER"),c("min","max","weight","N"))

nongame <- siteVisits[nongame,.(sDate,SiteID,SPECIES,N,RUN,FishSampleType,min,max,weight)] %>%
  .[FishSampleType %in% c("QUANT","QUAL")] %>%
  .[,FishSampleType:=NULL] %>%
  .[,.(sDate,SiteID,SPECIES,RUN,N)]

runs <- rbind(game[,.(sDate,SiteID,RUN)],nongame[,.(sDate,SiteID,RUN)]) %>%
  .[SiteID %in% sites$SiteID] %>%
  .[RUN<=4] %>%
  unique() %>%
  crossing(AGE=0:1) %>%
  data.table() %>%
  setkey(SiteID,sDate,RUN,AGE)

bkt <- game[SPECIES=="BKT"] %>%
  .[TL<80 & (is.na(AGE)|AGE==99),AGE:=0] %>%
  .[TL>110 & (is.na(AGE)|AGE==99),AGE:=0] %>%
  .[TL>120,AGE:=1] %>%
  .[!is.na(AGE) & RUN %in% 1:4,.N,.(sDate,SPECIES,SiteID,RUN,AGE)] %>%
  .[SiteID %in% sites$SiteID] %>%
  setkey(SiteID,sDate,RUN,AGE)


bkt <- bkt[runs] %>%
  .[,allNA:=all(is.na(N)),SiteID] %>%
  .[allNA==FALSE] %>%
  .[,allNA:=NULL] %>%
  .[is.na(N),N:=0] %>%
  setkey(SiteID,sDate)

bkt <- sites[hab[bkt]]

bkt <- bkt[!is.na(MAJ_GEOL)&!is.na(width)] %>%
  .[,nYears:=length(unique(year(sDate))),SiteID] %>%
  # .[nYears>=5] %>%
  .[,":="(year=year(sDate),
          siteNum=as.numeric(as.factor(as.character(SiteID))))] %>%
  .[,yearNum:=year-min(year)+1]

bkt <- bkt[!duplicated(bkt[,.(SiteID,year,RUN,AGE)])]

setkey(bkt[,.(siteNum,yearNum,RUN,AGE)])

allCombos <- crossing(1:max(bkt$siteNum),1:max(bkt$yearNum),1:4,0:1) %>%
  data.table() %>%
  setnames(c("siteNum","yearNum","RUN","AGE")) %>%
  setkey(siteNum,yearNum,RUN,AGE)

########Make Count Data##################
y <- bkt[,.(siteNum,yearNum,RUN,AGE,N)] %>%
  setkey(siteNum,yearNum,RUN,AGE) %>%
  .[allCombos] %>%
  melt(measure.vars="N") %>%
  acast(siteNum~yearNum~RUN~AGE)

#########Covariates#####################
allCombos <- crossing(1:max(bkt$siteNum),1:max(bkt$yearNum)) %>%
  data.table() %>%
  setnames(c("siteNum","yearNum")) %>%
  setkey(siteNum,yearNum)

#Day of year for sample
sampleDay <- bkt[,sDay:=yday(sDate)] %>%
  .[,.(sDay,yearNum,siteNum)] %>%
  unique() %>%
  setkey(siteNum,yearNum)

sampleDay <- sampleDay[allCombos] %>%
  # .[is.na(sDay),sDay:=median(.$sDay,na.rm=T)] %>%
  melt(measure.vars="sDay") %>%
  acast(siteNum~yearNum)

sampleDayStd <- (sampleDay-mean(sampleDay,na.rm=T))/sd(sampleDay,na.rm=T)
sampleDayStd[is.na(sampleDayStd)] <- 0

###Habitat and site characteristics
nCov <- unique(bkt[,.(siteNum,Elev_m,MAJ_GEOL)]) %>%
  setkey(siteNum) %>%
  .[,MAJ_GEOL:=as.numeric(as.factor(MAJ_GEOL))] %>% # Added 10/21/2024. Make sure that the order of numbered MAJ_GEOL matches that used later on...
  melt(id.vars=c("siteNum")) %>%
  acast(siteNum~variable)

elevStats <- c(mean(nCov[,1]),sd(nCov[,1]))

nCov[,1] <- (nCov[,1]-mean(nCov[,1]))/sd(nCov[,1])

pCov <- bkt[, .(siteNum, yearNum, width)] %>%
  setkey(siteNum, yearNum) %>%
  .[allCombos] %>%
  unique() %>%
  melt(id.vars = c("siteNum","yearNum")) %>%
  acast(siteNum ~ yearNum)

for(i in 1:nrow(pCov)){
  pCov[i,is.na(pCov[i,])] <- mean(pCov[i,],na.rm=T)
}

pCov <- (pCov-mean(pCov))/sd(pCov)

nInit <- apply(y,c(1,2,4),sum,na.rm=T)+100


# init <- function() list(mu=runif(2,0,5),
#                         N=nInit,
#                         sdSite=runif(2,0,2),
#                         sdYear=runif(2,0,2),
#                         sigma=runif(2,0,2),
#                         pMean=runif(2,0.2,0.8),
#                         pBeta=rnorm(2),
#                         sdPSite=runif(2,0,1))

init=function() list(N=nInit)

jagsData <- list(nSites=dim(y)[1],
                  nYears=dim(y)[2],
                  sampleDay=sampleDayStd, #[site,year]
                  nCov=nCov, #covariates for N at the site level
                  pCov=pCov, #covariates on p at the site visit level
                  y=y) #observed counts in this format y[site,year,pass]

nb=2000
ni=5000
nt=30
nc=3

parsToSave <- c("mu","sd2Site","sd2Year","sigma2","pMu","pBeta","sd2PSite",
              "beta1","beta2","trend")
modelFile <- here("analysis", "nModel.R") # changed 10/21/2024; previously "analysis/nModelHabCov.R"

out <- jags(data = jagsData,parameters.to.save=parsToSave,inits = init,
           model.file=modelFile,n.chains=nc,n.iter=ni,n.burnin=nb,n.thin=nt,
           parallel=T)


