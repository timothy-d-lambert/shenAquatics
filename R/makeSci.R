##required columns are count, order, family, famFfg, famHabit, famTolVal

makeSci<-function(data,byCols=c("SiteID","sDate"),subSize=200,
                      includeValues=F,includeScores=F){
  sub<-data[rep(1:nrow(data),count)] %>%
    .[,big:=.N>subSize,by=byCols] %>%
    .[big==F,include:=T] %>%
    .[big==T,include:=1:nrow(.SD) %in% sample(1:nrow(.SD),subSize),by=byCols] %>%
    .[include==T,.(count=.N),setdiff(names(data),c("count"))]
  
  metrics<-sub[,.(totalAbun=sum(count),
                  #VA SCI metrics based on family values
                  famRich=length(unique(family[!is.na(family)])),
                  eptFamRich=length(unique(family[!is.na(family)&order %in% c("Plecoptera","Ephemeroptera","Trichoptera")])),
                  ephemPerc=sum(count[order %in% c("Ephemeroptera") & !is.na(order)])/sum(count),
                  ptMinusHydro=(sum(count[order %in% c("Plecoptera","Trichoptera") & !is.na(order)])-
                                  sum(count[family=="Hydropsychidae" & !is.na(family)]))/sum(count),
                  scrapPerc=sum(count[famFfg=="Scraper"&!is.na(famFfg)])/sum(count[!is.na(famFfg)]),
                  chiroPerc=sum(count[family=="Chironomidae"&!is.na(family)])/sum(count),
                  famPtv=sum(famTolVal*count,na.rm=T)/sum(count[!is.na(famTolVal)])),
               by=byCols]
  #must combine within families for dominant 2 at family level
metrics<-cbind(metrics,
               sub[,.(count=sum(count)),
                   by=c("order","family",byCols)] %>% 
                 .[,.(dom2=sum(count[order(count,decreasing=T)][1:2])/sum(count)),
                   by=byCols] %>%
                 .[,.(dom2)])

indicize<-function(x,lower,upper,rev=F){
  index<-(upper-max(c(min(c(upper,x)),lower)))/(upper-lower)
  if(!rev){index<-1-index}
  return(index)
}

metrics[,":="(famRichScore=min(c(100,100*famRich/22)),
              eptFamRichScore=min(c(100,100*eptFamRich/11)),
              ephemPercScore=min(c(100,100*ephemPerc/0.613)),
              ptMinusHydroScore=min(c(100,100*ptMinusHydro/0.356)),
              scrapPercScore=min(c(100,100*scrapPerc/0.516)),
              chiroPercScore=100*(1-chiroPerc),
              dom2Score=min(100,100*(1-dom2)/(1-0.308)),
              famPtvScore=100*(10-famPtv)/(10-3.2)),
              by=byCols]
metrics[,sci:=mean(c(famRichScore,eptFamRichScore,ephemPercScore,
                         ptMinusHydroScore,scrapPercScore,
                         chiroPercScore,dom2Score,famPtvScore)),
        by=byCols]

if(includeValues){
  metrics
}
if(includeScores){
  includeCols<-c(byCols,
                 names(metrics)[grepl("Score",names(metrics))],
                 "sci")} else{
    includeCols<-c(byCols,"sci")}
  metrics[,includeCols,with=F]
}

