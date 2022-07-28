#'Make GLIMPSS and VA SCI indices from a data.table of aquatic macroinvertebrate counts
#'
#'@description Calculates GLIMPSS and vA SCI metrics using genus level data (rolled up to family for SCI)
#' organized in a data.table containing columns: count, order, family, ffg, habit, tolVal,
#' famTolVal, famFfg. The calculations are made after grouping by the byCol columns.
#'
#'@return A data.table of the macroinvertebrate indices by the byCol columns
#'
#'@param data A data.table containing counts and required columns listed above
#'@param byCols Vector of names of columns to group by for calculations (e.g., site, date)
#'@param subSize Size of subsample to use as an integer
#'@param includeValues Logical indicating whether to include the columns of intermediate calculations (e.g., percent Ephemeroptera)
#'@param includeScores Logical indicating whether to include columns of scores for each index (e.g., SCI score for percent scrapers)
#'
#'@export

makeMetrics<-function(data,byCols=c("SiteID","sDate"),subSize=200,
                      includeValues=F,includeScores=F){
  sub<-data[rep(1:nrow(data),count)] %>%
    .[,big:=.N>subSize,by=byCols] %>%
    .[big==F,include:=T] %>%
    .[big==T,include:=1:nrow(.SD) %in% sample(1:nrow(.SD),subSize),by=byCols] %>%
    .[include==T,.(count=.N),setdiff(names(data),c("count"))]

  metrics<-sub[,.(totalAbun=sum(count),
                  #GLIMPSS metrics
                  #chiro percent replaces chiro fams metric
                  #VA tolerance values used
                  numIntol4=sum(tolVal<4,na.rm=T),
                  ephemRich=length(count[order=="Ephemeroptera"&!is.na(order)]),
                  plecRich=length(count[order=="Plecoptera"&!is.na(order)]),
                  trichRich=length(count[order=="Trichoptera"&!is.na(order)]),
                  ephemPerc=sum(count[order %in% c("Ephemeroptera") & !is.na(order)])/sum(count),
                  dom5=sum(count[order(count,decreasing=T)][1:5])/sum(count),
                  ptv=sum(tolVal*count,na.rm=T)/sum(count[!is.na(tolVal)]),
                  scrapRich=length(count[ffg=="Scraper"&!is.na(ffg)]),
                  clingRich=length(count[habit=="Clinger"&!is.na(habit)]),
                  chiroPerc=sum(count[family=="Chironomidae"&!is.na(family)])/sum(count),
                  famRich=length(unique(family[!is.na(family)])),
                  eptFamRich=length(unique(family[!is.na(family)&order %in% c("Plecoptera","Ephemeroptera","Trichoptera")])),
                  ptMinusHydro=(sum(count[order %in% c("Plecoptera","Trichoptera") & !is.na(order)])-
                                  sum(count[family=="Hydropsychidae" & !is.na(family)]))/sum(count),
                  leuctraPerc=sum(count[order=="Plecoptera"&!is.na(order)&genus=="Leuctra"]/
                                    sum(count[order=="Plecoptera"&!is.na(order)])),
                  scrapPerc=sum(count[famFfg=="Scraper"&!is.na(famFfg)])/sum(count[!is.na(famFfg)]),
                  famPtv=sum(famTolVal*count,na.rm=T)/sum(count[!is.na(famTolVal)])),
                  by=byCols]
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

metrics[,":="(numIntol4Score=indicize(numIntol4,1,19),
              ephemRichScore=indicize(ephemRich,1,10),
              plecRichScore=indicize(plecRich,0,8),
              trichRichScore=indicize(trichRich,1,7),
              clingRichScore=indicize(clingRich,4,20),
              scrapRichScore=indicize(scrapRich,0,8),
              ptvScore=indicize(ptv,2.23,6.18,rev=T),
              dom5Score=indicize(dom5,0.48,0.92,rev=T),
              ephemPercScore=indicize(ephemPerc,0.005,0.597),
              chiroPercScore=indicize(chiroPerc,0.033,0.688,rev=T),
              famRichScore=min(c(100,100*famRich/22)),
              eptFamRichScore=min(c(100,100*eptFamRich/11)),
              ephemPercSciScore=min(c(100,100*ephemPerc/0.613)),
              ptMinusHydroScore=min(c(100,100*ptMinusHydro/0.356)),
              scrapPercScore=min(c(100,100*scrapPerc/0.516)),
              chiroPercSciScore=100*(1-chiroPerc),
              dom2Score=min(100,100*(1-dom2)/(1-0.308)),
              famPtvScore=100*(10-famPtv)/(10-3.2)),
              by=byCols]

metrics[,":="(glimpss=mean(c(numIntol4Score,ephemRichScore,plecRichScore,
                       trichRichScore,clingRichScore,scrapRichScore,
                       ptvScore,dom5Score,ephemPercScore,chiroPercScore)),
              sci=mean(c(famRichScore,eptFamRichScore,ephemPercSciScore,
                     ptMinusHydroScore,scrapPercScore,
                     chiroPercSciScore,dom2Score,famPtvScore))),
        by=byCols]

if(includeValues){
  return(metrics)
}
if(includeScores){
  includeCols<-c(byCols,
                 names(metrics)[grepl("Score",names(metrics))],
                 "glimpss","sci")} else{
    includeCols<-c(byCols,"glimpss","sci")}
  metrics[,includeCols,with=F]
}

