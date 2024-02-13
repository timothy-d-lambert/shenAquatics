library(shiny)
library(mapShen)
library(readxl)
library(ggplot2)
library(lme4)
library(MASS)
library(rMR)
library(merTools)
library(DT)

###newDb is the database from which to add new data
includeEntry<-F

newDb<-"data/AQUATIC_Data_Entry_01_be_xp.accdb"
oldDb<-"data/All_Stream_data_2020.accdb"

conNew<-odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                        DBQ=",newDb))
conOld<-odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                        DBQ=",oldDb))
############################
#####Non Game###############
###########################

if(includeEntry){
  ng1<-aqData("nongame",conTemp=conNew) %>%
    .[,type:="new"]
} else{ng1<-NULL}
nongame<-rbind(ng1,
  aqData("nongame") %>%
    .[,-"Counter"] %>%
    .[,type:="old"]) %>%
  setnames(c("MIN LENGTH","MAX LENGTH"),c("min","max")) %>%
  .[NUMBER==1,length:=mean(c(min,max),na.rm=T),.(SiteVisit_ID,SPECIES,RUN)] %>%
  .[,SPECIES:=tolower(SPECIES)]

meanWeight<-nongame[,.(meanWeight=WEIGHT/NUMBER),.(SiteID,sDate,SPECIES,RUN,type)]
setkey(meanWeight,SPECIES,meanWeight)

lw20<-read_excel("data/Non-game Length to Weight TEMP.xlsx") %>%
  data.table() %>%
  setnames(c("site","run","species","length",'weight',"notes")) %>%
  .[,species:=tolower(species)] %>%
  .[,weight:=round(as.numeric(weight),1)] %>%
  .[,length:=as.numeric(length)] %>%
  .[,-"notes"]

sp<-unique(nongame$SPECIES)
sp<-sp[!grepl("un",sp)]
sp<-sp[!sp %in% c("nofish","notshockd","bkt","rob","brt","rbt","smb","pks","rdb","cap","lmb","ped","rys","abl","sws","ame")]

nongame<-nongame[SPECIES %in% sp] %>%
  .[,year:=year(sDate)]

minBiggerMax<-weightOutliers<-mortBiggerCount<-NULL

colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# for(s in sp){
#
#   mortBiggerCount<-rbind(mortBiggerCount,nongame[MORTALITY>NUMBER])
#
#

setkey(lw20,site)
setkey(nongame,SiteID)

lw20<-unique(nongame[year(sDate)==2020,.(SiteID,sDate)])[lw20]

nongameLW<-rbind(nongame[NUMBER==1&!is.na(WEIGHT),.(SiteID,sDate,SPECIES,RUN,length,WEIGHT)] %>%
                   setnames("WEIGHT","weight"),
                 lw20[weight!=0&length>10,] %>%
                   setnames(c("SiteID","sDate","RUN","SPECIES","length","weight")) %>%
                   .[,.(SiteID,sDate,SPECIES,RUN,length,weight)])

predLW<-function(length,weight){
  if(length(weight)>2){
  lwMod<-rlm(log(weight)~log(length))
  pred<-exp(predict(lwMod,interval="prediction",level=0.995))
  list(pred[,1],pred[,2],pred[,3])
  }else{
  list(rep(NA,length(length)),
        rep(NA,length(length)),
        rep(NA,length(length)))
  }
}

nongameLW[!is.na(length)&!is.na(weight),c("fit","lwr","upr"):=predLW(length,weight),SPECIES]
nongameLW[,":="(fit=round(fit,2),lwr=round(lwr,2),upr=round(upr,2),
                length=round(length),weight=round(weight,2))]

nongame[,sDate:=as.Date(sDate)]
meanWeight[,":="(sDate=as.Date(sDate),
                 meanWeight=round(meanWeight,2))]

setkey(nongame,SPECIES)
setkey(nongameLW,SPECIES,length)

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
    .[,type:="old"]) %>%
  .[,":="(year=year(sDate),
          logTl=log(TL),
          logWt=log(WT))] %>%
  .[SPECIES!="NOgFISH"]

getOutliers<-function(dat,level=0.999){
  lw<-lm(log(WT)~log(TL),data=dat)
  prediction<-predict(lw,interval="prediction",level=level)
  dat[,list(exp(prediction[,1]),
             exp(prediction[,2]),
             exp(prediction[,3]))]
  # plot(log(WT)~log(TL),data=dat,main=unique(dat$SPECIES))
  # points(log(WT)~log(TL),data=o,pch=19,col='gray')
  # points(log(WT)~log(TL),data=o[type=="new"],pch=19,col='blue')

}

game[!is.na(logTl)&!is.na(logWt),c("fit","lwr","upr"):=getOutliers(.SD,level=0.999),SPECIES]
game[!is.na(logTl)&!is.na(logWt)&SPECIES=="BKT",
     c("fit","lwr","upr"):=getOutliers(.SD,level=0.9999),
     .(logTl>log(90))]
setkey(game,SPECIES,TL)

game[,":="(fit=round(fit,2),lwr=round(lwr,2),upr=round(upr,2))]

# svg.par("O:/Working/AQUATIC/Database/Data checking/gameSize/lengthWeight.svg",
#         mfrow=c(length(unique(data[type=="new",SPECIES])),1),
#         height=length(unique(data[type=="new",SPECIES]))*4,
#         width=6,mar=c(3.5,3.5,1,0))
# dev.off()
#
# svg.par("O:/Working/AQUATIC/Database/Data checking/gameSize/weightHists.svg",
#         mfrow=c(sum(outliers$type=="new"),1),height=sum(outliers$type=="new")*3,
#         width=6,mar=c(3.5,3.5,1,0))
# for(i in which(outliers$type=="new")){
#
#   hist(data[SPECIES==outliers$SPECIES[i]&TL==outliers$TL[i],WT],
#        main=outliers[i,paste0(SPECIES,", TL = ",TL,", WT = ",round(WT,2),", ",SiteID,", ",sDate,", Entry: ",EntryOrder)],
#        breaks=seq(0,ceiling(data[SPECIES==outliers$SPECIES[i]&TL==outliers$TL[i],max(WT,na.rm=T)]),0.1),
#        xlab="Weight (g)",ylab="Frequency of Fish with Same TL")
#   abline(v=outliers[i,WT],col='red',lwd=3)
#   abline(v=outliers[i,c(lwr,upr)],lty=2)
# }
# dev.off()

# write.csv(outliers[type=="new"],"O:/Working/AQUATIC/Database/Data checking/gameSize/gameSizeOutliers.csv")
nGameSp<-game[!is.na(logTl)&!is.na(logWt),length(unique(SPECIES))]
game[,sDate:=as.Date(sDate)]
game[,WT:=round(WT,2)]
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
wq[,hour:=hour(Time)+minute(Time)/60]
tempMod<-lmer(Temp~yday+hour+yday2+(1|SiteID),data=wq)
tempPred<-predictInterval(tempMod,which="all",level=0.95) %>%
  .[.$effect=="combined",2:4]
wq[,tempPred:=NULL]

wq[!is.na(Temp)&yday(sDate) %in% 50:275 &!is.na(hour),":="(tempPred=tempPred[,1],
                                             tempLwr=tempPred[,3],
                                             tempUpr=tempPred[,2])]

wq[,tempOutlier:=Temp<tempLwr|Temp>tempUpr]
wq[,dayOfYear:=as.Date("2022-12-31")+yday(sDate)]

wq[,":="(sDate=as.Date(sDate),hour=round(hour,1),
        Temp=round(Temp,2),tempPred=round(tempPred,2),
        tempLwr=round(tempLwr,2),tempUpr=round(tempUpr,2),
        DO=round(DO,2),doSat=round(doSat,2),
        pH=round(pH,2),Cond=round(Cond),TDS=round(TDS,4))]

q<-rbind(makeDischarge(conNew),
         makeDischarge(conOld)) %>%
  .[,sDate:=as.Date(sDate)] %>%
  setkey(SiteID,sDate) %>%
  .[!duplicated(.)]

setkey(wq,SiteID,sDate)

wq<-q[wq]



################################################################
#############   Shiny App   ###################################
###############################################################

ui <-fluidPage(
  tabsetPanel(
              tabPanel("Game",
                       fluidRow(
                         column(6,
                                fluidRow(column(6,selectInput("s","Species:",
                                              choices=unique(game$SPECIES))[order(unique(game$SPECIES))]),
                                         column(6,selectInput("y","Highlight Year",
                                              choices=unique(game$year)[rev(order(unique(game$year)))],
                                              selected=max(game$year)))),
                                fluidRow(column(12,uiOutput("gamePlotUI",
                                                          click="gamePlotClick"),
                                               value="gamePlot")),
                                fluidRow(column(12,h4("Selected Point"),
                                                   verbatimTextOutput("gameClick"))),
                                fluidRow(column(12,h4("Histogram of weights for fish with same TL as selection"))),
                                fluidRow(column(12,uiOutput("gameHistUI"),
                                                  value="gameHist"))
                                ),
                         column(6,
                                fluidRow(column(6,radioButtons("allGSp","All Species in Table?",choices=c("Yes","No"))),
                                         column(6,radioButtons("allGY","All Years in Table?",choices=c("Yes","No"),
                                                      selected="No"))),
                                fluidRow(column(12,dataTableOutput("gameTable")))
                                )
                         )
                       ),
              tabPanel("Nongame",
                       fluidRow(style = "border-bottom: solid;",
                         column(3,selectInput("ngSp","Species:",
                                              choices=unique(nongame$SPECIES)[order(unique(nongame$SPECIES))])),
                         column(2,selectInput("ngY","Highlight Year",
                                              choices=unique(nongame$year)[rev(order(unique(nongame$year)))],
                                              selected=max(nongame$year)))
                       ),
                       fluidRow(column(6,
                                       fluidRow(
                                         column(12,uiOutput("nongamePlotUI1"),
                                                  value="nongamePlot1")),
                                       fluidRow(style = "border-bottom: solid;",
                                                column(12,sliderInput("ngSlider1",
                                                                      "Min Table Filter",
                                                                      round(min(nongame$min,na.rm=T)),
                                                                      round(max(nongame$min,na.rm=T)),
                                                                      value=c(round(max(nongame$min,na.rm=T))-1,
                                                                              round(max(nongame$min,na.rm=T))),
                                                                      step=1,width="100%"))
                                       )),
                                column(6,
                                       fluidRow(
                                         column(12,dataTableOutput("nongameTable1")))
                                       )
                                ),
                       fluidRow(column(6,
                                       fluidRow(
                                         column(12,uiOutput("nongamePlotUI2"),
                                                  value="nongamePlot2")
                                         ),
                                       fluidRow(style = "border-bottom: solid;",
                                                column(12,sliderInput("ngSlider2",
                                                                      "Max Table Filter",
                                                                      round(min(nongame$max,na.rm=T)),
                                                                      round(max(nongame$max,na.rm=T)),
                                                                      value=c(round(max(nongame$max,na.rm=T))-1,
                                                                              round(max(nongame$max,na.rm=T))),
                                                                      step=1,width="100%")))
                                       ),
                                column(6,fluidRow(column(12,dataTableOutput("nongameTable2"))))
                       ),
                       fluidRow(
                         column(6,
                                fluidRow(column(12,uiOutput("nongamePlotUI3",
                                                           click="nongamePlotClick3"),
                                                value="nongamePlot3")),
                                fluidRow(style = "border-bottom: solid;",
                                         column(12,
                                                h4("Selected Point"),
                                                verbatimTextOutput("nongameClick3")))
                         ),
                         column(6,
                                fluidRow(column(12,dataTableOutput("nongameTable3"))),
                                fluidRow(column(6,radioButtons("allNgSp","All Species in Table?",choices=c("Yes","No"))),
                                         column(6,radioButtons("allNgY","All Years in Table?",choices=c("Yes","No"),
                                                                selected="No")))
                                )
                       ),
                       fluidRow(
                         column(6,
                                fluidRow(column(12,uiOutput("nongamePlotUI4"),
                                                    value="nongamePlot4")),
                                fluidRow(column(12,offset=3,
                                                sliderInput("ngSlider4",
                                                            "Mean Weight Table Filter",
                                                            round(min(meanWeight$meanWeight,na.rm=T)),
                                                            round(max(meanWeight$meanWeight,na.rm=T)),
                                                            value=c(round(max(meanWeight$meanWeight,na.rm=T))-1,
                                                                    round(max(meanWeight$meanWeight,na.rm=T))),
                                                            step=1,width="100%")))
                         ),
                         column(6,
                                fluidRow(column(12,dataTableOutput("nongameTable4")))
                                )
                       )
              ),
              tabPanel("Temp",
                       fluidRow(
                         column(6,
                                fluidRow(column(12,uiOutput("tempPlotSiteUI"))),
                                fluidRow(column(12,uiOutput("tempPlotYearUI")))
                                ),
                         column(6,
                                fluidRow(column(12,DTOutput("tempTable")))
                       )
                       )
              ),
              tabPanel("DO",
                       fluidRow(
                         column(6,
                                fluidRow(column(12,uiOutput("doPlotSiteUI"))),
                                fluidRow(column(12,uiOutput("doPlotSite2UI"))),
                                fluidRow(column(12,uiOutput("doPlotYearUI")))
                         ),
                         column(6,
                                fluidRow(column(12,sliderInput("doSlider",
                                                               "Weird DO Thresholds (% Saturation)",
                                                               0,2,
                                                               value=c(0.85,1.15),
                                                               step=0.01,width="100%"))),
                                fluidRow(column(12,DTOutput("doTable"))),
                                fluidRow(column(12,uiOutput("doHistUI")))
                         )
                       )
              ),
              tabPanel("pH",
                       fluidRow(
                         column(6,
                                fluidRow(column(12,uiOutput("phPlotSiteUI"))),
                                fluidRow(column(12,uiOutput("phPlotQUI"))),
                                fluidRow(column(12,uiOutput("phPlotYearUI")))
                         ),
                         column(6,
                                fluidRow(column(12,DTOutput("phTable")))
                         )
                       )
              ),
              tabPanel("Cond/TDS",
                       fluidRow(
                         column(8,
                                fluidRow(column(6,uiOutput("condPlotSiteUI")),
                                         column(6,uiOutput("tdsPlotSiteUI"))
                                ),
                                fluidRow(column(6,uiOutput("condPlotQUI")),
                                         column(6,uiOutput("tdsPlotQUI"))
                                ),
                                fluidRow(column(6,uiOutput("tdsCondPlotUI"))
                                )
                         ),
                         column(4,
                                fluidRow(column(12,DTOutput("condTable")))
                         )
                       )
              )
  )
)

server <- function(input, output) {
############# GAME ##########################
  gameDat<-reactive(game[!is.na(logTl)&!is.na(logWt)&SPECIES==input$s,] %>%
                      .[,highlight:=ifelse(year==input$y,input$y,"other")] %>%
                      setorder(-highlight))

  output$gamePlot <- renderPlot({

    ggplot(gameDat())+
      geom_point(aes(logTl,logWt,color=highlight),size=5)+
      scale_color_manual(values=c("red",gray(0.5,0.1)),
                         name=NULL)+
      geom_line(aes(logTl,log(fit)),size=1.5)+
      geom_line(aes(logTl,log(lwr),linetype="dashed"))+
      geom_line(aes(logTl,log(upr),linetype="dashed"))+
      theme_classic(base_size=20)

  })

  output$gamePlotUI<-renderUI(plotOutput("gamePlot",height=400,click="gamePlotClick"))

  gameOutlier<-reactive(game[(SPECIES==input$s|input$allGSp=="Yes")&
                                       ((year(sDate)==input$y&!is.na(sDate))|input$allGY=="Yes")&
                                       (WT>upr|WT<lwr),
                             .(SiteID,sDate,EntryOrder,SPECIES,RUN,TL,WT,fit,lwr,upr)])

  output$gameTable<-renderDataTable(
    gameOutlier()[,.(SiteID,sDate,EntryOrder,SPECIES,RUN,TL,WT,fit,lwr,upr)]
  )



    output$gameClick<-renderPrint(
      nearPoints(gameDat()[,.(SiteID,sDate,EntryOrder,SPECIES,RUN,TL,WT,
                              fit=round(fit,2),lwr=round(lwr,2),upr=round(upr,2),logTl,logWt)],
                 input$gamePlotClick,
                 maxpoints=5)
    )



  output$gameHist<-renderPlot(
    ggplot()+
      geom_histogram(data=gameDat()[TL==nearPoints(gameDat(),
                                              input$gamePlotClick,
                                              maxpoints=1)$TL] %>%
                       .[,highlight:=
                           TL==nearPoints(gameDat(),input$gamePlotClick,maxpoints=1)$TL&
                           WT==nearPoints(gameDat(),input$gamePlotClick,maxpoints=1)$WT],
                     aes(WT,fill=highlight))+
      scale_fill_manual(values=c("gray","red"),name=NULL,labels=c("other","selected"))+
      geom_vline(data=nearPoints(gameDat(),
                                 input$gamePlotClick,
                                 maxpoints=1),
                 aes(xintercept=upr),linetype="dashed")+
      geom_vline(data=nearPoints(gameDat(),
                                 input$gamePlotClick,
                                 maxpoints=1),
                 aes(xintercept=lwr),linetype="dashed")+
      ggtitle(paste("TL =",nearPoints(gameDat(),
                         input$gamePlotClick,
                         maxpoints=1)$TL))
      # geom_vline(data=nearPoints(gameDat(),
      #                            input$gamePlotClick,
      #                            maxpoints=1),
      #            aes(xintercept=WT),color="red",linewidth=3)
  )
  output$gameHistUI<-renderUI(plotOutput("gameHist",height=400))
############### NONGAME ########################################

  nongameDat<-reactive(nongame[SPECIES==input$ngSp,] %>%
                         .[,highlight:=ifelse(year==input$ngY,
                                                 as.character(input$ngY),
                                                 "other")])
  nongameLWDat<-reactive({
    nongameLW[SPECIES==input$ngSp] %>%
      .[,highlight:=ifelse(year(sDate)==input$ngY&!is.na(sDate),input$ngY,"other")]
  })

  nongameMeanWeight<-reactive(meanWeight[SPECIES==input$ngSp&!is.na(meanWeight)] %>%
                                .[,highlight:=ifelse(year(sDate)==input$ngY,input$ngY,"other")])


  output$nongamePlot1 <- renderPlot({

    ggplot(nongameDat(),aes(min,fill=highlight))+
      geom_histogram(binwidth = 1)+
      scale_fill_manual(values=c("red","gray"),name=NULL)+
      theme_classic(base_size=20)+
      ggtitle("Min Length")


  })
  output$nongamePlotUI1<-renderUI(plotOutput("nongamePlot1",height=400))

  minSubset<-reactive(nongame[min>=input$ngSlider1[1]&
                                min<=input$ngSlider1[2]&
                                SPECIES==input$ngSp,
                              .(SiteID,sDate,RUN,SPECIES,min)])

  output$nongameTable1<-renderDataTable(minSubset(),
                                        options=list(pageLength=10,
                                                     lengthMenu=c(10,15,25),
                                                     order=list(4,"desc")))

  output$nongamePlot2 <- renderPlot({
    ggplot(nongameDat(),aes(max,fill=highlight))+
      scale_fill_manual(values=c("red","gray"),name=NULL)+
      geom_histogram(binwidth = 1)+
      theme_classic(base_size=20)+
      ggtitle("Min Length")


  })

  output$nongamePlotUI2<-renderUI(plotOutput("nongamePlot2",height=400))

  maxSubset<-reactive(nongame[max>=input$ngSlider2[1]&
                              max<=input$ngSlider2[2]&
                                SPECIES==input$ngSp,
                              .(SiteID,sDate,RUN,SPECIES,max)])

  output$nongameTable2<-renderDataTable(maxSubset(),
                                        options=list(pageLength=10,
                                                     lengthMenu=c(10,15,25),
                                                     order=list(4,"desc")))

  output$nongamePlot3 <- renderPlot({
    ggplot(data=nongameLWDat(),aes(x=length))+
      geom_point(aes(y=weight,color=highlight),alpha=ifelse(nongameLWDat()$highlight==input$ngY,1,0.5),size=5)+
      geom_line(aes(y=fit))+
      geom_line(aes(y=upr),linetype=2)+
      geom_line(aes(y=lwr),linetype=2) +
      scale_color_manual(values=c("red",gray(0.5,0.4)),
                         name=NULL)+
      theme_classic(base_size=20)+
      ggtitle("Length vs Weight")+
      xlab("Length (mm)")+
      ylab("Weight (g)")

  })

  output$nongamePlotUI3<-renderUI(plotOutput("nongamePlot3",height=400,click="nongamePlotClick3"))

  nongameOutlier<-reactive(nongameLW[(SPECIES==input$ngSp|input$allNgSp=="Yes")&
                                       ((year(sDate)==input$ngY&!is.na(sDate))|input$allNgY=="Yes")&
                                       (weight>upr|weight<lwr)])

  output$nongameTable3<-renderDataTable(nongameOutlier(),
                                        options=list(pageLength=10,
                                                     lengthMenu=c(10,15,25)))

  output$nongameClick3<-renderPrint(
    nearPoints(nongameLWDat()[,.(SiteID,sDate,SPECIES,RUN,length,weight,fit,lwr,upr)],
               input$nongamePlotClick3)
  )


  output$nongamePlot4 <- renderPlot({

      ggplot(nongameMeanWeight(),aes(x=meanWeight,fill=highlight))+
        geom_histogram(bins=50)+
        scale_fill_manual(values=c("red","gray"),name=NULL)+
        theme_classic(base_size=20)

  })
  output$nongamePlotUI4<-renderUI(plotOutput("nongamePlot4",height=400))

  meanWeightSubset<-reactive(nongameMeanWeight()[meanWeight>=input$ngSlider4[1]&
                                                   meanWeight<=input$ngSlider4[2]])

  output$nongameTable4<-renderDataTable(meanWeightSubset()[,.(SiteID,sDate,SPECIES,RUN,meanWeight)],
                                        options=list(order=list(4,"desc")))



########################### Temp ##################################################

  tempSite<-reactive(
    wq[!is.na(tempOutlier)&tempOutlier==T,.(SiteID,sDate,hour,Temp,tempPred,
                                            tempLwr,tempUpr,dayOfYear)] %>%
      .[,SiteID[input$tempTable_rows_selected]]
    )

  tempDate<-reactive(
    wq[!is.na(tempOutlier)&tempOutlier==T,.(SiteID,sDate,hour,Temp,tempPred,
                                            tempLwr,tempUpr,dayOfYear)] %>%
      .[,sDate[input$tempTable_rows_selected]]
  )

  tempHighlight<-reactive(wq[!is.na(Temp)] %>%
                            .[,":="(siteMatch=SiteID==tempSite(),
                                    dateMatch=sDate==tempDate(),
                                    highlight=SiteID==tempSite()&sDate==tempDate())]
                          )

  output$tempTest<-renderPrint(tempHighlight()[highlight==T,paste(SiteID,year(sDate))])

  output$tempPlotSite<-renderPlot(
    ggplot(tempHighlight()[siteMatch==T],aes(x=dayOfYear,y=Temp,color=highlight))+
      geom_point(size=5)+
      theme_classic(base_size=20)+
      scale_color_manual(values=c(gray(0.5,0.4),"red"),
                         labels=c("other","selected"),
                         name=NULL)+
      ggtitle(tempSite())+
      ylab("Temp")+
      xlab("")
  )
  output$tempPlotSiteUI<-renderUI(plotOutput("tempPlotSite",height=400))


  output$tempPlotYear<-renderPlot(
    ggplot(tempHighlight()[sDate>(tempDate()-21)&sDate<(tempDate()+21)],aes(x=dayOfYear,y=Temp-tempPred,color=highlight))+
      geom_point(size=5)+
      theme_classic(base_size=20)+
      scale_color_manual(values=c(gray(0.5,0.4),"red"),
                         labels=c("other","selected"),
                         name=NULL)+
      ggtitle(year(tempDate()))+
      ylab("Temp - Predicted Temp") +
      xlab("")
  )
  output$tempPlotYearUI<-renderUI(plotOutput("tempPlotYear",height=400))


  output$tempTable<-renderDT(
    wq[!is.na(tempOutlier)&tempOutlier==T,.(SiteID,sDate,hour,Temp,tempPred,
                                            tempLwr,tempUpr)],
    options=list(order=list(2,"desc")),
    selection=list(mode="single",selected=  wq[!is.na(tempOutlier)&tempOutlier==T,which.max(sDate)]))

  ########################### DO ##################################################

  doSite<-reactive(
    wq[!is.na(doSat)&(doSat>input$doSlider[2]|doSat<input$doSlider[1]),.(SiteID,sDate,hour,DO,doSat,dayOfYear)] %>%
      .[,SiteID[input$doTable_rows_selected]]
  )

  doDate<-reactive(
    wq[!is.na(doSat)&(doSat>input$doSlider[2]|doSat<input$doSlider[1]),.(SiteID,sDate,hour,DO,doSat,dayOfYear)] %>%
      .[,sDate[input$doTable_rows_selected]]
  )

  doHighlight<-reactive(wq[!is.na(DO)] %>%
                            .[,":="(siteMatch=SiteID==doSite(),
                                    dateMatch=sDate==doDate(),
                                    highlight=SiteID==doSite()&sDate==doDate())]
  )

  output$doPlotSite<-renderPlot(
    ggplot(doHighlight()[siteMatch==T],aes(x=dayOfYear,y=DO,color=highlight))+
      geom_point(size=5)+
      theme_classic(base_size=20)+
      scale_color_manual(values=c(gray(0.5,0.4),"red"),
                         labels=c("other","selected"),
                         name=NULL)+
      ggtitle(doSite())+
      ylab("DO (mg/L)")+
      xlab("")
  )
  output$doPlotSiteUI<-renderUI(plotOutput("doPlotSite",height=400))

  output$doPlotSite2<-renderPlot(
    ggplot(doHighlight()[siteMatch==T],aes(x=dayOfYear,y=doSat,color=highlight))+
      geom_point(size=5)+
      scale_color_manual(values=c(gray(0.5,0.4),"red"),
                         labels=c("other","selected"),
                         name=NULL)+
      theme_classic(base_size=20)+
      ggtitle(doSite())+
      ylab("DO (% Saturation)")+
      xlab("")
  )
  output$doPlotSite2UI<-renderUI(plotOutput("doPlotSite2",height=400))


  output$doPlotYear<-renderPlot(
    ggplot(doHighlight()[sDate>(doDate()-21)&sDate<(doDate()+21)],aes(x=dayOfYear,y=doSat,color=highlight))+
      geom_point(size=5)+
      scale_color_manual(values=c(gray(0.5,0.4),"red"),
                         labels=c("other","selected"),
                         name=NULL)+
      theme_classic(base_size=20)+
      ggtitle(year(tempDate()))+
      ylab("DO (% Saturation)") +
      xlab("")
  )
  output$doPlotYearUI<-renderUI(plotOutput("doPlotYear",height=400))


  output$doTable<-renderDT(
    wq[(doSat>input$doSlider[2]|doSat<input$doSlider[1])&!is.na(doSat),.(SiteID,sDate,hour,DO,doSat)],
    options=list(order=list(2,"desc")),
    selection=list(mode="single",selected=  wq[(doSat>input$doSlider[2]|doSat<input$doSlider[1])&!is.na(doSat),which.max(sDate)])
  )

  output$doHist<-renderPlot(
    ggplot()+
      geom_histogram(data=doHighlight(),aes(doSat),binwidth = 0.01)+
      geom_vline(aes(xintercept=input$doSlider[1]),linetype="dashed")+
      geom_vline(aes(xintercept=input$doSlider[2]),linetype="dashed")+
      ggtitle(paste0("Context for Definition of Weird DO"))
  )

  output$doHistUI<-renderUI(plotOutput("doHist",height=400))

  ########################### pH ##################################################

  phSite<-reactive(
    wq[!is.na(pH)&(phOutlier==T),.(SiteID,sDate,hour,pH,dayOfYear)] %>%
      .[,SiteID[input$phTable_rows_selected]]
  )

  phDate<-reactive(
    wq[!is.na(pH)&(phOutlier==T),.(SiteID,sDate,hour,pH,dayOfYear)] %>%
      .[,sDate[input$phTable_rows_selected]]
  )

  phHighlight<-reactive(wq[!is.na(pH)] %>%
                          .[,":="(siteMatch=SiteID==phSite(),
                                  dateMatch=sDate==phDate(),
                                  highlight=SiteID==phSite()&sDate==phDate())]
  )

  output$phPlotSite<-renderPlot(
    ggplot(phHighlight()[siteMatch==T],aes(x=sDate,y=pH,color=highlight))+
      scale_color_manual(values=c(gray(0.5,0.4),"red"),
                         labels=c("other","selected"),
                         name=NULL)+
      geom_point(size=5)+
      theme_classic(base_size=20)+
      ggtitle(phSite())+
      ylab("pH")+
      xlab("")
  )
  output$phPlotSiteUI<-renderUI(plotOutput("phPlotSite",height=400))

  output$phPlotQ<-renderPlot(
    ggplot(phHighlight()[siteMatch==T],aes(x=q,y=pH,color=highlight))+
      scale_color_manual(values=c(gray(0.5,0.4),"red"),
                         labels=c("other","selected"),
                         name=NULL)+
      geom_point(size=5)+
      theme_classic(base_size=20)+
      ggtitle(phSite())+
      ylab("pH")+
      xlab("Discharge (m3/s)")
  )
  output$phPlotQUI<-renderUI(plotOutput("phPlotQ",height=400))


  output$phPlotYear<-renderPlot(
    ggplot(phHighlight() %>% arrange(highlight),aes(x=sDate,y=pH,color=highlight))+
             scale_color_manual(values=c(gray(0.5,0.4),"red"),
                                labels=c("other","selected"),
                                name=NULL)+
      geom_point(size=5)+
      theme_classic(base_size=20)+
      ggtitle("All pH Data over Time")+
      ylab("pH") +
      xlab("")
  )
  output$phPlotYearUI<-renderUI(plotOutput("phPlotYear",height=400))


  output$phTable<-renderDT(
    wq[!is.na(pH)&phOutlier==T,.(SiteID,sDate,hour,pH)],
    options=list(order=list(2,"desc")),
    selection=list(mode="single",selected= wq[!is.na(pH)&phOutlier==T,which.max(sDate)])
  )

  ###################Cond and TDS #######################################
  condSite<-reactive(
    wq[(condOutlier==T|tdsOutlier==T),.(SiteID,sDate,hour,Cond,TDS,dayOfYear)] %>%
      .[,SiteID[input$condTable_rows_selected]]
  )

  condDate<-reactive(
    wq[condOutlier==T|tdsOutlier==T,.(SiteID,sDate,hour,Cond,TDS,dayOfYear)] %>%
      .[,sDate[input$condTable_rows_selected]]
  )

  condHighlight<-reactive(wq %>%
                          .[,":="(siteMatch=SiteID==condSite(),
                                  dateMatch=sDate==condDate(),
                                  highlight=SiteID==condSite()&sDate==condDate())]
  )

  output$condPlotSite<-renderPlot(
    ggplot(condHighlight()[siteMatch==T],aes(x=sDate,y=Cond,color=highlight))+
      scale_color_manual(values=c(gray(0.5,0.4),"red"),
                         labels=c("other","selected"),
                         name=NULL)+
      geom_point(size=5)+
      theme_classic(base_size=20)+
      ggtitle(condSite())+
      ylab("Conductivity")+
      xlab("")
  )
  output$condPlotSiteUI<-renderUI(plotOutput("condPlotSite",height=400))

  output$tdsPlotSite<-renderPlot(
    ggplot(condHighlight()[siteMatch==T],aes(x=sDate,y=TDS,color=highlight))+
      scale_color_manual(values=c(gray(0.5,0.4),"red"),
                         labels=c("other","selected"),
                         name=NULL)+
      geom_point(size=5)+
      theme_classic(base_size=20)+
      ggtitle(condSite())+
      ylab("TDS")+
      xlab("")
  )
  output$tdsPlotSiteUI<-renderUI(plotOutput("tdsPlotSite",height=400))


  output$condPlotQ<-renderPlot(
    ggplot(condHighlight()[siteMatch==T],aes(x=q,y=Cond,color=highlight))+
      scale_color_manual(values=c(gray(0.5,0.4),"red"),
                         labels=c("other","selected"),
                         name=NULL)+
      geom_point(size=5)+
      theme_classic(base_size=20)+
      ggtitle(condSite())+
      ylab("Conducvity")+
      xlab("Discharge (m3/s)")
  )
  output$condPlotQUI<-renderUI(plotOutput("condPlotQ",height=400))


  output$tdsPlotQ<-renderPlot(
    ggplot(condHighlight()[siteMatch==T],aes(x=q,y=TDS,color=highlight))+
      scale_color_manual(values=c(gray(0.5,0.4),"red"),
                                  labels=c("other","selected"),
                                  name=NULL)+
      geom_point(size=5)+
      theme_classic(base_size=20)+
      ggtitle(condSite())+
      ylab("TDS")+
      xlab("Discharge (m3/s)")
  )
  output$tdsPlotQUI<-renderUI(plotOutput("tdsPlotQ",height=400))

  output$tdsCondPlot<-renderPlot(
    ggplot(data=condHighlight() %>% arrange(highlight),
           aes(x=TDS,y=Cond,color=highlight))+
      geom_point(size=5)+
      scale_color_manual(values=c(gray(0.5,0.4),"red"),
                         labels=c("other","selected"),
                         name=NULL)+
      theme_classic(base_size=20)+
      ggtitle(condSite())+
      ylab("Conductivity")+
      xlab("TDS")
  )
  output$tdsCondPlotUI<-renderUI(plotOutput("tdsCondPlot",height=400))


  output$condTable<-renderDT(
    wq[condOutlier==T|tdsOutlier==T,.(SiteID,sDate,hour,Cond,TDS,condOutlier,tdsOutlier)],
    options=list(order=list(2,"desc")),
    selection=list(mode="single",selected= wq[condOutlier==T|tdsOutlier==T,which.max(sDate)])
  )

}

# Run the application
shinyApp(ui = ui, server = server)

