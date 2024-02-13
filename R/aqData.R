#'Grabs a subset of available tables from the SHEN aquatics database
#'
#'@return Creates a link to the SHEN MS Access database if one does not exist, then downloads a table, converts it to a data.table, and spits it out.
#'
#'@param name My name for the table in the database. Available tables are listed in \code{dbNames}, which is lazy loaded with the package.
#'@param visitCols Columns from the siteVisits table to add to the output, SiteVisit_ID, SITEID, and sDate are added automatically if SiteVisit_ID is a column in the table
#'@param siteCols Columns from the sites table to add to the output (e.g., Elev_m)
#'
#'@export
aqData<-function(name,visitCols=NULL,siteCols=NULL,conTemp=NULL){
  # oldOpt<-default.stringsAsFactors() #deprecated
  # options(stringsAsFactors = F)
  if(is.null(conTemp)){
    aqConnector()
    conTemp<-con
  }

  d<-sqlQuery(conTemp,paste("select * from",dbNames[myName==name,dbName])) %>%
    data.table()

  if(name=="siteVisits"){setnames(d,"SITEID","SiteID")}

  if(name=="species"){
    d[grepl("-",COMMONNAME),
      COMMONNAME:=paste(tstrsplit(COMMONNAME,"- ")[[c(2)]],
                        tstrsplit(COMMONNAME,"- ")[[c(1)]])]
    d[grepl(",",COMMONNAME),
      COMMONNAME:=paste(tstrsplit(COMMONNAME,", ")[[c(2)]],
                        tstrsplit(COMMONNAME,", ")[[c(1)]])]
  }

  if((name %in% c("sites","siteVisits","siteInfo")&is.null(siteCols)&is.null(visitCols))|
     !"SiteVisit_ID" %in% names(d)) return(d)

  siteVisits<-sqlQuery(conTemp,paste("select ",
                                 paste(c("SiteVisit_ID","SITEID","sDate",visitCols),collapse=", "),
                                 " from R_SiteVisits")) %>%
    data.table() %>%
    setnames("SITEID","SiteID") %>%
    setkey(SiteVisit_ID)

  setkey(d,SiteVisit_ID)
  d<-siteVisits[d]

  if(!is.null(siteCols)){
    sites<-sqlQuery(conTemp,paste("select ",
                                   paste(c("SiteID",siteCols),collapse=", "),
                                   " from R_zdd_Sites")) %>%
      data.table() %>%
      setkey(SiteID)

    setkey(d,SiteID)
    d<-sites[d]
  }

  # options(stringsAsFactors=oldOpt)

  return(d)
}
