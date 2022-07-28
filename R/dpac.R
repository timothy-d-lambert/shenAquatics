#'Distribute parents among children
#'
#'@description Assign higher taxonomic levels to lower
#' ones based on the relative proportion of lower ones within a sample. The
#' function is designed for processing of a single sample (or combined samples from a site visit).
#' See examples for quick operation on multiple samples
#'
#'@return A data.table with individuals maximally distributed among lower taxonomic levels
#'
#'@param x A data.table containing the taxonomic hierarchy (i.e., all taxonomic levels for each taxon) and numbers of individuals
#'@param countCol Name of the column containing the counts of individuals
#'@param minLevel The taxonomic level at which distribution should be terminated and taxa should be lumped (defaults to genus)
#'@param keepCodeCols A vector of names for columns containing taxa codes to be retained in the output
#'
#'@examples
#'ai<-ai[,dpac(.SD,keepCodeCols=c("TAXON_CODE","TSN")),.(SiteID,sDate)]
#'
#'@export

dpac<-function(x,countCol="INSECT_COU",minLevel="genus",keepCodeCols=NULL){

  #get only the taxonomic levels represented in the table down to the minLevel
  #the list of possible levels may need to be expanded, these are the ones in my table
  taxLevels<-c("phylum","class","sublcass","order","suborder","superfamily","family","subfamily","tribe","genus")
  taxLevels<-taxLevels[taxLevels %in% tolower(names(x))]
  taxLevels<-taxLevels[1:which(taxLevels==minLevel)]

  #make sure taxonomic columns are named in lower case
  setnames(x,names(x)[tolower(names(x)) %in% taxLevels],
           taxLevels[match(tolower(names(x))[tolower(names(x)) %in% taxLevels],taxLevels)])

  #make sure that there aren't any duplicates by summing any
  x<-x[,.(count=as.numeric(sum(get(countCol)))),by=c(taxLevels,keepCodeCols)]

  x[,lowestLevel:=x[,taxLevels,with=F] %>% apply(1,function(x){max(which(!is.na(x)))})]

  for(t in taxLevels[unique(x$lowestLevel[order(x$lowestLevel)])]){
    uniqueTaxa<-x[lowestLevel==which(taxLevels==t)][[t]]

    for(t2 in uniqueTaxa){
      #if there aren't any children to which to distribute, move on
      if(nrow(x[get(t)==t2&lowestLevel>which(taxLevels==t)])==0) next

      unassignedCount<-x[get(t)==t2&lowestLevel==which(taxLevels==t),count]#get number that need to be distributed
      # if(x[get(t)==t2&lowestLevel>which(taxLevels==t),length(unique(lowestLevel))]>1){
      #   stop("children at multiple taxonomic levels")
      # }

      x[get(t)==t2&lowestLevel>which(taxLevels==t),prop:=count/sum(count)]#get proportion for receiving taxa
      x[get(t)==t2&lowestLevel>which(taxLevels==t),count:=count+prop*unassignedCount]#distribute to receiving taxa
      x<-x[get(t)!=t2|lowestLevel!=which(taxLevels==t)]#remove the parent
    }
  }
  suppressWarnings(x[,":="(lowestLevel=NULL,prop=NULL)])
  setnames(x,"count",countCol)
  return(x)
}
