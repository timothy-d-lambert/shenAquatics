#'Connect to Shen aquatics database
#'
#'Creates a link to the SHEN MS Access database for import into R.
#'@return returns \code{con}: An RODBC link to the database for use with the function \code{sqlQuery}
#'
#'@export

aqConnector<-function(){
  #if it doesn't exists in the proper form, make the connection
  if(!exists("con")){
  # con<<-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
  #                      DBQ=O:/Working/AQUATIC/Database/DataManagement/data/All_Stream_data_2020.accdb")
    con<<-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                       DBQ=C:/Users/echildress/Documents/mapShen/data/All_Stream_data_2020.accdb")
  } else{if(class(con)!="RODBC"|
            class(try(sqlQuery(con,"select * from R_zdd_Sites"),silent=T))!="data.frame"){
    # con<<-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
    #                    DBQ=O:/Working/AQUATIC/Database/DataManagement/data/All_Stream_data_2020.accdb")
    con<<-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                       DBQ=C:/Users/echildress/Documents/mapShen/data/All_Stream_data_2020.accdb")
    }
  }
}
