#'Connect to Shen aquatics database
#'
#'Creates a link to the SHEN MS Access database for import into R.
#'@return returns \code{con}: An RODBC link to the database for use with the function \code{sqlQuery}
#'
#'@export

aqConnector<-function(){
  # if it doesn't exists in the proper form, make the connection
  if(!exists("con")){
    # con<<-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
    #                    DBQ=C:/Users/echildress/OneDrive - DOI/Documents/mapShen/data/All_Stream_data_2020.accdb")
    root <- defineRoot() # define project root
    con <<- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file.path(root, "data/All_Stream_data_2020.accdb")))

  } else if(class(con)!="RODBC"|
            class(try(sqlQuery(con,"select * from R_zdd_Sites"),silent=T))!="data.frame"){
    # con<<-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
    #                    DBQ=C:/Users/echildress/OneDrive - DOI/Documents/mapShen/data/All_Stream_data_2020.accdb")
    con <<- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", here("data/All_Stream_data_2020.accdb")))
  }
}



# #### Legacy version (10/21/2024) ####
# aqConnector<-function(){
#   # if it doesn't exists in the proper form, make the connection
#   if(!exists("con")){
#     # con<<-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
#     #                    DBQ=C:/Users/echildress/OneDrive - DOI/Documents/mapShen/data/All_Stream_data_2020.accdb")
#     con <<- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", here("data/All_Stream_data_2020.accdb")))
#   } else{if(class(con)!="RODBC"|
#             class(try(sqlQuery(con,"select * from R_zdd_Sites"),silent=T))!="data.frame"){
#     # con<<-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
#     #                    DBQ=C:/Users/echildress/OneDrive - DOI/Documents/mapShen/data/All_Stream_data_2020.accdb")
#     con <<- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", here("data/All_Stream_data_2020.accdb")))
#   }
#   }
# }
