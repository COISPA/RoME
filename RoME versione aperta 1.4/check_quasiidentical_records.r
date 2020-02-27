###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013                                                                                                            #
###########################################################################################################################
# Check quasi-identical records

check_quasiidentical_records<-function(Result,Table){
  #Result = read.csv(paste(Data,".csv",sep=""), sep=";", header=TRUE)
#   if (Type_of_files==".csv"){
#     #write.xls(Result,file=paste(Data,".xls", sep = ""))
#   }
  check_without_errors = FALSE 
  
  # ------------------------------------------------------------------ TA
  
  if (Table == "TA"){
    write(paste("
                ----------- check quasi identical records - ",Result$YEAR[1]), file = Errors, append = TRUE)
    write(paste("TA:"), file = Errors, append = TRUE)                                                                                                   
    
    
    #channel <- odbcConnectExcel(paste(Data,".xls", sep = ""))
    Matrix=sqldf("select count(*) as count, TYPE_OF_FILE, AREA, VESSEL, GEAR, RIGGING, DOORS, YEAR from Result Group by TYPE_OF_FILE, AREA, VESSEL, GEAR, RIGGING, DOORS, YEAR")
   
    
    if (nrow(Matrix)>1){
      Max=max(Matrix$count)
      Matrix2=Matrix[Matrix$count!=Max,]
      
      ResultData = sqldf("select * from Result where HAUL_NUMBER is not null")
      
      for (i in 1:nrow(Matrix2)){
        Err = ResultData[as.character(ResultData$TYPE_OF_FILE) == as.character(Matrix2$TYPE_OF_FILE[i]) & 
                           as.character(ResultData$AREA) == as.character(Matrix2$AREA[i]) &
                           as.character(ResultData$VESSEL) == as.character(Matrix2$VESSEL[i]) &
                           as.character(ResultData$GEAR) == as.character(Matrix2$GEAR[i]) &
                           as.character(ResultData$RIGGING) == as.character(Matrix2$RIGGING[i]) &
                           as.character(ResultData$DOORS) == as.character(Matrix2$DOORS[i]) &
                           as.character(ResultData$YEAR) == as.character(Matrix2$YEAR[i]),] 
        
        
        for( k in 1:nrow(Err)){
          write(paste("Haul ",Err$HAUL_NUMBER[k],
                      "there is an inconsistent value in one or more of the fields that should be always identical in", 
                      Matrix$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
        }
      }  
    } else { 
      write(paste("No error occurred"), file = Errors, append = TRUE)
      check_without_errors = TRUE
    }
   # odbcClose(channel)
    
    
  } else if (Table=="TB") { 
    # ------------------------------------------------------------------ TB
    write(paste("TB:"), file = Errors, append = TRUE)                                                                                                       
    #Result = read.csv(paste(Data,".csv",sep=""), sep=";", header=TRUE)
    if (Type_of_files==".csv"){
     # write.xls(Result,file=paste(Data,".xls", sep = ""))
    }
    
    #channel <- odbcConnectExcel(paste(Data,".xls", sep = ""))
    Matrix=sqldf("select count(*) as count, TYPE_OF_FILE, AREA, VESSEL, YEAR from Result Group by TYPE_OF_FILE,  AREA, VESSEL, YEAR")
    
    ResultData = sqldf("select * from Result where HAUL_NUMBER is not null")
    
    if (nrow(Matrix)>1){
      Max=max(Matrix$count)
      Matrix2=Matrix[Matrix$count!=Max,]
      for (i in 1:nrow(Matrix2)){
        
        Err = ResultData[as.character(ResultData$TYPE_OF_FILE) == as.character(Matrix2$TYPE_OF_FILE[i]) & 
                           as.character(ResultData$AREA) == as.character(Matrix2$AREA[i]) &
                           as.character(ResultData$VESSEL) == as.character(Matrix2$VESSEL[i]) &
                           as.character(ResultData$YEAR) == as.character(Matrix2$YEAR[i]),] 
        
        for( k in 1:nrow(Err)){
          if (Err$YEAR[k] == Matrix2$YEAR[i]){ 
            write(paste("Haul ",Err$HAUL_NUMBER[k],Err$GENUS[k],Err$SPECIES[k],"there is an inconsistent value in one or more of the fields that should be always identical in", Matrix$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
          }
        }
      }  
    } else
    {
      write(paste("No error occurred"), file = Errors, append = TRUE)
      check_without_errors = TRUE
    }
    #odbcClose(channel)
  } else if (Table=="TC"){  
    # ------------------------------------------------------------------ TC
    write(paste("TC:"), file = Errors, append = TRUE)                                                                                                         
    #Result = read.csv(paste(Data,".csv",sep=""), sep=";", header=TRUE)
    if (Type_of_files==".csv"){
     # write.xls(Result,file=paste(Data,".xls", sep = ""))
    }
    
    #channel <- odbcConnectExcel(paste(Data,".xls", sep = ""))
    Matrix=sqldf("select count(*) as count, TYPE_OF_FILE, AREA, VESSEL, YEAR from Result Group by TYPE_OF_FILE, AREA, VESSEL, YEAR")
    
    ResultData = sqldf("select * from Result where HAUL_NUMBER is not null")
    
    if (nrow(Matrix)>1){
      Max=max(Matrix$count)
      Matrix2=Matrix[Matrix$count!=Max,]
      for (i in 1:nrow(Matrix2)){
        
        Err = ResultData[as.character(ResultData$TYPE_OF_FILE) == as.character(Matrix2$TYPE_OF_FILE[i]) & 
                           as.character(ResultData$AREA) == as.character(Matrix2$AREA[i]) &
                           as.character(ResultData$VESSEL) == as.character(Matrix2$VESSEL[i]) &
                           as.character(ResultData$YEAR) == as.character(Matrix2$YEAR[i]),] 
        
        for( k in 1:nrow(Err)){
          if (Err$YEAR[k] == Matrix2$YEAR[i]){ 
            write(paste("Haul",Err$HAUL_NUMBER[k],Err$GENUS[k],Err$SPECIES[k],ifelse(Err$SEX[k]=="FALSE","F", "M"),"length",Err$LENGTH_CLASS[k],"there is an inconsistent value in one or more of the fields that should be always identical in", Matrix$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
          }
        }
      }  
    } else
    {
      write(paste("No error occurred"), file = Errors, append = TRUE)
      check_without_errors = TRUE
    }
    
    #odbcClose(channel)
  }
  
#   if (Type_of_files==".csv"){
#     unlink(paste(Data,".xls", sep = ""))
#     }
#   

return(check_without_errors)
}