###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013                                                                                                            #
###########################################################################################################################
# Check if all the species in TC are listed in TB

check_haul_species_TCTB<-function(DataTB,DataTC){
  
  #library(RODBC)
  numberError = 0
  ResultTC = ResultDataTC #read.csv(paste(DataTC,".csv",sep=""), sep=";", header=TRUE)
  write(paste("
              ----------- check presence in TB of TC species - ", ResultTC$YEAR[1]), file = Errors, append = TRUE)
  

  if (Type_of_files==".csv"){
    #write.xlsx(ResultTC,file=paste(DataTC,".xls", sep = ""))
    }
  ResultTB = ResultDataTB #read.csv(paste(DataTB,".csv",sep=""), sep=";", header=TRUE)
  #channelTC <- odbcConnectExcel(paste(DataTC,".xls", sep = ""))
  
  
  ResultTB= ResultTB[,which(names(ResultTB)=="YEAR" | names(ResultTB)=="HAUL_NUMBER" | names(ResultTB)=="GENUS" | names(ResultTB)=="SPECIES")]
  
  queryTC = "SELECT YEAR, HAUL_NUMBER, GENUS, SPECIES from ResultTC where  HAUL_NUMBER is not NULL group by YEAR, HAUL_NUMBER, GENUS, SPECIES"
  ResultTC=sqldf(queryTC)
  #odbcClose(channelTC)
  
  if ( (nrow(ResultTC)!=0)){
    for (j in 1:nrow(ResultTC)){
      StrSpecies= paste(ResultTC$GENUS[j], ResultTC$SPECIES[j], sep="" )
      FoundInTB=ResultTB[as.character(ResultTB$GENUS)==as.character(ResultTC$GENUS[j]) & as.character(ResultTB$SPECIES)==as.character(ResultTC$SPECIES[j]) & ResultTB$HAUL_NUMBER==ResultTC$HAUL_NUMBER[j],]
      if (nrow(FoundInTB) == 0) {
        numberError = numberError+1
        write(paste("Haul",ResultTC$HAUL_NUMBER[j],ResultTC$GENUS[j], ResultTC$SPECIES[j], "not found in TB"), file = Errors, append = TRUE)
      }
      
    }
  }
  
  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  }  
  if (Type_of_files==".csv"){
    unlink(paste(DataTC,".xls", sep = ""))}
  
  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) } 
  
  
  
  
  
}

################################################################################