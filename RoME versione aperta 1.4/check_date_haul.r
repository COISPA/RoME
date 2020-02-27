###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013                                                                                                            #
################################################################################
# Check if in TB, TC and TE the date by haul is the same of the one reported in TA

check_date_haul <- function (ResultData){
  
  numberError = 0
  Dataset = ResultData #read.csv(paste(Data,".csv",sep=""), sep=";",header=TRUE)
  
  if (as.character(ResultData$TYPE_OF_FILE[1]) == "TB") {
    
write(paste("
                ----------- check correctness of date by haul in TB - ", Dataset$YEAR[1]), file = Errors, append = TRUE) 
  } else if (as.character(ResultData$TYPE_OF_FILE[1]) == "TC") {
    write(paste("
                ----------- check correctness of date by haul in TC - ", Dataset$YEAR[1]), file = Errors, append = TRUE) 
  } else if (as.character(ResultData$TYPE_OF_FILE[1]) == "TE") {
    write(paste("
                ----------- check correctness of date by haul in TE - ", Dataset$YEAR[1]), file = Errors, append = TRUE) 
  }
  else if (as.character(ResultData$TYPE_OF_FILE[1]) == "TL") {
    write(paste("
                ----------- check correctness of date by haul in TL - ", Dataset$YEAR[1]), file = Errors, append = TRUE) 
  }
  

Dataset$Date = paste (Dataset$HAUL_NUMBER, "/",Dataset$MONTH,"/",Dataset$DAY,sep="")
TA =ResultDataTA #read.csv(paste(DataTA,".csv",sep=""), sep=";",header=TRUE)
TA$Date = paste (TA$HAUL_NUMBER,"/",TA$MONTH,"/",TA$DAY,sep="")

for (i in 1:nrow(Dataset)){
  if(! (Dataset$Date[i] %in% TA$Date)) {  
  write(paste("Warning: Haul",Dataset$HAUL_NUMBER[i],", code species", Dataset$GENUS[i] , Dataset$SPECIES[i] ,": the date is not consistent with the date reported in TA."), file = Errors, append = TRUE)  
numberError = numberError+1
}
  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  }  
  
  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }
}

}