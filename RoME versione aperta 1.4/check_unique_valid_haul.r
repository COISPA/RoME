###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013                                                                                                            #
###########################################################################################################################
# Among hauls with the same code only one must be valid

check_unique_valid_haul<-function(ResultDataTA){
 # library(RODBC)
  numberError = 0
  Result = ResultDataTA # read.csv(paste(DataTA,".csv",sep=""), sep=";", header=TRUE)
  write(paste("
              ----------- check uniqueness of valid hauls TA - ",Result$YEAR[1]), file = Errors, append = TRUE)
  
  if (Type_of_files==".csv"){
    #write.xlsx(Result,file=paste(DataTA,".xls", sep = ""))
    }
  #channelTA <- odbcConnectExcel(paste(DataTA,".xls", sep = ""))
  
  queryHauls= "SELECT HAUL_NUMBER, count(*) as no_hauls from Result where HAUL_NUMBER is not null group by HAUL_NUMBER"
  
  ResultTA=sqldf(queryHauls)
  
  moreThanOneHaul = which(ResultTA$no_hauls > 1, arr.ind=TRUE)
  
  if (length(moreThanOneHaul)>1){
    for (j in 1:length(moreThanOneHaul)){
      index= moreThanOneHaul[j]
      queryCheck = paste("SELECT * from Result where HAUL_NUMBER=", ResultTA$HAUL_NUMBER[index], " and VALIDITY='V'", sep="" )
      ResultCheck=sqlQuery(channelTA, queryCheck) 
      if (nrow(ResultCheck)>1)   {
        write(paste("Haul",ResultTA$HAUL_NUMBER[index],": only one row for the haul is allowed to be VALID"), file = Errors, append = TRUE)
        numberError = numberError +1 
      }
    }
  }
  #odbcClose(channelTA)
  
  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  }	
  if (Type_of_files==".csv"){
    unlink(paste(DataTA,".xls", sep = ""))
  }
  
  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }
  
}

################################################################################