###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013                                                                                                            #
###########################################################################################################################
# Check if all the hauls in TB are in TA

check_hauls_TBTA<-function(ResultDataTA,ResultDataTB){
#   library(RODBC)
  
  numberError = 0
  ResultTB = ResultDataTB #read.csv(paste(DataTB,".csv",sep=""), sep=";", header=TRUE)
  write(paste("
----------- check presence in TA of TB hauls - ", ResultTB$YEAR[1]), file = Errors, append = TRUE) 
  ResultTA = ResultDataTA #read.csv(paste(DataTA,".csv",sep=""), sep=";", header=TRUE)
  

  ResultTB=unique(ResultTB$HAUL_NUMBER)
  if (length(ResultTB)!=0){
    for (j in 1:length(ResultTB)){

      ResultTA_temp=ResultTA[which(ResultTA$HAUL_NUMBER==ResultTB[j]),]
      if (nrow(ResultTA_temp)==0)   {
        write(paste("No haul",ResultTB[j],"in TA"), file = Errors, append = TRUE)
        numberError = numberError +1
      }
    }
  }
  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  }	
  
  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }
}

################################################################################