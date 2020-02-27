###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013                                                                                                            #
###########################################################################################################################
# Check if all the hauls in TA are in TB

check_hauls_TATL<-function(ResultDataTA,ResultDataTL){

  
  numberError = 0
  ResultTA = ResultDataTA # read.csv(paste(DataTA,".csv",sep=""), sep=";", header=TRUE) 
  write(paste("
----------- check presence in TB of TA hauls - ",ResultTA$YEAR[1]), file = Errors, append = TRUE) 
 
  ResultTA=ResultTA[,which(names(ResultTA)=="HAUL_NUMBER" | names(ResultTA)=="VALIDITY")]
  ResultTA=ResultTA[ResultTA$VALIDITY=="V",]
  ResultTL = ResultDataTL 
  
  if (nrow(ResultTA)!=0){
    for (j in 1:nrow(ResultTA)){

      ResultTL_temp=ResultTL[which(ResultTL$HAUL_NUMBER==ResultTA$HAUL_NUMBER[j]),]
      if (nrow(ResultTL_temp)==0)   {
        write(paste("No haul",ResultTA$HAUL_NUMBER[j],"in TL"), file = Errors, append = TRUE)
        numberError = numberError +1
      }
    }
  }
  
  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  }	
  
 # if (numberError ==0) {
    return(TRUE)
  #} else { return(FALSE) }
  
} 

################################################################################