###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013                                                                                                            #
###########################################################################################################################
# Check if the haul start in the same quadrant

check_quadrant<-function(ResultDataTA){
  numberError = 0
  ResultData = ResultDataTA #read.csv(paste(Data,".csv",sep=""), sep=";", header=TRUE)
  write(paste("
----------- check start quadrant and end quadrant TA - ",ResultData$YEAR[1]), file = Errors, append = TRUE)
  
  
  ResultData=ResultData[ResultData$VALIDITY=="V",]
  for (i in 1:nrow(ResultData)){
    if (ResultData$SHOOTING_QUADRANT[i]!=ResultData$HAULING_QUADRANT[i])  {
      write(paste("Warning: Haul",ResultData$HAUL_NUMBER[i]," starts in the quadrant",ResultData$SHOOTING_QUADRANT[i]," and finishes in the quadrant", ResultData$HAULING_QUADRANT[i]," in",ResultData$TYPE_OF_FILE[1]), file = Errors, append = TRUE)}
  }
  
  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  }	
  
  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }
  
}
################################################################################