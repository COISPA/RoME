#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013 
################################################################################
# Check if that difference between start depth and end depth is not greater than 20% 
check_depth<-function(ResultDataTA){
  numberError = 0
  ResultData = ResultDataTA #read.csv(paste(Data,".csv",sep=""), sep=";", header=TRUE)
	
  write(paste("
----------- check difference between start depth and end depth (not greater than 20%) TA ", "-",ResultData$YEAR[1]), file = Errors, append = TRUE)

  
  ResultData$diff_depth=(ResultData$SHOOTING_DEPTH-ResultData$HAULING_DEPTH)/ResultData$SHOOTING_DEPTH*100
  Err=which((abs(ResultData$diff_depth)>20)==TRUE)
  if (length(Err)!=0){
    for (i in 1:length(Err)){
      write(paste("Warning: Haul",ResultData$HAUL_NUMBER[Err[i]],"difference between start depth", ResultData$SHOOTING_DEPTH[Err[i]],"and end depth",ResultData$HAULING_DEPTH[Err[i]],"greater than 20% in", ResultData$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
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