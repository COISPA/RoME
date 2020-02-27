###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013                                                                                                            #
###########################################################################################################################
# Check if LENGTH_CLASS measures are correct

check_step_length_distr<-function(ResultData){
  numberError = 0
  # ResultData = read.csv(paste(DataTC,".csv",sep=""), sep=";", header=TRUE)
  write(paste("
              ----------- check consistency of length distribution TC - ",ResultData$YEAR[1]), file = Errors, append = TRUE)
  
  
  
  fishes_cefalopods= ResultData[ResultData$LENGTH_CLASSES_CODE!="m",]
  
  
  
  for (i in 1:nrow(ResultData)){
    if (check.integer(ResultData$LENGTH_CLASS[i])==FALSE){
      write(paste("Haul",ResultData$HAUL_NUMBER[i],ResultData$GENUS[i],ResultData$SPECIES[i],ResultData$SEX[i],ResultData$LENGTH_CLASS[i],": LENGTH_CLASS value must be an integer number in", ResultData$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
      numberError = numberError +1
    }       
    
  }
  
  for (j in 1:nrow(fishes_cefalopods)){
    if ((fishes_cefalopods$LENGTH_CLASSES_CODE[j])=="1"){
      if (check.integer(fishes_cefalopods$LENGTH_CLASS[j]/10)==FALSE)
      {write(paste("Haul",fishes_cefalopods$HAUL_NUMBER[j],fishes_cefalopods$GENUS[j],fishes_cefalopods$SPECIES[j],fishes_cefalopods$SEX[j],fishes_cefalopods$LENGTH_CLASS[j],": in", ResultData$TYPE_OF_FILE[1],"LENGTH_CLASS value for fishes and cefalopods must have a full step, because LENGTH_CLASSES_CODE=1"), file = Errors, append = TRUE)
       numberError = numberError +1
      }
    } else { 
      if (check.integer(fishes_cefalopods$LENGTH_CLASS[j]/5)==FALSE)
      {
        write(paste("Haul",fishes_cefalopods$HAUL_NUMBER[j],fishes_cefalopods$GENUS[j],fishes_cefalopods$SPECIES[j],fishes_cefalopods$SEX[j],fishes_cefalopods$LENGTH_CLASS[j],": in", ResultData$TYPE_OF_FILE[1],"LENGTH_CLASS value for fishes and cefalopods must have a full or half step"), file = Errors, append = TRUE)
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