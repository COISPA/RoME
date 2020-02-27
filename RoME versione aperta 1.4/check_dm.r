###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013                                                                                                            #
###########################################################################################################################
# Check consistency of wing ad vertical opening in TA according to INSTRUCTION MANUAL VERSION 5 MEDITS 2007 

check_dm<-  function(ResultDataTA){
  numberError = 0
  Matrix = ResultDataTA #read.csv(paste(Data,".csv",sep=""), sep=";", header=TRUE) 
  write(paste("
----------- check dm TA - ", Matrix$YEAR[1]), file = Errors, append = TRUE) 
  
  wing=which(Matrix$WING_OPENING<=50 | Matrix$WING_OPENING>=250)
  vertical=which(Matrix$VERTICAL_OPENING<=10 | Matrix$VERTICAL_OPENING>=99)
  if (length(wing)!=0){
    for (i in 1:length(wing)){
      write(paste("Haul ",Matrix$HAUL_NUMBER[wing[i]],"WING_OPENING out of boundaries (50,250) in", Matrix$TYPE_OF_FILE[1],". Please check if the measure unit is dm"), file = Errors, append = TRUE)
      numberError = numberError +1
    }  
  }
  if (length(vertical)!=0){
    for (j in 1:length(vertical)){
      write(paste("Haul ",Matrix$HAUL_NUMBER[vertical[j]],"VERTICAL_OPENING out of boundaries (10,99)in", Matrix$TYPE_OF_FILE[1],". Please check if the measure unit is dm"), file = Errors, append = TRUE)
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
