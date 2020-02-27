###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013                                                                                                            #
###########################################################################################################################
# Check if in TB  NB_TOTAL equals NB_F+NB_M+NB_I 

check_nbtotTB<-function(ResultDataTB){
  numberError = 0
   Matrix = ResultDataTB #read.csv(paste(Data,".csv",sep=""), sep=";", header=TRUE)
  write(paste("
----------- check consistency of TOTAL_NUMBER_IN_THE_HAUL and number per sex in TB - ", Matrix$YEAR[1]), file = Errors, append = TRUE)
  
 
  
  if (Format=="before_2012"){
  Err_vec=which(round(Matrix$TOTAL_NUMBER_IN_HAUL,6)!=round(Matrix$NUMBER_OF_FEMALES + Matrix$NUMBER_OF_MALES + Matrix$NUMBER_OF_UNDETERMINED,6))
  } else {
    Matrix =  Matrix
    Err_vec=which(round(Matrix$TOTAL_NUMBER_IN_THE_HAUL,6)!=round(Matrix$NB_OF_FEMALES + Matrix$NB_OF_MALES + Matrix$NB_OF_UNDETERMINED,6))
  }
  if  (length(Err_vec)!=0){
  for (i in 1:length(Err_vec)){
      Err=cbind(as.character(Matrix$TYPE_OF_FILE[1]),Matrix$HAUL_NUMBER[Err_vec[i]],ifelse(is.factor(Matrix$GENUS[Err_vec[i]]), as.character(Matrix$GENUS[Err_vec[i]]), Matrix$GENUS[Err_vec[i]]),ifelse(is.factor(Matrix$SPECIES[Err_vec[i]]), as.character(Matrix$SPECIES[Err_vec[i]]), Matrix$SPECIES[Err_vec[i]]))
      write(paste("Warning: Haul",Err[1,2],Err[1,3],Err[1,4],": NB_TOTAL doesn't equal NB_F+NB_M+NB_I", Err[1,1]), file = Errors, append = TRUE)  
      
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