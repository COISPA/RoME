###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #
#   March 2013                                                                                                            #
###########################################################################################################################
# Check if wing opening or/and vertical opening fields are null in TA

check_0_fieldsTA<-function(Data){
  numberError = 0

  write(paste("
----------- check 0 fields TA"), file = Errors, append = TRUE)
  Matrix = ResultDataTA # read.csv(paste(Data,".csv",sep=""), sep=";", header=TRUE)
  empty_X=which(Matrix$VERTICAL_OPENING==0)
  if (length(empty_X)!=0) {
    for (i in 1:length(empty_X)){
      write(paste("Haul ",Matrix$HAUL_NUMBER[empty_X[i]],"0 value for VERTICAL_OPENING in ", Matrix$TYPE_OF_FILE[1],"-", Matrix$YEAR[1] ), file = Errors, append = TRUE)

    }
  }
  empty2_X=which(Matrix$WING_OPENING==0)
  if (length(empty2_X)!=0) {
    for (j in 1:length(empty2_X)){
      write(paste("Haul ",Matrix$HAUL_NUMBER[empty2_X[j]],"0 value for WING_OPENING in", Matrix$TYPE_OF_FILE[1],"-", Matrix$YEAR[1] ), file = Errors, append = TRUE)
      numberError = numberError +1
    }
  }

  empty3_X=which(Matrix$WARP_DIAMETER==0)
  if (length(empty3_X)!=0) {
    for (k in 1:length(empty3_X)){
      write(paste("Warning: Haul ",Matrix$HAUL_NUMBER[empty3_X[k]],"0 value for WARP_DIAMETER in", Matrix$TYPE_OF_FILE[1],"-", Matrix$YEAR[1] ), file = Errors, append = TRUE)
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
