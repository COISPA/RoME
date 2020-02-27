############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.eu #
#   March 2020                                                                                                             #
############################################################################################################################


if (FALSE){
  wd <- "D:\\Documents and Settings\\Utente\\Documenti\\GitHub\\RoME\\temp"
  suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  ResultDataTA = read.csv("~/GitHub/RoME/data/TA_GSA18_1994-2018.csv", sep=";")

  # check_0_fieldsTA(ResultDataTA,wd,suffix)

}



check_0_fieldsTA<-function(ResultDataTA,wd, suffix){
  numberError = 0

  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  }

  Errors <<- paste(wd,"/Logfiles/Logfile_",suffix,".dat",sep="")


  write(paste("----------- check 0 fields TA"), file = Errors, append = TRUE)
  Matrix = ResultDataTA # read.csv(paste(Data,".csv",sep=""), sep=";", header=TRUE)

  ## VERTICAL_OPENING
  empty_X=which(Matrix$VERTICAL_OPENING==0)
  if (length(empty_X)!=0) {
    i=1
    for (i in 1:length(empty_X)){
      write(paste("Haul ",Matrix$HAUL_NUMBER[empty_X[i]],"0 value for VERTICAL_OPENING in ", Matrix$TYPE_OF_FILE[1],"-", Matrix$YEAR[1] ), file = Errors, append = TRUE)
      numberError = numberError +1
    }
  }

  ## WING_OPENING
  empty2_X=which(Matrix$WING_OPENING==0)
  if (length(empty2_X)!=0) {
    for (j in 1:length(empty2_X)){
      write(paste("Haul ",Matrix$HAUL_NUMBER[empty2_X[j]],"0 value for WING_OPENING in", Matrix$TYPE_OF_FILE[1],"-", Matrix$YEAR[1] ), file = Errors, append = TRUE)
      numberError = numberError +1
    }
  }

  ## WARP_DIAMETER
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
