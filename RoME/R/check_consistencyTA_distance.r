############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.eu #
#   March 2020                                                                                                             #
############################################################################################################################
# Check between duration of the haul and distance (tolerance of 15%)

check_consistencyTA_distance<-function(DataTA, wd, suffix){

  if (FALSE){
    library(RoME)
    wd <- tempdir()
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
    DataTA = MEDITS::TA
    # check_consistencyTA_distance(DataTA, wd, suffix)
  }

  if (!file.exists("Logfiles")){
    dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
  }

  numberError = 0
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  }
  Errors <- paste(wd,"\\Logfiles\\Logfile_",suffix,".dat",sep="")

  Matrix = DataTA
  write(paste("\n----------- check consistency between duration and distance TA - ",Matrix$YEAR[1]), file = Errors, append = TRUE)

  Matrix$mean_distance=1852*Matrix$HAUL_DURATION/20
  Matrix$low_distance=Matrix$mean_distance-Matrix$mean_distance*0.15
  Matrix$up_distance=Matrix$mean_distance+Matrix$mean_distance*0.15
  distance=which(Matrix$DISTANCE<Matrix$low_distance | Matrix$DISTANCE>Matrix$up_distance)
  if (length(distance)!=0){
    for (j in 1:length(distance)){
      write(paste("Warning: in haul",Matrix$HAUL_NUMBER[distance[j]],"distance measure",Matrix$DISTANCE[distance[j]],"inconsistent with the duration of the haul (",Matrix$HAUL_DURATION[distance[j]],"min )"), file = Errors, append = TRUE)
    }
  }



  if (numberError ==0) {
    write("No error occurred",file = Errors, append = TRUE)
    return(TRUE)
  } else { return(FALSE) }

}
