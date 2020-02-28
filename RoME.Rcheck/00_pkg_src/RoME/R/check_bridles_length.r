############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.eu #
#   March 2020                                                                                                             #
############################################################################################################################
# Check if the value of bridles length is consistent according to the mean depth (see INSTRUCTION MANUAL VERSION 5 MEDITS 2007)

check_bridles_length<-function(DataTA, wd, suffix){


  if (FALSE){
    library(RoME)
    wd <- tempdir()
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
    DataTA = MEDITS::TA
    # check_bridles_length(DataTA, wd, suffix)
  }

  if (!file.exists(paste(wd,"Logfiles",sep="/"))){
    dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
  }

  numberError = 0
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  }
  Errors <- paste(wd,"\\Logfiles\\Logfile_",suffix,".dat",sep="")

   ResultData = DataTA
   write(paste("\n----------- check consistency of bridles length TA - ",ResultData$YEAR[1]), file = Errors, append = TRUE)

  ResultData=ResultData[ResultData$VALIDITY=="V",]
  ResultData$mean_depth=(ResultData$SHOOTING_DEPTH +ResultData$HAULING_DEPTH)/2
  if (ResultData$YEAR[1]<1995){
    for (i in 1:nrow(ResultData)){
      if ( ( (ResultData$mean_depth[i]>=0) & (ResultData$mean_depth[i]<200)& (ResultData$BRIDLES_LENGTH[i]!=100) ) | ( (ResultData$mean_depth[i]>=200) & (ResultData$BRIDLES_LENGTH[i]!=150) ) )
      {
        write(paste("Haul",ResultData$HAUL_NUMBER[i],": BRIDLES_LENGTH not correct"), file = Errors, append = TRUE)
        numberError = numberError +1
      }
    }
  } else {for (i in 1:nrow(ResultData)){
    if ( ( (ResultData$mean_depth[i]>=0) & (ResultData$mean_depth[i]<200)& (ResultData$BRIDLES_LENGTH[i]!=100) ) | ( (ResultData$mean_depth[i]>=200) & ((ResultData$BRIDLES_LENGTH[i]!=150)&(ResultData$BRIDLES_LENGTH[i]!=200)) )   )
    {
      write(paste("Haul",ResultData$HAUL_NUMBER[i],": BRIDLES_LENGTH not correct"), file = Errors, append = TRUE)
      numberError = numberError +1
    }
    if  ( (ResultData$mean_depth[i]>=500) & (ResultData$BRIDLES_LENGTH[i]!=200) ){
      write(paste("Warning: Haul",ResultData$HAUL_NUMBER[i],": MEDITS handbook recommends to increase the bridle length to 200 m in depths deeper than 500 m"), file = Errors, append = TRUE)
    }
  }
  }


  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  }	else {
    write(paste("ATTENTION: BRIDLES_LENGTH=100 between 0 and 200 m, BRIDLES_LENGTH=150 over 200 m"), file = Errors, append = TRUE)
  }
    return(TRUE)
}

