############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.eu #
#   March 2020                                                                                                             #
############################################################################################################################
# Check if in TB  NB_TOTAL equals NB_F+NB_M+NB_I

check_nbtotTB<-function(DataTB, wd, suffix){

  if (FALSE){
    library(RoME)
    wd <- tempdir()
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
    DataTB = read.csv("~/GitHub/RoME/data/TB_GSA18_1994-2018.csv", sep=";")
    DataTB = DataTB[DataTB$YEAR == 2012 ,   ]

    # check_nbtotTB(DataTB, wd, suffix)
  }
  if (!file.exists(file.path(wd, "Logfiles"))){
    dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
  }
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  }
  numberError = 0
  Errors <- file.path(wd,"Logfiles",paste("Logfile_",suffix,".dat",sep=""))

  Errors <- paste(wd,"\\Logfiles\\Logfile_",suffix,".dat",sep="")
  Matrix = DataTB #read.csv(paste(Data,".csv",sep=""), sep=";", header=TRUE)
  write(paste("\n----------- check consistency of TOTAL_NUMBER_IN_THE_HAUL and number per sex in TB - ", Matrix$YEAR[1]), file = Errors, append = TRUE)
  Err_vec=which(round(Matrix$TOTAL_NUMBER_IN_THE_HAUL,6)!=round(Matrix$NB_OF_FEMALES + Matrix$NB_OF_MALES + Matrix$NB_OF_UNDETERMINED,6))

  if  (length(Err_vec)!=0){
    for (i in 1:length(Err_vec)){
      Err=cbind(as.character(Matrix$TYPE_OF_FILE[1]),Matrix$HAUL_NUMBER[Err_vec[i]],ifelse(is.factor(Matrix$GENUS[Err_vec[i]]), as.character(Matrix$GENUS[Err_vec[i]]), Matrix$GENUS[Err_vec[i]]),ifelse(is.factor(Matrix$SPECIES[Err_vec[i]]), as.character(Matrix$SPECIES[Err_vec[i]]), Matrix$SPECIES[Err_vec[i]]))
      write(paste("Warning: Haul",Err[1,2],Err[1,3],Err[1,4],": NB_TOTAL doesn't equal NB_F+NB_M+NB_I", Err[1,1]), file = Errors, append = TRUE)
    }
  }
  if (numberError ==0) {
      write(paste("No error occurred"), file = Errors, append = TRUE)
  }
  unlink(file.path(tempdir(),"Logfiles"),recursive=T)
  #unlink(file.path(tempdir(),"Graphs"),recursive=T)
  #unlink(file.path(tempdir(),"files R-Sufi"),recursive=T)

  if (numberError ==0) {
    return(TRUE)
    } else {
      return(FALSE)
    }
}
