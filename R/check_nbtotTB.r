############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files)                                #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.it #
#   January 2022                                                                                                           #
############################################################################################################################
# Check if in TB  NB_TOTAL equals NB_F+NB_M+NB_I

check_nbtotTB<-function(DataTB, year, wd, suffix){

  if (FALSE){
    # library(RoME)
    wd <- tempdir()
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
    DataTB = RoME::TB # read.csv("~/GitHub/RoME/data/TB_GSA18_1994-2018.csv", sep=";")
    year=2012

    DataTB = DataTB[DataTB$YEAR == year ,   ]
    DataTB[1,"TOTAL_NUMBER_IN_THE_HAUL"] <- 6
    # check_nbtotTB(DataTB, year, wd, suffix)
  }


  if (!file.exists(file.path(wd, "Logfiles"))){
    dir.create(file.path(wd, "Logfiles"), recursive = TRUE, showWarnings = FALSE)
  }
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
  }
  numberError = 0
  Errors <- file.path(wd,"Logfiles",paste("Logfile_",suffix,".dat",sep=""))
  if (!file.exists(Errors)){
    file.create(Errors)
  }

  ### FILTERING DATA FOR THE SELECTED YEAR
  arg <- "year"
  if (!exists(arg)) {
    stop(paste0("'", arg, "' argument should be provided"))
  } else if (length(year) != 1) {
    stop(paste0("only one value should be provided for '", arg, "' argument"))
  } else if (is.na(year)) {
    stop(paste0(arg, " argument should be a numeric value"))
  }
  DataTB <- DataTB[DataTB$YEAR == year, ]
  ########################################

  Matrix = DataTB #read.csv(paste(Data,".csv",sep=""), sep=";", header=TRUE)
  write(paste("\n----------- check consistency of TOTAL_NUMBER_IN_THE_HAUL and number per sex in TB - ", Matrix$YEAR[1]), file = Errors, append = TRUE)
  Err_vec=which(round(Matrix$TOTAL_NUMBER_IN_THE_HAUL,6)!=round(Matrix$NB_OF_FEMALES + Matrix$NB_OF_MALES + Matrix$NB_OF_UNDETERMINED,6))

  if  (length(Err_vec)!=0){
    for (i in 1:length(Err_vec)){
      Err=cbind(as.character(Matrix$TYPE_OF_FILE[1]),Matrix$HAUL_NUMBER[Err_vec[i]],ifelse(is.factor(Matrix$GENUS[Err_vec[i]]), as.character(Matrix$GENUS[Err_vec[i]]), Matrix$GENUS[Err_vec[i]]),ifelse(is.factor(Matrix$SPECIES[Err_vec[i]]), as.character(Matrix$SPECIES[Err_vec[i]]), Matrix$SPECIES[Err_vec[i]]))
      write(paste("Error: Haul",Err[1,2],Err[1,3],Err[1,4],": NB_TOTAL doesn't equal NB_F+NB_M+NB_I", Err[1,1]), file = Errors, append = TRUE)
      numberError <- numberError + 1
    }
  }
  if (numberError ==0) {
      write(paste("No error occurred"), file = Errors, append = TRUE)
  }
   if (file.exists(file.path(tempdir(), "Logfiles"))){
  unlink(file.path(tempdir(),"Logfiles"),recursive=T)
  }
  if (file.exists(file.path(tempdir(), "Graphs"))){
  unlink(file.path(tempdir(),"Graphs"),recursive=T)
    }
	if (file.exists(file.path(tempdir(), "files R-Sufi"))){
  unlink(file.path(tempdir(),"files R-Sufi"),recursive=T)
    }
  if (numberError ==0) {
    return(TRUE)
    } else {
      return(FALSE)
    }
}
