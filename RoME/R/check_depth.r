############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files)                                #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.it #
#   January 2022                                                                                                           #
############################################################################################################################
# Check if that difference between start depth and end depth is not greater than 20%

check_depth<-function(DataTA, year, wd, suffix){
  if (FALSE){
    wd <- tempdir() # "D:\\Documents and Settings\\Utente\\Documenti\\GitHub\\RoME\\temp"
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
    year <- 2007
    DataTA = RoME::TA #read.csv("~/GitHub/RoME/data/TA_GSA18_1994-2018.csv", sep=";")

    # check_depth(DataTA,year,wd,suffix)
  }


  if (!file.exists(file.path(wd, "Logfiles"))){
    dir.create(file.path(wd, "Logfiles"), recursive = TRUE, showWarnings = FALSE)
  }
  numberError = 0
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
  }
  Errors <- file.path(wd,"Logfiles",paste("Logfile_", suffix ,".dat",sep=""))
  if (!file.exists(Errors)){
    file.create(Errors)
  }

  ### FILTERING DATA FOR THE SELECTED YEAR
  arg <- "year"
  if (!exists(arg)) {
    stop(paste0("'",arg,"' argument should be provided"))
  } else if (length(year)!= 1) {
    stop(paste0("only one value should be provided for '",arg,"' argument"))
  } else if (is.na(year)){
    stop(paste0(arg," argument should be a numeric value"))
  }

  DataTA <- DataTA[DataTA$YEAR == year, ]
  ########################################

  ResultData = DataTA #read.csv(paste(Data,".csv",sep=""), sep=";", header=TRUE)

  write(paste("\n----------- check difference between start depth and end depth (not greater than 20%) TA ", "-",ResultData$YEAR[1]), file = Errors, append = TRUE)


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
  } else { return(FALSE) }

}
