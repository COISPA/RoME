############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.it #
#   January 2022                                                                                                           #
############################################################################################################################

check_0_fieldsTA<-function(DataTA,wd, suffix, year){

  if (FALSE){
    wd <- tempdir() # "D:\\Documents and Settings\\Utente\\Documenti\\GitHub\\RoME\\temp"
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
    DataTA = ta # read.csv("~/GitHub/RoME/data/TA_GSA18_1994-2018.csv", sep=";")
    DataTA[1,"WARP_DIAMETER"] <-0
    # check_0_fieldsTA(DataTA,wd,suffix, year=2015)
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



  write(paste("\n----------- check 0 fields TA"), file = Errors, append = TRUE)
  Matrix = DataTA
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
      numberError = numberError +1
    }
  }

  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  }
  #  if (file.exists(file.path(tempdir(), "Logfiles"))){
  # unlink(file.path(tempdir(),"Logfiles"),recursive=T)
  # }

  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }


  #unlink(file.path(tempdir(),"Graphs"),recursive=T)
  #unlink(file.path(tempdir(),"files R-Sufi"),recursive=T)


  }
################################################################################
