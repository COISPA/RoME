############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files)                                #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.it #
#   January 2022                                                                                                           #
############################################################################################################################



# Check if LENGTH_CLASS measures are correct


if (FALSE){
  ResultData = tc #RoME::TC
  tc[83,"LENGTH_CLASS"] <- 100.3
  year=2015
  wd <- tempdir()
  suffix=NA
  check_step_length_distr(ResultData,year,wd,suffix)
}




check_step_length_distr<-function(ResultData,year,wd,suffix){
  Format="from_2012"

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
  ResultData <- ResultData[ResultData$YEAR == year, ]
  ########################################

  ResultData <- ResultData[!is.na(ResultData$LENGTH_CLASS),]
  write(paste("\n----------- check consistency of length distribution TC - ",ResultData$YEAR[1]), file = Errors, append = TRUE)

  fishes_cefalopods= ResultData[ResultData$LENGTH_CLASSES_CODE!="m",]

  for (i in 1:nrow(ResultData)){
    if ((ResultData$LENGTH_CLASS[i])!=round((ResultData$LENGTH_CLASS[i]),0)){
      write(paste("Haul",ResultData$HAUL_NUMBER[i],ResultData$GENUS[i],ResultData$SPECIES[i],ResultData$SEX[i],ResultData$LENGTH_CLASS[i],": LENGTH_CLASS value must be an integer number in", ResultData$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
      numberError = numberError +1
    }
  }

  if (nrow(fishes_cefalopods)>0){

  for (j in 1:nrow(fishes_cefalopods)){
    if (as.character(fishes_cefalopods$LENGTH_CLASSES_CODE[j])=="1"){
      if ((fishes_cefalopods$LENGTH_CLASS[j]/10)!=round((fishes_cefalopods$LENGTH_CLASS[j]/10),0))
      {
       write(paste("Haul",fishes_cefalopods$HAUL_NUMBER[j],fishes_cefalopods$GENUS[j],fishes_cefalopods$SPECIES[j],fishes_cefalopods$SEX[j],fishes_cefalopods$LENGTH_CLASS[j],": in", ResultData$TYPE_OF_FILE[1],"LENGTH_CLASS value for fishes and cefalopods must have a full step, because LENGTH_CLASSES_CODE=1"), file = Errors, append = TRUE)
       numberError = numberError +1
      }
    } else {
      if ((fishes_cefalopods$LENGTH_CLASS[j]/5)!=round((fishes_cefalopods$LENGTH_CLASS[j]/5),0))
      {
        write(paste("Haul",fishes_cefalopods$HAUL_NUMBER[j],fishes_cefalopods$GENUS[j],fishes_cefalopods$SPECIES[j],fishes_cefalopods$SEX[j],fishes_cefalopods$LENGTH_CLASS[j],": in", ResultData$TYPE_OF_FILE[1],"LENGTH_CLASS value for fishes and cefalopods must have a full or half step"), file = Errors, append = TRUE)
        numberError = numberError +1
      }
    }
  }

  }

  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  }
#    if (file.exists(file.path(tempdir(), "Logfiles"))){
#   unlink(file.path(tempdir(),"Logfiles"),recursive=T)
#   }
#   if (file.exists(file.path(tempdir(), "Graphs"))){
#   unlink(file.path(tempdir(),"Graphs"),recursive=T)
#     }
# 	if (file.exists(file.path(tempdir(), "files R-Sufi"))){
#   unlink(file.path(tempdir(),"files R-Sufi"),recursive=T)
#     }
  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }
}

################################################################################
