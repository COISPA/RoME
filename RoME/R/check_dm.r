############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.eu #
#   March 2020                                                                                                             #
############################################################################################################################
# Check consistency of wing ad vertical opening in TA according to INSTRUCTION MANUAL VERSION 9 MEDITS 2017

check_dm<-  function(DataTA,wd,suffix){

  if (FALSE){
    wd <- tempdir() # "D:\\Documents and Settings\\Utente\\Documenti\\GitHub\\RoME\\temp"
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
    DataTA = read.csv("~/GitHub/RoME/data/TA_GSA18_1994-2018.csv", sep=";")
    DataTA$VERTICAL_OPENING[1] <- 101
    # check_dm(DataTA,wd,suffix)
  }

  if (FALSE) {
    # library(MEDITS)
    wd <- tempdir()
    suffix="2020-03-05_time_h17m44s55"
    DataTA = MEDITS::TA
    check_dm(DataTA,wd,suffix)
  }


  if (!file.exists(file.path(wd, "Logfiles"))){
    dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
  }

  numberError = 0
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
  }
  Errors <- file.path(wd,"Logfiles",paste("Logfile_",suffix,".dat",sep=""))
  if (!file.exists(Errors)){
    file.create(Errors)
  }

  Matrix = DataTA
  write(paste("\n----------- check dm TA - ", Matrix$YEAR[1]), file = Errors, append = TRUE)

  wing=which(Matrix$WING_OPENING < 50 | Matrix$WING_OPENING > 250)
  vertical=which(Matrix$VERTICAL_OPENING < 10 | Matrix$VERTICAL_OPENING > 100)
  wing.decimal=which(!(Matrix$WING_OPENING%%1==0))
  vertical.decimal=which(!(Matrix$VERTICAL_OPENING%%1==0))

  if (length(wing)!=0){
    for (i in 1:length(wing)){
      write(paste("Haul ",Matrix$HAUL_NUMBER[wing[i]],"WING_OPENING out of boundaries (50,250) in", Matrix$TYPE_OF_FILE[1],". Please check if the measure unit is dm"), file = Errors, append = TRUE)
      numberError = numberError +1
    }
  }
  if (length(vertical)!=0){
    for (j in 1:length(vertical)){
      write(paste("Haul ",Matrix$HAUL_NUMBER[vertical[j]],"VERTICAL_OPENING out of boundaries (10,100)in", Matrix$TYPE_OF_FILE[1],". Please check if the measure unit is dm"), file = Errors, append = TRUE)
    }
  }

  if (length(wing.decimal)!=0){
    for (i in 1:length(wing.decimal)){
      write(paste("Haul ",Matrix$HAUL_NUMBER[wing.decimal[i]],"WING_OPENING is not an INTEGER number in", Matrix$TYPE_OF_FILE[1],". Please check the format of the value"), file = Errors, append = TRUE)
    }
  }

  if (length(vertical.decimal)!=0){
    for (i in 1:length(vertical.decimal)){
      write(paste("Haul ",Matrix$HAUL_NUMBER[vertical.decimal[i]],"VERTICAL_OPENING is not an INTEGER number in", Matrix$TYPE_OF_FILE[1],". Please check the format of the value"), file = Errors, append = TRUE)
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
