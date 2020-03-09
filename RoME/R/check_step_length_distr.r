###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #
#   March 2020
################################################################################

# Check if LENGTH_CLASS measures are correct


if (FALSE){
  ResultData = read.csv("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/data/TC_GSA18_1994-2018.csv", sep=";")
  ResultData=ResultData[ResultData$YEAR==1994,]
  #ResultTB = read.csv("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/data/TB_GSA18_1994-2018.csv", sep=";")

  wd <- "C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/temp"
  suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  #load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//DataTargetSpecies.rda")
  #load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//Maturity_parameters.rda")
  #load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//TM_list.rda")
  check_step_length_distr(ResultData,wd,suffix)
}




check_step_length_distr<-function(ResultData,wd,suffix){
  Format="from_2012"

  if (!file.exists(file.path(wd, "Logfiles"))){
    dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
  }
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  }
  numberError = 0
  Errors <- file.path(wd,"Logfiles",paste("Logfile_",suffix,".dat",sep=""))

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

  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }
}

################################################################################
