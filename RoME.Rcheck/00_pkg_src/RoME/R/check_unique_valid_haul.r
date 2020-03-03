###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #
#   March 2020
################################################################################

# Among hauls with the same code only one must be valid


if (FALSE){
  ResultDataTA = read.csv("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/data/TA_GSA18_1994-2018.csv", sep=";")
  ResultDataTA=ResultDataTA[ResultDataTA$YEAR==2017,]

  wd <- "C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/temp"
  suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  #load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//DataTargetSpecies.rda")
  #load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//Maturity_parameters.rda")
  #load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//TM_list.rda")
  check_unique_valid_haul(ResultDataTA,wd,suffix)
}


check_unique_valid_haul<-function(ResultDataTA,wd,suffix){
  Format="from_2012"
  if (!file.exists(paste(wd,"Logfiles",sep="/"))){
    dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
  }

  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  }

  if (!file.exists(paste(wd,"Graphs",sep="/"))){
    dir.create(file.path(wd, "Graphs"), showWarnings = FALSE)
  }


  Errors <- paste(wd,"/Logfiles/Logfile_",suffix,".dat",sep="")

    numberError = 0
  Result = ResultDataTA # read.csv(paste(DataTA,".csv",sep=""), sep=";", header=TRUE)
  write(paste("
              ----------- check uniqueness of valid hauls TA - ",Result$YEAR[1]), file = Errors, append = TRUE)


  #channelTA <- odbcConnectExcel(paste(DataTA,".xls", sep = ""))

  #queryHauls= "SELECT HAUL_NUMBER, count(*) as no_hauls from Result where HAUL_NUMBER is not null group by HAUL_NUMBER"

  ResultTA= aggregate(Result$HAUL_NUMBER, by=list(Result$HAUL_NUMBER),FUN="length")
  colnames(ResultTA)=c("HAUL_NUMBER","no_hauls")
  #ResultTA=sqldf(queryHauls)

  moreThanOneHaul = which(ResultTA$no_hauls > 1, arr.ind=TRUE)

  if (length(moreThanOneHaul)>1){
    for (j in 1:length(moreThanOneHaul)){
      index= moreThanOneHaul[j]
      queryCheck = paste("SELECT * from Result where HAUL_NUMBER=", ResultTA$HAUL_NUMBER[index], " and VALIDITY='V'", sep="" )
      ResultCheck=sqlQuery(channelTA, queryCheck)
      if (nrow(ResultCheck)>1)   {
        write(paste("Haul",ResultTA$HAUL_NUMBER[index],": only one row for the haul is allowed to be VALID"), file = Errors, append = TRUE)
        numberError = numberError +1
      }
    }
  }
  #odbcClose(channelTA)

  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  }


  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }

}

################################################################################
