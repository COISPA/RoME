############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.eu #
#   March 2020                                                                                                             #
############################################################################################################################
# Check if in TB, TC and TE the date by haul is the same of the one reported in TA

check_date_haul <- function (DataTA, Data, wd, suffix){

  if (FALSE){
    library(RoME)
    wd <- tempdir()
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
    DataTA = read.csv("~/GitHub/RoME/data/TA_GSA18_1994-2018.csv", sep=";") # MEDITS::TA
    Data = read.csv("~/GitHub/RoME/data/TB_GSA18_1994-2018.csv", sep=";") # MEDITS::TA
    Data = read.csv("~/GitHub/RoME/data/TC_GSA18_1994-2018.csv", sep=";") # MEDITS::TA
    Data = read.csv("~/GitHub/RoME/data/TE_2012-2018 _GSA18.csv", sep=";") # MEDITS::TA
    Data = read.csv("~/GitHub/RoME/data/TL_GSA18 2012-2018.csv", sep=";") # MEDITS::TA
    # check_date_haul(DataTA, Data, wd, suffix)
  }

  if (!file.exists("Logfiles")){
    dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
  }

  numberError = 0
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  }
  Errors <- paste(wd,"\\Logfiles\\Logfile_",suffix,".dat",sep="")

  Dataset = Data

         if (as.character(Dataset$TYPE_OF_FILE[1]) == "TB") {
    write(paste("\n----------- check correctness of date by haul in TB - ", Dataset$YEAR[1]), file = Errors, append = TRUE)
  } else if (as.character(Dataset$TYPE_OF_FILE[1]) == "TC") {
    write(paste("\n----------- check correctness of date by haul in TC - ", Dataset$YEAR[1]), file = Errors, append = TRUE)
  } else if (as.character(Dataset$TYPE_OF_FILE[1]) == "TE") {
    write(paste("\n----------- check correctness of date by haul in TE - ", Dataset$YEAR[1]), file = Errors, append = TRUE)
  } else if (as.character(Dataset$TYPE_OF_FILE[1]) == "TL") {
    write(paste("\n----------- check correctness of date by haul in TL - ", Dataset$YEAR[1]), file = Errors, append = TRUE)
  }


Dataset$Date = paste (Dataset$HAUL_NUMBER, "/",Dataset$MONTH,"/",Dataset$DAY,sep="")
TA_df =DataTA
TA_df$Date = paste (TA_df$HAUL_NUMBER,"/",TA_df$MONTH,"/",TA_df$DAY,sep="")

for (i in 1:nrow(Dataset)){
  if( !(Dataset$Date[i] %in% TA_df$Date)) {
  write(paste("Warning: Haul",Dataset$HAUL_NUMBER[i],", code species", Dataset$GENUS[i] , Dataset$SPECIES[i] ,": the date is not consistent with the date reported in TA."), file = Errors, append = TRUE)
  numberError = numberError+1
}
  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  }

  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }
}

}
