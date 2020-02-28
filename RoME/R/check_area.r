############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.eu #
#   March 2020                                                                                                             #
############################################################################################################################
# Check if TA, TB and TC files have the same area and year

check_area <- function(DataTA, DataTB,DataTC,DataTD=NA,DataTT=NA,DataTE=NA,DataTL=NA, wd, suffix){

  if (FALSE){
    wd <- tempdir()
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
    DataTA = MEDITS::TA # read.csv("~/GitHub/RoME/data/TA_GSA18_1994-2018.csv", sep=";")
    DataTB = MEDITS::TB # read.csv("~/GitHub/RoME/data/TB_GSA18_1994-2018.csv", sep=";")
    DataTC = MEDITS::TC # read.csv("~/GitHub/RoME/data/TC_GSA18_1994-2018.csv", sep=";")
    DataTD = NA
    DataTE = NA
    DataTT = NA
    DataTL = NA

    check_area(DataTA, DataTB,DataTC,DataTD=NA,DataTT=NA,DataTE=NA,DataTL=NA, wd, suffix)
  }

  if (!file.exists("Logfiles")){
      dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
  }

  numberError = 0
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  }
  Errors <<- paste(wd,"\\Logfiles\\Logfile_",suffix,".dat",sep="")

  write(paste("\n----------- check consistency of area and year TX - ", DataTA$YEAR[1]), file = Errors, append = TRUE)

  GSA_TA=unique(DataTA$AREA)
  GSA_TB=unique(DataTB$AREA)
  GSA_TC=unique(DataTC$AREA)

  if  (!is.na(DataTD))  {
  GSA_TD=unique(DataTD$AREA)
  }
  if (!is.na(DataTT)){
  GSA_TT=unique(DataTT$AREA)
  }
  if (!is.na(DataTE)) {
  GSA_TE=unique(DataTE$AREA)
  }
  if (!is.na(DataTL)) {
  GSA_TL=unique(DataTL$AREA)
  }

if ((is.na(DataTD) &  is.na(DataTT)) | is.na(DataTE)){
  if ( (GSA_TA != GSA_TB) | (GSA_TB != GSA_TC) | (GSA_TA != GSA_TC)){
      write(paste("Different value for field AREA in TA, TB and TC files"), file = Errors, append = TRUE)
      numberError = numberError +1
    }

  } else if (!is.na(DataTD) &  !is.na(DataTT) & is.na(DataTE))  {
  if ( (GSA_TA != GSA_TB) | (GSA_TB != GSA_TC) | (GSA_TA != GSA_TC) | (GSA_TD != GSA_TA)| (GSA_TT != GSA_TA)| (GSA_TT != GSA_TD) | (GSA_TD != GSA_TB)| (GSA_TT != GSA_TB)| (GSA_TD != GSA_TC)| (GSA_TT != GSA_TC)){
    write(paste("Different value for field AREA in TA, TB, TC, TD and TT files"), file = Errors, append = TRUE)
    numberError = numberError +1
  }

} else if ((is.na(DataTD) &  is.na(DataTT)) & !is.na(DataTE)){
 if ( (GSA_TA != GSA_TB) | (GSA_TB != GSA_TC) | (GSA_TA != GSA_TC) | (GSA_TE != GSA_TA)| (GSA_TE != GSA_TB)| (GSA_TE != GSA_TC)){
    write(paste("Different value for field AREA in TA, TB, TC, TE files"), file = Errors, append = TRUE)
    numberError = numberError +1
  }

} else if (!is.na(DataTT) & is.na(DataTD) & is.na(DataTE))  {
  if ( (GSA_TA != GSA_TB) | (GSA_TB != GSA_TC) | (GSA_TA != GSA_TC) | (GSA_TT != GSA_TA)| (GSA_TT != GSA_TB)| (GSA_TT != GSA_TC)){
    write(paste("Different value for field AREA in TA, TB, TC and TT files"), file = Errors, append = TRUE)
    numberError = numberError +1
  }

} else if (!is.na(DataTD) & is.na(DataTT) & is.na(DataTE))  {
  if ( (GSA_TA != GSA_TB) | (GSA_TB != GSA_TC) | (GSA_TA != GSA_TC) | (GSA_TD != GSA_TA)| (GSA_TD != GSA_TB)| (GSA_TD != GSA_TC)){
    write(paste("Different value for field AREA in TA, TB, TC and TD files"), file = Errors, append = TRUE)
    numberError = numberError +1
  }

} else if (!is.na(DataTL) & is.na(DataTD) & is.na(DataTT) & is.na(DataTE))  {
  if ( (GSA_TA != GSA_TB) | (GSA_TB != GSA_TC) | (GSA_TA != GSA_TC) | (GSA_TL != GSA_TA)| (GSA_TL != GSA_TB)| (GSA_TL != GSA_TC)){
    write(paste("Different value for field AREA in TA, TB, TC and TL files"), file = Errors, append = TRUE)
    numberError = numberError +1
  }

} else if (!is.na(DataTE) &  !is.na(DataTL) & is.na(DataTD) & is.na(DataTT))  {
  if ( (GSA_TA != GSA_TB) | (GSA_TB != GSA_TC) | (GSA_TA != GSA_TC) | (GSA_TL != GSA_TA)| (GSA_TE != GSA_TA)| (GSA_TE != GSA_TL) | (GSA_TL != GSA_TB)| (GSA_TE != GSA_TB)| (GSA_TL != GSA_TC)| (GSA_TE != GSA_TC)){
    write(paste("Different value for field AREA in TA, TB, TC, TE and TL files"), file = Errors, append = TRUE)
    numberError = numberError +1
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
