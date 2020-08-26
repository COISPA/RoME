############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.eu #
#   March 2020                                                                                                             #
############################################################################################################################
# Check if TA, TB and TC files have the same area and year

check_area <- function(DataTA, DataTB,DataTC,DataTE=NA,DataTL=NA, wd, suffix){

  if (FALSE){
    wd <- tempdir()
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
    DataTA = MEDITS::TA # read.csv("~/GitHub/RoME/data/TA_GSA18_1994-2018.csv", sep=";")
    DataTB = MEDITS::TB # read.csv("~/GitHub/RoME/data/TB_GSA18_1994-2018.csv", sep=";")
    DataTC = MEDITS::TC # read.csv("~/GitHub/RoME/data/TC_GSA18_1994-2018.csv", sep=";")
    DataTE = NA
    DataTL = NA

    # check_area(DataTA, DataTB,DataTC,DataTE=NA,DataTL=NA, wd, suffix)
  }

  if (!file.exists(file.path(wd, "Logfiles"))){
    dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
  }

  numberError = 0
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  }
  Errors <- file.path(wd,"Logfiles",paste("Logfile_", suffix ,".dat",sep=""))

  #### CHECK TL FIELDS ####
  {
    if ("LITTER_SUB.CATEGORY" %in% colnames(DataTL)){
      colnames(DataTL)[which(colnames(DataTL)=="LITTER_SUB.CATEGORY")] <- "LITTER_SUB-CATEGORY"
    }
    if ("TOTAL_WEIGHT_IN_THE_SUB.CATEGORY_HAUL" %in% colnames(DataTL)){
      colnames(DataTL)[which(colnames(DataTL)=="TOTAL_WEIGHT_IN_THE_SUB.CATEGORY_HAUL")] <- "TOTAL_WEIGHT_IN_THE_SUB-CATEGORY_HAUL"
    }
    if ("TOTAL_NUMBER_IN_THE_SUB.CATEGORY_HAUL" %in% colnames(DataTL)){
      colnames(DataTL)[which(colnames(DataTL)=="TOTAL_NUMBER_IN_THE_SUB.CATEGORY_HAUL")] <- "TOTAL_NUMBER_IN_THE_SUB-CATEGORY_HAUL"
    }
  }
  #### CHECK TL FIELDS - END ####

  write(paste("\n----------- check consistency of area in TX - ", DataTA$YEAR[1]), file = Errors, append = TRUE)

  GSA_TA=unique(DataTA$AREA)
  if (!(GSA_TA %in% RoME::GSAs$GSA)){
    write(paste("Warning: the AREA code used in TA file is not present in the GFCM's GSA list"), file = Errors, append = TRUE)
  }
  GSA_TB=unique(DataTB$AREA)
  if (!(GSA_TB %in% RoME::GSAs$GSA)){
    write(paste("Warning: the AREA code used in TB file is not present in the GFCM's GSA list"), file = Errors, append = TRUE)
  }
  GSA_TC=unique(DataTC$AREA)
  if (!(GSA_TC %in% RoME::GSAs$GSA)){
    write(paste("Warning: the AREA code used in TC file is not present in the GFCM's GSA list"), file = Errors, append = TRUE)
  }


  if (length(DataTE)>1 & any(!is.na(DataTE))) {
  GSA_TE=unique(DataTE$AREA)
  if (!(GSA_TE %in% RoME::GSAs$GSA)){
    write(paste("Warning: the AREA code used in TE file is not present in the GFCM's GSA list"), file = Errors, append = TRUE)
  }
  }
  if (length(DataTL)>1 & any(!is.na(DataTL))) {
  GSA_TL=unique(DataTL$AREA)
  if (!(GSA_TL %in% RoME::GSAs$GSA)){
    write(paste("Warning: the AREA code used in TL file is not present in the GFCM's GSA list"), file = Errors, append = TRUE)
  }
  }

if (  (all(is.na(DataTE)) & length(DataTE)==1  ) & (all(is.na(DataTL)) & length(DataTL)==1  ) ){
  if ( (GSA_TA != GSA_TB) | (GSA_TB != GSA_TC) | (GSA_TA != GSA_TC)){
      write(paste("Different value for field AREA in TA, TB and TC files"), file = Errors, append = TRUE)
      numberError = numberError +1
    }


} else if (!((all(is.na(DataTE)) & length(DataTE)==1  )) & (all(is.na(DataTL)) & length(DataTL)==1  )){
 if ( (GSA_TA != GSA_TB) | (GSA_TB != GSA_TC) | (GSA_TA != GSA_TC) | (GSA_TE != GSA_TA)| (GSA_TE != GSA_TB)| (GSA_TE != GSA_TC)){
    write(paste("Different value for field AREA in TA, TB, TC, TE files"), file = Errors, append = TRUE)
    numberError = numberError +1
  }


} else if (!((all(is.na(DataTL)) & length(DataTL)==1  )) & (all(is.na(DataTE)) & length(DataTE)==1  ))  {
  if ( (GSA_TA != GSA_TB) | (GSA_TB != GSA_TC) | (GSA_TA != GSA_TC) | (GSA_TL != GSA_TA)| (GSA_TL != GSA_TB)| (GSA_TL != GSA_TC)){
    write(paste("Different value for field AREA in TA, TB, TC and TL files"), file = Errors, append = TRUE)
    numberError = numberError +1
  }

} else if (!((all(is.na(DataTL)) & length(DataTL)==1  )) &  !((all(is.na(DataTE)) & length(DataTE)==1  )))  {
  if ( (GSA_TA != GSA_TB) | (GSA_TB != GSA_TC) | (GSA_TA != GSA_TC) | (GSA_TL != GSA_TA)| (GSA_TE != GSA_TA)| (GSA_TE != GSA_TL) | (GSA_TL != GSA_TB)| (GSA_TE != GSA_TB)| (GSA_TL != GSA_TC)| (GSA_TE != GSA_TC)){
    write(paste("Different value for field AREA in TA, TB, TC, TE and TL files"), file = Errors, append = TRUE)
    numberError = numberError +1
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
  } else { return(FALSE) }
}
