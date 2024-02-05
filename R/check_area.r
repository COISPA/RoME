############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.it #
#   January 2022                                                                                                           #
############################################################################################################################
# Check if TA, TB and TC files have the same area and year

check_area <- function(DataTA, DataTB,DataTC,DataTE=NA,DataTL=NA, year, wd, suffix){

  if (FALSE){
    wd <- tempdir()
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
    DataTA = TA # RoME::TA # read.csv("~/GitHub/RoME/data/TA_GSA18_1994-2018.csv", sep=";")
    DataTB = TB # RoME::TB # read.csv("~/GitHub/RoME/data/TB_GSA18_1994-2018.csv", sep=";")
    DataTC = TC #RoME::TC # read.csv("~/GitHub/RoME/data/TC_GSA18_1994-2018.csv", sep=";")
    DataTE = TE #RoME::TE
    DataTL = TL #RoME::TL
    year=2015
    # check_area(DataTA, DataTB,DataTC,DataTE=NA,DataTL=NA,year, wd, suffix)
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
  DataTB <- DataTB[DataTB$YEAR == year, ]
  DataTC <- DataTC[DataTC$YEAR == year, ]


  if (all(is.na(DataTE))){
    DataTE = NA
  } else {
    DataTE <- DataTE[DataTE$YEAR == year, ]
  }

  if (all(is.na(DataTL))){
    DataTL = NA
  } else {
    DataTL <- DataTL[DataTL$YEAR == year, ]
  }
  ########################################

  write(paste("\n----------- check consistency of area in TX - ", DataTA$YEAR[1]), file = Errors, append = TRUE)

  GSA_TA=unique(DataTA$AREA)

  if (any(is.na(GSA_TA))) {
    GSA_TA <- GSA_TA[!is.na(GSA_TA)]
    write(paste("NA value in AREA field of TA table"), file = Errors, append = TRUE)
    numberError = numberError +1
    }

  if (length(GSA_TA)>1) {
    write(paste("AREA field in TA table contains data from more than 1 GSA. The checks are performed only on data referred to a single GSA and further analysis will be conducted only on the first GSA included in data"), file = Errors, append = TRUE)
    numberError = numberError +1
  }

  if (!(GSA_TA[1] %in% RoME::GSAs$GSA)){
    write(paste("Warning: the AREA code used in TA file is not present in the GFCM's GSA list"), file = Errors, append = TRUE)
  }

  ###########

  GSA_TB=unique(DataTB$AREA)

  if (any(is.na(GSA_TB))) {
    GSA_TB <- GSA_TB[!is.na(GSA_TB)]
    write(paste("NA value in AREA field of TB TBble"), file = Errors, append = TRUE)
    numberError = numberError +1
  }

  if (length(GSA_TB)>1) {
    write(paste("AREA field in TB table contains data from more than 1 GSA. The checks are performed only on data referred to a single GSA and further analysis will be conducted only on the first GSA included in data"), file = Errors, append = TRUE)
    numberError = numberError +1
  }

  if (!(GSA_TB %in% RoME::GSAs$GSA)){
    write(paste("Warning: the AREA code used in TB file is not present in the GFCM's GSA list"), file = Errors, append = TRUE)
  }

  #######################

  GSA_TC=unique(DataTC$AREA)

  if (any(is.na(GSA_TC))) {
    GSA_TC <- GSA_TC[!is.na(GSA_TC)]
    write(paste("NA value in AREA field of TC TCble"), file = Errors, append = TRUE)
    numberError = numberError +1
  }

  if (!(GSA_TC %in% RoME::GSAs$GSA)){
    write(paste("Warning: the AREA code used in TC file is not present in the GFCM's GSA list"), file = Errors, append = TRUE)
  }

  if (length(GSA_TC)>1) {
    write(paste("AREA field in TC table contains data from more than 1 GSA. The checks are performed only on data referred to a single GSA and further analysis will be conducted only on the first GSA included in data"), file = Errors, append = TRUE)
    numberError = numberError +1
  }

  #######################

  if (length(DataTE)>1 & any(!is.na(DataTE))) {
  DataTE <- DataTE[DataTE$YEAR == year, ]
  GSA_TE=unique(DataTE$AREA)

  if (any(is.na(GSA_TE))) {
    GSA_TE <- GSA_TE[!is.na(GSA_TE)]
    write(paste("NA value in AREA field of TE TEble"), file = Errors, append = TRUE)
    numberError = numberError +1
  }

  if (length(GSA_TE)>1) {
    write(paste("AREA field in TE table contains data from more than 1 GSA. The checks are performed only on data referred to a single GSA and further analysis will be conducted only on the first GSA included in data"), file = Errors, append = TRUE)
    numberError = numberError +1
  }

  if (!(GSA_TE %in% RoME::GSAs$GSA)){
    write(paste("Warning: the AREA code used in TE file is not present in the GFCM's GSA list"), file = Errors, append = TRUE)
  }
  }

  ########################

  if (length(DataTL)>1 & any(!is.na(DataTL))) {
  DataTL <- DataTL[DataTL$YEAR == year, ]
  GSA_TL=unique(DataTL$AREA)

  if (any(is.na(GSA_TL))) {
    GSA_TL <- GSA_TL[!is.na(GSA_TL)]
    write(paste("NA value in AREA field of TL TLble"), file = Errors, append = TRUE)
    numberError = numberError +1
  }

  if (length(GSA_TL)>1) {
    write(paste("AREA field in TL table contains data from more than 1 GSA. The checks are performed only on data referred to a single GSA and further analysis will be conducted only on the first GSA included in data"), file = Errors, append = TRUE)
    numberError = numberError +1
  }

  if (!(GSA_TL %in% RoME::GSAs$GSA)){
    write(paste("Warning: the AREA code used in TL file is not present in the GFCM's GSA list"), file = Errors, append = TRUE)
  }
  }

  ###########

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
  #   if (file.exists(file.path(tempdir(), "Logfiles"))){
  # unlink(file.path(tempdir(),"Logfiles"),recursive=T)
  # }

  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }
}
