############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files)                                #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.it #
#   January 2022                                                                                                           #
############################################################################################################################
#  Check if all the hauls in TL are in TA

check_hauls_TLTA<-function(DataTA,DataTL,year,wd,suffix){

  if (FALSE){
    library(RoME)
    wd <- tempdir()
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
    DataTL = RoME::TL # read.table("~/GitHub/RoME/data/TL_GSA18 2012-2018.csv", sep=";", header=T)
    DataTA = RoME::TA # read.csv("~/GitHub/RoME/data/TA_GSA18_1994-2018.csv", sep=";")
    year=2008
    # check_hauls_TLTA(DataTA,DataTL,year,wd,suffix)
  }

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
    stop(paste0("'",arg,"' argument should be provided"))
  } else if (length(year)!= 1) {
    stop(paste0("only one value should be provided for '",arg,"' argument"))
  } else if (is.na(year)){
    stop(paste0(arg," argument should be a numeric value"))
  }
  DataTA <- DataTA[DataTA$YEAR == year, ]
  DataTL <- DataTL[DataTL$YEAR == year, ]
  ########################################

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

  ResultTL = DataTL
  write(paste("\n----------- check presence in TA of TL hauls - ", ResultTL$YEAR[1]), file = Errors, append = TRUE)
  ResultTA = DataTA

  ResultTL=unique(ResultTL$HAUL_NUMBER)
  if (length(ResultTL)!=0){
    for (j in 1:length(ResultTL)){

      ResultTA_temp=ResultTA[which(ResultTA$HAUL_NUMBER==ResultTL[j]),]
      if (nrow(ResultTA_temp)==0)   {
        write(paste("No haul",ResultTL[j],"in TA"), file = Errors, append = TRUE)
        numberError = numberError +1
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
