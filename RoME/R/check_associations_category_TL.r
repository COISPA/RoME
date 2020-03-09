############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.eu #
#   March 2020                                                                                                             #
############################################################################################################################
# Check corretness of association between category and sub-category in TL consistent according to INSTRUCTION MANUAL VERSION 9
# MEDITS 2017

check_associations_category_TL<-function(DataTL,assTL, wd, suffix){

  if (FALSE){
    library(RoME)
    wd <- tempdir()

    # assTL <- read.csv("D:\\Documents and Settings\\Utente\\Documenti\\GitHub\\RoME\\RoME versione aperta 1.4\\Tables\\Associations_cat_TL.csv", sep=";")
    # str(assTL)
    # colnames(assTL) <- c("LITTER_CATEGORY","LITTER_SUB-CATEGORY")
    # assTL$LITTER_CATEGORY <- as.character(assTL$LITTER_CATEGORY)
    # assTL[ ,"LITTER_SUB-CATEGORY"]<- as.character(assTL[ ,"LITTER_SUB-CATEGORY"])
    # str(assTL)
    # save(assTL, file="data/assTL.rda")

    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
    DataTL = read.table("D:\\Documents and Settings\\Utente\\Documenti\\__ DATI MEDITS AGGIORNATI __\\BKP\\GSA18 - 2018\\NUOVI\\2018 completo TL.csv", sep=";", header=T)
    # colnames(DataTL)[which(colnames(DataTL)=="LITTER_SUB.CATEGORY")] <- "LITTER_SUB-CATEGORY"
    # check_associations_category_TL(DataTL, assTL, wd, suffix)
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


  ResultData = DataTL
  write(paste(" ----------- check consistency of category/subcategory codes in ",ResultData$TYPE_OF_FILE[1],"-", ResultData$YEAR[1]), file = Errors, append = TRUE)

  if (nrow(ResultData)!=0){

    i=1
  for (i in 1:nrow(ResultData)){

    ass_allowed_TL = assTL[assTL$LITTER_CATEGORY==ResultData$LITTER_CATEGORY[i],"LITTER_SUB-CATEGORY"]

    if (!(as.character(ResultData[i,"LITTER_SUB-CATEGORY"]) %in% ass_allowed_TL)){
      numberError=numberError+1
      write(paste(ResultData$YEAR[i], " " ,ResultData$HAUL_NUMBER[i], ": Association between category and sub-category not allowed."), file = Errors, append = TRUE)
    }


  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  }

  }
  }

    return(TRUE)
}
