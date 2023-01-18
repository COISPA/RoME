############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files)                                #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.it #
#   January 2022                                                                                                           #
############################################################################################################################
# Check corretness of association between category and sub-category in TL consistent according to INSTRUCTION MANUAL VERSION 9
# MEDITS 2017

check_associations_category_TL<-function(DataTL,assTL, year, wd, suffix){

  if (FALSE){
    library(RoME)
    wd <- temdir() #"C:\\Users\\walte\\Documents\\GitHub\\RoME\\data TEST Neglia" # tempdir()

    # assTL <- read.csv("D:\\Documents and Settings\\Utente\\Documenti\\GitHub\\RoME\\RoME versione aperta 1.4\\Tables\\Associations_cat_TL.csv", sep=";")
    # str(assTL)
    # colnames(assTL) <- c("LITTER_CATEGORY","LITTER_SUB-CATEGORY")
    # assTL$LITTER_CATEGORY <- as.character(assTL$LITTER_CATEGORY)
    # assTL[ ,"LITTER_SUB-CATEGORY"]<- as.character(assTL[ ,"LITTER_SUB-CATEGORY"])
    # str(assTL)
    # save(assTL, file="data/assTL.rda")

    suffix= NA # paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
    DataTL = RoME::TL# read.table(file=paste(wd,"\\2019 GSA18 TL.csv",sep=""), sep=";", header=T) #read.table("D:\\Documents and Settings\\Utente\\Documenti\\__ DATI MEDITS AGGIORNATI __\\BKP\\GSA18 - 2018\\NUOVI\\2018 completo TL.csv", sep=";", header=T)
    # check_associations_category_TL(DataTL, assTL, year=2012, wd, suffix)
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

  DataTL <- DataTL[DataTL$YEAR == year, ]
  ########################################

  ResultData = DataTL
  write(paste("\n----------- check consistency of category/subcategory codes in ",ResultData$TYPE_OF_FILE[1],"-", ResultData$YEAR[1]), file = Errors, append = TRUE)

  if (nrow(ResultData)!=0){

    i=1
  for (i in 1:nrow(ResultData)){

    ass_allowed_TL = assTL[assTL$LITTER_CATEGORY==ResultData$LITTER_CATEGORY[i],"LITTER_SUB-CATEGORY"]

    if (!(as.character(ResultData[i,"LITTER_SUB-CATEGORY"]) %in% ass_allowed_TL)){
      write(paste(ResultData$YEAR[i], " " ,ResultData$HAUL_NUMBER[i], ": Association between category and sub-category not allowed."), file = Errors, append = TRUE)
      numberError=numberError+1
    }
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
    return(TRUE)

 # unlink(file.path(tempdir(),"Graphs"),recursive=T)
  #unlink(file.path(tempdir(),"files R-Sufi"),recursive=T)

  }
