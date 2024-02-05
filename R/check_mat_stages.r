############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files)                                #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.it #
#   January 2022                                                                                                           #
############################################################################################################################
# Check if maturity stages in TC are consistent according to INSTRUCTION MANUAL VERSION 9 MEDITS 2017


if (FALSE){
  # library(RoME)
  ## Dal laptop
  # Data = read.table("D:\\COISPA\\RoME_RDBFIS\\data\\TC GSA18 2017-2020.csv", sep=";",header=TRUE)
  # wd <- "D:\\COISPA\\RoME_RDBFIS\\check_RoME\\Logfiles"

  # dal fisso
  # Data = read.table("D:\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\RDB\\RoME_RDBFIS\\data\\TC GSA18 2017-2020.csv", sep=";",header=TRUE)

  Data = tc # read.table("D:\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\RDB\\RoME_RDBFIS\\data\\TE GSA18 2017-2020.csv", sep=";",header=TRUE)
  Data[102,"MATURITY"] <- "Ae"
  wd <- "D:\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\RDB\\RoME_RDBFIS\\check_RoME\\Logfiles"
  # stages <- read.csv("D:/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/RDB/RoME_RDBFIS/data/TC_TE_maturity.csv", sep=";")


  suffix= "pippo"
  year=2015

    # DataTargetSpecies=RoME::DataTargetSpecies
    # DataSpecies=RoME::TM_list
  check_mat_stages(Data, year, wd, suffix, stages)
}



check_mat_stages<-function(Data, year, wd, suffix,stages=RoME::mat_stages){


  Format="from_2012"

  if (!file.exists(file.path(wd, "Logfiles"))){
    dir.create(file.path(wd, "Logfiles"), recursive = TRUE, showWarnings = FALSE)
  }
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
  }
  numberError = 0
  numberWarning = 0
  Errors <- file.path(wd,"Logfiles",paste("Logfile_",suffix,".dat",sep=""))
  if (!file.exists(Errors)){
    file.create(Errors)
  }

  ### FILTERING DATA FOR THE SELECTED YEAR
  arg <- "year"
  if (!exists(arg)) {
    stop(paste0("'", arg, "' argument should be provided"))
  } else if (length(year) != 1) {
    stop(paste0("only one value should be provided for '", arg, "' argument"))
  } else if (is.na(year)) {
    stop(paste0(arg, " argument should be a numeric value"))
  }
  Data <- Data[Data$YEAR == year, ]
  ########################################

  ResultData = Data
  write(paste(" ----------- check consistency of maturity stages in ",ResultData$TYPE_OF_FILE[1],"-", ResultData$YEAR[1]), file = Errors, append = TRUE)

  if ((as.character(ResultData$TYPE_OF_FILE[1])=="TC"))    {
      type_of_file <- "TC"
  } else if ((as.character(ResultData$TYPE_OF_FILE[1])=="TE")){
      type_of_file <- "TE"
  }

  ResultData <- ResultData[ResultData$FAUNISTIC_CATEGORY %in% c("Ao","Ae","B","C","Bst"), ]
  ResultData$MATURITY <- as.character(ResultData$MATURITY)
  ResultData$MATSUB <- as.character(ResultData$MATSUB)
  ResultData[is.na(ResultData$MATSUB) | ResultData$MATSUB =="","MATSUB"] <- "NA"
  ResultData$code <- paste(ResultData$FAUNISTIC_CATEGORY,ResultData$SEX,ResultData$MATURITY,ResultData$MATSUB,sep="_")

  stages$MATURITY <- as.character(stages$MATURITY)
  stages$MATSUB <- as.character(stages$MATSUB)
  stages[(is.na(stages$MATSUB) | stages$MATSUB ==""),"MATSUB" ] <- "NA"
  stages$code <- paste(stages$FAUNISTIC_CATEGORY, stages$SEX, stages$MATURITY, stages$MATSUB , sep="_")

  if (type_of_file=="TE"){
      stages=stages[stages$TYPE_OF_FILE =="TE",]                                       # for TE the maturity stage ND are not allowed
  } else if(type_of_file=="TC") {
      stages=stages[stages$TYPE_OF_FILE =="TC",]
  }

  ResultData <- ResultData[which(!(ResultData$code %in% stages$code)), ]

  if (nrow(ResultData)!=0){

    not_in <- unique(ResultData$code)
    not_in <- strsplit(not_in,"_")
    not_in <-as.data.frame(do.call(rbind, not_in))
    colnames(not_in) <- c("FAUNISTIC_CATEGORY","SEX","MATURITY","MATSUB")

    i=1
    for (i in 1:nrow(not_in)){
      write(paste("SEX, MATURITY and MATSUB combination (",not_in$SEX[i] , not_in$MATURITY[i],not_in$MATSUB[i],") inconsistent for",not_in$FAUNISTIC_CATEGORY[i],"FAUNISTIC_CATEGORY according to MEDITS INSTRUCTIONS MANUAL in",type_of_file), file = Errors, append = TRUE)
      numberWarning <- numberWarning + 1
      }
  }            # (nrow(ResultData)!=0)

  if (numberWarning ==0) {
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

