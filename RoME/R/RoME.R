####################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)  #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it
#   August 2020
####################################################################################################################

# TEST INIZIALIZING --------------------------------------------------------
if (FALSE) {
  library(RoME)
  wd <-  "C:\\Users\\walte\\Documents\\GitHub\\RoME\\data TEST Neglia" # tempdir()
  suffix=NA
  TA <-  read.table(file=paste(wd, "\\2019 GSA18 TA.csv",sep=""), sep=";", header=T) # RoME::TA
  TB <-  read.table(file=paste(wd, "\\2019 GSA18 TB.csv",sep=""), sep=";", header=T) # RoME::TB
  TC <-  read.table(file=paste(wd, "\\2019 GSA18 TC.csv",sep=""), sep=";", header=T) # RoME::TC
  TE <-  NA # RoME::TE
  TL <-  read.table(file=paste(wd,"\\2019 GSA18 TL.csv",sep=""), sep=";", header=T) # RoME::TL
  verbose = TRUE
  create_RSufi_files=TRUE
  create_global_RSufi_files=TRUE
  Year_start=2007
  Year_end=2016

TB$TYPE_OF_FILE <- as.character(TB$TYPE_OF_FILE)
TB$TYPE_OF_FILE[1] <- "TC"

  # RoME(TA=RoME::TA,TB=RoME::TB,TC=RoME::TC,TE=RoME::TE,TL=RoME::TL,wd=tempdir(),suffix=NA,create_RSufi_files=TRUE,create_global_RSufi_files=TRUE,Year_start=2007,Year_end=2016, verbose=TRUE)
}
# TEST END --------------------------------------------------------


RoME <- function(TA,TB,TC,TE=NA,TL=NA,wd,suffix=NA,create_RSufi_files=FALSE,create_global_RSufi_files=FALSE,Year_start=NA,Year_end=NA, verbose=TRUE,Stratification=MEDITS::stratification_scheme, TM_list=TM_list,DataTargetSpecies=DataTargetSpecies,Maturity_parameters=Maturity_parameters,stages=RoME::mat_stages,assTL=assTL){

  stringsAsFactors=FALSE
  Format <- "from_2012"

  if (!file.exists(file.path(wd, "Logfiles"))){
    dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
  }
  if (!file.exists(file.path(wd,"Graphs"))){
    dir.create(file.path(wd, "Graphs"), showWarnings = FALSE)
  }
  if (!file.exists(file.path(wd,"/files R-Sufi",sep="/"))){
    dir.create(file.path(wd, "files R-Sufi"), showWarnings = FALSE)
  }

  if (is.na(suffix)){
    suffix <- paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  }
  Errors <- file.path(wd,"Logfiles",paste("Logfile_",suffix,".dat",sep=""))
  write(paste("\n ",date(),sep=""), file = Errors, append = TRUE)


  stop_ = FALSE
  check_without_errors = TRUE

# START -------------------------------------------------------------------

  write(paste("-------------------------------------------------------------\n
              LIST OF ERRORS
              \n-------------------------------------------------------------"), file = Errors, append = TRUE)



  years = unique (TA$YEAR)

# check degli header ------------------------------------------------------

  checkHeader(TA,"TA")
  checkHeader(TB,"TB")
  checkHeader(TC,"TC")

  ResultDataTA_bkp <- TA
  ResultDataTB_bkp <- TB
  ResultDataTC_bkp <- TC


###  TE  ###
  if (!(all(is.na(TE)) & length(TE)==1))
  {
  checkHeader(TE,"TE")
  ResultDataTE_bkp <- TE
  }

###  TL  ###
  if (!(all(is.na(TL)) & length(TL)==1))
  {
    checkHeader(TL,"TL")
    ResultDataTL_bkp <- TL
  }
###

    ###########

  ### CHECK YEAR

checkName = "Check YEAR"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName, "in progress..."), quote = FALSE)}
   check_without_errors = check_year(TA, TB, TC, TE, TL, years, wd, Errors)
}
 if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}
  ### CHECK TYPE_OF_FILE

  checkName = "Check TYPE_OF_FILE"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName, "in progress..."), quote = FALSE)}
   check_without_errors = check_type(TA, TB, TC, TE, TL, years, wd, Errors)
}
 if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}



## CICLO PER ANNO ##

yea <- 2019
for (yea in years) {

if (check_without_errors == TRUE & verbose==TRUE) {
print(paste("Checking year ",yea ),quote=F)
}

#------------------

      ResultDataTA <-ResultDataTA_bkp[ResultDataTA_bkp$YEAR == yea,]
      ResultDataTB <-ResultDataTB_bkp[ResultDataTB_bkp$YEAR == yea,]
      ResultDataTC <-ResultDataTC_bkp[ResultDataTC_bkp$YEAR == yea,]

      if (!(all(is.na(TE)) & length(TE)==1))
      {
        ResultDataTE <- ResultDataTE_bkp[ResultDataTE_bkp$YEAR==yea,]
      } else {
        ResultDataTE <- NA
	    }

      if (!(all(is.na(TL)) & length(TL)==1))
      {
        ResultDataTL <- ResultDataTL_bkp[ResultDataTL_bkp$YEAR==yea,]
      } else {
        ResultDataTL <- NA
      }

# --------------------------------
# Check identical records

checkName = "Check identical record TA"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_identical_records(Data=ResultDataTA, wd, suffix)
}
if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName= "Check identical record TB"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_identical_records(Data=ResultDataTB,wd,suffix)
}
if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName = "Check identical record TC"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_identical_records(Data=ResultDataTC,wd,suffix)
}
if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

if (!(all(is.na(TE)) & length(TE)==1))
{
  if (nrow(ResultDataTE)>0){
  checkName = "Check identical record TE"
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
    check_without_errors = check_identical_records(Data=ResultDataTE,wd,suffix)
  }
  if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}
  }
}

if (!(all(is.na(TL)) & length(TL)==1))
{
  if (nrow(ResultDataTL)>0){
  checkName = "Check identical record TL"
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
    check_without_errors = check_identical_records(Data=ResultDataTL,wd,suffix)
  }
  if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}
  }
}


# --------------------------------
# Check quasi-identical record

checkName = "Check quasi-identical record in TA"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName, "in progress..."), quote = FALSE)}
   check_without_errors = check_quasiidentical_records(ResultDataTA,wd,suffix)
}
if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}


checkName = "Check quasi-identical record in TB"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
   check_without_errors = check_quasiidentical_records(ResultDataTB,wd,suffix)
}
if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}


checkName = "Check quasi-identical record in TC"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_quasiidentical_records(ResultDataTC,wd,suffix)
}
if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}


if (!(all(is.na(TE)) & length(TE)==1))
{
  if (nrow(ResultDataTE)>0){
    checkName = "Check quasi-identical record in TE"
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
      check_without_errors = check_quasiidentical_records(ResultDataTE,wd,suffix)
    }
    if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}
      }
}

if (!(all(is.na(TL)) & length(TL)==1))
{
  if (nrow(ResultDataTL)>0){
    checkName = "Check quasi-identical record in TL"
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
      check_without_errors = check_quasiidentical_records(ResultDataTL,wd,suffix)
    }
    if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}
  }
}


# --------------------------------
# Check consistency of area

checkName = "Check consistency of area TA, TB, TC, TE, TL"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_area(TA, TB, TC, TE, TL, wd, suffix)
}
if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}


#--------------------------------------------------
# Dictionary checks
#--------------------------------------------------

#TA

checkName = "Check dictionary for field:"
Field = "COUNTRY"
Values = as.character(unique(Stratification$COUNTRY))
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, wd, suffix)
}
if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}


Field = "DAY"
Values = seq(1,31,1)
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, wd, suffix)
}
if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}


Field = "MONTH"
Values = seq(1,12,1)
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, wd, suffix)
}
if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}

Field = "VALIDITY"
Values = c("V","I")
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, wd, suffix)
}
if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}

Field = "COURSE"
Values = c("R","N")
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, wd, suffix)
}
if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}

Field = "CODEND_CLOSING"
Values = c("S","C")
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, wd, suffix)
}
if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}


Field = "PART_OF_THE_CODEND"
Values = c("A","M","P","S")
if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
    check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, wd, suffix)
}
if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}

Field = "GEOMETRICAL_PRECISION"
Values = c("M","E")
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, wd, suffix)
}
if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}

Field = "GEAR"
Values = c("GOC73")
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, wd, suffix)
}
if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}

Field = "RIGGING"
Values = c("GC73")
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, wd, suffix)
}
if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}

Field = "DOORS"
Values = c("WHS8")
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, wd, suffix)
}
if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}


Field = "SHOOTING_QUADRANT"
Values = c("1", "3", "5", "7")
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, wd, suffix)
}
if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}

Field = "SHOOTING_TIME"
Values = seq(0,2400,1)
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, wd, suffix)
}
if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}

Field = "SHOOTING_DEPTH"
Values = c(0,10:800)
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, wd, suffix)
}
if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}

Field = "HAULING_QUADRANT"
Values = c("1", "3", "5", "7")
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, wd, suffix)
}
if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}

Field = "HAULING_TIME"
Values = seq(0,2400,1)
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, wd, suffix)
}
if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}

Field = "HAULING_DEPTH"
Values = c(0,10:800)
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, wd, suffix)
}
if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}

  Field = "MEASURING_SYSTEM"
  Values = c("VA","SO","XA","SA","SI","CT","SB")
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
    check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, wd, suffix)
  }
  if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}

  Field = "RECORDED_SPECIES"
  Values = seq(0,4,1)
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
    check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, wd, suffix)
  }
  if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}

  Field = "WARP_LENGTH"
  Values = seq(100,2200,1)
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
    check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, wd, suffix)
  }
  if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}

  Field = "WARP_DIAMETER"
  Values = seq(10,30,1)
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
    check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, wd, suffix)
  }
  if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}

  Field = "OBSERVATIONS"
  Values = seq(0,9,1)
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
    check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, wd, suffix)
  }
  if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}

#TB
  Field = "COUNTRY"
  Values = as.character(unique(Stratification$COUNTRY))
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
    check_without_errors = check_dictionary(ResultData = ResultDataTB, Field, Values, wd, suffix)
  }
  if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}


  Field = "DAY"
  Values = seq(1,31,1)
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
    check_without_errors = check_dictionary(ResultData = ResultDataTB, Field, Values, wd, suffix)
  }
  if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}


  Field = "MONTH"
  Values = seq(1,12,1)
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
    check_without_errors = check_dictionary(ResultData = ResultDataTB, Field, Values, wd, suffix)
  }
  if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}


  # Field = "YEAR"
  # Values = seq(1900,2100,1)
  # if (check_without_errors == TRUE) {
  #   if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  #   check_without_errors = check_dictionary(ResultData = ResultDataTB, Field, Values, wd, suffix)
  # }
  # if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}


Field = "CODEND_CLOSING"
Values = c("S","C")
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTB, Field, Values, wd, suffix)
}
if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}

Field = "PART_OF_THE_CODEND"
Values = c("A", "M", "P", "S")
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTB, Field, Values, wd, suffix)
}
if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}




#TC
Field = "COUNTRY"
Values = as.character(unique(Stratification$COUNTRY))
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTC, Field, Values, wd, suffix)
}
if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}


Field = "DAY"
Values = seq(1,31,1)
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTC, Field, Values, wd, suffix)
}
if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}


Field = "MONTH"
Values = seq(1,12,1)
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTC, Field, Values, wd, suffix)
}
if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}


# Field = "YEAR"
# Values = seq(1900,2100,1)
# if (check_without_errors == TRUE) {
#   if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
#   check_without_errors = check_dictionary(ResultData = ResultDataTC, Field, Values, wd, suffix)
# }
# if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}


Field = "CODEND_CLOSING"
Values = c("S","C")
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTC, Field, Values, wd, suffix)
}
if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}


Field = "PART_OF_THE_CODEND"
Values = c("A", "M", "P", "S")
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTC, Field, Values, wd, suffix)
}
if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}


Field = "SEX"
Values = c("M", "F", "I", "N")
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTC, Field, Values, wd, suffix)
}
if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}


  Field = "LENGTH_CLASSES_CODE"
  Values = c("0","m")
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
    check_without_errors = check_dictionary(ResultData = ResultDataTC, Field, Values, wd, suffix)
  }
  if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}

#TE

  if (!(all(is.na(TE)) & length(TE)==1)){
  if (nrow(ResultDataTE)>0){
    Field = "COUNTRY"
    Values = as.character(unique(Stratification$COUNTRY))
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_dictionary(ResultData = ResultDataTE, Field, Values, wd, suffix)
    }
    if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}



    Field = "DAY"
    Values = seq(1,31,1)
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_dictionary(ResultData = ResultDataTE, Field, Values, wd, suffix)
    }
    if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}


    Field = "MONTH"
    Values = seq(1,12,1)
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_dictionary(ResultData = ResultDataTE, Field, Values, wd, suffix)
    }
    if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}


    # Field = "YEAR"
    # Values = seq(1900,2100,1)
    # if (check_without_errors == TRUE) {
    #   if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
    #   check_without_errors = check_dictionary(ResultData = ResultDataTE, Field, Values, wd, suffix)
    # }
    # if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}


    Field = "LENGTH_CLASSES_CODE"
    Values = c("0","m")
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_dictionary(ResultData = ResultDataTE, Field, Values, wd, suffix)
    }
    if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}

    # ResultDataTE[is.na(ResultDataTE$LENGTH_CLASSES_CODE),]

    Field = "SEX"
    Values = c("M", "F", "I", "N")
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_dictionary(ResultData = ResultDataTE, Field, Values, wd, suffix)
    }
    if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}
  }
  }

  # TL
  if (!(all(is.na(TL)) & length(TL)==1)){
    if (nrow(ResultDataTL)>0){
      Field = "COUNTRY"
      Values = as.character(unique(Stratification$COUNTRY))
      if (check_without_errors == TRUE) {
        if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
        check_without_errors = check_dictionary(ResultData = ResultDataTL, Field, Values, wd, suffix)
      }
      if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}



      Field = "DAY"
      Values = seq(1,31,1)
      if (check_without_errors == TRUE) {
        if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
        check_without_errors = check_dictionary(ResultData = ResultDataTL, Field, Values, wd, suffix)
      }
      if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}


      Field = "MONTH"
      Values = seq(1,12,1)
      if (check_without_errors == TRUE) {
        if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
        check_without_errors = check_dictionary(ResultData = ResultDataTL, Field, Values, wd, suffix)
      }
      if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}


      # Field = "YEAR"
      # Values = seq(1900,2100,1)
      # if (check_without_errors == TRUE) {
      #   if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      #   check_without_errors = check_dictionary(ResultData = ResultDataTL, Field, Values, wd, suffix)
      # }
      # if(verbose){stop_ = printError(paste(checkName,Field),check_without_errors, stop_)}
    }
    }

# End dictionary checks
#-------------------------------------

#-------------------------------------
# Check no empty fields

checkName = "Check no empty fields TA"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_no_empty_fields(ResultDataTA, wd, suffix)
}
  if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName =  "Check no empty fields TB"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_no_empty_fields(ResultDataTB, wd, suffix)
}
if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName = "Check no empty fields TC"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_no_empty_fields(ResultDataTC, wd, suffix)
}
if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}


if (!(all(is.na(TE)) & length(TE)==1)) {
   if (nrow(ResultDataTE)>0){
  checkName = "Check no empty fields TE"
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
    check_without_errors = check_no_empty_fields(ResultDataTE, wd, suffix)
  }
  if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}
  }
}

if (!(all(is.na(TL)) & length(TL)==1)) {
  if (nrow(ResultDataTL)>0){
    checkName = "Check no empty fields TL"
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
      check_without_errors = check_no_empty_fields(ResultDataTL, wd, suffix)
    }
    if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}
  }
}

#-------------------------------------
# checks on TA

checkName = "Check 0 fields TA"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_0_fieldsTA(ResultDataTA,wd,suffix)
}
if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName = "Check dm TA"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_dm(ResultDataTA,wd,suffix)
}
if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName = "Check consistency between duration and time TA"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_consistencyTA_duration(ResultDataTA, wd, suffix)
}
if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName = "Check consistency between distance and duration of the haul TA"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_consistencyTA_distance(ResultDataTA, wd, suffix)
}
if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName = "Check consistency of the hauls coordinates with the distance (difference not greater than 30%)"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_distance(ResultDataTA, wd, suffix)
}
if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName = "Check consistency of bridles length TA"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_bridles_length(ResultDataTA, wd, suffix)
}
if(verbose){stop_ =printError(checkName,check_without_errors, stop_)}

checkName = "Check difference between start depth and end depth (not greater than 20%) in TA"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_depth(ResultDataTA, wd, suffix)
}
if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName = "Check start depth and end depth in the same stratum TA"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_stratum(ResultDataTA,wd,suffix)
}
if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName = "Check consistency of stratum code in TA"
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
    check_without_errors = check_stratum_code(ResultDataTA,Stratification=MEDITS::stratification_scheme,wd,suffix)
  }
if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName = "Check start quadrant and end quadrant TA"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_quadrant(ResultDataTA,wd,suffix)
}
if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName = "Check uniqueness of valid hauls TA"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_unique_valid_haul(ResultDataTA,wd,suffix)
}
if(verbose){stop_ =printError(checkName,check_without_errors, stop_)}

checkName = "Visual check of the haul positions"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_position(ResultDataTA,wd,suffix)
}
if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName = "Relation between shooting depth and warp length, and between warp length and wing opening"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  graphs_TA(ResultDataTA,wd,suffix)
}
if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName = "Check if the coordinates are in the Mediterranean Sea"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_position_in_Med(ResultDataTA,wd,suffix)
}
if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName = "Check on temperature by haul"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_temperature(ResultDataTA,wd,suffix)
}
if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}



# checks on TB

checkName = "Check correctness of species codes TB"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_rubincode(ResultDataTB,TM_list=TM_list,wd,suffix)
}
  if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName = "Check consistency of NB_TOTAL and number per sex TB"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_nbtotTB(ResultDataTB,wd,suffix)
}
  if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName = "Check presence of NB_TOTAL and number per sex TB for species G1"
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
    check_without_errors = check_nm_TB(ResultDataTB, ResultDataTC,wd,suffix)
  }
  if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName = "Check consistency between not null weight and not null total number"
if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
    check_without_errors = check_weight_tot_nb(ResultDataTB,wd,suffix)
  }
  if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName = "Check consistency of weight and number TB"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_weight(ResultDataTB,DataTargetSpecies=RoME::DataTargetSpecies,wd,suffix)
}
  if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

# checks on TC

graphics.off()

checkName = "Check correctness of species codes TC"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_rubincode(ResultDataTC,TM_list=TM_list,wd,suffix)
}
if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}


checkName = "Check correctness of LENGTH_CLASSES_CODE TC"
if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
      check_without_errors = check_length_class_codeTC(ResultDataTC,Specieslist=TM_list, wd,suffix)
  }
  if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName = "Check consistency of length classes TC"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_length(ResultDataTC,DataSpecies=NA,wd,suffix)
}
  if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName = "Check correctness of number per sex in TC"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_nb_per_sexTC(ResultDataTC,wd,suffix)
}
  if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName = "Check consistency of maturity stages TC"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_mat_stages(ResultDataTC, wd, suffix, DataTargetSpecies=DataTargetSpecies,DataSpecies=TM_list,stages=stages)
}
  if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName = "Check sub-sampling"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_subsampling(ResultDataTC,wd,suffix)
}
  if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName = "Check consistency of maturity stages TC by the comparison with the length of smallest mature individuals reported in bibliography"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_smallest_mature(ResultDataTC,Maturity_parameters=Maturity_parameters,DataTargetSpecies=DataTargetSpecies,wd,suffix)
}
  if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName = "Check consistency of sex TC by means of spawning period"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_spawning_period(ResultDataTA,ResultDataTC,Maturity_parameters=Maturity_parameters,DataTargetSpecies=DataTargetSpecies,wd,suffix)
}
  if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName = "Check consistency of sex data TC by means of sex-inversion size"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_sex_inversion(ResultDataTC,Maturity_parameters,wd,suffix)
}
  if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName = "Check consistency of length distribution TC"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_step_length_distr(ResultDataTC,wd,suffix)
}
  if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName = "Check total weight in the haul in TC"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_individual_weightTC(ResultDataTC,LW=NA,wd,suffix, verbose=FALSE)
}
  if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName = "Check correctness of species codes TC"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_rubincode(ResultDataTC,TM_list=TM_list,wd,suffix)
}
  if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName = "Check presence of lengths for G1 and G2 Medits species in TC"
if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
    check_without_errors = check_G1_G2(ResultDataTC, wd, suffix)
  }
    if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

# cross checks

checkName = "Check presence in TB of TA hauls"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_hauls_TATB(ResultDataTA,ResultDataTB,wd,suffix)
}
if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName = "Check presence in TA of TB hauls"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_hauls_TBTA(ResultDataTA,ResultDataTB,wd,suffix)
}
  if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName = "Check presence in TC of TB target species"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_species_TBTC(ResultDataTB,ResultDataTC,DataSpecies=DataTargetSpecies,wd,suffix)
}
  if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName = "Check presence in TB of TC species"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_haul_species_TCTB(ResultDataTB,ResultDataTC,wd, suffix)
}
if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName = "Check correctness of the number per sex in TB in case of sub-sampling in TC  "
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_raising(ResultDataTB,ResultDataTC,wd,suffix)
}
  if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}


checkName = "Check on date by haul TB"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_date_haul(ResultDataTA, ResultDataTB, wd, suffix)
}
if(verbose){stop_ =printError(checkName,check_without_errors, stop_)}

checkName = "Check on date by haul TC"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_date_haul(ResultDataTA, ResultDataTC, wd, suffix)
}
if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}


#Check on TE

if (!(all(is.na(TE)) & length(TE)==1)) {
  if (nrow(ResultDataTE)>0){
  checkName = "Check on date by haul TE"
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
    check_without_errors = check_date_haul(ResultDataTA,ResultDataTE,wd,suffix)
  }
    if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}



  checkName = "Check consistency of maturity stages TE"
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
    check_without_errors = check_mat_stages(ResultDataTE, wd, suffix, DataTargetSpecies=DataTargetSpecies,DataSpecies=TM_list,stages=stages)
  }
  if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}



  checkName = "Cross check on number between TC and TE"
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
    check_without_errors = check_TE_TC(ResultDataTC,ResultDataTE,wd,suffix)
  }
    if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}


  checkName = "Check consistency of maturity stages TE by the comparison with the length of smallest mature individuals reported in bibliography"
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
    check_without_errors = check_smallest_mature(ResultDataTE,Maturity_parameters=Maturity_parameters,DataTargetSpecies=DataTargetSpecies,wd,suffix)
  }
    if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}



  # questo check è nella sezione dei check dei TE ma sembra essere solo per il TC

  checkName = "Check consistency of maturity stages in TE by means of spawning period"
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
    check_without_errors = check_spawning_period(ResultDataTA,ResultDataTE,Maturity_parameters=Maturity_parameters,DataTargetSpecies=DataTargetSpecies,wd,suffix)
  }
    if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}


  checkName = "Check individual weight in TE"
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
    check_without_errors = check_individual_weightTE(ResultDataTE,LW=NA,wd,suffix,verbose=FALSE)
  }
  if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

  checkName = "Check correctness of species codes TE"
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
    check_without_errors = check_rubincode(ResultDataTE,TM_list=TM_list,wd,suffix)
  }
  if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

  checkName = "Check consistency TE check-fields"
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
    check_without_errors = check_nb_TE(ResultDataTE,wd,suffix)
  }
  if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

  checkName = "Summary individual data sampling"
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
    check_without_errors = scheme_individual_data(DataTC=ResultDataTC,DataTE=ResultDataTE,wd,suffix)
  }
  if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

    }
  }



# Checks on TL
if (!(all(is.na(TL)) & length(TL)==1)) {
  if (nrow(ResultDataTL)>0){


checkName = "Check allowed values for category on Litter data"
Field = "LITTER_CATEGORY"
Values = c("L0","L1","L2","L3","L4","L5","L6","L7","L8")
if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
      check_without_errors = check_dictionary(ResultData=ResultDataTL,Field, Values, wd, suffix)
    }
      if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName = "Check allowed values for sub-category on Litter data"
Field = "LITTER_SUB-CATEGORY"
Values = c("0","A","B","C","D","E","F","G","H","I","J")
colnames(ResultDataTL)[10]="LITTER_SUB-CATEGORY"

if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
      check_without_errors = check_dictionary(ResultData=ResultDataTL,Field, Values, wd, suffix)
    }
    if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName = "Check correctness of associations between category and sub-category on Litter data"
if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
      check_without_errors = check_associations_category_TL(ResultDataTL,assTL, wd, suffix)
    }
    if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName = "Check if the number is always filled in on Litter data"
if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
      check_without_errors = check_no_empty_fields(ResultDataTL,wd,suffix)
    }
    if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName = "Check if the hauls in TL are present in TA"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_hauls_TLTA(ResultDataTA,ResultDataTL, wd, suffix)
}
if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName = "Check if the hauls in TA are present in TL"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_hauls_TATL(ResultDataTA,ResultDataTL,wd,suffix)
}
if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

checkName = "Check if the date in TL is consistent with TA"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
    check_without_errors = check_date_haul(ResultDataTA, ResultDataTL, wd, suffix)
  }
  if(verbose){stop_ = printError(checkName,check_without_errors, stop_)}

   }
}


if (!stop_) {
  if(verbose){print("All the checks have been performed!",quote=FALSE)}
}



# Create files for R_Sufi

if ((!stop_) & (create_RSufi_files==TRUE)){
  AREA <- ResultDataTA[1,"AREA"]
  create_strata(Stratification=MEDITS::stratification_scheme,AREA,wd)
  create_haul(ResultDataTA,wd,suffix)
  create_catch(ResultDataTB,wd)


  if (!(all(is.na(TE)) & length(TE)==1)) {
    if (nrow(ResultDataTE)>0){
        create_length(ResultDataTE,DataSpecies=TM_list,wd)
    }
  }
  print(paste("R-Sufi files have been created for the ",yea, "and the GSA selected! They have been stored in files R-Sufi directory."),quote=FALSE)
}



} # ciclo years

# END ---------------------------------------------------------------------




  # -------------------------------------------------------
  # -------------------------------------------------------
  # -------------------------------------------------------
  # -------------------------------------------------------


  if ((!stop_) & (create_global_RSufi_files==TRUE)){
    if (is.na(Year_start) | is.na(Year_end)){
      warning("One or both variables Year_start and Year_end not declared")
    } else {
      RSufi_files(Year_start,Year_end,AREA,wd)
      print("R-Sufi files have been created for the Years and the GSA selected! They have been stored in files R-Sufi directory.",quote=FALSE)
    }
  }
  # -------------------------------------------------------
  if (file.exists(file.path(tempdir(), "Logfiles"))){
  unlink(file.path(tempdir(),"Logfiles"),recursive=T)
  }
  if (file.exists(file.path(tempdir(), "Graphs"))){
  unlink(file.path(tempdir(),"Graphs"),recursive=T)
    }
	if (file.exists(file.path(tempdir(), "Graphs"))){
  unlink(file.path(tempdir(),"files R-Sufi"),recursive=T)
    }
	


} # funzione RoME

