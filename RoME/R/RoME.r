###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)  #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                          #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #
#   March 2020                                                                                                            #
###########################################################################################################################

# IMPORTANT: set the name of your TA, TB, TC, TD, TT, TE (that have to be in your working directory)

# VAR INIZIALIZING --------------------------------------------------------




# TA <- RoME::TA
# TB <- RoME::TB
# TC <- RoME::TC
# TE <- RoME::TE
# TL <- RoME::TL

# VAR END --------------------------------------------------------

RoME <- function(TA,TB,TC,TE=NA,TL=NA,wd,suffix)
{
  stringsAsFactors=FALSE
  assign("Format",value="from_2012",envir=.GlobalEnv)

  # wd <- tempdir()

  if (!file.exists(paste(wd,"Logfiles",sep="/"))){
    dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
  }
  if (!file.exists(paste(wd,"Graphs",sep="/"))){
    dir.create(file.path(wd, "Graphs"), showWarnings = FALSE)
  }
  if (!file.exists(paste(wd,"files R-Sufi",sep="/"))){
    dir.create(file.path(wd, "files R-Sufi"), showWarnings = FALSE)
  }


  suffix <- paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  Errors <<- paste(wd,"/Logfiles/Logfile_",suffix,".dat",sep="")
  write(paste("\n ",date(),sep=""), file = Errors, append = TRUE)


  stop_ = FALSE


# START -------------------------------------------------------------------

  write(paste("-------------------------------------------------------------\nLIST OF ERRORS\n-------------------------------------------------------------"), file = Errors, append = TRUE)

check_without_errors = TRUE

# check degli header ------------------------------------------------------

  checkHeader(TA,"TA")
  checkHeader(TB,"TB")
  checkHeader(TC,"TC")

  ResultDataTA_bkp <- TA
  ResultDataTB_bkp <- TB
  ResultDataTC_bkp <- TC



  if (!(all(is.na(TE)) & length(TE)==1))
  {
  checkHeader(TE,"TE")
  ResultDataTE_bkp <- TE
  }

  if (!(all(is.na(TL)) & length(TL)==1))
  {
    checkHeader(TL,"TL")
    ResultDataTL_bkp <- TL
  }

years = unique (TA$YEAR)

yea <- 2012
for (yea in years) {

if (check_without_errors == TRUE) {
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

#---------------------


checkName = "Check identical record TA"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_identical_records(Data=ResultDataTA, wd, suffix)
}
stop_ = printError(checkName,check_without_errors, stop_)

checkName= "Check identical record TB"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_identical_records(Data=ResultDataTB,wd,suffix)
}
stop_ = printError(checkName,check_without_errors, stop_)

checkName = "Check identical record TC"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_identical_records(Data=ResultDataTC,wd,suffix)
}
stop_ = printError(checkName,check_without_errors, stop_)

if (!(all(is.na(TE)) & length(TE)==1))
{
  if (nrow(ResultDataTE)>0){
  checkName = "Check identical record TE"
  if (check_without_errors == TRUE) {
    print(paste(checkName,"in progress..."), quote = FALSE)
    check_without_errors = check_identical_records(Data=ResultDataTE,wd,suffix)
  }
  stop_ = printError(checkName,check_without_errors, stop_)
  }
}

if (!(all(is.na(TL)) & length(TL)==1))
{
  if (nrow(ResultDataTL)>0){
  checkName = "Check identical record TL"
  if (check_without_errors == TRUE) {
    print(paste(checkName,"in progress..."), quote = FALSE)
    check_without_errors = check_identical_records(Data=ResultDataTL,wd,suffix)
  }
  stop_ = printError(checkName,check_without_errors, stop_)
  }
}


# --------------------------------

checkName = "Check quasi-identical record in TA"
if (check_without_errors == TRUE) {
   print(paste(checkName, "in progress..."), quote = FALSE)
   check_without_errors = check_quasiidentical_records(ResultDataTA,wd,suffix)
}
stop_ = printError(checkName,check_without_errors, stop_)


checkName = "Check quasi-identical record in TB"
if (check_without_errors == TRUE) {
   print(paste(checkName,"in progress..."), quote = FALSE)
check_without_errors = check_quasiidentical_records(ResultDataTB,wd,suffix)
}
stop_ = printError(checkName,check_without_errors, stop_)


checkName = "Check quasi-identical record in TC"
if (check_without_errors == TRUE) {
   print(paste(checkName,"in progress..."), quote = FALSE)
check_without_errors = check_quasiidentical_records(ResultDataTC,wd,suffix)
}
stop_ = printError(checkName,check_without_errors, stop_)


if (!(all(is.na(TE)) & length(TE)==1))
{
  if (nrow(ResultDataTE)>0){
    checkName = "Check quasi-identical record in TE"
    if (check_without_errors == TRUE) {
      print(paste(checkName,"in progress..."), quote = FALSE)
      check_without_errors = check_quasiidentical_records(ResultDataTE,wd,suffix)
    }
    stop_ = printError(checkName,check_without_errors, stop_)
      }
}

if (!(all(is.na(TL)) & length(TL)==1))
{
  if (nrow(ResultDataTL)>0){
    checkName = "Check quasi-identical record in TL"
    if (check_without_errors == TRUE) {
      print(paste(checkName,"in progress..."), quote = FALSE)
      check_without_errors = check_quasiidentical_records(ResultDataTL,wd,suffix)
    }
    stop_ = printError(checkName,check_without_errors, stop_)
  }
}


# --------------------------------


checkName = "Check consistency of area TA, TB, TC, TE, TL"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_area(TA, TB, TC, TE, TL, wd, suffix)
}
stop_ = printError(checkName,check_without_errors, stop_)


#--------------------------------------------------
# Dictionary checks
#--------------------------------------------------

#TA
checkName = "Check dictionary for field:"

Field = "VALIDITY"
Values = c("V","I")
if (check_without_errors == TRUE) {
  print(paste(checkName,Field,"in progress..."), quote = FALSE)
  check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, wd, suffix)
}
stop_ = printError(paste(checkName,Field),check_without_errors, stop_)

Field = "COURSE"
Values = c("R","N")
if (check_without_errors == TRUE) {
  print(paste(checkName,Field,"in progress..."), quote = FALSE)
  check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, wd, suffix)
}
stop_ = printError(paste(checkName,Field),check_without_errors, stop_)

Field = "CODEND_CLOSING"
Values = c("S","C")
if (check_without_errors == TRUE) {
  print(paste(checkName,Field,"in progress..."), quote = FALSE)
  check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, wd, suffix)
}
stop_ = printError(paste(checkName,Field),check_without_errors, stop_)

Field = "PART_OF_THE_CODEND"
Values = c("A","M","P","S")
if (check_without_errors == TRUE) {
    print(paste(checkName,Field,"in progress..."), quote = FALSE)
    check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, wd, suffix)
}
stop_ = printError(paste(checkName,Field),check_without_errors, stop_)

Field = "GEOMETRICAL_PRECISION"
Values = c("M","E")
if (check_without_errors == TRUE) {
  print(paste(checkName,Field,"in progress..."), quote = FALSE)
  check_without_errors = check_dictionary(ResultDataTA, Field, Values)
}
stop_ = printError(paste(checkName,Field),check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

Field = "GEAR"
Values = c("GOC73")
if (check_without_errors == TRUE) {
  print(paste(checkName,Field,"in progress..."), quote = FALSE)
  check_without_errors = check_dictionary(ResultDataTA, Field, Values)
}
stop_ = printError(paste(checkName,Field),check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

Field = "RIGGING"
Values = c("GC73")
if (check_without_errors == TRUE) {
  print(paste(checkName,Field,"in progress..."), quote = FALSE)
  check_without_errors = check_dictionary(ResultDataTA, Field, Values)
}
stop_ = printError(paste(checkName,Field),check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

Field = "DOORS"
Values = c("WHS8")
if (check_without_errors == TRUE) {
  print(paste(checkName,Field,"in progress..."), quote = FALSE)
  check_without_errors = check_dictionary(ResultDataTA, Field, Values)
}
stop_ = printError(paste(checkName,Field),check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

Field = "SHOOTING_QUADRANT"
Values = c("1", "3", "5", "7")
if (check_without_errors == TRUE) {
  print(paste(checkName,Field,"in progress..."), quote = FALSE)
  check_without_errors = check_dictionary(ResultDataTA, Field, Values)
}
stop_ = printError(paste(checkName,Field),check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

Field = "HAULING_QUADRANT"
Values = c("1", "3", "5", "7")
if (check_without_errors == TRUE) {
  print(paste(checkName,Field,"in progress..."), quote = FALSE)
  check_without_errors = check_dictionary(ResultDataTA, Field, Values)
}
stop_ = printError(paste(checkName,Field),check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

if (Format=="from_2012"){

  Field = "MEASURING_SYSTEM"
  Values = c("VA","SO","XA","SA","SI","CT","SB")
  if (check_without_errors == TRUE) {
    print(paste(checkName,Field,"in progress..."), quote = FALSE)
    check_without_errors = check_dictionary(ResultDataTA, Field, Values)
  }
  stop_ = printError(paste(checkName,Field),check_without_errors, stop_)
  if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                         unlink(paste(DataTB,".csv",sep=""))
                                         unlink(paste(DataTC,".csv",sep=""))}
} else {
  if (as.character(DataTD)!=""){
    Field = "METHOD"
    Values = c("VA","XA","SA","CTD")
    if (check_without_errors == TRUE) {
      print(paste(checkName,Field,"in progress..."), quote = FALSE)
      check_without_errors = check_dictionary(ResultDataTD, Field, Values)
    }
    stop_ = printError(paste(checkName,Field),check_without_errors, stop_)
    if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                           unlink(paste(DataTB,".csv",sep=""))
                                           unlink(paste(DataTC,".csv",sep=""))
                                           unlink(paste(DataTD,".csv",sep=""))  }



  }
}

#TB
Field = "CODEND_CLOSING"
Values = c("S","C")
if (check_without_errors == TRUE) {
  print(paste(checkName,Field,"in progress..."), quote = FALSE)
  check_without_errors = check_dictionary(ResultDataTB, Field, Values)
}
stop_ = printError(paste(checkName,Field),check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

Field = "PART_OF_THE_CODEND"
Values = c("A", "M", "P", "S")
if (check_without_errors == TRUE) {
  print(paste(checkName,Field,"in progress..."), quote = FALSE)
  check_without_errors = check_dictionary(ResultDataTB, Field, Values)
}
stop_ = printError(paste(checkName,Field),check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}



#TC
Field = "CODEND_CLOSING"
Values = c("S","C")
if (check_without_errors == TRUE) {
  print(paste(checkName,Field,"in progress..."), quote = FALSE)
  check_without_errors = check_dictionary(ResultDataTC, Field, Values)
}
stop_ = printError(paste(checkName,Field),check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

Field = "PART_OF_THE_CODEND"
Values = c("A", "M", "P", "S")
if (check_without_errors == TRUE) {
  print(paste(checkName,Field,"in progress..."), quote = FALSE)
  check_without_errors = check_dictionary(ResultDataTC, Field, Values)
}
stop_ = printError(paste(checkName,Field),check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

Field = "SEX"
Values = c("M", "F", "I", "N")
if (check_without_errors == TRUE) {
  print(paste(checkName,Field,"in progress..."), quote = FALSE)
  check_without_errors = check_dictionary(ResultDataTC, Field, Values)
}
stop_ = printError(paste(checkName,Field),check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}


if(Format=="before_2012"){
  Field = "LENGTH_CLASSES_CODE"
  Values = c("0","1","m")
  if (check_without_errors == TRUE) {
    print(paste(checkName,Field,"in progress..."), quote = FALSE)
    check_without_errors = check_dictionary(ResultDataTC, Field, Values)
  }
  stop_ = printError(paste(checkName,Field),check_without_errors, stop_)
  if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                         unlink(paste(DataTB,".csv",sep=""))
                                         unlink(paste(DataTC,".csv",sep=""))}
} else {
  Field = "LENGTH_CLASSES_CODE"
  Values = c("0","m")
  if (check_without_errors == TRUE) {
    print(paste(checkName,Field,"in progress..."), quote = FALSE)
    check_without_errors = check_dictionary(ResultDataTC, Field, Values)
  }
  stop_ = printError(paste(checkName,Field),check_without_errors, stop_)
  if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                         unlink(paste(DataTB,".csv",sep=""))
                                         unlink(paste(DataTC,".csv",sep=""))}

  if (DataTE!=""){
    Field = "LENGTH_CLASSES_CODE"
    Values = c("0","m")
    if (check_without_errors == TRUE) {
      print(paste(checkName,Field,"in progress..."), quote = FALSE)
      check_without_errors = check_dictionary(ResultDataTE, Field, Values)
    }
    stop_ = printError(paste(checkName,Field),check_without_errors, stop_)
    if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                           unlink(paste(DataTB,".csv",sep=""))
                                           unlink(paste(DataTC,".csv",sep=""))
                                           unlink(paste(DataTE,".csv",sep=""))}



    Field = "SEX"
    Values = c("M", "F", "I", "N")
    if (check_without_errors == TRUE) {
      print(paste(checkName,Field,"in progress..."), quote = FALSE)
      check_without_errors = check_dictionary(ResultDataTE, Field, Values)
    }
    stop_ = printError(paste(checkName,Field),check_without_errors, stop_)
    if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                           unlink(paste(DataTB,".csv",sep=""))
                                           unlink(paste(DataTC,".csv",sep=""))
                                           unlink(paste(DataTE,".csv",sep=""))}
  }
}
# End dictionary checks
#--------------------------------------------------

checkName = "Check no empty fields TA"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_no_empty_fields(ResultDataTA, "TA")
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName =  "Check no empty fields TB"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_no_empty_fields(ResultDataTB, "TB")
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check no empty fields TC"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_no_empty_fields(ResultDataTC, "TC")
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

if ((Format=="from_2012")& (DataTE!="")){

  checkName = "Check no empty fields TE"
  if (check_without_errors == TRUE) {
    print(paste(checkName,"in progress..."), quote = FALSE)
    check_without_errors = check_no_empty_fields(ResultDataTE, "TE")
  }
  stop_ = printError(checkName,check_without_errors, stop_)
  if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                         unlink(paste(DataTB,".csv",sep=""))
                                         unlink(paste(DataTC,".csv",sep=""))
                                         unlink(paste(DataTE,".csv",sep=""))}
}
# checks on TA

checkName = "Check 0 fields TA"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_0_fieldsTA(ResultDataTA)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check dm TA"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_dm(ResultDataTA)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check consistency between duration and time TA"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_consistencyTA_duration(ResultDataTA)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check consistency between distance and duration of the haul TA"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_consistencyTA_distance(ResultDataTA)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check consistency of the hauls coordinates with the distance(difference not greater than 30%)"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_distance(ResultDataTA)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check consistency of bridles length TA"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_bridles_length(ResultDataTA)
}
stop_ =printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check difference between start depth and end depth (not greater than 20%) in TA"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_depth(ResultDataTA)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check start depth and end depth in the same stratum TA"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_stratum(ResultDataTA)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

if ((as.character(DataTT)!="" )| (Format=="from_2012")){
  checkName = "Check consistency of stratum code in TA or TT"
  if (check_without_errors == TRUE) {
    print(paste(checkName,"in progress..."), quote = FALSE)
    check_without_errors = check_stratum_code()
  }
  stop_ = printError(checkName,check_without_errors, stop_)
  if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                         unlink(paste(DataTB,".csv",sep=""))
                                         unlink(paste(DataTC,".csv",sep=""))}

}
checkName = "Check start quadrant and end quadrant TA"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_quadrant(ResultDataTA)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check uniqueness of valid hauls TA"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_unique_valid_haul(ResultDataTA)
}
stop_ =printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Visual check of the haul positions"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
check_position(ResultDataTA)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}


checkName = "Relation between shooting depth e warp length and between warp length e wing opening"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  graphs_TA(ResultDataTA)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check if the coordinates are in the Mediterranean Sea"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_position_in_Med(ResultDataTA)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}


checkName = "Check on temperature by haul"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
#   if((Format=="before_2012")){if (DataTD != ""){.check_without_errors = .check_temperature(ResultDataTD)}} else {check_without_errors = .check_temperature(ResultDataTA)}
  if((Format=="before_2012")){if (DataTD != ""){check_without_errors = check_temperature()}} else {check_without_errors = check_temperature()}
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}


# checks on TB

checkName = "Check correctness of species codes TB"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_rubincode(DataSpecies,ResultDataTB)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check consistency of NB_TOTAL and number per sex TB"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_nbtotTB(ResultDataTB)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

if (Format=="from_2012"){
  checkName = "Check presence of NB_TOTAL and number per sex TB for species G1"
  if (check_without_errors == TRUE) {
    print(paste(checkName,"in progress..."), quote = FALSE)
    check_without_errors = check_nm_TB()
  }
  stop_ = printError(checkName,check_without_errors, stop_)
  if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                         unlink(paste(DataTB,".csv",sep=""))
                                         unlink(paste(DataTC,".csv",sep=""))}

}

checkName = "Check consistency between not null weight and not null total number"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_weight_tot_nb(ResultDataTB)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check consistency of weight and number TB"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_weight(ResultDataTB, DataTargetSpecies)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

# checks on TC

graphics.off()
checkName = "Check correctness of LENGTH_CLASSES_CODE TC"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_length_class_codeTC(ResultDataTC,DataSpecies)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

#if(Format=="before_2012"){

checkName = "Check correctness of species codes TC"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_rubincode(DataSpecies,ResultDataTC)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}
#}

checkName = "Check consistency of length classes TC"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_length(ResultDataTC,DataTargetSpecies)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}



checkName = "Check correctness of number per sex in TC"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_nb_per_sexTC(ResultDataTC)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check consistency of maturity stages TC"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_mat_stages(ResultDataTC,DataTargetSpecies)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check sub-sampling"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_subsampling(ResultDataTC)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}


checkName = "Check consistency of maturity stages TC by the comparison with the length of smallest mature individuals reported in bibliography"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_smallest_mature(ResultDataTC)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check consistency of sex TC by means of spawning period"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_spawning_period(ResultDataTA,ResultDataTC)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check consistency of sex data TC by means of sex-inversion size"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_sex_inversion(ResultDataTC)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check consistency of length distribution TC"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_step_length_distr(ResultDataTC)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check total weight in the haul in TC"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_individual_weightTC()
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}


checkName = "Check correctness of species codes TC"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_rubincode(DataSpecies,ResultDataTC)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}
#}
if (Format=="from_2012") {
  checkName = "Check presence of lengths for G1 and G2 Medits species in TC"
  if (check_without_errors == TRUE) {
    print(paste(checkName,"in progress..."), quote = FALSE)
    check_without_errors = check_G1_G2()
  }
  stop_ = printError(checkName,check_without_errors, stop_)

  if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                         unlink(paste(DataTB,".csv",sep=""))
                                         unlink(paste(DataTC,".csv",sep=""))}
}
# cross checks

checkName = "Check presence in TB of TA hauls"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_hauls_TATB(ResultDataTA,ResultDataTB)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check presence in TA of TB hauls"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_hauls_TBTA(ResultDataTA,ResultDataTB)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check presence in TC of TB target species"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_species_TBTC(DataTargetSpecies,ResultDataTB,ResultDataTC)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check presence in TB of TC species"
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_haul_species_TCTB(ResultDataTB,ResultDataTC)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

checkName = "Check correctness of the number per sex in TB in case of sub-sampling in TC  "
if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_raising(ResultDataTB,ResultDataTC)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                       unlink(paste(DataTB,".csv",sep=""))
                                       unlink(paste(DataTC,".csv",sep=""))}

if (Format=="from_2012"){

  checkName = "Check on date by haul TB"
  if (check_without_errors == TRUE) {
    print(paste(checkName,"in progress..."), quote = FALSE)
    check_without_errors = check_date_haul(ResultDataTB)
  }
  stop_ =printError(checkName,check_without_errors, stop_)
  if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                         unlink(paste(DataTB,".csv",sep=""))
                                         unlink(paste(DataTC,".csv",sep=""))
                                         unlink(paste(DataTE,".csv",sep=""))}

  checkName = "Check on date by haul TC"
  if (check_without_errors == TRUE) {
    print(paste(checkName,"in progress..."), quote = FALSE)
    check_without_errors = check_date_haul(ResultDataTC)
  }
  stop_ = printError(checkName,check_without_errors, stop_)
  if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                         unlink(paste(DataTB,".csv",sep=""))
                                         unlink(paste(DataTC,".csv",sep=""))
                                         unlink(paste(DataTE,".csv",sep=""))}

  checkName = "Check on date by haul TE"
  if (DataTE!=""){
    if (check_without_errors == TRUE) {
      print(paste(checkName,"in progress..."), quote = FALSE)
      check_without_errors = check_date_haul(ResultDataTE)
    }
    stop_ = printError(checkName,check_without_errors, stop_)
    if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                           unlink(paste(DataTB,".csv",sep=""))
                                           unlink(paste(DataTC,".csv",sep=""))
                                           unlink(paste(DataTE,".csv",sep=""))}


    checkName = "Cross check on number between TC and TE"
    if (check_without_errors == TRUE) {
      print(paste(checkName,"in progress..."), quote = FALSE)
      check_without_errors = check_TE_TC()
    }
    stop_ = printError(checkName,check_without_errors, stop_)
    if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                           unlink(paste(DataTB,".csv",sep=""))
                                           unlink(paste(DataTC,".csv",sep=""))
                                           unlink(paste(DataTE,".csv",sep=""))}





    #Check on TE

    checkName = "Check consistency of maturity stages TE"
    if (as.character(DataTE)!=""){
      if (check_without_errors == TRUE) {
        print(paste(checkName,"in progress..."), quote = FALSE)
        check_without_errors = check_mat_stages(ResultDataTE,DataTargetSpecies)
      }
      stop_ = printError(checkName,check_without_errors, stop_)
      if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                             unlink(paste(DataTB,".csv",sep=""))
                                             unlink(paste(DataTC,".csv",sep=""))
                                             unlink(paste(DataTE,".csv",sep=""))}
    }

    checkName = "Check consistency of maturity stages TE by the comparison with the length of smallest mature individuals reported in bibliography"
    if (check_without_errors == TRUE) {
      print(paste(checkName,"in progress..."), quote = FALSE)
      check_without_errors = check_smallest_mature(ResultDataTE)
    }
    stop_ = printError(checkName,check_without_errors, stop_)
    if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                           unlink(paste(DataTB,".csv",sep=""))
                                           unlink(paste(DataTC,".csv",sep=""))
                                           unlink(paste(DataTE,".csv",sep=""))}

    checkName = "Check consistency of maturity stages in TE by means of spawning period"
    if (check_without_errors == TRUE) {
      print(paste(checkName,"in progress..."), quote = FALSE)
      check_without_errors = check_spawning_period(ResultDataTA,ResultDataTE)
    }
    stop_ = printError(checkName,check_without_errors, stop_)
    if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                           unlink(paste(DataTB,".csv",sep=""))
                                           unlink(paste(DataTC,".csv",sep=""))
                                           unlink(paste(DataTE,".csv",sep=""))}

    checkName = "Check individual weight in TE"
    if (check_without_errors == TRUE) {
      print(paste(checkName,"in progress..."), quote = FALSE)
      check_without_errors = check_individual_weightTE()
    }
    stop_ = printError(checkName,check_without_errors, stop_)
    if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                           unlink(paste(DataTB,".csv",sep=""))
                                           unlink(paste(DataTC,".csv",sep=""))
                                           unlink(paste(DataTE,".csv",sep=""))}

    checkName = "Check correctness of species codes TE"
    if (check_without_errors == TRUE) {
      print(paste(checkName,"in progress..."), quote = FALSE)
      check_without_errors = check_rubincode(DataSpecies,ResultDataTE)
    }
    stop_ = printError(checkName,check_without_errors, stop_)
    if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                           unlink(paste(DataTB,".csv",sep=""))
                                           unlink(paste(DataTC,".csv",sep=""))
                                           unlink(paste(DataTE,".csv",sep=""))}


    checkName = "Check consistency TE check-fields"
    if (check_without_errors == TRUE) {
      print(paste(checkName,"in progress..."), quote = FALSE)
      check_without_errors = check_nb_TE()
    }
    stop_ = printError(checkName,check_without_errors, stop_)
    if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                           unlink(paste(DataTB,".csv",sep=""))
                                           unlink(paste(DataTC,".csv",sep=""))
                                           unlink(paste(DataTE,".csv",sep=""))}

    checkName = "Summary individual data sampling"
    if (check_without_errors == TRUE) {
      print(paste(checkName,"in progress..."), quote = FALSE)
      check_without_errors = scheme_individual_data()
    }
    stop_ = printError(checkName,check_without_errors, stop_)
    if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
                                           unlink(paste(DataTB,".csv",sep=""))
                                           unlink(paste(DataTC,".csv",sep=""))
                                           unlink(paste(DataTE,".csv",sep=""))}

  }
}

   if (as.character(DataTL)!=""){
   # Checks on TL
checkName = "Check allowed values for category on Litter data"
Field = "LITTER_CATEGORY"
Values = c("L0","L1","L2","L3","L4","L5","L6","L7")


if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_dictionary(ResultDataTL,Field, Values)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
  unlink(paste(DataTB,".csv",sep=""))
  unlink(paste(DataTC,".csv",sep=""))
  unlink(paste(DataTE,".csv",sep=""))
  unlink(paste(DataTL,".csv",sep=""))
  }

checkName = "Check allowed values for category on Litter data"
Field = "LITTER_SUB-CATEGORY"
Values = c("0","a","b","c","d","e","f","g","h","i","j")


if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_dictionary(ResultDataTL,Field, Values)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
  unlink(paste(DataTB,".csv",sep=""))
  unlink(paste(DataTC,".csv",sep=""))
  unlink(paste(DataTE,".csv",sep=""))
  unlink(paste(DataTL,".csv",sep=""))
}


checkName = "Check correctness of associations between category and sub-category on Litter data"

AssociationsTL=read.table("Tables/Associations_cat_TL.csv",sep=";",header=T)

if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_associations_category_TL(ResultDataTL,AssociationsTL)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
  unlink(paste(DataTB,".csv",sep=""))
  unlink(paste(DataTC,".csv",sep=""))
  unlink(paste(DataTE,".csv",sep=""))
  unlink(paste(DataTL,".csv",sep=""))
}

checkName = "Check if the number is always filled in on Litter data"


if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_no_empty_fields(ResultDataTL,"TL")
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
  unlink(paste(DataTB,".csv",sep=""))
  unlink(paste(DataTC,".csv",sep=""))
  unlink(paste(DataTE,".csv",sep=""))
  unlink(paste(DataTL,".csv",sep=""))
}


checkName = "Check if the date in TL is consistent with TA"


if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_date_haul(ResultDataTL)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
  unlink(paste(DataTB,".csv",sep=""))
  unlink(paste(DataTC,".csv",sep=""))
  unlink(paste(DataTE,".csv",sep=""))
  unlink(paste(DataTL,".csv",sep=""))
}

checkName = "Check if the hauls in TL are present in TA"


if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_hauls_TLTA(ResultDataTA,ResultDataTL)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
  unlink(paste(DataTB,".csv",sep=""))
  unlink(paste(DataTC,".csv",sep=""))
  unlink(paste(DataTE,".csv",sep=""))
  unlink(paste(DataTL,".csv",sep=""))
}

checkName = "Check if the hauls in TA are present in TL"


if (check_without_errors == TRUE) {
  print(paste(checkName,"in progress..."), quote = FALSE)
  check_without_errors = check_hauls_TATL(ResultDataTA,ResultDataTL)
}
stop_ = printError(checkName,check_without_errors, stop_)
if ((stop_) & (Type_of_files==".xls")){unlink(paste(DataTA,".csv",sep=""))
  unlink(paste(DataTB,".csv",sep=""))
  unlink(paste(DataTC,".csv",sep=""))
  unlink(paste(DataTE,".csv",sep=""))
  unlink(paste(DataTL,".csv",sep=""))
}
}


if (!stop_) {
  print("All the checks have been performed!",quote=FALSE)
}



# Create files for R_Sufi

if ((!stop_) & as.character(create_RSufi_files)=="Y"){
  create_strata(Stratification,AREA)
  create_haul(ResultDataTA)
  create_catch(ResultDataTB)
  if ((Format=="before_2012")| (DataTE=="") ){
    create_length(ResultDataTC)
  } else if (DataTE!=""){
    create_length(ResultDataTE)
  }
  print(paste("R-Sufi files have been created for the ",yea, "and the GSA selected! They have been stored in files R-Sufi directory."),quote=FALSE)
}

#----- bkp
if(Format=="before_2012"){
DataTT = DataTT_bkp
DataTD = DataTD_bkp
} else {
DataTE = DataTE_bkp
}


#------ bkp

} # ciclo years

# END ---------------------------------------------------------------------




  # -------------------------------------------------------
  # -------------------------------------------------------
  # -------------------------------------------------------
  # -------------------------------------------------------


  if ((!stop_) & as.character(create_global_RSufi_files)=="Y"){
    RSufi_files()
    print("R-Sufi files have been created for the Years and the GSA selected! They have been stored in files R-Sufi directory.",quote=FALSE)
  }
  # -------------------------------------------------------
} # funzione RoME

