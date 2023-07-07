

# TEST INIZIALIZING --------------------------------------------------------
if (FALSE) {
  library(RoME)
  # wd <- "D:\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\_CONDIVISA con COSMIDANO_\\RoME_2022\\CHECK_WZ_CN"
  # suffix=NA  # non modificare
  # ta <- read.table(file=paste(wd, "\\TA.csv",sep=""), sep=";", header=T)
  # ta[1,"TYPE_OF_FILE"] <- NA
  # tb <- read.table(file=paste(wd, "\\TB.csv",sep=""), sep=";", header=T)
  # tc <- read.table(file=paste(wd, "\\TC.csv",sep=""), sep=";", header=T)
  # te <- NA
  # tl <- NA


  # ta <- read.table("D:\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\RDB\\RoME_RDBFIS\\data\\TA GSA18 2017-2020.csv", sep=";",header=TRUE)
  # tb <- read.table("D:\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\RDB\\RoME_RDBFIS\\data\\TB GSA18 2017-2020.csv", sep=";",header=TRUE)
  # tc <- read.table("D:\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\RDB\\RoME_RDBFIS\\data\\TC GSA18 2017-2020.csv", sep=";",header=TRUE)
  # te <- read.table("D:\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\RDB\\RoME_RDBFIS\\data\\TE GSA18 2017-2020.csv", sep=";",header=TRUE)
  # tl <- read.table("D:\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\RDB\\RoME_RDBFIS\\data\\TL GSA18 2017-2020.csv", sep=";",header=TRUE)
  #
  # ta <- ta[ta$YEAR==2017,]
  # tb <- tb[tb$YEAR==2017,]
  # tc <- tc[tc$YEAR==2017,]
  # te <- te[te$YEAR==2017,]
  # tl <- tl[tl$YEAR==2017,]

  unique(ta$YEAR)
  # ta <-  RoME::TA[TA$YEAR %in% c(2012,2013,2014,2015,2016), ] # read.table(file=paste(wd, "\\TA.csv",sep=""), sep=";", header=T) # RoME::TA
  # tb <-  RoME::TB[TB$YEAR %in% c(2012,2013,2014,2015,2016), ] # read.table(file=paste(wd, "\\TB.csv",sep=""), sep=";", header=T) # RoME::TB
  # tc <-  RoME::TC[TC$YEAR %in% c(2012,2013,2014,2015,2016), ] # read.table(file=paste(wd, "\\TC.csv",sep=""), sep=";", header=T) # RoME::TC
  # te <-  RoME::TE[TE$YEAR %in% c(2012,2013,2014,2015,2016), ] # NA # read.table(file=paste(wd, "\\TE.csv",sep=""), sep=";", header=T)
  # tl <-  RoME::TL[TL$YEAR %in% c(2012,2013,2014,2015,2016), ] # NA #read.table(file=paste(wd, "\\TL.csv",sep=""), sep=";", header=T) # RoME::TL
  #
  # TA <-  RoME::TA[TA$YEAR %in% c(2012,2013,2014,2015,2016), ] # read.table(file=paste(wd, "\\TA.csv",sep=""), sep=";", header=T) # RoME::TA
  # TB <-  RoME::TB[TB$YEAR %in% c(2012,2013,2014,2015,2016), ] # read.table(file=paste(wd, "\\TB.csv",sep=""), sep=";", header=T) # RoME::TB
  # TC <-  RoME::TC[TC$YEAR %in% c(2012,2013,2014,2015,2016), ] # read.table(file=paste(wd, "\\TC.csv",sep=""), sep=";", header=T) # RoME::TC
  # TE <-  RoME::TE[TE$YEAR %in% c(2012,2013,2014,2015,2016), ] # NA # read.table(file=paste(wd, "\\TE.csv",sep=""), sep=";", header=T)
  # TL <-  RoME::TL[TL$YEAR %in% c(2012,2013,2014,2015,2016), ] # NA #read.table(file=paste(wd, "\\TL.csv",sep=""), sep=";", header=T) # RoME::TL

  verbose = TRUE
  # create_RSufi_files=TRUE
  # create_global_RSufi_files=TRUE
  # Year_start=2012
  # Year_end=2016

  Stratification=RoME::stratification_scheme
  Ref_list=RoME::TM_list
  DataTargetSpecies=DataTargetSpecies
  Maturity=RoME::Maturity_parameters
  ab_parameters=RoME::LW
  stages_list=RoME::mat_stages
  assTL=RoME::assTL

  # tb[2,"YEAR"] <- 2008
  # tb[2,1] <- "TA"
  # tb[1,"TOTAL_NUMBER_IN_THE_HAUL"] <- 6
  # tb[1,]
# TB$TYPE_OF_FILE <- as.character(TB$TYPE_OF_FILE)
# TB$TYPE_OF_FILE[1] <- "TC"

  TA=ta
  TB=tb
  TC=tc
  TE=te
  TL=tl

  res <- RoMEcc(TA=ta,TB=tb,TC=tc,TE=te,TL=tl,wd=wd,suffix=NA,
         verbose=TRUE,
         Stratification=RoME::stratification_scheme,
         Ref_list=RoME::TM_list,
         DataTargetSpecies=RoME::DataTargetSpecies,
         Maturity=RoME::Maturity_parameters,
         ab_parameters=RoME::LW,
         stages_list=RoME::mat_stages,
         assTL=RoME::assTL)
  res[!is.na(res$errors),]
}
# TEST END --------------------------------------------------------


RoMEcc <- function(TA,TB,TC,TE=NA,TL=NA,wd,suffix=NA,verbose=TRUE,Stratification=RoME::stratification_scheme, Ref_list=RoME::TM_list,DataTargetSpecies=RoME::DataTargetSpecies,Maturity=RoME::Maturity_parameters, ab_parameters=RoME::LW,stages_list=RoME::mat_stages,assTL=RoME::assTL){

  stringsAsFactors=FALSE
  Format <- "from_2012"

  if (!file.exists(file.path(wd, "Logfiles"))){
    dir.create(file.path(wd, "Logfiles"), recursive = TRUE, showWarnings = FALSE)
  }
  if (!file.exists(file.path(wd,"Graphs"))){
    dir.create(file.path(wd, "Graphs"), showWarnings = FALSE)
  }
  # if (!file.exists(file.path(wd,"/files R-Sufi",sep="/"))){
  #   dir.create(file.path(wd, "files R-Sufi"), showWarnings = FALSE)
  # }

  if (is.na(suffix)){
    suffix <- paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
  }
  Errors <- file.path(wd,"Logfiles",paste("Logfile_",suffix,".dat",sep=""))
  if (!file.exists(Errors)){
    file.create(Errors)
  }
  write(paste("\n ",date(),sep=""), file = Errors, append = TRUE)


  stop_ = FALSE
  check_without_errors = TRUE
  check_without_warnings = TRUE
  nErrors <- 0
  nWarnings <- 0
  headers_without_error <- TRUE
  year_without_error <- TRUE
  class_without_error <- TRUE
   TYPE_OF_FILE_without_error <- TRUE
  check.df <- data.frame(matrix(ncol=5,nrow=0))
  colnames(check.df) <- c("errors","check type","table","variable","year") # ,"warnings"
  yea <- NA
  Field <- NA
  variable <- NA
# START -------------------------------------------------------------------

  write(paste("-------------------------------------------------------------\n
              CHECK RESULTS
              \n-------------------------------------------------------------"), file = Errors, append = TRUE)

  # check degli header ------------------------------------------------------
  table <- "TA"
  checkName = "Check Headers"
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,table, "in progress..."), quote = FALSE)}
    check_without_errors =   checkHeader(TA,"TA",wd,suffix)
    if (headers_without_error){
       headers_without_error <- check_without_errors
    }
  }
  if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
  check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
  check.df <- check.list[[1]]
  check_without_errors <- check.list[[2]]
  check_without_warnings <- check.list[[3]]


  table <- "TB"
  checkName = "Check Headers"
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,table, "in progress..."), quote = FALSE)}
    check_without_errors =   checkHeader(TB,"TB",wd,suffix)
    if (headers_without_error){
      headers_without_error <- check_without_errors
    }
  }
  if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
  check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
  check.df <- check.list[[1]]
  check_without_errors <- check.list[[2]]
  check_without_warnings <- check.list[[3]]


  table <- "TC"
  checkName = "Check Headers"
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,table, "in progress..."), quote = FALSE)}
    check_without_errors =   checkHeader(TC,"TC",wd,suffix)
    if (headers_without_error){
      headers_without_error <- check_without_errors
    }
  }
  if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
  check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
  check.df <- check.list[[1]]
  check_without_errors <- check.list[[2]]
  check_without_warnings <- check.list[[3]]

  ResultDataTA_bkp <- TA
  ResultDataTB_bkp <- TB
  ResultDataTC_bkp <- TC


  ###  TE  ###
  if (!(all(is.na(TE)) & length(TE)==1))
  {
    table <- "TE"
    checkName = "Check Headers"
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,table, "in progress..."), quote = FALSE)}
      check_without_errors =   checkHeader(TE,"TE",wd,suffix)
      if (headers_without_error){
        headers_without_error <- check_without_errors
      }
    }
    if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
    check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
    check.df <- check.list[[1]]
    check_without_errors <- check.list[[2]]
    check_without_warnings <- check.list[[3]]
    ResultDataTE_bkp <- TE
  }

  ###  TL  ###
  if (!(all(is.na(TL)) & length(TL)==1))
  {
    table <- "TL"
    checkName = "Check Headers"
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,table, "in progress..."), quote = FALSE)}
      check_without_errors =   checkHeader(TL,"TL",wd,suffix)
      if (headers_without_error){
        headers_without_error <- check_without_errors
      }
    }
    if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
    check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
    check.df <- check.list[[1]]
    check_without_errors <- check.list[[2]]
    check_without_warnings <- check.list[[3]]
    ResultDataTL_bkp <- TL
  }
 ###########

  if (headers_without_error) {

    ### CHECK CLASS
    ### TA
    table <- "TA"
    checkName = "Check CLASS"
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,table, "in progress..."), quote = FALSE)}
      check_without_errors =   check_class(data=TA,tab="TA",wd=wd,suffix=suffix)
      if (class_without_error){
        class_without_error <- check_without_errors
      }
    }
    if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
    check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
    check.df <- check.list[[1]]
    check_without_errors <- check.list[[2]]
    check_without_warnings <- check.list[[3]]

    ### TB
    table <- "TB"
    checkName = "Check CLASS"
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,table, "in progress..."), quote = FALSE)}
      check_without_errors =   check_class(data=TB,tab="TB",wd=wd,suffix=suffix)
      if (class_without_error){
        class_without_error <- check_without_errors
      }
    }
    if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
    check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
    check.df <- check.list[[1]]
    check_without_errors <- check.list[[2]]
    check_without_warnings <- check.list[[3]]


    ### TC
    table <- "TC"
    checkName = "Check CLASS"
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,table, "in progress..."), quote = FALSE)}
      check_without_errors =   check_class(data=TC,tab="TC",wd=wd,suffix=suffix)
      if (class_without_error){
        class_without_error <- check_without_errors
      }
    }
    if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
    check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
    check.df <- check.list[[1]]
    check_without_errors <- check.list[[2]]
    check_without_warnings <- check.list[[3]]


    ### TE
    if (!(all(is.na(TE)) & length(TE)==1))
    {
    table <- "TE"
    checkName = "Check CLASS"
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,table, "in progress..."), quote = FALSE)}
      check_without_errors =   check_class(data=TE,tab="TE",wd=wd,suffix=suffix)
      if (class_without_error){
        class_without_error <- check_without_errors
      }
    }
    if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
    check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
    check.df <- check.list[[1]]
    check_without_errors <- check.list[[2]]
    check_without_warnings <- check.list[[3]]
    }

    ### TL
    if (!(all(is.na(TL)) & length(TL)==1))
    {
    table <- "TL"
    checkName = "Check CLASS"
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,table, "in progress..."), quote = FALSE)}
      check_without_errors =   check_class(data=TL,tab="TL",wd=wd,suffix=suffix)
      if (class_without_error){
        class_without_error <- check_without_errors
      }
    }
    if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
    check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
    check.df <- check.list[[1]]
    check_without_errors <- check.list[[2]]
    check_without_warnings <- check.list[[3]]
    }

#################

  ### CHECK YEAR

  years = sort(unique (TA$YEAR))

table <- "ALL"

checkName = "Check YEAR"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName, "in progress..."), quote = FALSE)}
   check_without_errors = check_year(TA, TB, TC, TE, TL, years, wd, Errors)
   year_without_error <- check_without_errors
}
 if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
 check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
 check.df <- check.list[[1]]
 check_without_errors <- check.list[[2]]
 check_without_warnings <- check.list[[3]]

  ### CHECK TYPE_OF_FILE
if (year_without_error){
  checkName = "Check TYPE_OF_FILE"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName, "in progress..."), quote = FALSE)}
   check_without_errors = check_type(TA, TB, TC, TE, TL, years, wd, Errors)
   TYPE_OF_FILE_without_error <- check_without_errors
}
 if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
     check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
     check.df <- check.list[[1]]
     check_without_errors <- check.list[[2]]
     check_without_warnings <- check.list[[3]]
}


if (class_without_error & year_without_error & TYPE_OF_FILE_without_error){

## CICLO PER ANNO ##

yea <- years[1]
for (yea in years) {

  cat(paste0("\n########################"))
  cat(paste0("\n###       ",yea,"       ###"))
  cat(paste0("\n########################"))

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
table <- "TA"
checkName = "Check identical record TA"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_identical_records(Data=ResultDataTA, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
      check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
      check.df <- check.list[[1]]
      check_without_errors <- check.list[[2]]
      check_without_warnings <- check.list[[3]]

table <- "TB"
checkName= "Check identical record TB"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_identical_records(Data=ResultDataTB,year=yea,wd,suffix)
}
if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
      check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
      check.df <- check.list[[1]]
      check_without_errors <- check.list[[2]]
      check_without_warnings <- check.list[[3]]

table <- "TC"
checkName = "Check identical record TC"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_identical_records(Data=ResultDataTC,year=yea,wd,suffix)
}
if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
      check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
      check.df <- check.list[[1]]
      check_without_errors <- check.list[[2]]
      check_without_warnings <- check.list[[3]]

table <- "TE"
if (!(all(is.na(TE)) & length(TE)==1))
{
  if (nrow(ResultDataTE)>0){
  checkName = "Check identical record TE"
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
    check_without_errors = check_identical_records(Data=ResultDataTE,year=yea,wd,suffix)
  }
  if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
         check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
         check.df <- check.list[[1]]
         check_without_errors <- check.list[[2]]
         check_without_warnings <- check.list[[3]]
  }
}

table <- "TL"
if (!(all(is.na(TL)) & length(TL)==1))
{
  if (nrow(ResultDataTL)>0){
  checkName = "Check identical record TL"
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
    check_without_errors = check_identical_records(Data=ResultDataTL,year=yea,wd,suffix)
  }
  if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
       check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
       check.df <- check.list[[1]]
       check_without_errors <- check.list[[2]]
       check_without_warnings <- check.list[[3]]
  }
}


# --------------------------------
# Check quasi-identical record
table <- "TA"
checkName = "Check quasi-identical record in TA"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName, "in progress..."), quote = FALSE)}
   check_without_errors = check_quasiidentical_records(ResultDataTA,year=yea,wd,suffix)
}
if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
      check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
      check.df <- check.list[[1]]
      check_without_errors <- check.list[[2]]
      check_without_warnings <- check.list[[3]]

table <- "TB"
checkName = "Check quasi-identical record in TB"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
   check_without_errors = check_quasiidentical_records(ResultDataTB,year=yea,wd,suffix)
}
if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
       check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
       check.df <- check.list[[1]]
       check_without_errors <- check.list[[2]]
       check_without_warnings <- check.list[[3]]


table <- "TC"
checkName = "Check quasi-identical record in TC"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_quasiidentical_records(ResultDataTC,year=yea,wd,suffix)
}
if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
      check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
      check.df <- check.list[[1]]
      check_without_errors <- check.list[[2]]
      check_without_warnings <- check.list[[3]]

table <- "TE"
if (!(all(is.na(TE)) & length(TE)==1))
{
  if (nrow(ResultDataTE)>0){
    checkName = "Check quasi-identical record in TE"
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
      check_without_errors = check_quasiidentical_records(ResultDataTE,year=yea,wd,suffix)
    }
    if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
         check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
         check.df <- check.list[[1]]
         check_without_errors <- check.list[[2]]
         check_without_warnings <- check.list[[3]]
      }
}

table <- "TL"
if (!(all(is.na(TL)) & length(TL)==1))
{
  if (nrow(ResultDataTL)>0){
    checkName = "Check quasi-identical record in TL"
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
      check_without_errors = check_quasiidentical_records(ResultDataTL,year=yea,wd,suffix)
    }
    if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
        check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
        check.df <- check.list[[1]]
        check_without_errors <- check.list[[2]]
        check_without_warnings <- check.list[[3]]
  }
}


# --------------------------------
# Check consistency of area

table <- "ALL"
checkName = "Check consistency of area TA, TB, TC, TE, TL"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = suppressWarnings(check_area(ResultDataTA, ResultDataTB, ResultDataTC, ResultDataTE, ResultDataTL, year=yea, wd, suffix))
}
if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
     check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
     check.df <- check.list[[1]]
     check_without_errors <- check.list[[2]]
     check_without_warnings <- check.list[[3]]

#--------------------------------------------------
# Dictionary checks
#--------------------------------------------------

#TA
table <- "TA"
checkName = "Check dictionary"
Field = "COUNTRY"
Values = as.character(unique(Stratification$COUNTRY))
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
     check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
     check.df <- check.list[[1]]
     check_without_errors <- check.list[[2]]
     check_without_warnings <- check.list[[3]]


table <- "TA"
checkName = "Check dictionary"
Field = "AREA"
Values = as.character(unique(Stratification$GSA))
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
       check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, year=yea, wd, suffix)
     }
  if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
     check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
     check.df <- check.list[[1]]
     check_without_errors <- check.list[[2]]
     check_without_warnings <- check.list[[3]]


     Field = "GEAR"
     Values = c("GOC73")
     if (check_without_errors == TRUE) {
       if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
       check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, year=yea, wd, suffix)
     }
     if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
     check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
     check.df <- check.list[[1]]
     check_without_errors <- check.list[[2]]
     check_without_warnings <- check.list[[3]]


     Field = "RIGGING"
     Values = c("GC73")
     if (check_without_errors == TRUE) {
       if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
       check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, year=yea, wd, suffix)
     }
     if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
     check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
     check.df <- check.list[[1]]
     check_without_errors <- check.list[[2]]
     check_without_warnings <- check.list[[3]]


     Field = "DOORS"
     Values = c("WHS8")
     if (check_without_errors == TRUE) {
       if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
       check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, year=yea, wd, suffix)
     }
     if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
     check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
     check.df <- check.list[[1]]
     check_without_errors <- check.list[[2]]
     check_without_warnings <- check.list[[3]]


     Field = "MONTH"
     Values = seq(1,12,1)
     if (check_without_errors == TRUE) {
       if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
       check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, year=yea, wd, suffix)
     }
     if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
     check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
     check.df <- check.list[[1]]
     check_without_errors <- check.list[[2]]
     check_without_warnings <- check.list[[3]]


     Field = "DAY"
     Values = seq(1,31,1)
     if (check_without_errors == TRUE) {
       if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
       check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, year=yea, wd, suffix)
     }
     if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
     check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
     check.df <- check.list[[1]]
     check_without_errors <- check.list[[2]]
     check_without_warnings <- check.list[[3]]

     Field = "HAUL_NUMBER"
     Values = seq(1,999,1)
     if (check_without_errors == TRUE) {
       if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
       check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, year=yea, wd, suffix)
     }
     if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
     check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
     check.df <- check.list[[1]]
     check_without_errors <- check.list[[2]]
     check_without_warnings <- check.list[[3]]


Field = "CODEND_CLOSING"
Values = c("S","C")
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


Field = "PART_OF_THE_CODEND"
Values = c("A","M","P","S")
if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
    check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


Field = "SHOOTING_TIME"
Values = seq(0,2400,1)
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


Field = "SHOOTING_QUADRANT"
Values = c("1", "3", "5", "7")
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


checkName = "Check numeric range"
Field = "SHOOTING_LATITUDE"
Values = c(3400,4600)
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_numeric_range(Data = ResultDataTA, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


Field = "SHOOTING_LONGITUDE"
Values = c(0,3500)
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_numeric_range(Data = ResultDataTA, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]

checkName = "Check dictionary"
Field = "SHOOTING_DEPTH"
Values = c(0,10:800)
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


Field = "HAULING_QUADRANT"
Values = c("1", "3", "5", "7")
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]

checkName = "Check numeric range"
Field = "HAULING_LATITUDE"
Values = c(3400,4600)
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_numeric_range(Data = ResultDataTA, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


Field = "HAULING_LONGITUDE"
Values = c(0,3500)
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_numeric_range(Data = ResultDataTA, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


checkName = "Check dictionary"
Field = "HAULING_TIME"
Values = seq(0,2400,1)
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


Field = "HAULING_DEPTH"
Values = c(0,10:800)
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


Field = "HAUL_DURATION"
Values = c(5:90)
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


Field = "VALIDITY"
Values = c("V","I")
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


Field = "COURSE"
Values = c("R","N")
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


Field = "DISTANCE"
Values = c(1000:9999)
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


Field = "VERTICAL_OPENING"
Values = c(10:100)
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


Field = "WING_OPENING"
Values = c(50:250)
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


Field = "GEOMETRICAL_PRECISION"
Values = c("M","E")
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


  Field = "MEASURING_SYSTEM"
  Values = c("VA","SO","XA","SA","SI","CT","SB",NA)
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
    check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, year=yea, wd, suffix)
  }
  if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
  check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
  check.df <- check.list[[1]]
  check_without_errors <- check.list[[2]]
  check_without_warnings <- check.list[[3]]



  Field = "RECORDED_SPECIES"
  Values = seq(0,4,1)
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
    check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, year=yea, wd, suffix)
  }
  if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
  check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
  check.df <- check.list[[1]]
  check_without_errors <- check.list[[2]]
  check_without_warnings <- check.list[[3]]



  Field = "WARP_LENGTH"
  Values = seq(100,2200,1)
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
    check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, year=yea, wd, suffix)
  }
  if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
  check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
  check.df <- check.list[[1]]
  check_without_errors <- check.list[[2]]
  check_without_warnings <- check.list[[3]]


  Field = "WARP_DIAMETER"
  Values = seq(10,30,1)
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
    check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, year=yea, wd, suffix)
  }
  if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
  check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
  check.df <- check.list[[1]]
  check_without_errors <- check.list[[2]]
  check_without_warnings <- check.list[[3]]


  Field = "OBSERVATIONS"
  Values = seq(0,9,1)
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
    check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, year=yea, wd, suffix)
  }
  if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
  check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
  check.df <- check.list[[1]]
  check_without_errors <- check.list[[2]]
  check_without_warnings <- check.list[[3]]


  checkName = "Check numeric range"
  Field = "BOTTOM_TEMPERATURE_BEGINNING"
  Values = c(0,30)
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
    check_without_errors = check_numeric_range(Data = ResultDataTA, Field, Values, year=yea, wd, suffix)
  }
  if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
  check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
  check.df <- check.list[[1]]
  check_without_errors <- check.list[[2]]
  check_without_warnings <- check.list[[3]]


  Field = "BOTTOM_TEMPERATURE_END"
  Values = c(0,30)
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
    check_without_errors = check_numeric_range(Data = ResultDataTA, Field, Values, year=yea, wd, suffix)
  }
  if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
  check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
  check.df <- check.list[[1]]
  check_without_errors <- check.list[[2]]
  check_without_warnings <- check.list[[3]]


  Field = "BOTTOM_SALINITY_BEGINNING"
  Values = c(0,50)
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
    check_without_errors = check_numeric_range(Data = ResultDataTA, Field, Values, year=yea, wd, suffix)
  }
  if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
  check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
  check.df <- check.list[[1]]
  check_without_errors <- check.list[[2]]
  check_without_warnings <- check.list[[3]]


  Field = "BOTTOM_SALINITY_END"
  Values = c(0,50)
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
    check_without_errors = check_numeric_range(Data = ResultDataTA, Field, Values, year=yea, wd, suffix)
  }
  if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
  check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
  check.df <- check.list[[1]]
  check_without_errors <- check.list[[2]]
  check_without_warnings <- check.list[[3]]




#TB

  checkName = "Check dictionary"
  table <- "TB"
  Field = "COUNTRY"
  Values = as.character(unique(Stratification$COUNTRY))
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
    check_without_errors = check_dictionary(ResultData = ResultDataTB, Field, Values, year=yea, wd, suffix)
  }
  if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
  check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
  check.df <- check.list[[1]]
  check_without_errors <- check.list[[2]]
  check_without_warnings <- check.list[[3]]


  Field = "AREA"
  Values = as.character(unique(Stratification$GSA))
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
    check_without_errors = check_dictionary(ResultData = ResultDataTA, Field, Values, year=yea, wd, suffix)
  }
  if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
  check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
  check.df <- check.list[[1]]
  check_without_errors <- check.list[[2]]
  check_without_warnings <- check.list[[3]]


  Field = "MONTH"
  Values = seq(1,12,1)
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
    check_without_errors = check_dictionary(ResultData = ResultDataTB, Field, Values, year=yea, wd, suffix)
  }
  if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
  check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
  check.df <- check.list[[1]]
  check_without_errors <- check.list[[2]]
  check_without_warnings <- check.list[[3]]


  Field = "DAY"
  Values = seq(1,31,1)
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
    check_without_errors = check_dictionary(ResultData = ResultDataTB, Field, Values, year=yea, wd, suffix)
  }
  if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
  check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
  check.df <- check.list[[1]]
  check_without_errors <- check.list[[2]]
  check_without_warnings <- check.list[[3]]


  Field = "HAUL_NUMBER"
  Values = seq(1,999,1)
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
    check_without_errors = check_dictionary(ResultData = ResultDataTB, Field, Values, year=yea, wd, suffix)
  }
  if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
  check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
  check.df <- check.list[[1]]
  check_without_errors <- check.list[[2]]
  check_without_warnings <- check.list[[3]]


Field = "CODEND_CLOSING"
Values = c("S","C")
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTB, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]



Field = "PART_OF_THE_CODEND"
Values = c("A", "M", "P", "S")
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTB, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


checkName = "Check numeric range"
Field = "TOTAL_WEIGHT_IN_THE_HAUL"
Values = c(0,9999999)
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_numeric_range(Data = ResultDataTB, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


Field = "TOTAL_NUMBER_IN_THE_HAUL"
Values = c(0,9999999)
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_numeric_range(Data = ResultDataTB, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


Field = "NB_OF_FEMALES"
Values = c(0,9999999)
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_numeric_range(Data = ResultDataTB, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


Field = "NB_OF_MALES"
Values = c(0,9999999)
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_numeric_range(Data = ResultDataTB, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


Field = "NB_OF_UNDETERMINED"
Values = c(0,9999999)
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_numeric_range(Data = ResultDataTB, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]



#TC
checkName = "Check dictionary"
table <- "TC"
Field = "COUNTRY"
Values = as.character(unique(Stratification$COUNTRY))
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTC, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


Field = "AREA"
Values = as.character(unique(Stratification$GSA))
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTC, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


Field = "MONTH"
Values = seq(1,12,1)
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTC, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


Field = "DAY"
Values = seq(1,31,1)
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTC, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


Field = "HAUL_NUMBER"
Values = seq(1,999,1)
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTC, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


Field = "CODEND_CLOSING"
Values = c("S","C")
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTC, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


Field = "PART_OF_THE_CODEND"
Values = c("A", "M", "P", "S")
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTC, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


Field = "LENGTH_CLASSES_CODE"
Values = c("m", "0", "1")
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTC, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


checkName = "Check numeric range"
Field = "WEIGHT_OF_THE_FRACTION"
Values = c(0,999999)
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_numeric_range(Data = ResultDataTC, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


Field = "WEIGHT_OF_THE_SAMPLE_MEASURED"
Values = c(0,999999)
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_numeric_range(Data = ResultDataTC, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


Field = "NO_OF_INDIVIDUAL_OF_THE_ABOVE_SEX_MEASURED"
Values = c(0,999999)
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_numeric_range(Data = ResultDataTC, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


Field = "NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE"
Values = c(0,999999)
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_numeric_range(Data = ResultDataTC, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


checkName = "Check dictionary"
Field = "SEX"
Values = c("M", "F", "I", "N")
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTC, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


Field = "LENGTH_CLASS"
Values = c(1:9999)
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
  check_without_errors = check_dictionary(ResultData = ResultDataTC, Field, Values, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


#TE
checkName = "Check dictionary"
table <- "TE"
  if (!(all(is.na(TE)) & length(TE)==1)){
  if (nrow(ResultDataTE)>0){
    Field = "COUNTRY"
    Values = as.character(unique(Stratification$COUNTRY))
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_dictionary(ResultData = ResultDataTE, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
    check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
    check.df <- check.list[[1]]
    check_without_errors <- check.list[[2]]
    check_without_warnings <- check.list[[3]]


    Field = "AREA"
    Values = as.character(unique(Stratification$GSA))
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_dictionary(ResultData = ResultDataTE, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
    check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
    check.df <- check.list[[1]]
    check_without_errors <- check.list[[2]]
    check_without_warnings <- check.list[[3]]


    Field = "MONTH"
    Values = seq(1,12,1)
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_dictionary(ResultData = ResultDataTE, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
    check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
    check.df <- check.list[[1]]
    check_without_errors <- check.list[[2]]
    check_without_warnings <- check.list[[3]]


    Field = "DAY"
    Values = seq(1,31,1)
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_dictionary(ResultData = ResultDataTE, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
    check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
    check.df <- check.list[[1]]
    check_without_errors <- check.list[[2]]
    check_without_warnings <- check.list[[3]]


    Field = "HAUL_NUMBER"
    Values = seq(1,999,1)
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_dictionary(ResultData = ResultDataTE, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
    check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
    check.df <- check.list[[1]]
    check_without_errors <- check.list[[2]]
    check_without_warnings <- check.list[[3]]


    Field = "LENGTH_CLASSES_CODE"
    Values = c("0","m","1")
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_dictionary(ResultData = ResultDataTE, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
    check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
    check.df <- check.list[[1]]
    check_without_errors <- check.list[[2]]
    check_without_warnings <- check.list[[3]]


    Field = "SEX"
    Values = c("M", "F", "I", "N")
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_dictionary(ResultData = ResultDataTE, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
    check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
    check.df <- check.list[[1]]
    check_without_errors <- check.list[[2]]
    check_without_warnings <- check.list[[3]]


    checkName = "Check numeric range"
    Field = "NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_OTOLITH"
    Values = c(0,999999)
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_numeric_range(Data = ResultDataTE, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
    check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
    check.df <- check.list[[1]]
    check_without_errors <- check.list[[2]]
    check_without_warnings <- check.list[[3]]


    Field = "NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_WEIGHT"
    Values = c(1,999999)
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_numeric_range(Data = ResultDataTE, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
    check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
    check.df <- check.list[[1]]
    check_without_errors <- check.list[[2]]
    check_without_warnings <- check.list[[3]]


    Field = "NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_AGEING"
    Values = c(0,999999)
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_numeric_range(Data = ResultDataTE, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
    check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
    check.df <- check.list[[1]]
    check_without_errors <- check.list[[2]]
    check_without_warnings <- check.list[[3]]


    Field = "RECORD_NUMBER"
    Values = c(0,100000)
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_numeric_range(Data = ResultDataTE, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
    check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
    check.df <- check.list[[1]]
    check_without_errors <- check.list[[2]]
    check_without_warnings <- check.list[[3]]


    checkName = "Check dictionary"

    Field = "LENGTH_CLASS"
    Values = c(1:9999)
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_dictionary(ResultData = ResultDataTE, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
    check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
    check.df <- check.list[[1]]
    check_without_errors <- check.list[[2]]
    check_without_warnings <- check.list[[3]]


    Field = "MATURITY"
    Values = c(0:4)
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_dictionary(ResultData = ResultDataTE, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
    check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
    check.df <- check.list[[1]]
    check_without_errors <- check.list[[2]]
    check_without_warnings <- check.list[[3]]


    Field = "MATSUB"
    Values = c("A","B","C","D","E","O")
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_dictionary(ResultData = ResultDataTE, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
    check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
    check.df <- check.list[[1]]
    check_without_errors <- check.list[[2]]
    check_without_warnings <- check.list[[3]]

    Field = "OTOLITH_SAMPLED"
    Values = c("Y","N","NR")
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_dictionary(ResultData = ResultDataTE, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
    check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
    check.df <- check.list[[1]]
    check_without_errors <- check.list[[2]]
    check_without_warnings <- check.list[[3]]


    Field = "OTOLITH_READ"
    Values = c("Y","N","NR")
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_dictionary(ResultData = ResultDataTE, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
    check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
    check.df <- check.list[[1]]
    check_without_errors <- check.list[[2]]
    check_without_warnings <- check.list[[3]]

    Field = "AGE"
    Values = c(seq(-1,99,0.1),"UR","NR")
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
      check_without_errors = check_dictionary(ResultData = ResultDataTE, Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
    check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
    check.df <- check.list[[1]]
    check_without_errors <- check.list[[2]]
    check_without_warnings <- check.list[[3]]

  }
  }

  # TL
table <- "TL"
  if (!(all(is.na(TL)) & length(TL)==1)){
    if (nrow(ResultDataTL)>0){
      checkName = "Check dictionary"
      Field = "COUNTRY"
      Values = as.character(unique(Stratification$COUNTRY))
      if (check_without_errors == TRUE) {
        if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
        check_without_errors = check_dictionary(ResultData = ResultDataTL, Field, Values, year=yea, wd, suffix)
      }
      if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
      check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
      check.df <- check.list[[1]]
      check_without_errors <- check.list[[2]]
      check_without_warnings <- check.list[[3]]


      Field = "AREA"
      Values = as.character(unique(Stratification$GSA))
      if (check_without_errors == TRUE) {
        if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
        check_without_errors = check_dictionary(ResultData = ResultDataTL, Field, Values, year=yea, wd, suffix)
      }
      if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
      check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
      check.df <- check.list[[1]]
      check_without_errors <- check.list[[2]]
      check_without_warnings <- check.list[[3]]


      Field = "MONTH"
      Values = seq(1,12,1)
      if (check_without_errors == TRUE) {
        if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
        check_without_errors = check_dictionary(ResultData = ResultDataTL, Field, Values, year=yea, wd, suffix)
      }
      if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
      check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
      check.df <- check.list[[1]]
      check_without_errors <- check.list[[2]]
      check_without_warnings <- check.list[[3]]



      Field = "DAY"
      Values = seq(1,31,1)
      if (check_without_errors == TRUE) {
        if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
        check_without_errors = check_dictionary(ResultData = ResultDataTL, Field, Values, year=yea, wd, suffix)
      }
      if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
      check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
      check.df <- check.list[[1]]
      check_without_errors <- check.list[[2]]
      check_without_warnings <- check.list[[3]]


      Field = "HAUL_NUMBER"
      Values = seq(1,999,1)
      if (check_without_errors == TRUE) {
        if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
        check_without_errors = check_dictionary(ResultData = ResultDataTL, Field, Values, year=yea, wd, suffix)
      }
      if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
      check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
      check.df <- check.list[[1]]
      check_without_errors <- check.list[[2]]
      check_without_warnings <- check.list[[3]]


      checkName = "Check numeric range"
      Field = "TOTAL_WEIGHT_IN_THE_CATEGORY_HAUL"
      Values = c(0,9999999)
      if (check_without_errors == TRUE) {
        if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
        check_without_errors = check_numeric_range(Data = ResultDataTL, Field, Values, year=yea, wd, suffix)
      }
      if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
      check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
      check.df <- check.list[[1]]
      check_without_errors <- check.list[[2]]
      check_without_warnings <- check.list[[3]]



      checkName = "Check numeric range"
      Field = "TOTAL_NUMBER_IN_THE_CATEGORY_HAUL"
      Values = c(0,9999999)
      if (check_without_errors == TRUE) {
        if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
        check_without_errors = check_numeric_range(Data = ResultDataTL, Field, Values, year=yea, wd, suffix)
      }
      if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
      check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
      check.df <- check.list[[1]]
      check_without_errors <- check.list[[2]]
      check_without_warnings <- check.list[[3]]


      checkName = "Check numeric range"
      Field = "TOTAL_WEIGHT_IN_THE_SUB-CATEGORY_HAUL"
      Values = c(0,9999999)
      if (check_without_errors == TRUE) {
        if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
        check_without_errors = check_numeric_range(Data = ResultDataTL, Field, Values, year=yea, wd, suffix)
      }
      if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
      check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
      check.df <- check.list[[1]]
      check_without_errors <- check.list[[2]]
      check_without_warnings <- check.list[[3]]


      checkName = "Check numeric range"
      Field = "TOTAL_NUMBER_IN_THE_SUB-CATEGORY_HAUL"
      Values = c(0,9999999)
      if (check_without_errors == TRUE) {
        if(verbose){print(paste(checkName,Field,"in progress..."), quote = FALSE)}
        check_without_errors = check_numeric_range(Data = ResultDataTL, Field, Values, year=yea, wd, suffix)
      }
      if(verbose){stop_ = printError_cc(paste(checkName,Field),check_without_errors, stop_)}
      check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
      check.df <- check.list[[1]]
      check_without_errors <- check.list[[2]]
      check_without_warnings <- check.list[[3]]


    }
    }

# End dictionary checks
#-------------------------------------

Field <- NA
#-------------------------------------
# Check no empty fields
table <- "TA"
checkName = "Check no empty fields TA"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_no_empty_fields(ResultDataTA,year=yea, wd, suffix)
}
  if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]



table <- "TB"
checkName =  "Check no empty fields TB"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_no_empty_fields(ResultDataTB, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


table <- "TC"
checkName = "Check no empty fields TC"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_no_empty_fields(ResultDataTC,year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


table <- "TE"
if (!(all(is.na(TE)) & length(TE)==1)) {
   if (nrow(ResultDataTE)>0){
  checkName = "Check no empty fields TE"
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
    check_without_errors = check_no_empty_fields(ResultDataTE, year=yea, wd, suffix)
  }
  if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
  check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
  check.df <- check.list[[1]]
  check_without_errors <- check.list[[2]]
  check_without_warnings <- check.list[[3]]
  }
}


table <- "TL"
if (!(all(is.na(TL)) & length(TL)==1)) {
  if (nrow(ResultDataTL)>0){
    checkName = "Check no empty fields TL"
    if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
      check_without_errors = check_no_empty_fields(ResultDataTL, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
    check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
    check.df <- check.list[[1]]
    check_without_errors <- check.list[[2]]
    check_without_warnings <- check.list[[3]]
  }
}

#-------------------------------------
# checks on TA
table <- "TA"

checkName = "Check 0 fields TA"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_0_fieldsTA(ResultDataTA,wd,suffix, year=yea)
}
if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]



checkName = "Check dm TA"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_dm(ResultDataTA,year=yea,wd,suffix)
}
if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]



checkName = "Check consistency between duration and time TA"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_consistencyTA_duration(ResultDataTA, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


checkName = "Check consistency between distance and duration of the haul TA"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_consistencyTA_distance(ResultDataTA,year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]



checkName = "Check consistency of the hauls coordinates with the distance (difference not greater than 30%)"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_distance(ResultDataTA,year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]



checkName = "Check consistency of bridles length TA"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_bridles_length(ResultDataTA, year=yea, wd, suffix)
}
if(verbose){stop_ =printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]



checkName = "Check difference between start depth and end depth (not greater than 20%) in TA"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_depth(ResultDataTA, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]



checkName = "Check start depth and end depth in the same stratum TA"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_stratum(ResultDataTA,year=yea,wd,suffix)
}
if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]



checkName = "Check consistency of stratum code in TA"
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
    check_without_errors = check_stratum_code(ResultDataTA,year=yea,Strata=Stratification,wd,suffix)
  }
if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]



checkName = "Check start quadrant and end quadrant TA"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_quadrant(ResultDataTA,year=yea,wd,suffix)
}
if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]



checkName = "Check uniqueness of valid hauls TA"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_unique_valid_haul(ResultDataTA,year=yea,wd,suffix)
}
if(verbose){stop_ =printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]



checkName = "Visual check of the haul positions"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_position(ResultDataTA,year=yea,wd,suffix)
}
if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]



checkName = "Relation between shooting depth and warp length, and between warp length and wing opening"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  graphs_TA(ResultDataTA,year=yea,wd,suffix)
}
if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]



checkName = "Check if the coordinates are in the Mediterranean or Black Sea"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_position_in_Med(ResultDataTA,year=yea,wd,suffix)
}
if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]



checkName = "Check on temperature by haul"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_temperature(ResultDataTA,year=yea,wd,suffix)
}
if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]



# checks on TB
table <- "TB"
checkName = "Check correctness of species codes TB"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_rubincode(ResultDataTB,year=yea,TM_list=Ref_list,wd,suffix)
}
  if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]



checkName = "Check consistency of NB_TOTAL and number per sex TB"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_nbtotTB(ResultDataTB,year=yea,wd,suffix)
}
  if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]



checkName = "Check presence of NB_TOTAL and number per sex TB for species G1"
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
    check_without_errors = check_nm_TB(ResultDataTB, year=yea,wd,suffix)
  }
  if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]



checkName = "Check consistency between not null weight and not null total number"
if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
    check_without_errors = check_weight_tot_nb(ResultDataTB,year=yea,wd,suffix)
  }
  if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]



checkName = "Check consistency of weight and number TB"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_weight(ResultDataTB,year=yea,DataTargetSpecies=RoME::DataTargetSpecies,wd,suffix)
}
  if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]

graphics.off()

# checks on TC
table <- "TC"
checkName = "Check correctness of species codes TC"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_rubincode(ResultDataTC,year=yea,TM_list=Ref_list,wd,suffix)
}
if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]



checkName = "Check correctness of LENGTH_CLASSES_CODE TC"
if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
      check_without_errors = check_length_class_codeTC(ResultDataTC,Specieslist=Ref_list,year=yea, wd,suffix)
  }
  if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]



checkName = "Check consistency of length classes TC"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_length(ResultDataTC,DataSpecies=NA,year=yea,wd,suffix)
}
  if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]



checkName = "Check correctness of number per sex in TC"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_nb_per_sexTC(ResultDataTC,year=yea,wd,suffix)
}
  if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


checkName = "Check consistency of maturity stages TC"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
      check_without_errors = check_mat_stages(ResultDataTC,year=yea, wd, suffix, stages=stages_list)
  }
  if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
  check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
  check.df <- check.list[[1]]
  check_without_errors <- check.list[[2]]
  check_without_warnings <- check.list[[3]]



checkName = "Check sub-sampling"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_subsampling(ResultDataTC,year=yea,wd,suffix)
}
  if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]



checkName = "Check consistency of maturity stages TC by the comparison with the length of smallest mature individuals reported in bibliography"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_smallest_mature(ResultDataTC,year=yea,Maturity_parameters=Maturity,DataTargetSpecies=DataTargetSpecies,wd,suffix)
}
  if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]



# checkName = "Check consistency of sex TC by means of spawning period"
# if (check_without_errors == TRUE) {
#   if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
#   check_without_errors = check_spawning_period(ResultDataTA,ResultDataTC,year=yea,Maturity_parameters=Maturity,DataTargetSpecies=DataTargetSpecies,wd,suffix)
# }
#   if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
# check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
# check.df <- check.list[[1]]
# check_without_errors <- check.list[[2]]
# check_without_warnings <- check.list[[3]]



# checkName = "Check consistency of sex data TC by means of sex-inversion size"
# if (check_without_errors == TRUE) {
#   if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
#   check_without_errors = check_sex_inversion(ResultDataTC,Maturity_parameters,wd,suffix)
# }
#   if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
# check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
# check.df <- check.list[[1]]
# check_without_errors <- check.list[[2]]
# check_without_warnings <- check.list[[3]]



checkName = "Check consistency of length distribution TC"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_step_length_distr(ResultDataTC,year=yea,wd,suffix)
}
  if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]



checkName = "Check total weight in the haul in TC"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_individual_weightTC(ResultDataTC,LW=ab_parameters,year=yea,wd,suffix, verbose=FALSE)
}
  if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]



checkName = "Check correctness of species codes TC"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_rubincode(ResultDataTC,year=yea,TM_list=Ref_list,wd,suffix)
}
  if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]



checkName = "Check presence of lengths for G1 and G2 Medits species in TC"
if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
    check_without_errors = check_G1_G2(ResultDataTC, year=yea, wd, suffix)
  }
    if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]



# cross checks
table <- "TB-TA"
checkName = "Check presence in TB of TA hauls"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_hauls_TATB(ResultDataTA,ResultDataTB,year=yea,wd,suffix)
}
if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


table <- "TA-TB"
checkName = "Check presence in TA of TB hauls"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_hauls_TBTA(ResultDataTA,ResultDataTB,year=yea,wd,suffix)
}
  if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


table <- "TC-TB"
checkName = "Check presence in TC of TB target species"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_species_TBTC(ResultDataTB,ResultDataTC,year=yea,DataSpecies=DataTargetSpecies,wd,suffix)
}
  if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


table <- "TB-TC"
checkName = "Check presence in TB of TC species"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_haul_species_TCTB(ResultDataTB,ResultDataTC,year=yea,wd, suffix)
}
if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]



checkName = "Check correctness of the number per sex in TB in case of sub-sampling in TC  "
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_raising(ResultDataTB,ResultDataTC,year=yea,wd,suffix)
}
  if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


table <- "TA-TB"
checkName = "Check on date by haul TB"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_date_haul(ResultDataTA, ResultDataTB, year=yea, wd, suffix)
}
if(verbose){stop_ =printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


table <- "TA-TC"
checkName = "Check on date by haul TC"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_date_haul(ResultDataTA, ResultDataTC, year=yea, wd, suffix)
}
if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


#Check on TE

if (!(all(is.na(TE)) & length(TE)==1)) {
  if (nrow(ResultDataTE)>0){

  table <- "TA-TE"
  checkName = "Check on date by haul TE"
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
    check_without_errors = check_date_haul(ResultDataTA,ResultDataTE, year=yea,wd,suffix)
  }
    if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
  check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
  check.df <- check.list[[1]]
  check_without_errors <- check.list[[2]]
  check_without_warnings <- check.list[[3]]


  table <- "TE"
  checkName = "Check consistency of maturity stages TE"
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
    check_without_errors = check_mat_stages(ResultDataTE,year=yea, wd, suffix, stages = stages_list)
  }
  if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
  check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
  check.df <- check.list[[1]]
  check_without_errors <- check.list[[2]]
  check_without_warnings <- check.list[[3]]



  table <- "TC-TE"
  checkName = "Cross check on number between TC and TE"
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
    check_without_errors = check_TE_TC(ResultDataTC,ResultDataTE,year=yea,wd,suffix)
  }
    if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
  check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
  check.df <- check.list[[1]]
  check_without_errors <- check.list[[2]]
  check_without_warnings <- check.list[[3]]


  table <- "TE"
  checkName = "Check consistency of maturity stages TE by the comparison with the length of smallest mature individuals reported in bibliography"
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
    check_without_errors = check_smallest_mature(ResultDataTE,year=yea,Maturity_parameters=Maturity,DataTargetSpecies=DataTargetSpecies,wd,suffix)
  }
    if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
  check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
  check.df <- check.list[[1]]
  check_without_errors <- check.list[[2]]
  check_without_warnings <- check.list[[3]]

  # table <- "TA-TE"
  # checkName = "Check consistency of maturity stages in TE by means of spawning period"
  # if (check_without_errors == TRUE) {
  #   if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  #   check_without_errors = check_spawning_period(ResultDataTA,ResultDataTE,year=yea,Maturity_parameters=Maturity,DataTargetSpecies=DataTargetSpecies,wd,suffix)
  # }
  #   if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
  # check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
  # check.df <- check.list[[1]]
  # check_without_errors <- check.list[[2]]
  # check_without_warnings <- check.list[[3]]

  table <- "TE"
  checkName = "Check individual weight in TE"
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
    check_without_errors = check_individual_weightTE(ResultDataTE,LW=ab_parameters,year=yea,wd,suffix,verbose=FALSE)
  }
  if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
  check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
  check.df <- check.list[[1]]
  check_without_errors <- check.list[[2]]
  check_without_warnings <- check.list[[3]]



  checkName = "Check correctness of species codes TE"
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
    check_without_errors = check_rubincode(ResultDataTE,year=yea,TM_list=Ref_list,wd,suffix)
  }
  if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
  check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
  check.df <- check.list[[1]]
  check_without_errors <- check.list[[2]]
  check_without_warnings <- check.list[[3]]



  checkName = "Check consistency TE check-fields"
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
    check_without_errors = check_nb_TE(ResultDataTE,year=yea,wd,suffix)
  }
  if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
  check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
  check.df <- check.list[[1]]
  check_without_errors <- check.list[[2]]
  check_without_warnings <- check.list[[3]]


  table <- "TC-TE"
  checkName = "Summary individual data sampling"
  if (check_without_errors == TRUE) {
    if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
    check_without_errors = scheme_individual_data(DataTC=ResultDataTC,DataTE=ResultDataTE,year=yea,wd,suffix)
  }
  if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
  check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
  check.df <- check.list[[1]]
  check_without_errors <- check.list[[2]]
  check_without_warnings <- check.list[[3]]

    }
  }



# Checks on TL
if (!(all(is.na(TL)) & length(TL)==1)) {
  if (nrow(ResultDataTL)>0){


table <- "TL"
checkName = "Check allowed values for category on Litter data"
Field = "LITTER_CATEGORY"
Values = c("L0","L1","L2","L3","L4","L5","L6","L7","L8","L9")
if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
      check_without_errors = check_dictionary(ResultData=ResultDataTL,Field, Values, year=yea, wd, suffix)
    }
      if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]



checkName = "Check allowed values for sub-category on Litter data"
Field = "LITTER_SUB-CATEGORY"
Values = c("0","A","B","C","D","E","F","G","H","I","J","a","b","c","d","e","f","g","h","i","j")
colnames(ResultDataTL)[10]="LITTER_SUB-CATEGORY"

if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
      check_without_errors = check_dictionary(ResultData=ResultDataTL,Field, Values, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]

Field <- NA

checkName = "Check correctness of associations between category and sub-category on Litter data"
if (check_without_errors == TRUE) {
      if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
      check_without_errors = check_associations_category_TL(ResultDataTL,assTL, year=yea, wd, suffix)
    }
    if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]



# checkName = "Check if the number is always filled in on Litter data"
# if (check_without_errors == TRUE) {
#       if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
#       check_without_errors = check_no_empty_fields(ResultDataTL,year=yea,wd,suffix)
#     }
#     if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
# check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
# check.df <- check.list[[1]]
# check_without_errors <- check.list[[2]]
# check_without_warnings <- check.list[[3]]


table <- "TA-TL"
checkName = "Check if the hauls in TL are present in TA"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_hauls_TLTA(ResultDataTA,ResultDataTL,year=yea,wd,suffix)
}
if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]



table <- "TA-TL"
checkName = "Check if the hauls in TA are present in TL"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
  check_without_errors = check_hauls_TATL(ResultDataTA,ResultDataTL,year=yea,wd,suffix)
}
if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


table <- "TA-TL"
checkName = "Check if the date in TL is consistent with TA"
if (check_without_errors == TRUE) {
  if(verbose){print(paste(checkName,"in progress..."), quote = FALSE)}
    check_without_errors = check_date_haul(ResultDataTA, ResultDataTL, year=yea, wd, suffix)
  }
  if(verbose){stop_ = printError_cc(checkName,check_without_errors, stop_)}
check.list <- error.table(check.df,check_without_errors,check_without_warnings,checkName,table,Field,yea)
check.df <- check.list[[1]]
check_without_errors <- check.list[[2]]
check_without_warnings <- check.list[[3]]


   }
}


if (!stop_) {
  if(verbose){print("All the checks have been performed!",quote=FALSE)}
}



# # Create files for R_Sufi
#
# if ((!stop_) & (create_RSufi_files==TRUE)){
#   AREA <- ResultDataTA[1,"AREA"]
#   create_strata(Stratification=Stratification,AREA,wd,save=TRUE)
#   create_haul(ResultDataTA,year=yea,wd,save=TRUE)
#   create_catch(ResultDataTB,year=yea,wd,save=TRUE)
#
#
#   # if (!(all(is.na(TE)) & length(TE)==1)) {
#   #   if (nrow(ResultDataTE)>0){
#   #       create_length(ResultDataTE,DataSpecies=Ref_list,wd)
#   #   }
#   # }
#
#     if (nrow(ResultDataTC)>0){
#       create_length(ResultDataTC,year=yea,DataSpecies=Ref_list,wd,save=TRUE)
#     }
#
#   print(paste("R-Sufi files have been created for the ",yea, "and the GSA selected! They have been stored in files R-Sufi directory."),quote=FALSE)
# }



} # ciclo years
} # year_without_error | TYPE_OF_FILE_without_error are FALSE
} # check_header
# END ---------------------------------------------------------------------




  # -------------------------------------------------------
  # -------------------------------------------------------
  # -------------------------------------------------------
  # -------------------------------------------------------


  # if ((!stop_) & (create_global_RSufi_files==TRUE)){
  #   if (is.na(Year_start) | is.na(Year_end)){
  #     warning("One or both variables Year_start and Year_end not declared")
  #   } else {
  #     RSufi_files(Year_start,Year_end,AREA,wd)
  #     print("R-Sufi files have been created for the Years and the GSA selected! They have been stored in files R-Sufi directory.",quote=FALSE)
  #   }
  # }
  # -------------------------------------------------------

write.table(check.df, file.path(wd, paste0("ERRORS_summary_",suffix,".csv")), sep=";", row.names=FALSE)

  ## save zip file of the outputs
if (wd != tempdir()){
  old_wd <- getwd()
  setwd(wd)
  files2zip <- dir(wd, full.names = TRUE, include.dirs = TRUE, recursive=T) # list.files(wd) #
  exclude <- grep(".zip",files2zip)
  if (length(exclude)>0){
    files2zip <- files2zip[-exclude]
  }
  named<-paste0(suffix,".zip")
  utils::zip(zipfile = file.path(wd,named), files = files2zip)
  setwd(old_wd)
}

if (file.exists(file.path(tempdir(), "Logfiles"))){
  unlink(file.path(tempdir(),"Logfiles"),recursive=T)
  }
  if (file.exists(file.path(tempdir(), "Graphs"))){
  unlink(file.path(tempdir(),"Graphs"),recursive=T)
  }
# 	if (file.exists(file.path(tempdir(), "files R-Sufi"))){
#   unlink(file.path(tempdir(),"files R-Sufi"),recursive=T)
#     }



return(check.df)

} # funzione RoME

