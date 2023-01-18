############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files)                                #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.it #
#   January 2022                                                                                                           #
############################################################################################################################
# Check consistency between not null weight and not null total number

check_weight_tot_nb<-function(ResultDataTB,year,wd,suffix){

if (FALSE){
  wd <- tempdir()
  ResultDataTB = tb # RoME::TB
  year=2015
  suffix=NA
  check_weight_tot_nb(ResultDataTB,year,wd,suffix)

}

   oldpar <- par(no.readonly = TRUE)
   on.exit(suppressWarnings(par(oldpar)))

  if (!file.exists(file.path(wd,"Logfiles"))){
    dir.create(file.path(wd, "Logfiles"), recursive = TRUE, showWarnings = FALSE)
  }

  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
  }

  if (!file.exists(file.path(wd,"Graphs"))){
    dir.create(file.path(wd, "Graphs"), showWarnings = FALSE)
  }


  Errors <- file.path(wd,"/Logfiles",paste("Logfile_",suffix,".dat",sep=""))
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
  ResultDataTB <- ResultDataTB[ResultDataTB$YEAR == year, ]
  ########################################

  ResultDataTB <- ResultDataTB[!is.na(ResultDataTB$TOTAL_WEIGHT_IN_THE_HAUL) & !is.na(ResultDataTB$TOTAL_NUMBER_IN_THE_HAUL),]

  numberError = 0
  ResultData = ResultDataTB #read.csv(paste(DataTB,".csv",sep=""), sep=";", header=TRUE)
  write(paste("
              ----------- check consistency between not null weight and not null total number in TB - ",ResultData$YEAR[1]), file = Errors, append = TRUE)

  for (i in 1:nrow(ResultData)){

      if ((ResultData$TOTAL_WEIGHT_IN_THE_HAUL[i]==0) & (ResultData$TOTAL_NUMBER_IN_THE_HAUL[i]!=0)){

        write(paste("Warning: Haul ",ResultData$HAUL_NUMBER[i]," species ",ResultData$GENUS[i],ResultData$SPECIES[i]," Total weight equals 0, but total number is not null ", sep=""), file = Errors, append = TRUE)
      }



    if ((ResultData$TOTAL_NUMBER_IN_THE_HAUL[i]==0) & (ResultData$TOTAL_WEIGHT_IN_THE_HAUL[i]!=0) &
          (   ((str_extract(as.character(ResultData$FAUNISTIC_CATEGORY[i]),"[A-Z]"))!="E") &
                (  (str_extract(as.character(ResultData$FAUNISTIC_CATEGORY[i]),"[A-Z]"))!="D") &
                (  (str_extract(as.character(ResultData$FAUNISTIC_CATEGORY[i]),"[A-Z]"))!="V") &
                (  (str_extract(as.character(ResultData$FAUNISTIC_CATEGORY[i]),"[A-Z]"))!="G") &
                (  (str_extract(as.character(ResultData$FAUNISTIC_CATEGORY[i]),"[A-Z]"))!="H")  )){

      write(paste("Warning: Haul ",ResultData$HAUL_NUMBER[i]," species ",ResultData$GENUS[i],ResultData$SPECIES[i]," Total number equals 0, but total weight is not null", sep=""), file = Errors, append = TRUE)
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

  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }

}

################################################################################
