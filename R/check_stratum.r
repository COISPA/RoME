############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files)                                #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.it #
#   January 2022                                                                                                           #
############################################################################################################################

# Start depth and end depth of each haul should be in the same stratum

utils::globalVariables("stratification_scheme")

if (FALSE){
  ResultData = read.table("D:\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\QualiTrain\\Task 2\\Data\\MEDITS\\ROU_Med&BS_MEDITS_TA(rev).csv",sep=";", header=TRUE)# RoME::TA
  wd <- "D:\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\QualiTrain\\Task 2\\Data\\MEDITS"
  suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
  year=2019
  check_stratum(ResultData,year,wd,suffix)
}


check_stratum<-function(ResultData,year,wd,suffix){

  Format="from_2012"
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
    stop(paste0("'", arg, "' argument should be provided"))
  } else if (length(year) != 1) {
    stop(paste0("only one value should be provided for '", arg, "' argument"))
  } else if (is.na(year)) {
    stop(paste0(arg, " argument should be a numeric value"))
  }
  ResultData <- ResultData[ResultData$YEAR == year, ]
  ########################################

  #ResultData = read.csv(paste(Data,".csv",sep=""), sep=";", header=TRUE)
  write(paste("\n----------- check start depth and end depth in the same stratum TA - ",ResultData$YEAR[1]), file = Errors, append = TRUE)

  ResultData=ResultData[ResultData$VALIDITY=="V",]
  ResultData$stratum_s <- NA
  ResultData$stratum_e <- NA
  GSA  <- unique(ResultData$AREA)[1]
  strata <- stratification_scheme[stratification_scheme$GSA == GSA, ]



  i=1
  for (i in 1:nrow(ResultData)){
    if (
      1 %in% strata[strata$COUNTRY==ResultData$COUNTRY[i],"CODE"] &
        (ResultData$SHOOTING_DEPTH[i]>= strata[strata$COUNTRY==ResultData$COUNTRY[i] & strata$CODE==1,"MIN_DEPTH"][1]) &
        (ResultData$SHOOTING_DEPTH[i]<=strata[strata$COUNTRY==ResultData$COUNTRY[i] & strata$CODE==1,"MAX_DEPTH"][1])
        ){
      ResultData$stratum_s[i]="1"
    } else if (
      2 %in% strata[strata$COUNTRY==ResultData$COUNTRY[i],"CODE"] &
      (ResultData$SHOOTING_DEPTH[i]>= strata[strata$COUNTRY==ResultData$COUNTRY[i] & strata$CODE==2,"MIN_DEPTH"][1]) &
      (ResultData$SHOOTING_DEPTH[i]<=strata[strata$COUNTRY==ResultData$COUNTRY[i] & strata$CODE==2,"MAX_DEPTH"][1])
      ){
      ResultData$stratum_s[i]="2"
    } else if (3 %in% strata[strata$COUNTRY==ResultData$COUNTRY[i],"CODE"] & (ResultData$SHOOTING_DEPTH[i]>= strata[strata$COUNTRY==ResultData$COUNTRY[i] & strata$CODE==3,"MIN_DEPTH"][1]) & (ResultData$SHOOTING_DEPTH[i]<=strata[strata$COUNTRY==ResultData$COUNTRY[i] & strata$CODE==3,"MAX_DEPTH"][1])){
      ResultData$stratum_s[i]="3"
    } else if (4 %in% strata[strata$COUNTRY==ResultData$COUNTRY[i],"CODE"] & (ResultData$SHOOTING_DEPTH[i]>= strata[strata$COUNTRY==ResultData$COUNTRY[i] & strata$CODE==4,"MIN_DEPTH"][1]) & (ResultData$SHOOTING_DEPTH[i]<=strata[strata$COUNTRY==ResultData$COUNTRY[i] & strata$CODE==4,"MAX_DEPTH"][1])){
      ResultData$stratum_s[i]="4"
    } else if (5 %in% strata[strata$COUNTRY==ResultData$COUNTRY[i],"CODE"] & (ResultData$SHOOTING_DEPTH[i]>= strata[strata$COUNTRY==ResultData$COUNTRY[i] & strata$CODE==5,"MIN_DEPTH"][1]) & (ResultData$SHOOTING_DEPTH[i]<=strata[strata$COUNTRY==ResultData$COUNTRY[i] & strata$CODE==5,"MAX_DEPTH"][1])){
      ResultData$stratum_s[i]="5"
    }
  }

  for (j in 1:nrow(ResultData)){
    if (1 %in% strata[strata$COUNTRY==ResultData$COUNTRY[j],"CODE"] & (ResultData$HAULING_DEPTH[j]>= strata[strata$COUNTRY==ResultData$COUNTRY[j] & strata$CODE==1,"MIN_DEPTH"][1]) & (ResultData$HAULING_DEPTH[j]<=strata[strata$COUNTRY==ResultData$COUNTRY[j] & strata$CODE==1,"MAX_DEPTH"][1])){
      ResultData$stratum_e[j]="1"
    } else if (2 %in% strata[strata$COUNTRY==ResultData$COUNTRY[j],"CODE"] & (ResultData$HAULING_DEPTH[j]>= strata[strata$COUNTRY==ResultData$COUNTRY[j] & strata$CODE==2,"MIN_DEPTH"][1]) & (ResultData$HAULING_DEPTH[j]<=strata[strata$COUNTRY==ResultData$COUNTRY[j] & strata$CODE==2,"MAX_DEPTH"][1])){
      ResultData$stratum_e[j]="2"
    } else if (3 %in% strata[strata$COUNTRY==ResultData$COUNTRY[j],"CODE"] & (ResultData$HAULING_DEPTH[j]>= strata[strata$COUNTRY==ResultData$COUNTRY[j] & strata$CODE==3,"MIN_DEPTH"][1]) & (ResultData$HAULING_DEPTH[j]<=strata[strata$COUNTRY==ResultData$COUNTRY[j] & strata$CODE==3,"MAX_DEPTH"][1])){
      ResultData$stratum_e[j]="3"
    } else if (4 %in% strata[strata$COUNTRY==ResultData$COUNTRY[j],"CODE"] & (ResultData$HAULING_DEPTH[j]>= strata[strata$COUNTRY==ResultData$COUNTRY[j] & strata$CODE==4,"MIN_DEPTH"][1]) & (ResultData$HAULING_DEPTH[j]<=strata[strata$COUNTRY==ResultData$COUNTRY[j] & strata$CODE==4,"MAX_DEPTH"][1])){
      ResultData$stratum_e[j]="4"
    } else if (5 %in% strata[strata$COUNTRY==ResultData$COUNTRY[j],"CODE"] & (ResultData$HAULING_DEPTH[j]>= strata[strata$COUNTRY==ResultData$COUNTRY[j] & strata$CODE==5,"MIN_DEPTH"][1]) & (ResultData$HAULING_DEPTH[j]<=strata[strata$COUNTRY==ResultData$COUNTRY[j] & strata$CODE==5,"MAX_DEPTH"][1])){
      ResultData$stratum_e[j]="5"
    }
  }

  k=1
  for (k in 1:nrow(ResultData)){
    if (any(is.na(c(ResultData$stratum_s[k],ResultData$stratum_e[k])))){
      write(paste("Error: Haul",ResultData$HAUL_NUMBER[k]," one between SHOOTING_DEPTH or HAULING_DEPTH is out of the stratification range"), file = Errors, append = TRUE)
      numberError <- numberError + 1
    }

    if ((ResultData$stratum_s[k]!= ResultData$stratum_e[k]) & all(!is.na(c(ResultData$stratum_s[k],ResultData$stratum_e[k]))) ){
      write(paste("Warning: Haul",ResultData$HAUL_NUMBER[k]," starts in the stratum",ResultData$stratum_s[k],"(",ResultData$SHOOTING_DEPTH[k],"m ) and finishes in the stratum", ResultData$stratum_e[k],"(",ResultData$HAULING_DEPTH[k],"m ) in",ResultData$TYPE_OF_FILE[k]), file = Errors, append = TRUE)
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
