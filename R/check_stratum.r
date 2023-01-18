############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files)                                #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.it #
#   January 2022                                                                                                           #
############################################################################################################################

# Start depth and end depth of each haul should be in the same stratum



if (FALSE){
  ResultDataTA = RoME::TA
  wd <- tempdir()
  suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
  year=2007
  check_stratum(ResultDataTA,year,wd,suffix)
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
  for (i in 1:nrow(ResultData)){
    if ((ResultData$SHOOTING_DEPTH[i]>=10) & (ResultData$SHOOTING_DEPTH[i]<=50))
    {ResultData$stratum_s[i]="10-50"
    } else {
      if ((ResultData$SHOOTING_DEPTH[i]>50) & (ResultData$SHOOTING_DEPTH[i]<=100)){
        ResultData$stratum_s[i]="51-100"
      } else {
        if ((ResultData$SHOOTING_DEPTH[i]>100) & (ResultData$SHOOTING_DEPTH[i]<=200)){
          ResultData$stratum_s[i]="101-200"
        } else {
          if ((ResultData$SHOOTING_DEPTH[i]>200) & (ResultData$SHOOTING_DEPTH[i]<=500)){
            ResultData$stratum_s[i]="201-500"
          } else {
            if (ResultData$SHOOTING_DEPTH[i]>=500 ){
              ResultData$stratum_s[i]="501-800"
            }
          }
        }
      }
    }
  }
  for (j in 1:nrow(ResultData)){
    if ((ResultData$HAULING_DEPTH[j]>=10) & (ResultData$HAULING_DEPTH[j]<=50))
    {ResultData$stratum_e[j]="10-50"
    } else {
      if ((ResultData$HAULING_DEPTH[j]>50) & (ResultData$HAULING_DEPTH[j]<=100)){
        ResultData$stratum_e[j]="51-100"
      } else {
        if ((ResultData$HAULING_DEPTH[j]>100) & (ResultData$HAULING_DEPTH[j]<=200)){
          ResultData$stratum_e[j]="101-200"
        } else {
          if ((ResultData$HAULING_DEPTH[j]>200) & (ResultData$HAULING_DEPTH[j]<=500)){
            ResultData$stratum_e[j]="201-500"
          } else {
            if ((ResultData$HAULING_DEPTH[j]>500)){
              ResultData$stratum_e[j]="501-800"
            }
          }
        }
      }
    }
  }
  for (k in 1:nrow(ResultData)){
    if (ResultData$stratum_s[k]!= ResultData$stratum_e[k]){
      write(paste("Warning: Haul",ResultData$HAUL_NUMBER[k]," starts in the stratum",ResultData$stratum_s[k],"(",ResultData$SHOOTING_DEPTH[k],"m ) and finishes in the stratum", ResultData$stratum_e[k],"(",ResultData$HAULING_DEPTH[k],"m ) in",ResultData$TYPE_OF_FILE[k]), file = Errors, append = TRUE)}
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
