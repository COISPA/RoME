###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #
#   March 2020
################################################################################
# Start depth and end depth of each haul should be in the same stratum



if (FALSE){
  ResultDataTA = read.csv("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/data/TA_GSA18_1994-2018.csv", sep=";")
  ResultDataTA=ResultDataTA[ResultDataTA$YEAR==1994,]
  #ResultTB = read.csv("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/data/TB_GSA18_1994-2018.csv", sep=";")

  wd <- "C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/temp"
  suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  #load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//DataTargetSpecies.rda")
  #load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//Maturity_parameters.rda")
  #load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//TM_list.rda")
  check_stratum(ResultDataTA,wd,suffix)
}


check_stratum<-function(ResultData,wd,suffix){

  Format="from_2012"
  if (!file.exists(file.path(wd, "Logfiles"))){
    dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
  }
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  }
  numberError = 0
  Errors <- file.path(wd,"Logfiles",paste("Logfile_",suffix,".dat",sep=""))

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
  unlink(file.path(tempdir(),"Logfiles"),recursive=T)
  #unlink(file.path(tempdir(),"Graphs"),recursive=T)
  #unlink(file.path(tempdir(),"files R-Sufi"),recursive=T)

  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }
}
