###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #
#   March 2020
################################################################################

# Check consistency between not null weight and not null total number

if (FALSE){
  ResultDataTB = read.csv("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/data/TB_GSA18_1994-2018.csv", sep=";")
  #ResultDataTB=ResultDataTB[ResultDataTB$YEAR==2017,]

  wd <- "C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/temp"
  suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  #load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//DataTargetSpecies.rda")
  #load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//Maturity_parameters.rda")
  #load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//TM_list.rda")
  check_weight_tot_nb(ResultDataTB,wd,suffix)

}

check_weight_tot_nb<-function(ResultDataTB,wd,suffix){

  Format="from_2012"
  if (!file.exists(paste(wd,"Logfiles",sep="/"))){
    dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
  }

  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  }

  if (!file.exists(paste(wd,"Graphs",sep="/"))){
    dir.create(file.path(wd, "Graphs"), showWarnings = FALSE)
  }


  Errors <- paste(wd,"/Logfiles/Logfile_",suffix,".dat",sep="")


  numberError = 0
  ResultData = ResultDataTB
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

  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }

}

################################################################################
