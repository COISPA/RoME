###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #
#   March 2020
################################################################################

# Check if the stratum code in TA (or TT) is consistent with the stratification code table

if (FALSE){
  ResultDataTA = read.csv("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/data/TA_GSA18_1994-2018.csv", sep=";")
  ResultDataTA=ResultDataTA[ResultDataTA$YEAR==1994,]
  #ResultTB = read.csv("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/data/TB_GSA18_1994-2018.csv", sep=";")

  wd <- "C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/temp"
  suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  #load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//DataTargetSpecies.rda")
  #load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//Maturity_parameters.rda")
  #load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//TM_list.rda")
  check_stratum_code(ResultDataTA,stratification_scheme,wd,suffix)
}

check_stratum_code <- function (ResultDataTA,Stratification=stratification_scheme,wd,suffix){
  stratification_scheme=MEDITS::stratification_scheme

  Format="from_2012"
  if (!file.exists(paste(wd,"Logfiles",sep="/"))){
    dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
  }

  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  }

  Errors <- paste(wd,"/Logfiles/Logfile_",suffix,".dat",sep="")

  numberError = 0


    Dataset = ResultDataTA #read.csv(paste(DataTA,".csv",sep=""), sep=";",header=TRUE)

   write(paste("
              ----------- check correctness of stratum code - ",Dataset$YEAR[1]), file = Errors, append = TRUE)

  Strat_table = Stratification #read.csv(paste(Stratification,".csv",sep=""), sep=";",header=TRUE)

  Strat_table = Strat_table[Strat_table$GSA== Dataset$AREA[1],] # selection of the area

    mean_depth = rowMeans(cbind(Dataset$SHOOTING_DEPTH,Dataset$HAULING_DEPTH))

  for (i in 1:nrow(Dataset))
  {
    # check if the stratum code in TA is one of the codes in the Stratification table

  if (!(Dataset$NUMBER_OF_THE_STRATUM[i] %in%  Strat_table$STRATUM)) {
    numberError = numberError+1
    write(paste("Warning: Haul ",Dataset$HAUL_NUMBER[i],"wrong stratum code"), file = Errors, append = TRUE)

    }


  if ((mean_depth[i] >=10) & (mean_depth[i] <=50)) {
    Strat_table_temp = Strat_table[Strat_table$CODE==1,]
    if (!(Dataset$NUMBER_OF_THE_STRATUM[i] %in% Strat_table_temp$STRATUM)){
    write(paste("Warning: Haul ",Dataset$HAUL_NUMBER[i],"stratum code not consistent with the mean detph of the haul"), file = Errors, append = TRUE)
    numberError ==numberError +1
    }
  }  else if ((mean_depth[i] >50) & (mean_depth[i] <=100)){
    Strat_table_temp = Strat_table[Strat_table$CODE==2,]
    if (!(Dataset$NUMBER_OF_THE_STRATUM[i] %in% Strat_table_temp$STRATUM)){
      write(paste("Warning: Haul ",Dataset$HAUL_NUMBER[i],"stratum code not consistent with the mean detph of the haul"), file = Errors, append = TRUE)
      numberError ==numberError +1
    }
  } else if ((mean_depth[i] >100) & (mean_depth[i] <=200)){
    Strat_table_temp = Strat_table[Strat_table$CODE==3,]
    if (!(Dataset$NUMBER_OF_THE_STRATUM[i] %in% Strat_table_temp$STRATUM)){
      write(paste("Warning: Haul ",Dataset$HAUL_NUMBER[i],"stratum code not consistent with the mean detph of the haul"), file = Errors, append = TRUE)
      numberError ==numberError +1
    }
  } else if ((mean_depth[i] >200) & (mean_depth[i] <=500)){
    Strat_table_temp = Strat_table[Strat_table$CODE==4,]
    if (!(Dataset$NUMBER_OF_THE_STRATUM[i] %in% Strat_table_temp$STRATUM)){
      write(paste("Warning: Haul ",Dataset$HAUL_NUMBER[i],"stratum code not consistent with the mean detph of the haul"), file = Errors, append = TRUE)
      numberError ==numberError +1
    }
  } else if ((mean_depth[i] >500) & (mean_depth[i] <=800)){
    Strat_table_temp = Strat_table[Strat_table$CODE==5,]
    if (!(Dataset$NUMBER_OF_THE_STRATUM[i] %in% Strat_table_temp$STRATUM)){
      write(paste("Warning: Haul ",Dataset$HAUL_NUMBER[i],"stratum code not consistent with the mean detph of the haul"), file = Errors, append = TRUE)
      numberError ==numberError +1
    }
  }

  }


  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  }

    return(TRUE)



}
