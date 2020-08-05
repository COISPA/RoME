###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #
#   March 2020
################################################################################

# Check if the stratum code in TA (or TT) is consistent with the stratification code table

if (FALSE){
  # ResultDataTA = read.csv("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/data/TA_GSA18_1994-2018.csv", sep=";")
  ResultDataTA= read.table(file=paste(wd, "\\TA.csv",sep=""), sep=";", header=T)
  #ResultTB = read.csv("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/data/TB_GSA18_1994-2018.csv", sep=";")
  wd <- "C:\\Users\\walte\\Documents\\GitHub\\RoME\\Test Loredana"
  suffix= NA# paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  #load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//DataTargetSpecies.rda")
  #load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//Maturity_parameters.rda")
  #load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//TM_list.rda")
  check_stratum_code(ResultDataTA,Stratification=MEDITS::stratification_scheme,wd,suffix)
}

check_stratum_code <- function (ResultDataTA,Stratification=MEDITS::stratification_scheme,wd,suffix){
  stratification_scheme=Stratification

  Format="from_2012"
  if (!file.exists(file.path(wd, "Logfiles"))){
    dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
  }
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  }
  numberError = 0
  Errors <- file.path(wd,"Logfiles",paste("Logfile_",suffix,".dat",sep=""))

   Dataset = ResultDataTA #read.csv(paste(DataTA,".csv",sep=""), sep=";",header=TRUE)
   write(paste("\n----------- check correctness of stratum code - ",Dataset$YEAR[1]), file = Errors, append = TRUE)

  Strat_table = Stratification
  Strat_table = Strat_table[Strat_table$GSA== Dataset$AREA[1],] # selection of the area
  strata.no.letter <-Strat_table
  strata.no.letter$STRATUM <- as.character(strata.no.letter$STRATUM)
  for (n in 1:nrow(Strat_table)){
  strata.no.letter$STRATUM[n] <- substr( strata.no.letter$STRATUM[n] , 1 , (nchar(strata.no.letter$STRATUM[n])-1) )
  }

  mean_depth = rowMeans(cbind(Dataset$SHOOTING_DEPTH,Dataset$HAULING_DEPTH))

  i=1
  for (i in 1:nrow(Dataset))
  {
    # check if the stratum code in TA is one of the codes in the Stratification table

  if (!(Dataset$NUMBER_OF_THE_STRATUM[i] %in%  Strat_table$STRATUM)) {
    numberError = numberError+1
    write(paste("Warning: Haul ",Dataset$HAUL_NUMBER[i],"wrong stratum code, check if it is the most recent version of MEDITS codes"), file = Errors, append = TRUE)

    }


  if ((mean_depth[i] >=10) & (mean_depth[i] <=50)) {
    Strat_table_temp = Strat_table[Strat_table$CODE==1,]
    Strat_table_temp.no.lettera = strata.no.letter[strata.no.letter$CODE ==1, ]
    if (!(Dataset$NUMBER_OF_THE_STRATUM[i] %in% Strat_table_temp$STRATUM | Dataset$NUMBER_OF_THE_STRATUM[i] %in% Strat_table_temp.no.lettera$STRATUM  )){
    write(paste("Warning: Haul ",Dataset$HAUL_NUMBER[i],"stratum code not consistent with the mean detph of the haul"), file = Errors, append = TRUE)
    }
  }  else if ((mean_depth[i] >50) & (mean_depth[i] <=100)){
    Strat_table_temp = Strat_table[Strat_table$CODE==2,]
    Strat_table_temp.no.lettera = strata.no.letter[strata.no.letter$CODE ==2, ]
    if (!(Dataset$NUMBER_OF_THE_STRATUM[i] %in% Strat_table_temp$STRATUM | Dataset$NUMBER_OF_THE_STRATUM[i] %in% Strat_table_temp.no.lettera$STRATUM  )){
      write(paste("Warning: Haul ",Dataset$HAUL_NUMBER[i],"stratum code not consistent with the mean detph of the haul"), file = Errors, append = TRUE)
    }
  } else if ((mean_depth[i] >100) & (mean_depth[i] <=200)){
    Strat_table_temp = Strat_table[Strat_table$CODE==3,]
    Strat_table_temp.no.lettera = strata.no.letter[strata.no.letter$CODE ==3, ]
    if (!(Dataset$NUMBER_OF_THE_STRATUM[i] %in% Strat_table_temp$STRATUM  | Dataset$NUMBER_OF_THE_STRATUM[i] %in% Strat_table_temp.no.lettera$STRATUM  )){
      write(paste("Warning: Haul ",Dataset$HAUL_NUMBER[i],"stratum code not consistent with the mean detph of the haul"), file = Errors, append = TRUE)
    }
  } else if ((mean_depth[i] >200) & (mean_depth[i] <=500)){
    Strat_table_temp = Strat_table[Strat_table$CODE==4,]
    Strat_table_temp.no.lettera = strata.no.letter[strata.no.letter$CODE ==4, ]
    if (!(Dataset$NUMBER_OF_THE_STRATUM[i] %in% Strat_table_temp$STRATUM  | Dataset$NUMBER_OF_THE_STRATUM[i] %in% Strat_table_temp.no.lettera$STRATUM  )){
      write(paste("Warning: Haul ",Dataset$HAUL_NUMBER[i],"stratum code not consistent with the mean detph of the haul"), file = Errors, append = TRUE)
    }
  } else if ((mean_depth[i] >500) & (mean_depth[i] <=800)){
    Strat_table_temp = Strat_table[Strat_table$CODE==5,]
    Strat_table_temp.no.lettera = strata.no.letter[strata.no.letter$CODE ==5, ]
    if (!(Dataset$NUMBER_OF_THE_STRATUM[i] %in% Strat_table_temp$STRATUM  | Dataset$NUMBER_OF_THE_STRATUM[i] %in% Strat_table_temp.no.lettera$STRATUM  )){
      write(paste("Warning: Haul ",Dataset$HAUL_NUMBER[i],"stratum code not consistent with the mean detph of the haul"), file = Errors, append = TRUE)
    }
  }

  }


  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  }

    return(TRUE)



}
