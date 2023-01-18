###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #
#   March 2020
################################################################################

# Check if the stratum code in TA (or TT) is consistent with the stratification code table

if (FALSE){
  ResultDataTA = RoME::TA
  year=2007
  wd <- tempdir()
  suffix= NA

  check_stratum_code(ResultDataTA,year,Strata=RoME::stratification_scheme,wd,suffix)
}

check_stratum_code <- function (ResultDataTA,year,Strata=RoME::stratification_scheme,wd,suffix){
  # stratification_scheme=Strata

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
  ResultDataTA <- ResultDataTA[ResultDataTA$YEAR == year, ]
  ########################################
  ResultDataTA <- ResultDataTA[!is.na(ResultDataTA$AREA), ]

   Dataset = ResultDataTA #read.csv(paste(DataTA,".csv",sep=""), sep=";",header=TRUE)
   write(paste("\n----------- check correctness of stratum code - ",Dataset$YEAR[1]), file = Errors, append = TRUE)

  Strat_table = Strata
  Strat_table = Strat_table[Strat_table$GSA== Dataset$AREA[1],] # selection of the area
  strata.no.letter <- Strat_table
  strata.no.letter$STRATUM <- as.character(strata.no.letter$STRATUM)
  for (n in 1:nrow(Strat_table)){
     strata.no.letter$STRATUM[n] <- substr( strata.no.letter$STRATUM[n] , 1 , (nchar(strata.no.letter$STRATUM[n])-1) )
  }

  mean_depth = rowMeans(cbind(Dataset$SHOOTING_DEPTH,Dataset$HAULING_DEPTH), na.rm = TRUE)

  i=1
  for (i in 1:nrow(Dataset))
  {
    # check if the stratum code in TA is one of the codes in the Stratification table

  if (!(Dataset$NUMBER_OF_THE_STRATUM[i] %in%  as.character(Strat_table$STRATUM)) & !(Dataset$NUMBER_OF_THE_STRATUM[i] %in%  as.character(strata.no.letter$STRATUM))) {
    numberError = numberError+1
    write(paste("Warning: Haul ",Dataset$HAUL_NUMBER[i],", wrong stratum code (",Dataset$NUMBER_OF_THE_STRATUM[i],"), check the most recent version of MEDITS codes"), file = Errors, append = TRUE)
    }


  if ((mean_depth[i] >=10) & (mean_depth[i] <=50)) {
    Strat_table_temp = Strat_table[Strat_table$CODE==1,]
    Strat_table_temp.no.lettera = strata.no.letter[strata.no.letter$CODE ==1, ]
    if (!(Dataset$NUMBER_OF_THE_STRATUM[i] %in% as.character(Strat_table_temp$STRATUM) | Dataset$NUMBER_OF_THE_STRATUM[i] %in% as.character(Strat_table_temp.no.lettera$STRATUM)  )){
    write(paste("Warning: Haul ",Dataset$HAUL_NUMBER[i],", stratum code (",Dataset$NUMBER_OF_THE_STRATUM[i],") not consistent with the mean detph of the haul"), file = Errors, append = TRUE)
    }
  }  else if ((mean_depth[i] >50) & (mean_depth[i] <=100)){
    Strat_table_temp = Strat_table[Strat_table$CODE==2,]
    Strat_table_temp.no.lettera = strata.no.letter[strata.no.letter$CODE ==2, ]
    if (!(Dataset$NUMBER_OF_THE_STRATUM[i] %in% as.character(Strat_table_temp$STRATUM) | Dataset$NUMBER_OF_THE_STRATUM[i] %in% as.character(Strat_table_temp.no.lettera$STRATUM)  )){
      write(paste("Warning: Haul ",Dataset$HAUL_NUMBER[i],", stratum code (",Dataset$NUMBER_OF_THE_STRATUM[i],") not consistent with the mean detph of the haul"), file = Errors, append = TRUE)
    }
  } else if ((mean_depth[i] >100) & (mean_depth[i] <=200)){
    Strat_table_temp = Strat_table[Strat_table$CODE==3,]
    Strat_table_temp.no.lettera = strata.no.letter[strata.no.letter$CODE ==3, ]
    if (!(Dataset$NUMBER_OF_THE_STRATUM[i] %in% as.character(Strat_table_temp$STRATUM)  | Dataset$NUMBER_OF_THE_STRATUM[i] %in% as.character(Strat_table_temp.no.lettera$STRATUM)  )){
      write(paste("Warning: Haul ",Dataset$HAUL_NUMBER[i],", stratum code (",Dataset$NUMBER_OF_THE_STRATUM[i],") not consistent with the mean detph of the haul"), file = Errors, append = TRUE)
    }
  } else if ((mean_depth[i] >200) & (mean_depth[i] <=500)){
    Strat_table_temp = Strat_table[Strat_table$CODE==4,]
    Strat_table_temp.no.lettera = strata.no.letter[strata.no.letter$CODE ==4, ]
    if (!(Dataset$NUMBER_OF_THE_STRATUM[i] %in% as.character(Strat_table_temp$STRATUM)  | Dataset$NUMBER_OF_THE_STRATUM[i] %in% (Strat_table_temp.no.lettera$STRATUM)  )){
      write(paste("Warning: Haul ",Dataset$HAUL_NUMBER[i],", stratum code (",Dataset$NUMBER_OF_THE_STRATUM[i],") not consistent with the mean detph of the haul"), file = Errors, append = TRUE)
    }
  } else if ((mean_depth[i] >500) & (mean_depth[i] <=800)){
    Strat_table_temp = Strat_table[Strat_table$CODE==5,]
    Strat_table_temp.no.lettera = strata.no.letter[strata.no.letter$CODE ==5, ]
    if (!(Dataset$NUMBER_OF_THE_STRATUM[i] %in% as.character(Strat_table_temp$STRATUM)  | Dataset$NUMBER_OF_THE_STRATUM[i] %in% as.character(Strat_table_temp.no.lettera$STRATUM)  )){
      write(paste("Warning: Haul ",Dataset$HAUL_NUMBER[i],", stratum code (",Dataset$NUMBER_OF_THE_STRATUM[i],") not consistent with the mean detph of the haul"), file = Errors, append = TRUE)
    }
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
    return(TRUE)



}
