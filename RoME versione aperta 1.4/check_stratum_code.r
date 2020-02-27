###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)  #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013                                                                                                            #
################################################################################
# Check if the stratum code in TA (or TT) is consistent with the stratification code table

check_stratum_code <- function (){
 
  
  numberError = 0
  
  if (Format=="before_2012"){
    Dataset = ResultDataTT #read.csv(paste(DataTT,".csv",sep=""), sep=";",header=TRUE)
    ResultData = ResultDataTA # read.csv(paste(DataTA,".csv",sep=""), sep=";", header=TRUE)
    ResultData = ResultData[ResultData$HAUL_NUMBER %in% Dataset$HAUL_NUMBER,]
    ResultData= ResultData[order(ResultData$HAUL_NUMBER),]
    Dataset= Dataset[order(Dataset$HAUL_NUMBER),]
  } else {
    Dataset = ResultDataTA #read.csv(paste(DataTA,".csv",sep=""), sep=";",header=TRUE)
  }
   write(paste("
              ----------- check correctness of stratum code - ",Dataset$YEAR[1]), file = Errors, append = TRUE)
			  
  Strat_table = read.csv(paste(Stratification,".csv",sep=""), sep=";",header=TRUE)
  
  Strat_table = Strat_table[Strat_table$GSA== Dataset$AREA[1],] # selection of the area
  
  if (Format=="before_2012"){
    mean_depth = rowMeans(cbind(ResultData$SHOOTING_DEPTH,ResultData$HAULING_DEPTH))
    
    for (i in 1:nrow(Dataset))
    {
      # check if the stratum code in TT is one of the codes in the Stratification table
      
      if (!(Dataset$STRATUM[i] %in%  Strat_table$STRATUM)) {
        numberError = numberError+1
        write(paste("Warning: Haul ",Dataset$HAUL_NUMBER[i],"wrong stratum code"), file = Errors, append = TRUE)
        
      }
      
      
      if ((mean_depth[i] >=10) & (mean_depth[i] <=50)) {
        Strat_table_temp = Strat_table[Strat_table$CODE==1,]
        if (!(Dataset$STRATUM[i] %in% Strat_table_temp$STRATUM)){
          write(paste("Warning: Haul ",Dataset$HAUL_NUMBER[i],"stratum code not consistent with the mean depth of the haul"), file = Errors, append = TRUE)
          numberError ==numberError +1
        }
      }  else if ((mean_depth[i] >50) & (mean_depth[i] <=100)){
        Strat_table_temp = Strat_table[Strat_table$CODE==2,]
        if (!(Dataset$STRATUM[i] %in% Strat_table_temp$STRATUM)){
          write(paste("Warning: Haul ",Dataset$HAUL_NUMBER[i],"stratum code not consistent with the mean detph of the haul"), file = Errors, append = TRUE)
          numberError ==numberError +1
        }
      } else if ((mean_depth[i] >100) & (mean_depth[i] <=200)){
        Strat_table_temp = Strat_table[Strat_table$CODE==3,]
        if (!(Dataset$STRATUM[i] %in% Strat_table_temp$STRATUM)){
          write(paste("Warning: Haul ",Dataset$HAUL_NUMBER[i],"stratum code not consistent with the mean detph of the haul"), file = Errors, append = TRUE)
          numberError ==numberError +1
        }
      } else if ((mean_depth[i] >200) & (mean_depth[i] <=500)){
        Strat_table_temp = Strat_table[Strat_table$CODE==4,]
        if (!(Dataset$STRATUM[i] %in% Strat_table_temp$STRATUM)){
          write(paste("Warning: Haul ",Dataset$HAUL_NUMBER[i],"stratum code not consistent with the mean detph of the haul"), file = Errors, append = TRUE)
          numberError ==numberError +1
        }
      } else if ((mean_depth[i] >500) & (mean_depth[i] <=800)){
        Strat_table_temp = Strat_table[Strat_table$CODE==5,]
        if (!(Dataset$STRATUM[i] %in% Strat_table_temp$STRATUM)){
          write(paste("Warning: Haul ",Dataset$HAUL_NUMBER[i],"stratum code not consistent with the mean detph of the haul"), file = Errors, append = TRUE)
          numberError ==numberError +1
        }
      }
      
    }
  }else {
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
  }
  
  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  }  

    return(TRUE)
  
  
  
}