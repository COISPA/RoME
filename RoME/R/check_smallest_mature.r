###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #
#   March 2020
###########################################################################################################################

# Check consistency of the length of smallest mature, comparing with literature

if (FALSE){
  ResultData = read.csv("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/data/TC_GSA18_1994-2018.csv", sep=";")

  wd <- "C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/temp"
  suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  check_smallest_mature(ResultData,Maturity_parameters=Maturity_parameters,DataTargetSpecies=DataTargetSpecies,wd,suffix)
  }



check_smallest_mature<-function(ResultData,Maturity_parameters=Maturity_parameters,DataTargetSpecies=DataTargetSpecies,wd,suffix){

  Format="from_2012"
  if (!file.exists(paste(wd,"Logfiles",sep="/"))){
    dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
  }

  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  }

  Errors <- paste(wd,"/Logfiles/Logfile_",suffix,".dat",sep="")

  numberError = 0

    write(paste("----------- check consistency of maturity stages", ResultData$TYPE_OF_FILE[1]," by means of the comparison with the lenght of smallest mature individuals in bibliography - ",ResultData$YEAR[1]), file = Errors, append = TRUE)
  ResultData$Species = paste(ResultData$GENUS,ResultData$SPECIES)
  ResultData$Maturity = paste(as.character(ResultData$MATURITY),ifelse(is.na(ResultData$MATSUB),"",as.character(ResultData$MATSUB)), sep="")

    maturity_table = Maturity_parameters # read.csv(file=paste(working_tables,"/Maturity_parameters.csv",sep=""),sep=";",header=TRUE)

     species_list = DataTargetSpecies #read.csv(file=paste(DataTargetSpecies,".csv",sep=""),sep=";",header=TRUE)
  mat_lmin = maturity_table[as.character(maturity_table$smallest_mature_individual_observed)!="n.a.",]

  for (i in unique(mat_lmin$Species)){
    cau_fau_temp =  species_list$FAUNISTIC_CATEGORY[paste(substring(species_list$SPECIES,1,4),substring(species_list$SPECIES,5,7)) == i]
    mat_lmin_temp = mat_lmin[mat_lmin$Species == i,]
    for (j in 1:nrow(mat_lmin_temp)) {
      Error_matrix = matrix(nrow=0, ncol=ncol(ResultData))
      if (as.character(mat_lmin_temp$SEX[j]) == "C"){
        ResultData_temp = ResultData[as.character(ResultData$Species) == as.character(mat_lmin_temp$Species[j]),]
        mat_lmin_temp_sex = mat_lmin_temp[mat_lmin_temp$SEX == "C",]
      } else {
        ResultData_temp = ResultData[as.character(ResultData$Species) == as.character(mat_lmin_temp$Species[j]) & as.character(ResultData$SEX) == as.character(mat_lmin_temp$SEX[j]),]
        mat_lmin_temp_sex = mat_lmin_temp[mat_lmin_temp$SEX == mat_lmin_temp$SEX[j],]
      }
      if ((as.character(cau_fau_temp)== "A"|(as.character(cau_fau_temp)== "Ao")) & as.character(mat_lmin_temp_sex$smallest_mature_individual_observed[1])!="n.a.") {
        Error_matrix = ResultData_temp[((as.character(ResultData_temp$Maturity) != "0")|(as.character(ResultData_temp$Maturity) != "1")|(as.character(ResultData_temp$Maturity) != "2A")) & as.numeric(as.character(ResultData_temp$LENGTH_CLASS)) < as.numeric(as.character(mat_lmin_temp_sex$smallest_mature_individual_observed[1]))-0.1*as.numeric(as.character(mat_lmin_temp_sex$smallest_mature_individual_observed[1]))*10,]
      } else if (as.character(cau_fau_temp)== "B"& as.character(mat_lmin_temp_sex$smallest_mature_individual_observed[1])!="n.a."){
        Error_matrix = ResultData_temp[((as.character(ResultData_temp$Maturity) != "0" )| (as.character(ResultData_temp$Maturity) != "1")|(as.character(ResultData_temp$Maturity) != "2A")) & as.numeric(as.character(ResultData_temp$LENGTH_CLASS)) < as.numeric(as.character(mat_lmin_temp_sex$smallest_mature_individual_observed[1]))-0.1*as.numeric(as.character(mat_lmin_temp_sex$smallest_mature_individual_observed[1]))*10,]
      } else if ( as.character(cau_fau_temp)== "C"& as.character(mat_lmin_temp_sex$smallest_mature_individual_observed[1])!="n.a.") {
        Error_matrix = ResultData_temp[((as.character(ResultData_temp$Maturity) != "0" )| (as.character(ResultData_temp$Maturity) != "1")|(as.character(ResultData_temp$Maturity) != "2A"))& as.numeric(as.character(ResultData_temp$LENGTH_CLASS)) < as.numeric(as.character(mat_lmin_temp_sex$smallest_mature_individual_observed[1]))-0.1*as.numeric(as.character(mat_lmin_temp_sex$smallest_mature_individual_observed[1]))*10,]
      } else if (( (as.character(cau_fau_temp)== "S")|(as.character(cau_fau_temp)== "Ae")) & as.character(mat_lmin_temp_sex$smallest_mature_individual_observed[1])!="n.a.") {
        Error_matrix = ResultData_temp[(as.character(ResultData_temp$Maturity) != "0" |as.character(ResultData_temp$Maturity) != "1") & as.numeric(as.character(ResultData_temp$LENGTH_CLASS)) < as.numeric(as.character(mat_lmin_temp_sex$smallest_mature_individual_observed[1]))-0.1*as.numeric(as.character(mat_lmin_temp_sex$smallest_mature_individual_observed[1]))*10,]
      }

      if (nrow(Error_matrix)!=0)  {
        for (k in 1:nrow(Error_matrix)){
          write(paste("Warning: Haul ",Error_matrix$HAUL_NUMBER[k],Error_matrix$Species[k],Error_matrix$SEX[k],"length",Error_matrix$LENGTH_CLASS[k],": specimen mature with size smaller than the smallest size reported in bibliography(",(as.numeric(as.character(mat_lmin_temp_sex$smallest_mature_individual_observed[1]))*10),").Please see Maturity_parameters.csv (folder 'Tables')"), file = Errors, append = TRUE)
        }
      }
    }
  }

  if (numberError ==0) {
    write(paste("Attention: if you decide to change the maturity stages detected, after the corrections, run again the code, because you could have entered duplicated records in TC."), file = Errors, append = TRUE)
  }

  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }
}
###########################################################################################################################
