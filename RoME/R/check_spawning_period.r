###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #
#   March 2020
###########################################################################################################################

# Check maturity stages using spawning season

if (FALSE){
  ResultDataTC = read.csv("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/data/TC_GSA18_1994-2018.csv", sep=";")
  ResultDataTA = read.csv("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/data/TA_GSA18_1994-2018.csv", sep=";")

  wd <- "C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/temp"
  suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
  load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//DataTargetSpecies.rda")
  load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//Maturity_parameters.rda")
}

check_spawning_period<-function(ResultDataTA,ResultDataTC,Maturity_parameters=Maturity_parameters,DataTargetSpecies=DataTargetSpecies,wd,suffix){

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

    write(paste("\n----------- check consistency of maturity stages",ResultDataTC$TYPE_OF_FILE[1],"by means of spawning season information - ",ResultDataTA$YEAR[1]), file = Errors, append = TRUE)

  ResultDataTC$Species = paste(ResultDataTC$GENUS,ResultDataTC$SPECIES)
  ResultDataTC$Maturity = paste(as.character(ResultDataTC$MATURITY),ifelse(is.na(ResultDataTC$MATSUB),"",as.character(ResultDataTC$MATSUB)), sep="")

    maturity_table = Maturity_parameters
  species_list = DataTargetSpecies #read.csv(file=paste(DataTargetSpecies,".csv",sep=""),sep=";",header=TRUE)
  for (i in unique(ResultDataTC$Species)){
    ResultData_temp =ResultDataTC[ResultDataTC$Species == i,]
    cau_fau_temp =  species_list$FAUNISTIC_CATEGORY[paste(substring(species_list$SPECIES,1,4),substring(species_list$SPECIES,5,7)) == i]

    for (j in 1:nrow(ResultData_temp))  {
      maturity_minitable = maturity_table[(as.character(maturity_table$Species) == i) & (as.character(maturity_table$SEX) == ResultData_temp$SEX[j]),]
      month = ResultDataTA[ResultDataTA$HAUL_NUMBER == ResultData_temp$HAUL_NUMBER[j],names(ResultDataTA) == "MONTH"]
      if (nrow(maturity_minitable)!=0){
        Start = maturity_minitable$Start_reproductive_season
        End =   maturity_minitable$End_reproductive_season

        # check if there is any immature in the spawning season greater than max L50 in bibliography
        if (Start < End){
          if (((as.character(ResultData_temp$Maturity[j])== "0" & as.character(ResultData_temp$SEX[j])!="M") | as.character(ResultData_temp$Maturity[j])== "1" |
                 as.character(ResultData_temp$Maturity[j])== "2A") & ((all(month >= Start)) & (all(month <= End))) & (ResultData_temp$LENGTH_CLASS[j] > ((maturity_minitable$max_L50[1]+0.2*maturity_minitable$max_L50[1])*10))){
            write(paste("Warning: Haul ",ResultData_temp$HAUL_NUMBER[j],ResultData_temp$Species[j],ResultData_temp$SEX[j],"length",ResultData_temp$LENGTH_CLASS[j],": specimen immature during the spawning period (STAGE",ResultData_temp$Maturity[j],") with length quite greater (+20%) than the maximum L50 in bibliography(",(maturity_minitable$max_L50[1]*10),"). Please check correctness of maturity stage."), file = Errors, append = TRUE)}
          } else {
              if (((as.character(ResultData_temp$Maturity[j])== "0" & as.character(ResultData_temp$SEX[j])!="M") | as.character(ResultData_temp$MATURITY[j])== "1" |
                     as.character(ResultData_temp$Maturity[j])== "2A") & ((all(month >= Start))| (all(month <= End)))& (ResultData_temp$LENGTH_CLASS[j] > ((maturity_minitable$max_L50[1]+0.2*maturity_minitable$max_L50[1])*10))){
                write(paste("Warning: Haul ",ResultData_temp$HAUL_NUMBER[j],ResultData_temp$Species[j],ResultData_temp$SEX[j],"length",ResultData_temp$LENGTH_CLASS[j],": specimen immature during the spawning period (STAGE",ResultData_temp$Maturity[j],") with length quite greater (+20%) than the maximum L50 in bibliography(",(maturity_minitable$max_L50[1]*10),"). Please check correctness of maturity stage."), file = Errors, append = TRUE) }
            }
      }
    }

    # check if there is any mature specimen outside the spawning period and any mature specimen outside the spawning period less than the smallest mature individual in literature

    maturity_sp_table = maturity_table[(as.character(maturity_table$Species) == i),]
    if (nrow(maturity_minitable)!=0) {
      for (sex in 1:nrow(maturity_sp_table)) {
        Error_matrix = matrix(nrow=0, ncol=ncol(ResultDataTC))
        Error_matrix2 = matrix(nrow=0, ncol=ncol(ResultDataTC))
        if (as.character(maturity_sp_table$smallest_mature_individual_observed[sex])!="n.a.") {
          smallest = as.numeric(as.character(maturity_sp_table$smallest_mature_individual_observed[sex]))*10}

        if  (as.character(maturity_sp_table$SEX[sex])!="C"){
          ResultData_temp2 = ResultData_temp[as.character(ResultData_temp$SEX) == as.character(maturity_sp_table$SEX[sex]),] } else {
            ResultData_temp2 = ResultData_temp
          }

        if (as.character(cau_fau_temp)== "A" & as.character(maturity_sp_table$smallest_mature_individual_observed[sex])!="n.a.") {
          Error_matrix = ResultData_temp2[as.character(ResultData_temp2$Maturity) == "3" & as.numeric(as.character(ResultData_temp2$LENGTH_CLASS)) < smallest-0.1*smallest,]
          Error_matrix2 = ResultData_temp2[as.character(ResultData_temp2$Maturity) == "3",]
        } else if (as.character(cau_fau_temp)== "B"& as.character(maturity_sp_table$smallest_mature_individual_observed[sex])!="n.a."){
          Error_matrix = ResultData_temp2[(as.character(ResultData_temp2$Maturity) == "2D"|as.character(ResultData_temp2$Maturity) == "2") & as.numeric(as.character(ResultData_temp2$LENGTH_CLASS)) < smallest-0.1*smallest,]
          Error_matrix2 = ResultData_temp2[(as.character(ResultData_temp2$Maturity) == "2D"|as.character(ResultData_temp2$Maturity) == "2"),]
        } else if ( as.character(cau_fau_temp)== "C"& as.character(maturity_sp_table$smallest_mature_individual_observed[sex])!="n.a.") {
          Error_matrix = ResultData_temp2[(as.character(ResultData_temp2$Maturity) == "3A" |as.character(ResultData_temp2$Maturity) == "3")& as.numeric(as.character(ResultData_temp2$LENGTH_CLASS)) < smallest-0.1*smallest,]
          Error_matrix2 = ResultData_temp2[(as.character(ResultData_temp2$Maturity) == "3A" |as.character(ResultData_temp2$Maturity) == "3"),]
        } else if ( as.character(cau_fau_temp)== "S"& as.character(maturity_sp_table$smallest_mature_individual_observed[sex])!="n.a.") {
          Error_matrix = ResultData_temp2[(as.character(ResultData_temp2$Maturity) == "3A" |as.character(ResultData_temp2$Maturity) == "3") & as.numeric(as.character(ResultData_temp2$LENGTH_CLASS)) < smallest-0.1*smallest,]
          Error_matrix2 = ResultData_temp2[(as.character(ResultData_temp2$Maturity) == "3A" |as.character(ResultData_temp2$Maturity) == "3") ,]
        }

      }

      Start_temp <- maturity_sp_table$Start_reproductive_season[sex]
      End_temp <-   maturity_sp_table$End_reproductive_season[sex]

      if (nrow(Error_matrix)!=0) {
        for (k in 1:nrow(Error_matrix)){
          month_temp = ResultDataTA[ResultDataTA$HAUL_NUMBER == Error_matrix$HAUL_NUMBER[k],names(ResultDataTA) == "MONTH"]
          if (Start_temp < End_temp){
            if (any(month_temp < Start_temp)| any(month_temp > End_temp)){
              write(paste("Warning: Haul ",Error_matrix$HAUL_NUMBER[k],Error_matrix$Species[k],Error_matrix$SEX[k],"length",Error_matrix$LENGTH_CLASS[k],": specimen mature (STAGE ",Error_matrix$Maturity[k],")outside the spawning period smaller than the smallest specimen reported in bibliography(",smallest,"). Please check correctness of maturity stage and length data."), file = Errors, append = TRUE)} }else {
                if (any(month_temp < Start_temp)& any(month_temp > End_temp)){
                  write(paste("Warning: Haul ",Error_matrix$HAUL_NUMBER[k],Error_matrix$Species[k],Error_matrix$SEX[k],"length",Error_matrix$LENGTH_CLASS[k],"specimen mature (STAGE ",Error_matrix$Maturity[k],") outside the spawning period smaller than the smallest mature specimen reported in bibliography(",smallest,"). Please check correctness of maturity stage and length data."), file = Errors, append = TRUE) }
              }
        }
      }
      if (nrow(Error_matrix2)!=0) {
        for (l in 1:nrow(Error_matrix2)){
          month_temp2 = ResultDataTA[ResultDataTA$HAUL_NUMBER == Error_matrix2$HAUL_NUMBER[l],names(ResultDataTA) == "MONTH"]
          if (Start_temp < End_temp){
            if (any(month_temp2 < Start_temp)| any(month_temp2 > End_temp)){
              write(paste("Warning: Haul ",Error_matrix2$HAUL_NUMBER[l],Error_matrix2$Species[l],Error_matrix2$SEX[l],"length",Error_matrix2$LENGTH_CLASS[l],": specimen mature (STAGE ",Error_matrix2$Maturity[l],")outside the spawning period. Please check correctness of maturity stage."), file = Errors, append = TRUE)} }else {
                if (any(month_temp2 < Start_temp)& any(month_temp2 > End_temp)){
                  write(paste("Warning: Haul ",Error_matrix2$HAUL_NUMBER[l],Error_matrix2$Species[l],Error_matrix2$SEX[l],"length",Error_matrix2$LENGTH_CLASS[l],"specimen mature (STAGE ",Error_matrix2$Maturity[l],") outside the spawning period. Please check correctness of maturity stage."), file = Errors, append = TRUE) }
              }
        }
      }


    }

  }

  if (numberError ==0) {
    write(paste("Attention: if you decide to change the maturity stages detected, after the corrections, run again the code, because you could have entered duplicated records in TC."), file = Errors, append = TRUE)
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
