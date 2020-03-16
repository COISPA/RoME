###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #
#   March 2020
################################################################################

# Check about the consistency of the number of individuals by length, sex and stage between TC and TE

if (FALSE){
  ResultDataTC = RoME::TC # read.csv("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/data/TC_GSA18_1994-2018.csv", sep=";")
  ResultDataTC = ResultDataTC[ResultDataTC$YEAR==2007,]
  ResultDataTE = RoME::TE # read.csv("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/data/TE_2012-2018 _GSA18.csv", sep=";")
  ResultDataTE = ResultDataTE[ResultDataTE$YEAR==2012,]

  wd <- tempdir() #"C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/temp"
  suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  #load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//DataTargetSpecies.rda")
  #load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//Maturity_parameters.rda")
  #load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//TM_list.rda")
  check_TE_TC(ResultDataTC,ResultDataTC,wd,suffix)
}

check_TE_TC <- function (ResultDataTC,ResultDataTE,wd,suffix){

  Format="from_2012"

  if (!file.exists(file.path(wd, "Logfiles"))){
    dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
  }
  if (!file.exists(file.path(wd,"Graphs"))){
    dir.create(file.path(wd, "Graphs"), showWarnings = FALSE)
  }
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  }
  numberError = 0
  Errors <- file.path(wd,"Logfiles",paste("Logfile_",suffix,".dat",sep=""))

  TC = ResultDataTC
  TE = ResultDataTE

if (nrow(TE[which(TE$MATSUB=="O"),])!=0){
  TE[which(TE$MATSUB=="O"),]$MATSUB="ND"
}

  write(paste("\n----------- check consistency nb of individuals TC and TE - ",TC$YEAR[1]), file = Errors, append = TRUE)
  i=1
  for (i in 1:nrow(TE)){
    TE$SEX[i]=ifelse(as.character(TE$SEX[i])=="FALSE","F",as.character(TE$SEX[i]))
  }

  for (i in 1:nrow(TE)){
  # check if the individual in TE is in TC
    TC_temp = TC[which((TC$HAUL_NUMBER==TE$HAUL_NUMBER[i])& (as.character(TC$GENUS)==as.character(TE$GENUS[i])) & (as.character(TC$SPECIES)==as.character(TE$SPECIES[i])) & (as.character(TC$SEX)==as.character(TE$SEX[i])) & (TC$LENGTH_CLASS==TE$LENGTH_CLASS[i]) & (as.character(TC$MATURITY)==as.character(TE$MATURITY[i])) & (as.character(TC$MATSUB)==as.character(TE$MATSUB[i]))),]
   nb_TC= TC_temp[,ncol(TC)]

    if (nrow(TC_temp)==0) { # record not present in TC
  write(paste("Haul ",TE$HAUL_NUMBER[i],as.character(TE$GENUS[i]),as.character(TE$SPECIES[i]),", sex ",ifelse(as.character(TE$SEX[i])=="FALSE","F",as.character(TE$SEX[i])),", length ",TE$LENGTH_CLASS[i],"mm, maturity",as.character(TE$MATURITY[i]),as.character(TE$MATSUB[i])," : record not present in TC"), file = Errors, append = TRUE)
  numberError=numberError+1

  } else { # record present: check on the number (must be <= the number in TC)
  # sum of individuals in TE:
    nb_TE = nrow(TE[which((TE$HAUL_NUMBER==TE$HAUL_NUMBER[i])& (as.character(TE$GENUS)==as.character(TE$GENUS[i]))& (as.character(TE$SPECIES)==as.character(TE$SPECIES[i]))& (as.character(TE$SEX)==as.character(TE$SEX[i]))& (TE$LENGTH_CLASS==TE$LENGTH_CLASS[i])& (as.character(TE$MATURITY)==as.character(TE$MATURITY[i]))& (as.character(TE$MATSUB)==as.character(TE$MATSUB[i]))),])
   if (nb_TC<nb_TE){
     write(paste("Haul ",TE$HAUL_NUMBER[i],as.character(TE$GENUS[i]),as.character(TE$SPECIES[i]),", sex ",ifelse(as.character(TE$SEX[i])=="FALSE","F",as.character(TE$SEX[i])),", length ",TE$LENGTH_CLASS[i],"mm, maturity",as.character(TE$MATURITY[i]),as.character(TE$MATSUB[i])," : the number of individuals in TE (=",nb_TE,") is greater than the number reported in TC(=",nb_TC,")"), file = Errors, append = TRUE)
    numberError=numberError+1
   }
  }

  }
      if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
   }
  if (numberError ==0) {
    return(TRUE)
   } else { return(FALSE)
   }

}
