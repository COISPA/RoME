############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.eu #
#   March 2020                                                                                                             #
############################################################################################################################
# Check if maturity stages in TC are consistent according to INSTRUCTION MANUAL VERSION 9 MEDITS 2017


if (FALSE){
  #library(RoME)
  Result = read.csv("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/data/TC_GSA18_1994-2018.csv", sep=";")
  Result[Result$YEAR==1994,]
  wd <- "C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/temp"
  suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  DataTC = MEDITS::TC

  check_mat_stages(Result, wd, suffix, DataTargetSpecies=DataTargetSpecies,DataSpecies=TM_list,stages=mat_stages)
}

if (FALSE){
    #library(MEDITS)
    wd <- tempdir()
    DataTargetSpecies=RoME::DataTargetSpecies
    DataSpecies=RoME::TM_list
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
    Data = MEDITS::TC #read.csv("~/GitHub/RoME/data/TC_GSA18_1994-2018.csv", sep=";")
    #Data <- Data[Data$YEAR == 1994, ]

    # check_mat_stages(Data, wd, suffix)
  }


check_mat_stages<-function(Data, wd, suffix, DataTargetSpecies=RoME::DataTargetSpecies,DataSpecies=RoME::TM_list,stages=RoME::mat_stages){



  if (!file.exists(paste(wd,"Logfiles",sep="/"))){
    dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
  }

  numberError = 0
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  }
  Errors <- paste(wd,"/Logfiles/Logfile_",suffix,".dat",sep="")

  ResultData = Data
  write(paste(" ----------- check consistency of maturity stages in ",ResultData$TYPE_OF_FILE[1],"-", ResultData$YEAR[1]), file = Errors, append = TRUE)



  cat_fau=DataSpecies
  cat_fau=cat_fau[cat_fau$CATFAU!="",]

  if ((as.character(ResultData$TYPE_OF_FILE[1])=="TC"))    {
  ResultData = ResultData[as.character(ResultData$MATURITY)!="ND",]
  } else if ((as.character(ResultData$TYPE_OF_FILE[1])=="TE")){
  ResultData_ND = ResultData[as.character(ResultData$MATURITY)=="ND",]
  if (nrow(ResultData_ND)!=0){
  write("Warning: in TE the records with maturity ND are not allowed.", file = Errors, append = TRUE)
  }
  }

  if(as.character(ResultData$TYPE_OF_FILE[1])=="TE"){
  stages=stages[stages$MEDITS_STAGE_from_2012!="NDND",]                                       # for TE the maturity stage ND are not allowed
  }

  if (nrow(ResultData)!=0){

  ResultData$species=paste(ResultData$GENUS,ResultData$SPECIES,sep="")

  if( (ResultData$YEAR[1] > 2006) )  {
  ResultData$maturity=ifelse(is.na(ResultData$MATSUB)==FALSE,paste(ResultData$MATURITY,ResultData$MATSUB,sep=""),ResultData$MATURITY)
  }

  i=1
  for (i in 1:nrow(ResultData)){

    cat_fau_one = as.character(cat_fau[cat_fau$MeditsCode==ResultData$species[i],"CATFAU"])

    if (length(cat_fau_one)==1){
      #
      # if( (ResultData$YEAR[i] > 2006) & (ResultData$YEAR[i] < 2012))  {
      # cat_fau_one = ifelse (cat_fau_one=="Ae","S",cat_fau_one)
      #   #ResultData$maturity=ifelse(is.na(ResultData$MATSUB)==FALSE,paste(ResultData$MATURITY,ResultData$MATSUB,sep=""),ResultData$MATURITY)
      #   stages_err = stages[as.character(stages$FAUNISTIC_CATEGORY)==substring(cat_fau_one ,1,1)      & as.character(stages$SEX)== as.character(ResultData$SEX[i])  & as.character(stages$MEDITS_STAGE)== as.character(ResultData$maturity[i]),]
      # }  else

        if ((ResultData$YEAR[i] <= 2006)) {
        #cat_fau_one = ifelse (cat_fau_one=="Ae","S",cat_fau_one)
        stages_err = stages[as.character(stages$FAUNISTIC_CATEGORY)==cat_fau_one & as.character(stages$SEX)== as.character(ResultData$SEX[i]) & as.character(stages$MEDITS_STAGE_up_to_2006)== as.character(ResultData$MATURITY[i]),]
      } else {
        stages_err = stages[as.character(stages$FAUNISTIC_CATEGORY)==cat_fau_one & as.character(stages$SEX)== as.character(ResultData$SEX[i])  & as.character(stages$MEDITS_STAGE_from_2012)== as.character(ResultData$maturity[i]),]
      }

      if (nrow(stages_err) == 0
          #particular case of NEPRNOR and sex 0 for males crustaceans before 2006
          |  ( (cat_fau_one == "B") & (ResultData$species[i] != "NEPRNOR") & (as.character(ResultData$SEX[i]) == "F") & (ResultData$MATURITY[i] == 3)) )

      {

      if ( (cat_fau_one == "B") & (as.character(ResultData$SEX[i]) == "M")  & ( (ResultData$MATURITY[i] == 0) | is.na(ResultData$MATURITY[i]) ) & (ResultData$YEAR[i]> 2006))     {
        if (is.na(ResultData$MATSUB[i])){
          write(paste("Warning: Haul",ResultData$HAUL_NUMBER[i],ResultData$species[i], ResultData$SEX[i] ,
                      ResultData$LENGTH_CLASS[i], ResultData$MATURITY[i],"FAUNISTIC_CATEGORY, SEX and MATURITY inconsistent according to MEDITS INSTRUCTIONS MANUAL in", ResultData$TYPE_OF_FILE[1]), file = Errors, append = TRUE)  } else
                      {
                        write(paste("Haul",ResultData$HAUL_NUMBER[i],ResultData$species[i], ResultData$SEX[i] ,
                                    ResultData$LENGTH_CLASS[i], ResultData$MATURITY[i],ResultData$MATSUB[i],"FAUNISTIC_CATEGORY, SEX and MATURITY inconsistent according to MEDITS INSTRUCTIONS MANUAL in", ResultData$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
                                    }

      }

       if (is.na(ResultData$MATSUB[i])){
          write(paste("Warning: Haul",ResultData$HAUL_NUMBER[i],ResultData$species[i], ResultData$SEX[i] ,
                      ResultData$LENGTH_CLASS[i], ResultData$MATURITY[i],"FAUNISTIC_CATEGORY, SEX and MATURITY inconsistent according to MEDITS INSTRUCTIONS MANUAL in", ResultData$TYPE_OF_FILE[1]), file = Errors, append = TRUE)  } else
                      {
                        write(paste("Haul",ResultData$HAUL_NUMBER[i],ResultData$species[i], ResultData$SEX[i] ,
                                    ResultData$LENGTH_CLASS[i], ResultData$MATURITY[i],ResultData$MATSUB[i],"FAUNISTIC_CATEGORY, SEX and MATURITY inconsistent according to MEDITS INSTRUCTIONS MANUAL in", ResultData$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
                      }


      }

    } else {
      write(paste(ResultData$species[i], ": in MATURITY_STAGES table (Tables directory) is not set the faunistic category. For checking the maturity stages, fill in the field FAUNISTIC_CATEGORY in that table."), file = Errors, append = TRUE)
    }     # ciclo catfau non nullo
  }            # ciclo for

  }

  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  }

  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }


}
