############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.eu #
#   March 2020                                                                                                             #
############################################################################################################################
# Check if in TE the total number sampled for weight and for ageing are consistent with individual data in TE

check_nb_TE<- function (DataTE,wd,suffix){

  if (FALSE){
    library(RoME)
    wd <- tempdir()
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
    DataTE = read.csv("~/GitHub/RoME/data/TE_2012-2018 _GSA18.csv", sep=";")
    DataTE = DataTE[DataTE$YEAR == 2012 ,   ]
    # SPECIES=NA
    # SEX=NA
    # AREA=NA
    # a=NA
    # b=NA
    # verbose=FALSE
    # check_nb_TE(DataTE, wd, suffix)
  }

  if (!file.exists(paste(wd,"Logfiles",sep="/"))){
    dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
  }
  if (!file.exists(paste(wd,"Graphs",sep="/"))){
    dir.create(file.path(wd, "Graphs"), showWarnings = FALSE)
  }
  numberError = 0
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  }
  Errors <- paste(wd,"\\Logfiles\\Logfile_",suffix,".dat",sep="")

  TE <- DataTE

  write(paste("\n----------- check consistency of number of individuals sampled for weight and ageing in TE - ",TE$YEAR[1]), file = Errors, append = TRUE)


  # check on NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_OTOLITH
  TE_temp1 = TE[as.character(TE$OTOLITH_SAMPLED) == "Y",]
  TE_temp1 = aggregate(TE_temp1$TYPE_OF_FILE, by= list(TE_temp1$HAUL_NUMBER,
                   TE_temp1$GENUS,TE_temp1$SPECIES, TE_temp1$SEX ,
                   TE_temp1$NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_OTOLITH),FUN="length") #
  colnames(TE_temp1)=c("HAUL_NUMBER","GENUS","SPECIES","SEX","NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_OTOLITH","NB")

  if (nrow(TE_temp1)!=0){
    j=1
    for (j in 1:nrow(TE_temp1)){
      if(TE_temp1$NB[j]!=TE_temp1$NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_OTOLITH[j]){
        write(paste(TE_temp1$HAUL_NUMBER[j],TE_temp1$GENUS[j],TE_temp1$SPECIES[j], TE_temp1$SEX[j],"inconsistent value for the field NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_OTOLITH"), file = Errors, append = TRUE)
        numberError =   numberError +1
      }
    }
  }else {
    write("No otolith sampled", file = Errors, append = TRUE)
  }

  # check on NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_WEIGHT
  TE_temp2 = TE[as.character(TE$INDIVIDUAL_WEIGHT) != "ND",]
  TE_temp2 = aggregate(TE_temp2$TYPE_OF_FILE, by= list(TE_temp2$HAUL_NUMBER,TE_temp2$GENUS,TE_temp2$SPECIES, TE_temp2$SEX,TE_temp2$NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_WEIGHT),FUN="length")
  colnames(TE_temp2)=c("HAUL_NUMBER","GENUS","SPECIES","SEX","NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_WEIGHT","NB")

  if (nrow(TE_temp2)!=0){
    for (j in 1:nrow(TE_temp2)){
      if(TE_temp2$NB[j]!=TE_temp2$NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_WEIGHT[j]){
        write(paste(TE_temp2$HAUL_NUMBER[j],TE_temp2$GENUS[j],TE_temp2$SPECIES[j], TE_temp2$SEX[j],"inconsistent value for the field NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_WEIGHT"), file = Errors, append = TRUE)
        numberError =   numberError +1
      }
    }
  } else {
    write("No weight measured.", file = Errors, append = TRUE)
  }
  # check on NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_AGEING
  TE_temp3 = TE[as.character(TE$OTOLITH_READ) == "Y",]


  if (nrow(TE_temp3)!=0){
    TE_temp3 = aggregate(TE_temp3$TYPE_OF_FILE, by= list(TE_temp3$HAUL_NUMBER,TE_temp3$GENUS,TE_temp3$SPECIES, TE_temp3$SEX,TE_temp3$NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_AGEING),FUN="length")
    colnames(TE_temp3)=c("HAUL_NUMBER","GENUS","SPECIES","SEX","NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_AGEING","NB")

    for (j in 1:nrow(TE_temp3)){
      if(TE_temp3$NB[j]!=TE_temp3$NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_AGEING[j]){
        write(paste(TE_temp3$HAUL_NUMBER[j],TE_temp3$GENUS[j],TE_temp3$SPECIES[j], TE_temp3$SEX[j],"inconsistent value for the field NO_PER_SEX_MEASURED_IN_SUB_SAMPLE_FOR_AGEING"), file = Errors, append = TRUE)
        numberError =   numberError +1
      }
    }
  } else {
    write("No otolith read.", file = Errors, append = TRUE)
  }
  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  }


  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }



}