############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.eu #
#   March 2020                                                                                                             #
############################################################################################################################
# Check if weight of the sample in TC is consistent with length-weight relationship

check_individual_weightTC<- function (DataTC,SPECIES=NA,SEX=NA,AREA=NA,a=NA,b=NA,wd,suffix, verbose=FALSE){

  if (FALSE){
    library(RoME)
    wd <- tempdir()
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
    DataTC = MEDITS::TC
    SPECIES=NA
    SEX=NA
    AREA=NA
    a=NA
    b=NA
    verbose=FALSE
    # check_individual_weightTC(DataTC=DataTC, wd=wd, suffix=suffix)
  }

  if (!file.exists(paste(wd,"Logfiles",sep="/"))){
    dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
  }

  numberError = 0
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  }
  Errors <- paste(wd,"\\Logfiles\\Logfile_",suffix,".dat",sep="")


  TC = DataTC

  write(paste("\n----------- check consistency total weight in the haul in TC - ",TC$YEAR[1]), file = Errors, append = TRUE)

  if (is.na(SPECIES) | is.na(SEX) | is.na(AREA) | is.na(a) | is.na(b) ){
    if (verbose){
    message("a and b parameters extracted from LW table")
    }
    LW <- RoME::LW
  } else {
    LW <- data.frame(AREA=AREA, SPECIES=SPECIES, SEX=SEX,a=a,b=b)
  }

  TC$mean_weight = NA
 i=1
  for (i in 1:nrow(TC)){
    ab=LW[(LW$SPECIES== paste(TC$GENUS[i],TC$SPECIES[i])) &(as.character(LW$SEX)==as.character(TC$SEX[i])) & (LW$AREA==TC$AREA[1]),]

    if (nrow(ab)!=0){
      A= ab$a[1]
      B= ab$b[1]
      if(as.character(TC$LENGTH_CLASSES_CODE[i])=="m"){
        mean_length =TC$LENGTH_CLASS[i] +0.5
      } else if (as.character(TC$LENGTH_CLASSES_CODE[i])=="0"){ # step: 0.5 cm
        mean_length =(TC$LENGTH_CLASS[i] +2.5)/10
      } else if (as.character(TC$LENGTH_CLASSES_CODE[i])=="1"){ # step: 1 cm
        mean_length =(TC$LENGTH_CLASS[i] +5)/10
      }
      mean_weight = A*mean_length^B

      TC$mean_weight[i] = mean_weight * TC$NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE[i] # estimated weight
      #TC$mean_individual_weight[i] = mean_weight


    }

  }

  TC_w= aggregate(TC$mean_weight, by=list(TC$HAUL_NUMBER,TC$GENUS, TC$SPECIES,TC$WEIGHT_OF_THE_SAMPLE_MEASURED), FUN="sum")
  colnames(TC_w) = c("HAUL_NUMBER","GENUS", "SPECIES","WEIGHT_OF_THE_SAMPLE_MEASURED","ESTIMATED_WEIGHT")


  TC_w$perc_diff =  (TC_w$WEIGHT_OF_THE_SAMPLE_MEASURED-TC_w$ESTIMATED_WEIGHT)/TC_w$ESTIMATED_WEIGHT*100



  for (i in 1:nrow(TC_w)){
  if (!is.na(TC_w$perc_diff[i]) & abs(TC_w$perc_diff[i])>50){
  numberError=numberError+1
  }
  }

  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  } else {

  write.table(TC_w,file=paste(wd,"/Comparison_estimated_observed_weight_in_TC_",  TC$YEAR[1],".csv",sep=""),sep=";",row.names=F)

    write("For some hauls the difference between estimated and observed total weight is greater than 50%. Please verify in the file Comparison_estimated_observed_weight_in_TC.csv automatically produced in the working directory", file = Errors, append = TRUE)
  }
  return(TRUE)


}
