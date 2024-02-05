############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files)                                #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.it #
#   January 2022                                                                                                           #
############################################################################################################################
# Check if weight of the sample in TC is consistent with length-weight relationship

check_individual_weightTC<- function (DataTC,LW=NA,year,wd,suffix, verbose=FALSE){

  if (FALSE){
    library(RoME)
    wd <- tempdir()
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
    DataTC = RoME::TC
    year=2007
    verbose=TRUE
    # check_individual_weightTC(DataTC=DataTC,year=2007, wd=wd, suffix=suffix, verbose=TRUE)
  }


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
  DataTC <- DataTC[DataTC$YEAR == year, ]
  ########################################

  TC = DataTC

  write(paste("\n----------- check consistency total weight in the haul in TC - ",TC$YEAR[1]), file = Errors, append = TRUE)

  if (class(LW) != "data.frame"){
    if (is.na(LW)){
    if (verbose){
    message("a and b parameters extracted from RoMEBS LW table")
    }
    LW <- RoME::LW
    }
  }

  TC$mean_weight = NA
 i=1
  for (i in 1:nrow(TC)){

    if (!unique(TC$AREA) %in% unique(LW$AREA) ) {
      ab=LW[(LW$SPECIES== paste(TC$GENUS[i],TC$SPECIES[i])) &(as.character(LW$SEX)==as.character(TC$SEX[i])) & (LW$AREA==18),]
      # write(paste("Warning: a e b parameters not available for GSA ",unique(TC$AREA),". GSA18 values were used for the analysis."), file = Errors, append = TRUE)
    } else {
      ab=LW[(LW$SPECIES== paste(TC$GENUS[i],TC$SPECIES[i])) &(as.character(LW$SEX)==as.character(TC$SEX[i])) & (LW$AREA==TC$AREA[1]),]
    }


    if (nrow(ab)!=0){
      A= ab$a[1]
      B= ab$b[1]
      if (!TC$LENGTH_CLASSES_CODE[i] %in% c("m","0","1")) {
        write(paste("LENGTH_CLASSES_CODE value '",TC$LENGTH_CLASSES_CODE[i],"' not expected in HAUL ",TC$HAUL_NUMBER[i],", ",TC$YEAR[i]), file = Errors, append = TRUE)
        numberError=numberError+1
      } else if(as.character(TC$LENGTH_CLASSES_CODE[i])=="m"){
        mean_length =TC$LENGTH_CLASS[i] +0.5
        mean_weight = A*mean_length^B
        TC$mean_weight[i] = mean_weight * TC$NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE[i]
      } else if (as.character(TC$LENGTH_CLASSES_CODE[i])=="0"){ # step: 0.5 cm
        mean_length =(TC$LENGTH_CLASS[i] +2.5)/10
        mean_weight = A*mean_length^B
        TC$mean_weight[i] = mean_weight * TC$NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE[i]
      } else if (as.character(TC$LENGTH_CLASSES_CODE[i])=="1"){ # step: 1 cm
        mean_length =(TC$LENGTH_CLASS[i] +5)/10
        mean_weight = A*mean_length^B
      TC$mean_weight[i] = mean_weight * TC$NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE[i] # estimated weight
      }
    }
  }

  TC_w= aggregate(TC$mean_weight, by=list(TC$HAUL_NUMBER,TC$GENUS, TC$SPECIES,TC$WEIGHT_OF_THE_SAMPLE_MEASURED), FUN="sum")
  colnames(TC_w) = c("HAUL_NUMBER","GENUS", "SPECIES","WEIGHT_OF_THE_SAMPLE_MEASURED","ESTIMATED_WEIGHT")


  TC_w$perc_diff =  (TC_w$WEIGHT_OF_THE_SAMPLE_MEASURED-TC_w$ESTIMATED_WEIGHT)/TC_w$ESTIMATED_WEIGHT*100



  for (i in 1:nrow(TC_w)){
  if (!is.na(TC_w$perc_diff[i]) & abs(TC_w$perc_diff[i])>50){
  # numberError=numberError+1
  }
  }

  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  } else {
  filename <- paste("Comparison_estimated_observed_weight_in_TC_",  TC$YEAR[1],".csv",sep="")
  write.table(TC_w,file=file.path(wd,filename),sep=";",row.names=F)

    write("For some hauls the difference between estimated and observed total weight is greater than 50%. Please verify in the file Comparison_estimated_observed_weight_in_TC.csv automatically produced in the working directory", file = Errors, append = TRUE)
  }
#    if (file.exists(file.path(tempdir(), "Logfiles"))){
#   unlink(file.path(tempdir(),"Logfiles"),recursive=T)
#   }
#   if (file.exists(file.path(tempdir(), "Graphs"))){
#   unlink(file.path(tempdir(),"Graphs"),recursive=T)
#     }
# 	if (file.exists(file.path(tempdir(), "files R-Sufi"))){
#   unlink(file.path(tempdir(),"files R-Sufi"),recursive=T)
# 	}
#
#
#     unlink(file.path(tempdir(),list.files(file.path(tempdir()))),recursive=T)


  return(TRUE)


}
