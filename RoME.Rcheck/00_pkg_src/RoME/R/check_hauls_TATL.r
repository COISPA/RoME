###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #
#   March 2013                                                                                                            #
###########################################################################################################################
# Check if all the hauls in TA are in TB

check_hauls_TATL<-function(DataTA,DataTL,wd,suffix){

  if (FALSE){
    library(RoME)
    wd <- tempdir()
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
    DataTL = read.table("~/GitHub/RoME/data/TL_GSA18 2012-2018.csv", sep=";", header=T)
    DataTA = read.csv("~/GitHub/RoME/data/TA_GSA18_1994-2018.csv", sep=";")

  }

  if (!file.exists(paste(wd,"Logfiles",sep="/"))){
    dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
  }

  numberError = 0
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  }
  Errors <- paste(wd,"\\Logfiles\\Logfile_",suffix,".dat",sep="")

  #### CHECK TL FIELDS ####
  {
    if ("LITTER_SUB.CATEGORY" %in% colnames(DataTL)){
      colnames(DataTL)[which(colnames(DataTL)=="LITTER_SUB.CATEGORY")] <- "LITTER_SUB-CATEGORY"
    }
    if ("TOTAL_WEIGHT_IN_ THE_SUB.CATEGORY_ HAUL" %in% colnames(DataTL)){
      colnames(DataTL)[which(colnames(DataTL)=="TOTAL_WEIGHT_IN_ THE_SUB.CATEGORY_ HAUL")] <- "TOTAL_WEIGHT_IN_ THE_SUB-CATEGORY_ HAUL"
    }
    if ("TOTAL_NUMBER_IN_ THE_SUB.CATEGORY_ HAUL" %in% colnames(DataTL)){
      colnames(DataTL)[which(colnames(DataTL)=="TOTAL_NUMBER_IN_ THE_SUB.CATEGORY_ HAUL")] <- "TOTAL_NUMBER_IN_ THE_SUB-CATEGORY_ HAUL"
    }
  }
  #### CHECK TL FIELDS - END ####

  ResultTA = DataTA
  write(paste("\n----------- check presence in TL of TA hauls - ",ResultTA$YEAR[1]), file = Errors, append = TRUE)

  ResultTA=ResultTA[,which(names(ResultTA)=="HAUL_NUMBER" | names(ResultTA)=="VALIDITY")]
  ResultTA=ResultTA[ResultTA$VALIDITY=="V",]
  ResultTL = DataTL

  if (nrow(ResultTA)!=0){
    j=1
    for (j in 1:nrow(ResultTA)){
      ResultTL_temp=ResultTL[which(ResultTL$HAUL_NUMBER==ResultTA$HAUL_NUMBER[j]),]
      if (nrow(ResultTL_temp)==0)   {
        write(paste("No haul",ResultTA$HAUL_NUMBER[j],"in TL"), file = Errors, append = TRUE)
        numberError = numberError +1
      }
    }
  }

  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  }

 # if (numberError ==0) {
    return(TRUE)
  #} else { return(FALSE) }

}