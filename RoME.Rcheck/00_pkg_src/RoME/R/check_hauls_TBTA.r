############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.eu #
#   March 2020                                                                                                             #
############################################################################################################################
# Check if all the hauls in TB are in TA

check_hauls_TBTA<-function(DataTA,DataTB,wd,suffix){

  if (FALSE){
    library(MEDITS)
    wd <- tempdir()
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
    DataTA <- read.csv("~/GitHub/RoME/data/TA_GSA18_1994-2018.csv", sep=";") # DataTA[DataTA$YEAR == 2018, ]
    DataTA <- DataTA[DataTA$YEAR ==2018 , ]
    DataTB <- read.csv("~/GitHub/RoME/data/TB_GSA18_1994-2018.csv", sep=";") # DataTB[DataTB$YEAR == 2018, ]
    DataTB <- DataTB[DataTB$YEAR ==2018 , ]
    # check_hauls_TBTA(DataTA,DataTB,wd,suffix)
  }


  if (!file.exists(paste(wd,"Logfiles",sep="/"))){
    dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
  }

  numberError = 0
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  }
  Errors <- paste(wd,"/Logfiles/Logfile_",suffix,".dat",sep="")

  ResultTB = DataTB
  write(paste("\n----------- check presence in TA of TB hauls - ", ResultTB$YEAR[1]), file = Errors, append = TRUE)
  ResultTA = DataTA


  ResultTB=unique(ResultTB$HAUL_NUMBER)
  if (length(ResultTB)!=0){
    j=1
    for (j in 1:length(ResultTB)){

      ResultTA_temp=ResultTA[which(ResultTA$HAUL_NUMBER==ResultTB[j]),]
      if (nrow(ResultTA_temp)==0)   {
        write(paste("No haul",ResultTB[j],"in TA"), file = Errors, append = TRUE)
        numberError = numberError +1
      }
    }
  }
  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  }

  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }
}
