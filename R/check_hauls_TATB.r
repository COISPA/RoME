############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files)                                #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.it #
#   January 2022                                                                                                           #
############################################################################################################################
# Check if all the hauls in TA are in TB

check_hauls_TATB<-function(DataTA,DataTB,year,wd,suffix){

  if (FALSE){
    #library(MEDITS)
    wd <- tempdir()
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
    DataTA <- RoME::TA # read.csv("~/GitHub/RoME/data/TA_GSA18_1994-2018.csv", sep=";") # DataTA[DataTA$YEAR == 2018, ]
    DataTB <- RoME::TB # read.csv("~/GitHub/RoME/data/TB_GSA18_1994-2018.csv", sep=";") # DataTB[DataTB$YEAR == 2018, ]
    year=2007
    # check_hauls_TATB(DataTA,DataTB,year,wd,suffix)
  }


  if (!file.exists(file.path(wd, "Logfiles"))){
    dir.create(file.path(wd, "Logfiles"), recursive = TRUE, showWarnings = FALSE)
  }
  numberError = 0
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
  }
  Errors <- file.path(wd,"Logfiles",paste("Logfile_",suffix,".dat",sep=""))
  if (!file.exists(Errors)){
    file.create(Errors)
  }

  ### FILTERING DATA FOR THE SELECTED YEAR
  arg <- "year"
  if (!exists(arg)) {
    stop(paste0("'",arg,"' argument should be provided"))
  } else if (length(year)!= 1) {
    stop(paste0("only one value should be provided for '",arg,"' argument"))
  } else if (is.na(year)){
    stop(paste0(arg," argument should be a numeric value"))
  }
  DataTA <- DataTA[DataTA$YEAR == year, ]
  DataTB <- DataTB[DataTB$YEAR == year, ]
  ########################################

  ResultTA = DataTA
  write(paste("\n----------- check presence in TB of TA hauls - ",ResultTA$YEAR[1]), file = Errors, append = TRUE)

  ResultTA=ResultTA[,which(names(ResultTA)=="HAUL_NUMBER" | names(ResultTA)=="VALIDITY")]
  ResultTA=ResultTA[ResultTA$VALIDITY=="V",]
  ResultTB = DataTB

  if (nrow(ResultTA)!=0){
    j=2
    for (j in 1:nrow(ResultTA)){

      ResultTB_temp=ResultTB[which(ResultTB$HAUL_NUMBER==ResultTA$HAUL_NUMBER[j]),]
      if (nrow(ResultTB_temp)==0)   {
        write(paste("No haul",ResultTA$HAUL_NUMBER[j],"in TB"), file = Errors, append = TRUE)
        numberError = numberError +1
      }
    }
  }

  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  }
#    if (file.exists(file.path(tempdir(), "Logfiles"))){
#   unlink(file.path(tempdir(),"Logfiles"),recursive=T)
#   }
#   if (file.exists(file.path(tempdir(), "Graphs"))){
#   unlink(file.path(tempdir(),"Graphs"),recursive=T)
#     }
# 	if (file.exists(file.path(tempdir(), "files R-Sufi"))){
#   unlink(file.path(tempdir(),"files R-Sufi"),recursive=T)
#     }
  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }

}
