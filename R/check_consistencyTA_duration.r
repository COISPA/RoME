############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files)                                #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.it #
#   January 2022                                                                                                           #
############################################################################################################################
# Check consistency among duration, start time and end time of the haul in TA

check_consistencyTA_duration<-function(DataTA, year, wd, suffix){

  if (FALSE){
    library(RoME)
    wd <- tempdir()
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
    DataTA = RoME::TA # read.csv("~/GitHub/RoME/data/TA_GSA18_1994-2018.csv", sep=";") 
    # check_consistencyTA_duration(DataTA, year=2012, wd, suffix)
  }

  if (!file.exists(file.path(wd, "Logfiles"))){
    dir.create(file.path(wd, "Logfiles"), recursive = TRUE, showWarnings = FALSE)
  }

  numberError = 0
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
  }
  Errors <- file.path(wd,"Logfiles",paste("Logfile_", suffix ,".dat",sep=""))
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
  ########################################

  Matrix = DataTA
  write(paste("\n----------- check consistency between duration and time TA - ", Matrix$YEAR[1]), file = Errors, append = TRUE)

  Matrix=Matrix[Matrix$VALIDITY=="V",]
  Matrix$Start=timeDate("01-01-2001.0000", format = "%d-%m-%Y.%H%M", zone = "GMT", FinCenter = "GMT")
  i=1
  for (i in 1:nrow(Matrix)){
    Matrix$Start[i]= timeDate(paste(paste(ifelse(str_length(Matrix$DAY[i])==1,paste("0",Matrix$DAY[i],sep=""),Matrix$DAY[i]),ifelse(str_length(Matrix$MONTH[i])==1,paste("0",Matrix$MONTH[i],sep=""),Matrix$MONTH[i]),Matrix$YEAR[i],sep="-"),ifelse(str_length(Matrix$SHOOTING_TIME[i])==3,paste("0",Matrix$SHOOTING_TIME[i],sep=""),Matrix$SHOOTING_TIME[i]),sep="."), format = "%d-%m-%Y.%H%M", zone = "GMT", FinCenter = "GMT")
  }
  Matrix$End=timeDate("01-01-2001.0000", format = "%d-%m-%Y.%H%M", zone = "GMT", FinCenter = "GMT")
  for (i in 1:nrow(Matrix)){
    Matrix$End[i]= timeDate(paste(paste(ifelse(str_length(Matrix$DAY[i])==1,paste("0",Matrix$DAY[i],sep=""),Matrix$DAY[i]),ifelse(str_length(Matrix$MONTH[i])==1,paste("0",Matrix$MONTH[i],sep=""),Matrix$MONTH[i]),Matrix$YEAR[i],sep="-"),ifelse(str_length(Matrix$HAULING_TIME[i])==3,paste("0",Matrix$HAULING_TIME[i],sep=""),Matrix$HAULING_TIME[i]),sep="."), format = "%d-%m-%Y.%H%M", zone = "GMT", FinCenter = "GMT")
  }
  Matrix$difference=difftimeDate(Matrix$End,Matrix$Start, units="mins")

    j=1
  for (j in nrow(Matrix))  {
    if (is.na(str_extract(Matrix$difference[j],"[0-9]{3,}?"))){
      Matrix$difference[j]=str_extract(Matrix$difference[j],"[0-9]{2,}?")  } else {
        Matrix$difference[j]=str_extract(Matrix$difference[j],"[0-9]{3,}?")
      }

  }
  # Matrix$difference=str_extract(Matrix$difference,"[0-9]{2,}?")
  duration=which((Matrix$difference==Matrix$HAUL_DURATION)==FALSE)
  if (length(duration)!=0){
    for (j in 1:length(duration)){
      write(paste("Haul ",Matrix$HAUL_NUMBER[duration[j]],"inconsistency between SHOOTING-HAULING_TIME and HAUL_DURATION in", Matrix$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
      numberError = numberError +1
    }
  }

  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  }
    if (file.exists(file.path(tempdir(), "Logfiles"))){
  unlink(file.path(tempdir(),"Logfiles"),recursive=T)
  }
  if (file.exists(file.path(tempdir(), "Graphs"))){
  unlink(file.path(tempdir(),"Graphs"),recursive=T)
    }
	if (file.exists(file.path(tempdir(), "Graphs"))){
  unlink(file.path(tempdir(),"files R-Sufi"),recursive=T)
    }
  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }
}
