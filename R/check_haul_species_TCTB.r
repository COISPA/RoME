############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files)                                #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.it #
#   January 2022                                                                                                           #
############################################################################################################################
#  Check if all the species in TC are listed in TB

check_haul_species_TCTB<-function(DataTB,DataTC,year,wd, suffix){

  #library(RODBC)
  if (FALSE){
    #library(MEDITS)
    wd <- tempdir() # "D:\\Documents and Settings\\Utente\\Documenti\\GitHub\\RoME\\temp"
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
    year=2007
    DataTB = RoME::TB # read.csv("~/GitHub/RoME/data/TB_GSA18_1994-2018.csv", sep=";")
    DataTB <- DataTB[-6, ]
    DataTC = RoME::TC # read.csv("~/GitHub/RoME/data/TC_GSA18_1994-2018.csv", sep=";")
    DataTB <- DataTB[DataTB$YEAR == 2007, ]
    DataTC <- DataTC[DataTC$YEAR == 2007, ]

    # check_haul_species_TCTB(DataTB,DataTC, year,wd,suffix)
  }


  if (!dir.exists(file.path(wd, "Logfiles"))){
    dir.create(file.path(wd, "Logfiles"), recursive = TRUE, showWarnings = FALSE)
  }

  numberError = 0
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
  }

  Errors <- file.path(wd,"Logfiles",paste("Logfile_",suffix,".dat",sep=""))
  Critical_Errors <- file.path(wd,paste("Critical_errors_",suffix,".dat",sep=""))


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
  DataTC <- DataTC[DataTC$YEAR == year, ]
  ########################################

  ResultTC = DataTC
  write(paste("\n----------- check presence in TB of TC species - ", ResultTC$YEAR[1]), file = Errors, append = TRUE)

  ResultTB = DataTB #read.csv(paste(DataTB,".csv",sep=""), sep=";", header=TRUE)

  ResultTB= ResultTB[,which(names(ResultTB)=="YEAR" | names(ResultTB)=="HAUL_NUMBER" | names(ResultTB)=="GENUS" | names(ResultTB)=="SPECIES")]


  ResultTC=aggregate(ResultTC$YEAR, by=list(ResultTC$YEAR,ResultTC$HAUL_NUMBER,ResultTC$GENUS,ResultTC$SPECIES),FUN="length")
  colnames(ResultTC)=c("YEAR", "HAUL_NUMBER", "GENUS", "SPECIES","length")

  if ( (nrow(ResultTC)!=0)){
    j=1
    for (j in 1:nrow(ResultTC)){
      StrSpecies= paste(ResultTC$GENUS[j], ResultTC$SPECIES[j], sep="" )
      FoundInTB=ResultTB[as.character(ResultTB$GENUS)==as.character(ResultTC$GENUS[j]) & as.character(ResultTB$SPECIES)==as.character(ResultTC$SPECIES[j]) & ResultTB$HAUL_NUMBER==ResultTC$HAUL_NUMBER[j],]
      if (nrow(FoundInTB) == 0) {
        numberError = numberError+1
        write(paste("Haul",ResultTC$HAUL_NUMBER[j],ResultTC$GENUS[j], ResultTC$SPECIES[j], "not found in TB"), file = Errors, append = TRUE)

        if (!file.exists(Critical_Errors)){
          file.create(Critical_Errors)
        }
        write(paste("Haul",ResultTC$HAUL_NUMBER[j], "YEAR", ResultTC$YEAR[j],ResultTC$GENUS[j], ResultTC$SPECIES[j], "not found in TB"), file = Critical_Errors, append = TRUE)
      }

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
	if (file.exists(file.path(tempdir(), "files R-Sufi"))){
  unlink(file.path(tempdir(),"files R-Sufi"),recursive=T)
    }
  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }


}
