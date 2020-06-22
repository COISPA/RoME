###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.eu                #
#   March 2020                                                                                                            #
###########################################################################################################################
# Check if all the species codes are correct according to INSTRUCTION MANUAL VERSION 9 MEDITS 2017

if (FALSE){
  # ResultData = read.csv("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/data/TB_GSA18_1994-2018.csv", sep=";")
  ResultData = read.table(file=paste(wd, "\\2019 GSA18 TC.csv",sep=""), sep=";", header=T) # read.csv("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/data/TC_GSA18_1994-2018.csv", sep=";")
  ResultData$SPECIES[28]  <- "SUP"
  wd <- "C:\\Users\\walte\\Documents\\GitHub\\RoME\\data TEST Neglia" # "C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/temp"
  suffix= NA # paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
load("C:\\Users\\Bitetto Isabella\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\Rome\\RoME\\RoME\\data\\TM_list.rda")
}


check_rubincode<-function(ResultData,TM_list=TM_list,wd,suffix){
  numberError = 0

  Format="from_2012"
  if (!file.exists(file.path(wd, "Logfiles"))){
    dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
  }
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  }
  numberError = 0
  Errors <- file.path(wd,"Logfiles",paste("Logfile_",suffix,".dat",sep=""))

  if (ResultData$TYPE_OF_FILE[1] == "TB") {
     write(paste("\n----------- check correctness of species codes in TB - ", ResultData$YEAR[1]), file = Errors, append = TRUE)
    Result=ResultData[,which(names(ResultData)=="TYPE_OF_FILE" | names(ResultData)=="HAUL_NUMBER" | names(ResultData)=="GENUS" | names(ResultData)=="SPECIES" | names(ResultData)=="FAUNISTIC_CATEGORY")]
  }    else # TC-TE
  { if (Format=="before_2012"){     # old format of TC (without faunistic category)
     write(paste("\n----------- check correctness of species codes in - ", ResultData$TYPE_OF_FILE[1], "-",ResultData$YEAR[1]), file = Errors, append = TRUE)
    Result=ResultData[,which(names(ResultData)=="TYPE_OF_FILE" | names(ResultData)=="HAUL_NUMBER" | names(ResultData)=="GENUS" | names(ResultData)=="SPECIES")]
    } else {      # new format of TC and TE (with faunistic category)
     write(paste("\n----------- check correctness of species codes in - ", ResultData$TYPE_OF_FILE[1],"-",ResultData$YEAR[1]), file = Errors, append = TRUE)
    Result=ResultData[,which(names(ResultData)=="TYPE_OF_FILE" | names(ResultData)=="HAUL_NUMBER" | names(ResultData)=="GENUS" | names(ResultData)=="SPECIES"| names(ResultData)=="FAUNISTIC_CATEGORY")]

    }
  }

  # data species
  ResultSpecies = TM_list
  ResultSpecies=ResultSpecies[,c(which(names(ResultSpecies)=="MeditsCode"),which(names(ResultSpecies)=="CATFAU"))]

  if (nrow(ResultData)!=0){
    j=28
    for (j in 1:nrow(ResultData)){
      if ((ResultData$TYPE_OF_FILE[1]=="TB") | (Format=="from_2012" & !(ResultData$TYPE_OF_FILE[1] %in% c("TC","TE"))))
      {
        Found=ResultSpecies[
          (
            as.character(ResultSpecies$MeditsCode)==paste(as.character(ResultData$GENUS[j]),as.character(ResultData$SPECIES[j]),sep="") &
              as.character(ResultSpecies$CATFAU)==as.character(ResultData$FAUNISTIC_CATEGORY[j])
          )
          ,]
        if (nrow(Found)==0)   {
          FoundSpecies=ResultSpecies[as.character(ResultSpecies$MeditsCode)==paste(as.character(ResultData$GENUS[j]),as.character(ResultData$SPECIES[j]),sep=""),]
          if (nrow(FoundSpecies)==0)   {
            write(paste("Warning: Haul",ResultData$HAUL_NUMBER[j],": code species", ResultData$GENUS[j] , ResultData$SPECIES[j] ," not present in MEDITS TM list in Tables directory"), file = Errors, append = TRUE)
          } else {
            write(paste("Warning: Haul",ResultData$HAUL_NUMBER[j],": species", ResultData$GENUS[j] , ResultData$SPECIES[j] ," wrong FAUNISTIC_CATEGORY according to MEDITS TM list in Tables directory"), file = Errors, append = TRUE)
          }

        }
      }

      if (ResultData$TYPE_OF_FILE[1]=="TC") # type of file = TC
      {
        FoundSpecies=ResultSpecies[as.character(ResultSpecies$MeditsCode)==paste(as.character(ResultData$GENUS[j]),as.character(ResultData$SPECIES[j]),sep=""),]
        if (nrow(FoundSpecies)==0)   {
          write(paste("Haul",ResultData$HAUL_NUMBER[j],": code species", as.character(ResultData$GENUS[j]) , as.character(ResultData$SPECIES[j]) ," not present in MEDITS TM list in", as.character(ResultData$TYPE_OF_FILE[j])), file = Errors, append = TRUE)
          numberError = numberError +1
        }

      }

      if (ResultData$TYPE_OF_FILE[1]=="TE") # type of file = TE
      {
        FoundSpecies=ResultSpecies[as.character(ResultSpecies$MeditsCode)==paste(as.character(ResultData$GENUS[j]),as.character(ResultData$SPECIES[j]),sep=""),]
        if (nrow(FoundSpecies)==0)   {
          write(paste("Haul",ResultData$HAUL_NUMBER[j],": code species", as.character(ResultData$GENUS[j]) , as.character(ResultData$SPECIES[j]) ," not present in MEDITS TM list in", as.character(ResultData$TYPE_OF_FILE[j])), file = Errors, append = TRUE)
          numberError = numberError +1
        }

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
