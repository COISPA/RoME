###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.eu                #
#   March 2020                                                                                                            #
###########################################################################################################################

if (FALSE){
  ResultDataTB = read.csv("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/data/TB_GSA18_1994-2018.csv", sep=";")
  ResultDataTC = read.csv("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/data/TC_GSA18_1994-2018.csv", sep=";")

  wd <- "C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/temp"
  suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")

}

# Check if all the species codes are correct according to INSTRUCTION MANUAL VERSION 9 MEDITS 2017

check_rubincode<-function(ResultData){
  numberError = 0

  if (ResultData$TYPE_OF_FILE[1] == "TB") {
     write(paste("
                ----------- check correctness of species codes in TB - ", ResultData$YEAR[1]), file = Errors, append = TRUE)
    Result=ResultData[,which(names(ResultData)=="TYPE_OF_FILE" | names(ResultData)=="HAUL_NUMBER" | names(ResultData)=="GENUS" | names(ResultData)=="SPECIES" | names(ResultData)=="FAUNISTIC_CATEGORY")]
  }    else # TC-TE
  { if (Format=="before_2012"){     # old format of TC (without faunistic category)
    write(paste(
                "----------- check correctness of species codes in - ", ResultData$TYPE_OF_FILE[1], "-",ResultData$YEAR[1]), file = Errors, append = TRUE)
    Result=ResultData[,which(names(ResultData)=="TYPE_OF_FILE" | names(ResultData)=="HAUL_NUMBER" | names(ResultData)=="GENUS" | names(ResultData)=="SPECIES")]
    } else {      # new format of TC and TE (with faunistic category)
    write(paste(
                "----------- check correctness of species codes in - ", ResultData$TYPE_OF_FILE[1],"-",ResultData$YEAR[1]), file = Errors, append = TRUE)
    Result=ResultData[,which(names(ResultData)=="TYPE_OF_FILE" | names(ResultData)=="HAUL_NUMBER" | names(ResultData)=="GENUS" | names(ResultData)=="SPECIES"| names(ResultData)=="FAUNISTIC_CATEGORY")]

    }
  }


  # data species
  ResultSpecies = read.csv(paste(DataSpecies,".csv",sep=""), sep=";", header=TRUE)
  ResultSpecies=ResultSpecies[,c(which(names(ResultSpecies)=="MeditsCode"),which(names(ResultSpecies)=="CATFAU"))]

  if (nrow(ResultData)!=0){
    for (j in 1:nrow(ResultData)){
      if ((ResultData$TYPE_OF_FILE[1]=="TB")  |(Format=="from_2012"))
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
            write(paste("Warning: Haul",ResultData$HAUL_NUMBER[j],": code species", ResultData$GENUS[j] , ResultData$SPECIES[j] ," not present in MEDITS FM list in Tables directory"), file = Errors, append = TRUE)
          } else {
            write(paste("Warning: Haul",ResultData$HAUL_NUMBER[j],": species", ResultData$GENUS[j] , ResultData$SPECIES[j] ," wrong FAUNISTIC_CATEGORY according to MEDITS FM list in Tables directory"), file = Errors, append = TRUE)
          }

        }
      }  else if (ResultData$TYPE_OF_FILE[1]=="TC") # type of file = TC
      {
        FoundSpecies=ResultSpecies[as.character(ResultSpecies$MeditsCode)==paste(as.character(ResultData$GENUS[j]),as.character(ResultData$SPECIES[j]),sep=""),]
        if (nrow(FoundSpecies)==0)   {
          write(paste("Warning: Haul",ResultData$HAUL_NUMBER[j],": code species", as.character(ResultData$GENUS[j]) , as.character(ResultData$SPECIES[j]) ," not present in MEDITS FM list in", as.character(ResultData$TYPE_OF_FILE[j])), file = Errors, append = TRUE)
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

################################################################################
