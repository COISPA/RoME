###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #
#   March 2020
################################################################################
# Check if all the target species in TB are present in TC



if (FALSE){
  ResultTC = read.csv("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/data/TC_GSA18_1994-2018.csv", sep=";")
  ResultTB = read.csv("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/data/TB_GSA18_1994-2018.csv", sep=";")

  wd <- "C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/temp"
  suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//DataTargetSpecies.rda")
  load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//Maturity_parameters.rda")
  load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//TM_list.rda")
  check_species_TBTC(ResultTB,ResultTC,DataTargetSpecies,wd,suffix)
  }




check_species_TBTC<-function(ResultTB,ResultTC,DataSpecies=DataTargetSpecies,wd,suffix){
DataTargetSpecies=RoME::DataTargetSpecies

  Format="from_2012"
  if (!file.exists(paste(wd,"Logfiles",sep="/"))){
    dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
  }

  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  }

  Errors <- paste(wd,"/Logfiles/Logfile_",suffix,".dat",sep="")


  numberError = 0
  write(paste(" ----------- check presence in TC of TB target species - ",ResultTC$YEAR[1]), file = Errors, append = TRUE)

  ResultTB= ResultTB[,which(names(ResultTB)=="YEAR" | names(ResultTB)=="HAUL_NUMBER" | names(ResultTB)=="GENUS" | names(ResultTB)=="SPECIES")]
  ResultTC=ResultTC[,which(names(ResultTC)=="YEAR" | names(ResultTC)=="HAUL_NUMBER" | names(ResultTC)=="GENUS" | names(ResultTC)=="SPECIES")]

  ResultSpecies=DataSpecies #read.csv(file=paste(DataSpecies,".csv",sep=""), sep=";", header=TRUE )

  if (nrow(ResultSpecies)!=0) {

    Start = is.na(ResultSpecies$TARGET_START) == FALSE
    End =  is.na(ResultSpecies$TARGET_END) == FALSE
    Target = ResultSpecies[1,]

    ntarget=1;
    for (l in 1:nrow(ResultSpecies)){

      if (Start[l] == TRUE)
      {
        if (End[l] == TRUE)
        {
          if (
            ( (ResultSpecies$TARGET_START[l]<=ResultTB$YEAR[1]) == TRUE) &
              ( (ResultSpecies$TARGET_END[l]>ResultTB$YEAR[1]) == TRUE) )
          {
            Target[ntarget,]= ResultSpecies[l,]
            ntarget=ntarget+1
          }
        }  else
        {
          if ( (ResultSpecies$TARGET_START[l]<=ResultTB$YEAR[1]) == TRUE)
          {
            Target[ntarget,]= ResultSpecies[l,]
            ntarget=ntarget+1
          }
        }
      }
    }

    if ( (nrow(ResultTB)!=0)){
      for (j in 1:nrow(ResultTB)){
        StrSpecies= paste(ResultTB$GENUS[j], ResultTB$SPECIES[j], sep="" )
        FoundTarget=Target[(Target$SPECIES==StrSpecies),]
        if (nrow(FoundTarget) != 0) {
          FoundInTC=ResultTC[as.character(ResultTC$GENUS)==as.character(ResultTB$GENUS[j]) & as.character(ResultTC$SPECIES)==as.character(ResultTB$SPECIES[j]) & ResultTC$HAUL_NUMBER==ResultTB$HAUL_NUMBER[j],]
          if (nrow(FoundInTC) == 0) {
            write(paste("Warning: Haul",ResultTB$HAUL_NUMBER[j],ResultTB$GENUS[j], ResultTB$SPECIES[j], "not found in TC"), file = Errors, append = TRUE)
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

}
################################################################################
