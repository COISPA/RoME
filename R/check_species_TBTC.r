############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files)                                #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.it #
#   January 2022                                                                                                           #
############################################################################################################################


# Check if all the target species in TB are present in TC



if (FALSE){
  ResultTC = RoME::TC
  ResultTB = RoME::TB
  year=2007
  wd <- tempdir()
  DataSpecies=RoME::DataTargetSpecies
  suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
  # load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//DataTargetSpecies.rda")
  # load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//Maturity_parameters.rda")
  # load("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME//RoME//data//TM_list.rda")
  check_species_TBTC(ResultTB,ResultTC,year,DataSpecies,wd,suffix)
  }

check_species_TBTC<-function(ResultTB,ResultTC,year,DataSpecies=RoME::DataTargetSpecies ,wd,suffix){


  Format="from_2012"

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
  ResultTB <- ResultTB[ResultTB$YEAR == year, ]
  ResultTC <- ResultTC[ResultTC$YEAR == year, ]
  ########################################

  write(paste("\n----------- check presence in TC of TB target species - ",ResultTC$YEAR[1]), file = Errors, append = TRUE)

  ResultTB= ResultTB[,which(names(ResultTB)=="YEAR" | names(ResultTB)=="HAUL_NUMBER" | names(ResultTB)=="GENUS" | names(ResultTB)=="SPECIES")]
  ResultTC= ResultTC[,which(names(ResultTC)=="YEAR" | names(ResultTC)=="HAUL_NUMBER" | names(ResultTC)=="GENUS" | names(ResultTC)=="SPECIES")]

  ResultSpecies=DataSpecies

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
#      if (file.exists(file.path(tempdir(), "Logfiles"))){
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

}
